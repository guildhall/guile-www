;;; (www mime)

;; Copyright (C) 2013 Thien-Thi Nguyen
;;
;; This file is part of Guile-WWW.
;;
;; Guile-WWW is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; Guile-WWW is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Guile-WWW; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA  02110-1301  USA

;;; Commentary:

;; This module is "internal": distributed, installed,
;; but not documented (at least, for now).
;;
;; It contains procedures to parse MIME type and multipart body.

;;; Code:

(define-module (www mime)
  #:export (p-ref
            parse-type
            parse-multipart)
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-n
                                               put-bytevector))
  #:use-module ((srfi srfi-4) #:select (make-u8vector
                                        u8vector
                                        u8vector?
                                        u8vector-set!
                                        u8vector-ref))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (string-trim-right
                                         string-index
                                         substring/shared))
  #:use-module ((srfi srfi-14) #:select (char-set-difference
                                         char-set-intersection
                                         char-set:ascii
                                         char-set:iso-control
                                         char-set:upper-case
                                         string->char-set
                                         char-set->string))
  #:use-module ((ice-9 regex) #:select (match:end
                                        match:substring))
  #:use-module ((www crlf) #:select (read-headers
                                     hsym-proc)))

;; Look in the alist (@sc{cdr}) portion of @var{form}
;; and return the value associated w/ @var{parameter} (a symbol),
;; or @code{#f} if not found.
;;
(define (p-ref form parameter)
  (assq-ref (cdr form) parameter))

(define (character-class-rx x)
  (make-regexp
   (string-append "[" (if (string? x)
                          x
                          (char-set->string x))
                  "]"
                  ;; NB: One or more; required!
                  "+")))

;; Naming convention: %FOO means regular expression FOO.

(define %TOKEN (character-class-rx
                (char-set-difference char-set:ascii
                                     char-set:iso-control
                                     (string->char-set
                                      " ()<>@,;:\\\"/[]?="))))

(define %SKIP-BEF (character-class-rx "; "))
(define %SKIP-MID (make-regexp " *= *"))

(define (downcase-if-necessary string)
  (if (string-index string char-set:upper-case)
      (string-downcase string)
      string))

(define (read-parameter start s)

  (define (sub beg . end)
    (apply substring/shared s beg end))

  (define (m! rx start)
    (regexp-exec rx s start))

  (define (quoted-string-value v-beg)
    (let ((port (open-input-string s)))
      (seek port v-beg SEEK_SET)
      ;; FIXME: We use ‘let*’ to enforce eval order.
      ;;        Is there a better way?
      (let* ((v (read port))
             (v-end (ftell port)))
        (values v v-end))))

  (define (token-value v-beg)
    (let ((m (m! %TOKEN v-beg)))
      (values (match:substring m 0)
              (match:end m))))

  (let* ((n-beg (match:end (m! %SKIP-BEF start)))
         (n-end (match:end (m! %TOKEN n-beg)))
         (v-beg (match:end (m! %SKIP-MID n-end))))
    (let-values (((v v-end) ((if (char=? #\" (string-ref s v-beg))
                                 quoted-string-value
                                 token-value)
                             v-beg)))
      (values
       (cons (string->symbol (downcase-if-necessary
                              (sub n-beg n-end)))
             v)
       v-end))))

(define (parse-parameters s)
  (set! s (string-trim-right s))
  (let ((len (string-length s)))
    (let loop ((start 0) (acc '()))
      (if (= len start)
          (reverse! acc)                ; rv
          (let-values (((p p-end) (read-parameter start s)))
            (loop p-end (cons p acc)))))))

(define (interesting buf boundary)
  (set! boundary (string-append "--" boundary))
  (let* ((buf-lim (u8vector-length buf))
         (boundary-len (string-length boundary))
         (etc (apply u8vector (map char->integer (string->list boundary))))
         (beg (u8vector-ref etc 0))
         (end (u8vector-ref etc (1- boundary-len))))

    (define (buf-ref i)
      (u8vector-ref buf i))

    ;; TODO: Use Boyer-Moore (write if necessary):
    ;; <http://en.wikipedia.org/wiki/Boyer-Moore_string_search_algorithm>.

    (define (find-beg i)
      (and (< i buf-lim)
           (if (= beg (buf-ref i))
               i
               (find-beg (1+ i)))))

    (define (rest-all-same i)
      (let loop ((idx (1- boundary-len)))
        (or (zero? idx)
            (let ((bx (+ i idx)))
              (and (< bx buf-lim)
                   (= (buf-ref bx)
                      (u8vector-ref etc idx))
                   (loop (1- idx)))))))

    (let loop ((acc (list 0)) (i 0))
      (cond ((find-beg i)
             => (lambda (at)
                  (if (rest-all-same at)
                      (let ((h-beg (max 0 (- at 2)))
                            (h-end (+ 2 at boundary-len)))
                        (loop (cons* h-end (cons (car acc)
                                                 h-beg)
                                     (cdr acc))
                              h-end))
                      (loop acc (1+ at)))))
            (else
             ;; rv
             (cdr (reverse! (cdr acc))))))))

(define (default-Content-Type)
  (copy-tree
   '((text . plain)
     ;; Hmm, is this correct?
     (charset . "ISO-8859-1"))))

(define (token-from s pos)
  (regexp-exec %TOKEN s pos))

(define (sym m)
  (string->symbol (downcase-if-necessary (match:substring m 0))))

(define (parms s m)
  (parse-parameters (substring/shared s (match:end m))))

;; Parse string @var{s} and return a form
;;
;; @example
;; ((MAJOR . MINOR) [PARAMETER...])
;; @end example
;;
;; Both @var{major} and @var{minor} are downcased symbols
;; (e.g., @code{(text . plain)} to represent @code{TEXT/Plain}).
;; Each @var{parameter} is a pair with @sc{car} a downcased
;; symbol and @sc{cdr} a string.
;;
(define (parse-type s)
  (let* ((m-top (token-from s 0))
         (m-sub (token-from s (1+ (match:end m-top)))))
    (acons (sym m-top)
           (sym m-sub)
           (parms s m-sub))))

(define parse-headers
  (let ((norm (hsym-proc string-titlecase)))
    ;; parse-headers
    (lambda (port)
      (let ((all (read-headers port norm)))

        (define (have header)
          (assq header all))

        ;; Decode/default various headers.
        (cond ((have 'Content-Type)
               => (lambda (pair)
                    (set-cdr! pair (parse-type (cdr pair)))))
              (else
               (set! all (acons 'Content-Type
                                (default-Content-Type)
                                all))))
        (and=> (have 'Content-Disposition)
               (lambda (pair)
                 (let* ((s (cdr pair))
                        (m-disp (token-from s 0)))
                   (set-cdr! pair (cons (sym m-disp)
                                        (parms s m-disp))))))
        all))))

;; Parse @var{len} bytes of raw multipart data from @var{port}
;; using the @code{boundary} parameter of @var{type}
;; (as returned by @code{parse-type}).
;; Return a list of forms:
;;
;; @example
;; ((HEADERS . MOVE) ...)
;; @end example
;;
;; where @var{headers} is an alist with titlecased symbolic keys.
;; The values depend on the key:
;;
;; @table @code
;; @item Content-Type
;; A form as returned by @code{parse-type}.
;;
;; @item Content-Length
;; An integer.
;;
;; @item Content-Disposition
;; A form:
;;
;; @example
;; (DISPOSITION [PARAMETER...])
;; @end example
;;
;; where @var{disposition} is a downcased symbol, and each
;; @var{parameter} is a pair with @sc{car} a downcased
;; symbol and @sc{cdr} a string.
;; @end table
;;
;; Values for other keys (if any) are left unparsed (as strings).
;; If the part type is @code{multipart/*}, then @var{move} is the
;; result of a recursive application of @code{parse-multipart}
;; (i.e., a list of forms, etc).  Otherwise, @var{move} is a proc
;; that takes one argument @var{to}:
;;
;; @table @asis
;; @item @var{port}
;; Send the part contents to @var{port}.
;;
;; @item @code{#t}
;; Return a u8vector of the part contents.
;;
;; @item @code{#f}
;; Discard the part contents and return @code{#f}.
;; @end table
;;
;; Should things go weird, throw to key @code{move-part}
;; with a symbol argument describing the weirdness:
;;
;; @table @code
;; @item no-longer-available
;; @var{move} called more than once
;;
;; @item bad-to
;; unrecognized @var{to}
;; @end table
;;
(define (parse-multipart type port len)
  (let* ((buf (get-bytevector-n port len))
         (spans (interesting buf (p-ref type 'boundary)))
         (eye 0))

    (define (eye! n)
      (set! eye n))

    (define (buf-get-one-char)
      ;; For speed, don't bother w/ range check, eof object,
      ;; and all the trappings of excruciating correctness;
      ;; all reads are delimited by ‘spans’ ((BEG . END) ...)
      ;; for which (< 0 BEG END len) strictly holds.
      (let ((ch (integer->char (u8vector-ref buf eye))))
        (eye! (1+ eye))
        ch))

    (define (buf-close)
      (set! buf #f))

    (define done-with-buf!
      (let ((n (length spans)))
        ;; done-with-buf!
        (lambda ()
          (set! n (1- n))
          (and (zero? n)
               (buf-close)))))

    ;; Guile 2 has ‘(ice-9 binary-ports) make-custom-binary-input-port’
    ;; but unfortunately Guile pre-2 cannot emulate it in Scheme only,
    ;; as proper operation requires enabling ‘seek’ support available
    ;; only via C func ‘scm_set_port_seek’.

    (let ((port (make-soft-port (vector #f #f #f
                                        buf-get-one-char
                                        buf-close)
                                "r")))

      (define (port-at n)
        (eye! n)
        port)

      (define (port-at-body)
        (port-at beg))

      (define (unflatten span)
        (let* ((headers (parse-headers (port-at (car span))))
               (beg eye)
               (len (- (cdr span) beg))
               (buf buf))

          (define (ok rv)
            (set! buf #f)
            (done-with-buf!)
            rv)

          (define (bad-move! reason)
            (throw 'move-part reason))

          (define (move to)
            (or buf (bad-move! 'no-longer-available))
            (ok (if (port? to)
                    (put-bytevector to buf beg len)
                    (case to
                      ((#t) (get-bytevector-n (port-at-body) len))
                      ((#f) to)
                      (else (bad-move! 'bad-to))))))

          ;; rv
          (cons (acons 'Content-Length len headers)
                ;; Check for sub multipart/foo and recurse.  We could
                ;; simply leave things to the caller, but that's lame.
                (let ((type (assq-ref headers 'Content-Type)))
                  (case (caar type)
                    ((multipart)
                     (ok (parse-multipart type (port-at-body) len)))
                    (else
                     move))))))

      (map unflatten spans))))

;;; (www mime) ends here
