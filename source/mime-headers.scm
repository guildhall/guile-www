;;; (www mime-headers)

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

(define-module (www mime-headers)
  #:export (p-ref
            parse-type
            typed?
            top-typed?
            parse-headers)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (string-trim-right
                                         string-titlecase
                                         string-index
                                         substring/shared))
  #:use-module ((srfi srfi-14) #:select (char-set-difference
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

;; Return @code{#t} iff the form @var{type}
;; has the MIME type @var{top}/@var{sub}.
;; Both @var{top} and @var{sub} are symbols.
;;
(define (typed? type top sub)
  (let ((pair (car type)))
    (and (eq? (car pair) top)
         (eq? (cdr pair) sub))))

;; Return @code{#t} iff the form @var{type}
;; has the MIME type @var{top}/* (@var{top} is a symbol).
;;
(define (top-typed? type top)
  (eq? (caar type) top))

;; Read from @var{port} and return an alist with titlecased
;; symbolic keys.  The values depend on the key:
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
;;
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
        (and=> (have 'Content-Length)
               (lambda (pair)
                 (set-cdr! pair (string->number (cdr pair)))))
        all))))

;;; (www mime-headers) ends here
