;;; (www mime-multipart)

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
;; You should have received a copy of the GNU General Public License
;; along with Guile-WWW.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is "internal": distributed, installed,
;; but not documented (at least, for now).

;;; Code:

(define-module (www mime-multipart)
  #:export (parse-multipart)
  #:use-module ((www mime-headers) #:select (p-ref
                                             parse-type
                                             top-typed?
                                             parse-headers))
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-n
                                               put-bytevector))
  #:use-module ((srfi srfi-4) #:select (u8vector
                                        u8vector-length
                                        u8vector-ref)))

(define-macro (ignore-default-port-encoding . body)
  `(cond-expand
    (guile-2
     (with-fluids ((%default-port-encoding #f))
       ,@body))
    (else
     ,@body)))

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

;; Parse @var{len} bytes of raw multipart data from @var{port}
;; using the @code{boundary} parameter of @var{type}
;; (as returned by @code{parse-type}).
;; Return a list of forms:
;;
;; @example
;; ((MOVE . HEADERS) ...)
;; @end example
;;
;; where @var{headers} are the result of calling @code{parse-headers}
;; on @var{port}@footnote{Both @code{parse-type} and
;; @code{parse-headers} are from module @code{(www mime-headers)}.}.
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

    (let ((port (ignore-default-port-encoding
                 (make-soft-port (vector #f #f #f
                                         buf-get-one-char
                                         buf-close)
                                 "r"))))

      (define (port-at n)
        (eye! n)
        port)

      (define (unflatten span)
        (let* ((headers (parse-headers (port-at (car span))))
               (beg eye)
               (len (- (cdr span) beg))
               (buf buf))

          (define (port-at-body)
            (port-at beg))

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
          (cons
           ;; Check for sub multipart/foo and recurse.  We could
           ;; simply leave things to the caller, but that's lame.
           (let ((type (assq-ref headers 'Content-Type)))
             (if (top-typed? type 'multipart)
                 (ok (parse-multipart type (port-at-body) len))
                 move))
           (acons 'Content-Length len
                  headers))))

      (map unflatten spans))))

;;; (www mime-multipart) ends here
