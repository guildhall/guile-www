;;; (www crlf)

;; Copyright (C) 2012 Thien-Thi Nguyen
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
;; It contains procedures to parse the HTTP byte-stream,
;; whose format makes heavy use of CRLF.

;;; Code:

(define-module (www crlf)
  #:export (read-through-CRLF
            read-three-part-line
            read-headers
            read-characters)
  #:use-module ((ice-9 rw) #:select (read-string!/partial))
  #:use-module ((ice-9 rdelim) #:select (read-delimited))
  #:use-module ((srfi srfi-4) #:select (make-u8vector))
  #:use-module ((srfi srfi-13) #:select (string-concatenate-reverse
                                         string-index
                                         string-join
                                         string-trim-right
                                         string-trim-both
                                         substring/shared))
  #:use-module ((srfi srfi-14) #:select (char-set:whitespace)))

(define CR "\r")

;; Read a string from @var{port} through the next @samp{CRLF},
;; discarding the @samp{CRLF}.  On EOF, throw to @code{unexpected-eof}
;; the accumulated string prior to the EOF.
;;
(define (read-through-CRLF port)
  (let loop ((acc '()))
    (let* ((s (read-delimited CR port))
           (c (peek-char port)))

      (define (so-far)
        (if (null? acc)
            s
            (string-concatenate-reverse acc s)))

      (cond
       ;; weird
       ((eof-object? s)
        (set! s "")
        (throw 'unexpected-eof (so-far)))
       ((eof-object? c)
        (throw 'unexpected-eof (so-far)))

       ;; normal
       ((char=? #\newline c)
        ;; Discard the LF (CR already forgotten) ...
        (read-char port)
        ;; ... and return a string.
        (so-far))

       ;; foolish
       (else
        (loop (cons* CR s acc)))))))

;; Read a @samp{CRLF}-terminated line from @var{port}
;; as three @code{#\space}-separated parts.
;; If there are only two parts, the third part is empty.
;; Discard the @samp{CRLF} and return three values, all strings.
;; On EOF, throw to @code{unexpected-eof}
;; the accumulated string prior to the EOF.
;;
(define (read-three-part-line port)
  (let* ((str (read-through-CRLF port))
         (one (string-index str #\space))
         (two (string-index str #\space (1+ one))))

    (define (subs b . e)
      (apply substring/shared str b e))

    (values (subs 0 one)
            (subs (1+ one) (or two (string-length str)))
            (if two
                (subs (1+ two))
                ""))))

;; Read headers from @var{port}; consume the trailing @var{CRLF}.
;; Return a list of string @code{(@var{name} . @var{value})},
;; where @var{name} is the result of calling @var{norm} on the
;; original (string) header name, and @var{value} is a string.
;; On error, throw to @code{parse-error} a symbol @var{problem}
;; and a @var{problem}-specific argument.
;;
;; @table
;; @item unexpected-eof
;; The argument is a pair @code{(@var{so-far} . @var{headers}),
;; where @var{so-far} is the accumulated string prior to EOF.
;;
;; @item unexpected-continuation
;; This problem occurs when a @dfn{continuation line}, i.e., one
;; that begins with whitespace, occurs as the first line read.
;; The argument is the offending line, a string.
;;
;; @item missing-colon
;; This problem occurs when the @code{#\:} (colon), which normally
;; separates the header name from its value, is not found.
;; The argument is a pair @code{(@var{line} . @var{headers}).
;; @end table
;;
(define (read-headers port norm)

  (define (badness k v)
    (throw 'parse-error k v))

  (let loop ((acc '()))

    (define (racc!)
      (reverse! acc))

    (let ((line (catch 'unexpected-eof (lambda ()
                                         (read-through-CRLF port))
                       (lambda (key so-far)
                         (badness key (cons so-far (racc!)))))))
      (cond ((string-null? line)
             (map (lambda (pair)
                    (let ((v (cdr pair)))
                      (if (string? v)
                          pair
                          (cons (car pair) (string-join (reverse! v) " ")))))
                  (racc!)))
            ((char-set-contains? char-set:whitespace (string-ref line 0))
             (or (pair? acc)
                 (badness 'unexpected-continuation line))
             (let ((more (string-trim-both line)))
               (or (string-null? more)
                   (let* ((prev (car acc))
                          (pls (cdr prev)))
                     (set-cdr! prev (cons more (if (string? pls)
                                                   (list pls)
                                                   pls)))))
               (loop acc)))
            (else
             (let ((colon (string-index line #\:)))
               (or colon (badness 'missing-colon (cons line (racc!))))
               (loop (acons
                      (norm (string-trim-right line char-set:whitespace 0 colon))
                      (string-trim-both line char-set:whitespace (1+ colon))
                      acc))))))))

;; Return a string made from reading @var{n} characters from @var{port}.
;;
(define (read-characters n port)
  (let ((s (make-string n)))
    (let loop ((start 0))
      (or (= start n)
          (and=> (read-string!/partial s port start)
                 (lambda (got)
                   (loop (+ start got))))))
    s))

;;; (www crlf) ends here
