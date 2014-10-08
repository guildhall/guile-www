;;; (www crlf)

;; Copyright (C) 2012, 2013 Thien-Thi Nguyen
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
;;
;; It contains procedures to parse (inbound) and format (outbound)
;; an HTTP byte-stream, whose particulars makes heavy use of CRLF.

;;; Code:

(define-module (www crlf)
  #:export (CRLF
            read-through-CRLF
            read-three-part-line
            read-headers
            read-characters
            hsym-proc
            get-body-proc
            read-headers/get-body
            out!)
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-n!
                                               put-bytevector))
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 rw) #:select (read-string!/partial
                                     write-string/partial))
  #:use-module ((ice-9 rdelim) #:select (read-delimited))
  #:use-module ((srfi srfi-1) #:select (append-map!))
  #:use-module ((srfi srfi-4) #:select (make-u8vector
                                        u8vector?
                                        u8vector-length
                                        u8vector-ref
                                        u8vector-set!))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (string-concatenate-reverse
                                         string-concatenate
                                         string-take
                                         string-suffix?
                                         string-index
                                         string-every
                                         string-join
                                         string-trim-right
                                         string-trim-both
                                         substring/shared))
  #:use-module ((srfi srfi-14) #:select (char-set:whitespace
                                         char-set-contains?)))

(define CR "\r")

(define CRLF "\r\n")

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

      (define (weird! new-s)
        (set! s new-s)
        (throw 'unexpected-eof (so-far)))

      (cond
       ;; weird
       ((eof-object? s) (weird! ""))
       ((eof-object? c) (weird! s))

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
;;
;; @item no-name
;; This problem occurs if there is nothing before the colon except
;; whitespace.  The argument is as for @code{missing-colon}.
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
                          (cons (car pair) (string-join (reverse! v))))))
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
               (and (or (zero? colon)
                        (string-every char-set:whitespace line 0 colon))
                    (badness 'no-name (cons line (racc!))))
               (loop (acons
                      (norm (string-trim-right line char-set:whitespace 0 colon))
                      (string-trim-both line char-set:whitespace (1+ colon))
                      acc))))))))

;; Return a string made from reading @var{n} characters from @var{port},
;; or current input port if @var{port} is unspecified.
;;
(define (read-characters n . port)
  (let ((port (if (pair? port)
                  (car port)
                  (current-input-port)))
        (s (make-string n)))
    (let loop ((start 0))
      (or (= start n)
          (and=> (read-string!/partial s port start)
                 (lambda (got)
                   (loop (+ start got))))))
    s))

(define (hsym-proc s2s)
  (if (eq? identity s2s)
      ;; trust is beautiful
      string->symbol
      ;; with experience comes wisdom (one hopes)
      (lambda (string)
        (string->symbol (s2s string)))))

(define (u8-read!-all v port)
  (get-bytevector-n! port v 0 (u8vector-length v)))

(define (get-body-proc sock hsym headers)

  (define (string-read!/partial s)
    (read-string!/partial s sock))

  (define (in! len)
    (read-characters len sock))

  (define (sub-u8 src n)
    (let ((v (make-u8vector n)))
      (do ((i 0 (1+ i)))
          ((= n i))
        (u8vector-set! v i (u8vector-ref src i)))
      v))

  (define (u8-concatenate-reverse ls)
    (let* ((len (map u8vector-length ls))
           (end (apply + len))
           (v (make-u8vector end)))
      (let loop ((ls ls) (len len))
        (or (null? ls)
            (let ((src (car ls)))
              (do ((si (1- (car len)) (1- si)))
                  ((negative? si))
                (set! end (1- end))
                (u8vector-set! v end (u8vector-ref src si)))
              (loop (cdr ls) (cdr len)))))
      v))

  (define (motion options)
    (cond ((memq 'custom options)
           => (lambda (ls)
                (let-values (((mkx r! cat-r subseq) ((cadr ls))))
                  (values mkx
                          (lambda (len)
                            (let ((x (mkx len)))
                              (r! x sock)
                              x))
                          (lambda (x)
                            (let ((count (r! x sock)))
                              (and (positive? count)
                                   count)))
                          cat-r
                          subseq))))
          ((memq 'u8 options)
           (motion (append `(custom ,(lambda ()
                                       (values make-u8vector
                                               u8-read!-all
                                               u8-concatenate-reverse
                                               sub-u8)))
                           (delq 'u8 options))))
          (else
           (values make-string
                   in!
                   string-read!/partial
                   string-concatenate-reverse
                   string-take))))

  (define (get-body options)
    (let-values (((mkx data-in! read!/partial concat-reverse first)
                  (motion options)))

      (define body<-acc
        (if (memq 'no-cat options)
            reverse!
            concat-reverse))

      (define (chunked?)
        (let ((t-enc (assq (hsym "Transfer-Encoding") headers)))
          (and (pair? t-enc)
               (equal? "chunked" (cdr t-enc))
               t-enc)))

      (define (handle-chunked t-enc)
        (let loop ((acc '()))
          (let* ((spec (read-through-CRLF sock))
                 (len (string->number
                       (cond ((string-index spec #\;)
                              => (lambda (semi)
                                   ;; ignore extensions for now
                                   (string-take spec semi)))
                             (else spec))
                       16)))
            (if (positive? len)
                ;; more
                (let* ((chunk (data-in! len))
                       (end (in! 2)))
                  (or (string=? CRLF end)
                      (throw 'chunked-transfer-encoding
                             'trailing-garbage end))
                  (loop (cons chunk acc)))
                ;; done
                (values
                 (append
                  ;; Self-deprecate when work is done.
                  (delq t-enc headers)
                  ;; New trailers, if any.
                  (read-headers sock hsym))
                 (body<-acc acc))))))

      (define (same-headers s)
        (values #f s))

      (define (known?)
        (assq-ref headers (hsym "Content-Length")))

      (define (handle-known s)
        (same-headers
         (data-in! (string->number s))))

      (define (drain)
        (let loop ((acc '()))
          (let ((x (mkx 1024)))
            (cond ((read!/partial x)
                   => (lambda (n)
                        (loop (cons (if (= 1024 n)
                                        x
                                        (first x n))
                                    acc))))
                  (else
                   (same-headers
                    (body<-acc acc)))))))

      ;; Use ‘cond’ instead of ‘or’ w/ combined test/handling because
      ;; the rv is two values, which ‘or’ cannot handle under Guile 2.x.
      (cond ((chunked?) => handle-chunked)
            ((known?) => handle-known)
            (else (drain)))))

  get-body)

(define* (read-headers/get-body sock s2s #:optional request)
  (let* ((hsym (hsym-proc s2s))
         (headers (read-headers sock hsym))
         (get-body (get-body-proc sock hsym headers)))
    (values headers
            (and (or (not (thunk? request))
                     (let-values (((method rcode) (request)))
                       ;; Sometimes no body is indicated.
                       ;; See RFC 2068: Section 4.3 "Message Body".
                       (not (or (eq? 'HEAD method)
                                (= 1 (quotient rcode 100))
                                (memq rcode '(204 304))))))
                 get-body))))

(define FRONT-FORMAT (string-append "~A ~A ~A" CRLF
                                    "~A" CRLF))

(define-macro (pop var)
  `(let ((head (car ,var)))
     (set! ,var (cdr ,var))
     head))

(define-macro (x-move what next-x)
  `(lambda (sel)
     (case sel
       ((footer-names) #f)
       ((content-length) (apply + lengths))
       ((next-chunk) (if (pair? ,what)
                         (values (pop lengths)
                                 ,next-x)
                         (values #f #f)))
       ((footers) #f))))

(define CHUNKS-DONE/FOOTER (string-append "0" CRLF
                                          "~A" CRLF))

(define (out! sock host a b c headers body flags)

  (define (fsock s . args)
    (apply simple-format sock s args))

  (define fkv
    (if (procedure? a)
        a
        (lambda (k v)
          (simple-format #f "~A: ~A" k v))))

  (define (h+! k v)
    (set! headers (cons (fkv k v) headers)))

  (define (string-out! stop s)
    (let loop ((start 0))
      (or (= start stop)
          (loop (+ start (write-string/partial s sock start stop))))))

  (define move!
    (cond ((not body) #f)
          ((null? body) (set! body #f) #f)
          ((procedure? body) body)
          ((or (and (u8vector? body) (list body))
               (and (pair? body) (and-map u8vector? body) body))
           => (lambda (vectors)
                (let ((lengths (map u8vector-length vectors)))
                  ;; move!
                  (x-move vectors (lambda (sock)
                                    (put-bytevector
                                     sock (pop vectors)))))))
          ((or (and (string? body) (list body))
               (and (pair? body) (and-map string? body) body))
           => (lambda (strings)
                (let ((lengths (map string-length strings)))
                  ;; move!
                  (x-move strings (pop strings)))))
          (else
           (throw 'bad 'body body))))

  (define (string<-elements ls)
    (string-concatenate
     (append-map! (lambda (s)
                    (if (string-suffix? CRLF s)
                        (list s)
                        (list s CRLF)))
                  ls)))

  (let* ((options (if (or (not flags) (null? flags))
                      '()
                      flags))
         (chunked? (memq 'chunked options))
         (close? (memq 'close options)))

    ;; All requests MUST include ‘Host’.  See RFC 2616
    ;; Sections: 9 "Method Definitions"; 14.23 "Host".
    (h+! 'Host host)

    (and body (if chunked?
                  (begin
                    (and=> (move! 'footer-names)
                           (lambda (ls)
                             (for-each (lambda (name)
                                         (h+! 'Trailer name))
                                       ls)))
                    (h+! 'Transfer-Encoding "chunked"))
                  (and=> (move! 'content-length)
                         (lambda (len)
                           (and (positive? len)
                                (h+! 'Content-Length len))))))

    (if (procedure? a)
        (let ((s (string-append b (string-concatenate headers) c)))
          (string-out! (string-length s) s))
        (fsock FRONT-FORMAT a b c (string<-elements headers)))

    (and body
         (let loop ()
           (let-values (((len s) (move! 'next-chunk)))
             (cond ((and len s)
                    (or (zero? len)     ; skip
                        (begin
                          (and chunked?
                               (fsock "~A~A" (number->string len 16) CRLF))
                          (if (string? s)
                              (string-out! len s)
                              (s sock))
                          (and chunked? (display CRLF sock))))
                    (loop))
                   (else
                    (and chunked?
                         (fsock CHUNKS-DONE/FOOTER
                                (cond ((move! 'footers)
                                       => (lambda (ls)
                                            (string<-elements
                                             (map fkv
                                                  (map car ls)
                                                  (map cdr ls)))))
                                      (else "")))))))))))

;;; (www crlf) ends here
