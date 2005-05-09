;;; www/server-utils/parse-request.scm --- Read HTTP first line and headers

;; 	Copyright (C) 2004 Free Software Foundation, Inc.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

(define-module (www server-utils parse-request)
  #:export (read-first-line read-headers skip-headers))

;; Parse the first line of the HTTP message from input @var{port} and
;; return a list of the method, URL path and HTTP version indicator, or
;; #f if the line ends prematurely or is otherwise malformed.  A
;; successful parse consumes the trailing CRLF of the line as well.  The
;; method is a symbol with its constituent characters upcased, such as
;; @code{GET}; the other elements are strings.  If the first line is
;; missing the HTTP version, @code{parse-first-line} returns the default
;; "HTTP/1.0".
;;
(define (read-first-line port)
  (let* ((rv (list #f #f "HTTP/1.0"))
         (box rv)
         (acc '()))

    (define (char-not-eol? c)
      (not (or (char=? c #\newline)
               (char=? c #\cr))))

    (define (read-until-ws . munge)
      (let loop ((c (read-char port)))
        (cond ((eof-object? c)
               #f)
              ((char-whitespace? c)
               (set-car! box ((if (null? munge) list->string (car munge))
                              (reverse acc)))
               (set! acc '())
               c)
              (else
               (set! acc (cons c acc))
               (loop (read-char port))))))

    (define (skip-ws!)
      (set! box (cdr box))
      (let loop ((c (read-char port)))
        (if (char-whitespace? c)
            (loop (read-char port))
            (unread-char c port)))
      #t)

    (define (ls->upcased-symbol ls)
      (string->symbol (list->string (map char-upcase ls))))

    ;; do it
    (and (and=> (read-until-ws ls->upcased-symbol) char-not-eol?)
         (skip-ws!)
         (and=> (read-until-ws)
                (lambda (2nd-ws)
                  (and (char-not-eol? 2nd-ws)
                       (set! 2nd-ws (and (skip-ws!) (read-until-ws))))
                  (and 2nd-ws
                       (char=? #\cr 2nd-ws)
                       (let ((c (read-char port)))
                         (and (not (eof-object? c))
                              (char=? #\newline c)))
                       rv))))))

;; Parse the headers of the HTTP message from input @var{port} and return a
;; list of key/value pairs, or #f if the message ends prematurely or is
;; otherwise malformed.  Both keys and values are strings.  Values are trimmed
;; of leading and trailing whitespace and may be empty.  Values that span more
;; than one line have their "continuation whitespace" reduced to a single space.
;; A successful parse consumes the trailing CRLF of the header block as well.
;;
(define (read-headers port)

  (define (next)
    (read-char port))

  (define (neof? c)
    (not (eof-object? c)))

  (define (pre-ws-trim nc)
    (if (memq nc '(#\space #\ht))
        (pre-ws-trim (next))
        (unread-char nc port)))

  (define (post-ws-trim ls)
    (if (and (pair? ls) (char-whitespace? (car ls)))
        (post-ws-trim (cdr ls))
        (list->string (reverse ls))))

  (define (read-one-header first?)      ; bad parse => #f
    (let ((k #f)
          (c1 (next)))
      (and (neof? c1)
           (cond

            ;; end of header block => #t
            ((char=? #\cr c1)
             (let ((c2 (next)))
               ;; rv
               (and (neof? c2) (char=? #\newline c2))))

            ;; continuation line => (#t . CONTINUED-VALUE)
            ((or (char=? #\space c1) (char=? #\ht c1))
             (and (not first?)
                  (let ((acc '(#\space)))
                    (pre-ws-trim (next))
                    (let loop ((c (next)))
                      (and (neof? c)
                           (case c
                             ((#\cr)
                              (let ((nc (next)))
                                (and (neof? nc)
                                     (char=? #\newline nc)
                                     ;; rv
                                     (cons #t (post-ws-trim acc)))))
                             (else
                              (set! acc (cons c acc))
                              (loop (next)))))))))

            ;; normal header => (KEY . VALUE)
            (else
             (let ((acc (list c1)))
               (and (let loop ((c (next)))
                      (and (neof? c)
                           (case c
                             ((#\cr) #f)
                             ((#\:)
                              (set! k (list->string (reverse acc)))
                              (set! acc '())
                              (pre-ws-trim (next))
                              #t)
                             (else
                              (set! acc (cons c acc))
                              (loop (next))))))
                    (let loop ((c (next)))
                      (and (neof? c)
                           (case c
                             ((#\cr)
                              (let ((c2 (next)))
                                (and (neof? c2)
                                     (char=? #\newline c2)
                                     ;; rv
                                     (cons k (post-ws-trim acc)))))
                             (else
                              (set! acc (cons c acc))
                              (loop (next)))))))))))))

  ;; do it
  (let loop ((header (read-one-header #t)) (acc '()))
    (and header
         (if (eq? #t header)
             (reverse acc)                      ;;; rv
             (loop (read-one-header #f)
                   (if (eq? #t (car header))
                       (let ((parent (car acc)))
                         (set-cdr! parent (string-append
                                           (cdr parent)
                                           (cdr header)))
                         acc)
                       (cons header acc)))))))

;; Scan without parsing the headers of the HTTP message from input @var{port},
;; and return the empty list, or #f if the message ends prematurely.  A
;; successful scan consumes the trailing CRLF of the header block as well.
;;
(define (skip-headers port)

  (define (next)
    (read-char port))

  (let* ((ring (let ((ring (list #\nul #\nul #\nul #\nul)))
                 (set-cdr! (last-pair ring) ring)
                 ring))
         (rv (let loop ((c (next)))
               (cond ((eof-object? c)
                      #f)
                     (else
                      (set-car! ring c)
                      (set! ring (cdr ring))
                      (if (and (char=? #\newline c)
                               (let ((r ring))
                                 (and (char=? #\cr (car r))
                                      (begin (set! r (cdr r))
                                             (char=? #\newline (car r)))
                                      (begin (set! r (cdr r))
                                             (char=? #\cr (car r))))))
                          '()           ; done
                          (loop (next))))))))
    (set-cdr! ring '())
    rv))

;;; www/server-utils/parse-request.scm ends here
