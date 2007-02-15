;; www/server-utils/answer.scm --- HTTP connection handling and responses
;;
;; 	Copyright (C) 2004,2006,2007 Free Software Foundation, Inc.
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

(define-module (www server-utils answer)
  #:use-module ((ice-9 rw) #:select (write-string/partial))
  #:export (CRLF fs walk-tree string<-header-components
                 mouthpiece))

(define-macro (+! v n)
  `(set! ,v (+ ,v ,n)))

(define CRLF "\r\n")

;; Return a new string made by using format string @var{s} on @var{args}.
;; As in @code{simple-format} (which this procedure uses), @code{~A} expands
;; as with @code{display}, while @code{~S} expands as with @code{write}.
;;
(define (fs s . args)
  (apply simple-format #f s args))

;; Call @var{proc} for each recursively-visited leaf in @var{tree}, excluding
;; empty lists.  It is an error for @var{tree} to contain improper lists.
;;
(define (walk-tree proc tree)
  (cond ((null? tree))
        ((pair? tree) (for-each (lambda (sub) (walk-tree proc sub)) tree))
        (else (proc tree))))

(define (length/tree<-header-components name value . etc)
  (let ((len 4)                         ; colon space CR LF
        (lhs (if (string? name)
                 name
                 (fs "~A" (if (keyword? name)
                              (keyword->symbol name)
                              name))))
        (rhs (if (or (pair? value) (null? value))
                 value
                 (fs "~A" value))))
    (define (more s)
      (+! len (string-length s)))
    (walk-tree more lhs)
    (walk-tree more rhs)
    (if (null? etc)
        (list len lhs ": " rhs CRLF)
        (let ((rest (apply length/tree<-header-components etc)))
          (list (+ len (car rest))
                lhs ": " rhs CRLF
                (cdr rest))))))

;; Return a string made from formatting header name @var{n} and value @var{v}.
;; Additional headers can be specified as alternating name and value args.
;; Each header is formatted like so: ``NAME: VALUE\r\n''.
;;
;; Each @var{n} may be a string, symbol or keyword.  Each @var{v} may be a
;; string, number, or a tree of strings.
;;
;;-sig: (n v [n1 v1...])
;;
(define (string<-header-components name value . etc)
  (let* ((l/t (apply length/tree<-header-components name value etc))
         (wp 0)
         (rv (make-string (car l/t))))
    (walk-tree (lambda (s)
                 (let ((len (string-length s)))
                   (substring-move! s 0 len rv wp)
                   (+! wp len)))
               (cdr l/t))
    rv))

;; Return a command-delegating closure capable of writing a properly formatted
;; HTTP 1.0 response to @var{out-port}.  Optional arg @var{status-box} is a
;; list whose @sc{car} is set to the numeric status code given to a
;; @code{#:set-reply-status} command.  If @var{status-box} has length of two
;; or more, its @sc{cadr} is set to the content-length on @code{#:send-reply}.
;; A content-length value of #f means there have been no calls to
;; @code{#:add-content}.  The commands and their args are:
;;
;; @table @code
;; @item #:reset-protocol!
;; Reset internal state, including reply status, headers and content.
;; This is called automatically by @code{#:send-reply}.
;;
;; @item #:set-reply-status NUMBER MESSAGE
;; Set the reply status.  MESSAGE is a short string.
;;
;; @item #:set-reply-status:success
;; This is equivalent to @code{#:set-reply-status 200 "OK"}.
;;
;; @item #:add-header NAME VALUE
;; NAME may be #f, #t, a string, symbol or keyword.  VALUE is a string.
;; If NAME is #f or #t, VALUE is taken to be a pre-formatted string,
;; "A: B" or "A: B\r\n", respectively.  If NAME is not a boolean, VALUE
;; may also be a tree of strings or a number.
;;
;; @item #:add-content [TREE ...]
;; TREE may be a string, a nested list of strings, or a series of such.
;; Subsequent calls to @code{#:add-content} append their trees to the
;; collected content tree thus far.
;;
;; @item #:add-formatted FORMAT-STRING [ARGS ...]
;; FORMAT-STRING may be #f to mean @code{~S}, #t to mean @code{~A}, or a
;; normal format string.  It is used to format ARGS, and the result passed
;; to @code{#:add-content}.
;;
;; @item #:content-length
;; Return the total number of bytes in the content added thus far.
;;
;; @item #:rechunk-content CHUNK
;; CHUNK may be #f, in which case a list of the string lengths collected
;; thus far is returned; #t which means to use the content length as the
;; chunk size (effectively producing one chunk); or a number specifying
;; the maximum size of a chunk.  The return value is a list of the chunk
;; sizes.
;;
;; @item #:send-reply
;; Send the properly formatted response to @var{out-port}, and reset
;; all internal state (status reset, content discarded, etc).  It is
;; an error to invoke @code{#:send-reply} without having first set
;; the reply status.
;; @end table
;;
;;-sig: (out-port [status-box])
;;
(define (mouthpiece out-port . status-box)

  (and (not (null? status-box))         ; normalize
       (not (list? (car status-box)))
       (set! status-box '()))

  (let* ((pre-tree (list #f))
         (pre-tp pre-tree)
         (pre-len 0)
         (preamble (make-string (- 1024 16)))
         (content '())
         (content-length #f))

    (define (reset-protocol!)
      (set! pre-tree (list #f))
      (set! pre-tp pre-tree)
      (set! pre-len 0)
      (set! content '())
      (set! content-length #f))

    (define (set-reply-status number msg)
      (or (null? status-box) (set-car! (car status-box) number))
      (let ((s (fs "HTTP/1.0 ~A ~A\r\n" number msg)))
        (+! pre-len (string-length s))
        (set-car! pre-tree s)))

    (define (set-reply-status:success)
      (+! pre-len 17)
      (set-car! pre-tree "HTTP/1.0 200 OK\r\n"))

    (define (preamble-append! len new)
      (+! pre-len len)
      (set-cdr! pre-tp (list new)))

    (define (add-header name value)
      (define (up! len new)
        (preamble-append! len new)
        (set! pre-tp (cdr pre-tp)))
      (cond ((eq? #f name)
             (up! (+ 2 (string-length value)) (list value CRLF)))
            ((eq? #t name)
             (up! (string-length value) value))
            (else
             (let ((l/t (length/tree<-header-components name value)))
               (up! (car l/t) (cdr l/t))))))

    (define (add-content . tree)
      (or content-length (set! content-length 0))
      (walk-tree (lambda (s)
                   (+! content-length (string-length s)))
                 tree)
      (set! content (append! content tree)))

    (define (add-formatted fstr . args)
      (add-content (list (apply fs
                                (cond ((eq? #f fstr) "~S")
                                      ((eq? #t fstr) "~A")
                                      (else fstr))
                                args))))

    (define (rechunk-content chunk)
      (define (upd! proc get-new-content)
        (walk-tree proc content)
        (set! content (get-new-content)))
      (cond ((eq? #f chunk)
             (let ((ls '()))
               (walk-tree (lambda (s)
                            (set! ls (cons (string-length s)
                                           ls)))
                          content)
               (reverse ls)))
            ((eq? #t chunk)
             (rechunk-content content-length))
            ((and (number? chunk) (not (< 0 chunk)))    ;;; slack
             '())
            ((number? chunk)
             (let* ((extra (remainder content-length chunk))
                    (dreck (make-list (quotient content-length chunk) chunk))
                    (frizz (if (= 0 extra)
                               dreck
                               (reverse (cons extra dreck))))
                    (noise (map (lambda (n) (make-string n)) frizz))
                    (nw noise)
                    (dest (car nw))
                    (dlen (string-length dest))
                    (wpos 0))
               (upd! (lambda (s)
                       (let* ((size (string-length s))
                              (dpos (remainder wpos chunk))
                              (left (- dlen dpos)))
                         (let loop ((start 0) (move (min size left)))
                           (substring-move! s start (+ start move)
                                            dest dpos)
                           (+! wpos move)
                           (let ((new-start (+ start move))
                                 (new-dpos (remainder wpos chunk)))
                             (cond ((= 0 new-dpos)
                                    (set! nw (cdr nw))
                                    (cond ((not (null? nw))
                                           (set! dest (car nw))
                                           (set! dlen (string-length dest))))))
                             (or (= size new-start)
                                 (begin
                                   (set! dpos new-dpos)
                                   (set! left (- dlen dpos))
                                   (loop new-start
                                         (min (- size new-start)
                                              left))))))))
                     (lambda () noise))
               frizz))
            (else
             (error "chunk must be #f, #t or a number:" chunk))))

    (define (send-reply)
      (define (out! s stop)
        ;; todo: use `write-string/partial/never-fewer' from Guile 1.4.1.108
        (let loop ((start 0))
          (set! start (+ start (write-string/partial s out-port start stop)))
          (or (= start stop)
              (loop start))))
      (or (car pre-tree) (error "reply status not set"))
      (and content-length (add-header #:Content-Length content-length))
      (preamble-append! 2 CRLF)
      (and (< (string-length preamble) pre-len)
           (set! preamble (make-string (+ pre-len 64))))
      (let ((wp 0))
        (walk-tree (lambda (s)
                     (let ((len (string-length s)))
                       (substring-move! s 0 len preamble wp)
                       (+! wp len)))
                   pre-tree))
      (out! preamble pre-len)
      (walk-tree (lambda (x)
                   (out! x (string-length x)))
                 content)
      (force-output out-port)
      (or (null? status-box)
          (let ((box (car status-box)))
            (or (null? (cdr box))
                (set-car! (cdr box) content-length))))
      (reset-protocol!))

    ;; rv
    (lambda (command . args)
      (or (keyword? command) (error "command not a keyword:" command))
      (apply
       (case command
         ((#:reset-protocol!) reset-protocol!)
         ((#:set-reply-status) set-reply-status)
         ((#:set-reply-status:success) set-reply-status:success)
         ((#:add-header) add-header)
         ((#:add-content) add-content)
         ((#:add-formatted) add-formatted)
         ((#:content-length) (lambda () content-length))
         ((#:rechunk-content) rechunk-content)
         ((#:send-reply) send-reply)
         (else (error "unrecognized command:" command)))
       args))))

;;; www/server-utils/answer.scm ends here
