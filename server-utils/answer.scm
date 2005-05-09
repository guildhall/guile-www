;; www/server-utils/answer.scm --- HTTP connection handling and responses
;;
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

(define-module (www server-utils answer)
  #:export (mouthpiece))

;; Return a command-delegating closure capable of writing a properly formatted
;; HTTP response to @var{out-port}.  Optional arg @var{status-box} is a list
;; whose @sc{car} is set to the numeric status code given to a
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
;; NAME may be a string, symbol or keyword.  VALUE is a string.
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

  (define (walk-tree proc tree)
    (if (pair? tree)
        (for-each (lambda (sub) (walk-tree proc sub)) tree)
        (proc tree)))

  (and (not (null? status-box))         ; normalize
       (not (list? (car status-box)))
       (set! status-box '()))

  (let ((reply-status #f)
        (header-lines '())
        (content '())
        (content-length #f))

    (define (reset-protocol!)
      (set! reply-status #f)
      (set! header-lines '())
      (set! content '())
      (set! content-length #f))

    (define (set-reply-status number msg)
      (or (null? status-box) (set-car! (car status-box) number))
      (set! reply-status (format #f "HTTP/1.0 ~A ~A\r\n" number msg)))

    (define (set-reply-status:success)
      (set-reply-status 200 "OK"))

    (define (add-header name value)
      (set! header-lines
            (cons (cond ((eq? #f name)
                         (string-append value "\r\n"))
                        ((eq? #t name)
                         value)
                        (else
                         (format #f "~A: ~A\r\n"
                                 (if (keyword? name)
                                     (keyword->symbol name)
                                     name)
                                 value)))
                  header-lines)))

    (define (add-content . tree)
      (walk-tree (lambda (s)
                   (set! content-length (+ (or content-length 0)
                                           (string-length s))))
                 tree)
      (set! content (append content tree)))

    (define (add-formatted fstr . args)
      (add-content (list (apply format #f
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
                           (set! wpos (+ wpos move))
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
      (define (>OUT x)
        (display x out-port))
      (or reply-status (error "reply status not set"))
      (and content-length (add-header #:Content-Length content-length))
      (>OUT reply-status)
      (>OUT (apply string-append (reverse! header-lines)))
      (>OUT "\r\n")
      (walk-tree >OUT content)
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
