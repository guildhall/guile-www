;;; www/server-utils/big-dishing-loop.scm --- Customizable listener and dispatch

;; 	Copyright (C) 2004,2005,2006 Free Software Foundation, Inc.
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

;;; Commentary:

;; The (www server-utils big-dishing-loop) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils big-dishing-loop)
  #:use-module (ice-9 optargs-kw)
  #:use-module (www server-utils parse-request)
  #:use-module (www server-utils answer)
  #:export (make-big-dishing-loop))

;;; Support

;; These two macros are used in `make-big-dishing-loop'.
;; Both are unhygienic and thus somewhat unsatisfying...

(define-macro (GET-simple . more-args)
  `(lambda (M upath headers inport)
     (GET-upath M upath ,@more-args)))

(define-macro (GET-return . more-args)
  `(lambda (M upath headers inport)
     (call-with-current-continuation
      (lambda (return)
        (GET-upath M upath ,@more-args return)
        (not loop-break-bool)))))

;; Return a proc @var{dish} that loops serving http requests from a socket.
;; @var{dish} takes one arg, either a TCP port number, or pre-configured
;; socket.  @var{dish} behavior is controlled by the keyword arguments given
;; to @code{make-big-dishing-loop}.  The following table is presented roughly
;; in order of the steps involved in processing a request, with default values
;; shown next to the keyword.
;;
;; @table @code
;; @item #:socket-setup #f
;; This may be a proc that takes a socket, or a list of opt/val pairs which
;; are passed to @code{setsockopt}.  Socket setup is done for newly created
;; sockets (when @var{dish} is passed a TCP port number), prior to the
;; @code{bind} call.
;;
;; @item #:queue-length 0
;; The number of clients to queue, as set by the @code{listen} system call.
;; Setting the queue length is done for both new and pre-configured sockets.
;;
;; @item #:concurrency #:new-process
;; The type of concurrency (or none if the value is not recognized).
;; Here are the recognized values:
;;
;; @table @code
;; @item #:new-process
;; @itemx #:new-process/nowait
;; Fork a new process for each request.  The latter does not wait for the
;; child process to terminate before continuing the listen loop.
;;
;; @item #f
;; Handle everything in the current in process (no concurrency).
;; Unrecognized values are treated the same as @code{#f}.
;; @end table
;;
;; @item #:bad-request-handler #f
;; If the first line of an HTTP message is not in the proper form, this
;; specifies a proc that takes a mouthpiece @var{m}.  Its return value should
;; be the opposite boston value of the @code{#:loop-break-bool} value, below.
;; @xref{answer}.
;;
;; @item #:unknown-http-method-handler #f
;; A vast majority of the time, an HTTP request is of the @code{GET} method.
;; A @code{#:unknown-http-method-handler} value of #f means to silently ignore
;; other request methods.  The value may also be a procedure that takes three
;; arguments: a mouthpiece @var{m}, the @var{method} (symbol) and the
;; @var{upath} (string).  Its return value should be the opposite boolean
;; value of the @code{#:loop-break-bool} value, below.
;; @xref{answer}.
;;
;; @item #:GET-upath echo-upath
;; This proc handles GET method requests.  It normally takes two arguments,
;; the mouthpiece @var{m} and the @var{upath} (string), composes and sends a
;; response, and returns non-#f to indicate that the big dishing loop should
;; continue.  The proc's argument list is configured by @code{#:need-headers},
;; @code{#:need-input-port} and @code{#:explicit-return}.  Interpretation of
;; the proc's return value is configured by @code{#:explicit-return} and
;; @code{#:loop-break-bool}.
;;
;; The default is named @code{echo-upath} because it composes and sends a
;; "text/plain" response which has the given upath as its sole content.
;; This can be used
;; to ensure basic network connectivity (i.e., aliveness testing).
;; @xref{answer}.
;;
;; @item #:need-headers #f
;; @itemx #:need-input-port #f
;; If non-#f, these cause additional arguments to be supplied to the
;; @code{#:GET-upath} proc.  If present, the headers arg precedes the input
;; port arg.  @xref{parse-request}.  The input port is always positioned at
;; the beginning of the HTTP message body.
;;
;; @item #:explicit-return #f
;; If non-#f, this arranges for a continuation to be passed (as the last
;; argument) to the @code{#:GET-upath} proc, and ignores that proc's normal
;; return value in favor of one explicitly passed through the continuation.
;; If the continuation is not used, the @dfn{effective return value} is
;; computed as @code{(not #:loop-break-bool)}.
;;
;; @item #:loop-break-bool #f
;; Looping stops if the effective return value of @code{#:GET-upath} is
;; @code{eq?} to this value.
;;
;; @item #:log #f
;; This proc is called after the @var{#:GET-upath} proc returns.
;; @xref{log}.
;;
;; @item #:status-box-size #f
;; This may be a non-negative integer, typically 0, 1 or 2.  It is used by
;; @code{#:log} (has no meaning if @code{#:log} is #f).  @xref{log}.
;; @end table
;;
;;-sig: ([#:keyword value ...])
;;
(define* (make-big-dishing-loop
          #:key
          (socket-setup #f)
          (need-headers #f)
          (need-input-port #f)
          (explicit-return #f)
          (GET-upath (lambda (M upath)
                       (M #:set-reply-status:success)
                       (M #:add-header #:Connection "close")
                       (M #:add-header #:Content-Type "text/plain")
                       (M #:add-content upath)
                       (M #:send-reply)))
          (unknown-http-method-handler #f)
          (status-box-size #f)
          (loop-break-bool #f)
          (queue-length 0)
          (bad-request-handler #f)
          (concurrency #:new-process)
          (log #f))

  (define (ferv n vector)
    (vector-ref vector n))

  (let ((GET (ferv (+ (if need-headers    1 0)          ;;; We are knocking
                      (if need-input-port 2 0)          ;;; at the doors of
                      (if explicit-return 4 0))         ;;; a combinatorial
                   (vector                              ;;; explosion; DWR!
                    (GET-simple)
                    (GET-simple headers)
                    (GET-simple inport)
                    (GET-simple headers inport)
                    (GET-return)
                    (GET-return headers)
                    (GET-return inport)
                    (GET-return headers inport))))

        (UNK (or unknown-http-method-handler
                 (lambda args (not loop-break-bool))))

        (sockprep (cond ((not socket-setup) #f)
                        ((procedure? socket-setup) socket-setup)
                        ((list? socket-setup)
                         (lambda (sock)
                           (for-each (lambda (pair)
                                       (setsockopt socket SOL_SOCKET
                                                   (car pair)
                                                   (cdr pair)))
                                     socket-setup)))
                        (else
                         (error "bad socket-setup:" socket-setup))))

        (make-h (ferv (+ (if need-headers    1 0)
                         (if need-input-port 2 0))
                      (vector
                       (lambda (p) #f)
                       read-headers
                       skip-headers
                       read-headers)))

        (make-b (if (number? status-box-size)
                    (lambda () (make-list status-box-size #f))
                    (lambda () #f)))

        (do-log (or log (lambda ignored #f)))

        (handle-bad-request (if bad-request-handler
                                (lambda (p)
                                  (bad-request-handler (mouthpiece p)))
                                (lambda ignored
                                  (not loop-break-bool))))

        (make-client (if log
                         (lambda (conn)
                           (inet-ntoa (sockaddr:addr (cdr conn))))
                         (lambda ignored #f))))

    (define (handle-request client p method upath http-version)
      (let* ((h (make-h p))             ; headers
             (b (make-b))               ; status box
             (M (mouthpiece p b))
             (res (case method
                    ((GET) (GET M upath h p))
                    (else (UNK M method upath)))))
        (do-log client method upath b)
        (not (eq? loop-break-bool res)))) ; return #t => keep going

    ;; rv
    (lambda (inet-port)
      (let ((sock (if (port? inet-port)
                      inet-port
                      (let ((new (socket AF_INET SOCK_STREAM 0)))
                        (and sockprep (sockprep new))
                        (bind new AF_INET INADDR_ANY inet-port)
                        new))))
        (listen sock queue-length)
        (let loop ((conn (accept sock)))
          (let ((p (car conn))
                (client (make-client conn)))
            (define (child)
              (let ((req (read-first-line p)))
                (if (not req)
                    (handle-bad-request p)
                    (apply handle-request client p req))))
            (define (butt-out!)
              (close-port p)
              (set! p #f))
            (case concurrency
              ((#:new-process #:new-process/nowait)
               (let ((pid (primitive-fork)))
                 (cond ((= 0 pid)
                        (exit (child)))
                       (else
                        (butt-out!)
                        (and (or (eq? #:new-process/nowait concurrency)
                                 (= 0 (status:exit-val (cdr (waitpid pid)))))
                             (loop (accept sock)))))))
              (else
               (and (child)
                    (begin (butt-out!)
                           (loop (accept sock))))))))))))

;;; www/server-utils/big-dishing-loop.scm ends here
