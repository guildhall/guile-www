;;; (www server-utils big-dishing-loop) --- Customizable listener and dispatch

;; Copyright (C) 2004,2005,2006,2007 Free Software Foundation, Inc.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
  #:export (echo-upath
            make-big-dishing-loop))

;; Use mouthpiece @var{M} (@pxref{answer}) to compose and send a
;; "text/plain" response which has the given @var{upath} (a string)
;; and any @var{extra-args} as its content.  Shut down the socket
;; for both transmission and reception, then return @code{#t}.
;;
;; This proc can be used to
;; ensure basic network connectivity (i.e., aliveness testing).
;;
(define (echo-upath M upath . extra-args)
  (M #:set-reply-status:success)
  (M #:add-header #:Connection "close")
  (M #:add-header #:Content-Type "text/plain")
  (M #:add-content upath "\n")
  (for-each (lambda (arg)
              (M #:add-formatted "extra-arg: ~S\n" arg))
            extra-args)
  (M #:send-reply 2)
  #t)

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
;; @item #:method-handlers ()
;; This alist describes how to handle the (valid) HTTP methods.
;; Each element has the form @code{(@var{method} . @var{handler})}.
;; @var{method} is a symbol, such as @code{GET}; and @var{handler} is
;; a procedure that handles the request for @var{method}.
;;
;; @var{handler} normally takes two arguments, the mouthpiece @var{m}
;; and the @var{upath} (string), composes and sends a response, and
;; returns non-#f to indicate that the big dishing loop should continue.
;; The proc's argument list is configured by @code{#:need-headers},
;; @code{#:need-input-port} and @code{#:explicit-return}.
;; Interpretation of the proc's return value is configured by
;; @code{#:explicit-return} and @code{#:loop-break-bool}.  See below.
;;
;; @item #:GET-upath echo-upath
;; This proc handles GET method requests.  It is a shorthand way of
;; specifying a @code{GET} entry in @code{#:method-handlers} (above).
;; Note, however, that this proc is ignored if there is a @code{GET}
;; entry specified in @code{#:method-handlers}.
;;
;; NOTE: @code{#:GET-upath} is obsoleted by @code{#:method-handlers}
;; and will be removed after 2009-12-31.  Do @emph{not} rely on it.
;;
;; @item #:need-headers #f
;; @itemx #:need-input-port #f
;; If non-#f, these cause additional arguments to be supplied to the
;; @code{#:GET-upath} proc.  If present, the headers arg precedes the input
;; port arg.  @xref{parse-request}.  The input port is always positioned at
;; the beginning of the HTTP message body.
;;
;; If @code{#:need-input-port} is @code{#f}, after the @code{#:GET-upath}
;; proc returns, the port is @code{shutdown} in both (r/w) directions.  When
;; operating concurrently, this is done on the child side of the split.
;; @xref{Network Sockets and Communication,,,
;; guile, The Guile Reference Manual}.
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
;; @item #:unknown-http-method-handler #f
;; If #f, silently ignore unknown HTTP methods, i.e., those not
;; specified in @code{#:method-handlers} and/or @code{#:GET-upath}.
;; The value may also be a procedure that takes three
;; arguments: a mouthpiece @var{m}, the @var{method} (symbol) and the
;; @var{upath} (string).  Its return value should be the opposite boolean
;; value of the @code{#:loop-break-bool} value, below.
;; @xref{answer}.
;;
;; @item #:parent-finish close-port
;; When operating concurrently (@code{#:concurrency} non-#f), the
;; ``parent'' applies this proc to the port after the split.
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
;;-sig: ([keyword value ...])
;;
(define* (make-big-dishing-loop
          #:key
          (socket-setup #f)
          (need-headers #f)
          (need-input-port #f)
          (explicit-return #f)
          (method-handlers '())
          (GET-upath echo-upath)
          (unknown-http-method-handler #f)
          (status-box-size #f)
          (loop-break-bool #f)
          (queue-length 0)
          (bad-request-handler #f)
          (concurrency #:new-process)
          (parent-finish close-port)
          (log #f))


  (define* (named-socket family name #:key
                         (socket-setup #f))
    (let ((new (socket family SOCK_STREAM 0)))
      ((cond ((not socket-setup) identity)
             ((procedure? socket-setup) socket-setup)
             ((list? socket-setup)
              (lambda (sock)
                (for-each (lambda (pair)
                            (setsockopt sock SOL_SOCKET
                                        (car pair)
                                        (cdr pair)))
                          socket-setup)))
             (else
              (error "bad socket-setup:" socket-setup)))
       new)
      (apply bind new name)
      new))

  (define (bdlcore queue-length sock handle-request)
    (listen sock queue-length)
    (let loop ((conn (accept sock)))
      (and (handle-request conn (read-first-line (car conn)))
           (loop (accept sock)))))

  (define (handle-request conn upath method)
    (let* ((p (car conn))
           ;; headers
           (h (cond ((and (not need-headers) (not need-input-port)))
                    (need-input-port (read-headers p))
                    (else (skip-headers p))))
           ;; status box
           (b (and (number? status-box-size)
                   (make-list status-box-size #f)))
           (M (mouthpiece p b))
           (res (cond ((assq-ref method-handlers method)
                       => (lambda (mh)
                            (call-with-current-continuation
                             (lambda (k)
                               (apply mh M upath
                                      (append
                                       (if need-headers    (list h) '())
                                       (if need-input-port (list p) '())
                                       (if explicit-return (list k) '())))
                               (not loop-break-bool)))))
                      (unknown-http-method-handler
                       => (lambda (umh)
                            (umh M method upath)))
                      (else
                       (not loop-break-bool)))))
      (and log (log (inet-ntoa (sockaddr:addr (cdr conn)))
                    method upath b))
      ;; return #t => keep going
      (not (eq? loop-break-bool res))))

  ;; backward compatibility
  (or (assq 'GET method-handlers)
      (set! method-handlers (assq-set! method-handlers 'GET GET-upath)))

  ;; rv
  (lambda (inet-port)
    (bdlcore
     queue-length

     (if (port? inet-port)
         inet-port
         (named-socket PF_INET (list AF_INET INADDR_ANY inet-port)
                       #:socket-setup socket-setup))

     (lambda (conn req)
       (let ((p (car conn)))

         (define (child)
           (return-it (cond (req
                             (apply handle-request conn (cdr (reverse! req))))
                            (bad-request-handler
                             (bad-request-handler (mouthpiece p)))
                            (else
                             (not loop-break-bool)))
             (or need-input-port (shutdown p 2))))

         (case concurrency
           ((#:new-process #:new-process/nowait)
            (let ((pid (primitive-fork)))
              (cond ((= 0 pid)
                     (exit (child)))
                    (else
                     (parent-finish p)
                     (set! p #f)
                     (or (eq? #:new-process/nowait concurrency)
                         (= 0 (status:exit-val (cdr (waitpid pid)))))))))
           (else
            (child))))))))

;;; (www server-utils big-dishing-loop) ends here
