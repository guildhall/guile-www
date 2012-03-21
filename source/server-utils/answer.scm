;;; (www server-utils answer) --- HTTP connection handling and responses

;; Copyright (C) 2008, 2009, 2010, 2011, 2012 Thien-Thi Nguyen
;; Copyright (C) 2004, 2006, 2007 Free Software Foundation, Inc.
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

(define-module (www server-utils answer)
  #:export (CRLF flat-length fs walk-tree tree-flat-length! string<-tree
                 string<-headers
                 compose-response
                 mouthpiece)
  #:use-module ((www crlf) #:select ((CRLF . the-actual-CRLF)
                                     out!))
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 q) #:select (make-q
                                    enq!
                                    q-empty?
                                    deq!
                                    sync-q!))
  #:use-module ((ice-9 rw) #:select (write-string/partial)))

(define-macro (+! v n)
  `(set! ,v (+ ,v ,n)))

;; The string ``\r\n''.
;;
;;-category: constant string
;;
(define CRLF the-actual-CRLF)

;; Return the flat length of @var{object},
;; or @code{#f} if not yet computed.
;;
;;-category: object property
;;-args: (1 0 0 object)
;;
(define flat-length (make-object-property))

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

;; If @var{tree} is a string, return its @code{string-length}.
;; If @var{tree} already has a @code{flat-length}, return that.
;; Otherwise, recursively compute, set, and return the
;; @code{flat-length} of @var{tree}.
;;
(define (tree-flat-length! tree)
  (cond ((string? tree) (string-length tree))
        ((null? tree) 0)
        ((flat-length tree))
        (else (let ((len (+ (tree-flat-length! (car tree))
                            (tree-flat-length! (cdr tree)))))
                (set! (flat-length tree) len)
                len))))

;; Return a new string made from flattening @var{tree}.
;; Set the @code{flat-length} (using @code{tree-flat-length!})
;; of @var{tree} by side effect.
;;
(define (string<-tree tree)
  (let ((wp 0)
        (rv (make-string (tree-flat-length! tree))))
    (walk-tree (lambda (s)
                 (let ((len (string-length s)))
                   (substring-move! s 0 len rv wp)
                   (+! wp len)))
               tree)
    rv))

(define ((ish-ref idx) ish) (vector-ref ish idx))

(define ish-eol      (ish-ref 0))
(define ish-proto-v  (ish-ref 1))
(define ish-status   (ish-ref 2))
(define ish-kv-sep   (ish-ref 3))
(define ish-neck     (ish-ref 4))

(define http-ish (vector CRLF
                         "HTTP/~A.~A "
                         #f
                         ": "
                         CRLF))

(define tree<-header-proc               ; => (lambda (key val) ...)
  (let ((cache '()))

    (define (k x)
      (if (string? x)
          x
          (fs "~A" (if (keyword? x)
                       (keyword->symbol x)
                       x))))

    (define (v x)
      (if (or (string? x) (pair? x) (null? x))
          x
          (fs "~A" x)))

    ;; tree<-header-proc
    (lambda (style)
      (define (trundle)
        (let* ((one (ish-kv-sep style))
               (two (ish-eol    style))
               (k-len (+ (string-length one)
                         (string-length two))))
          ;; rv
          (lambda (key val)
            (set! key (k key))
            (set! val (v val))
            (let ((tree (list key one
                              val two)))
              (set! (flat-length tree) (+ (string-length key)
                                          (tree-flat-length! val)
                                          k-len))
              tree))))
      (or (assq-ref cache style)
          (let ((rv (trundle)))
            (set! cache (acons style rv cache))
            rv)))))

;; Return a string made from formatting name/value pairs in @var{alist},
;; according to the optional @code{style} argument.  If unspecified or
;; specified as @code{#f}, the default is to format headers like so:
;;
;; @example
;; NAME #\: #\space VALUE #\cr #\lf
;; @end example
;;
;; Each name may be a string, symbol or keyword.  Each value may be a
;; string, number, symbol, or a tree.
;;
(define* (string<-headers alist #:optional (style #f))
  (string<-tree (map (tree<-header-proc (or style http-ish))
                     (map car alist)
                     (map cdr alist))))

(define status-format-string
  (let ((cache '()))
    ;; status-format-string
    (lambda (protocol-version style)
      (define (trundle)
        (string-append (cond ((and protocol-version
                                   (ish-proto-v style))
                              => (lambda (fmt)
                                   (fs fmt
                                       (car protocol-version)
                                       (cdr protocol-version))))
                             (else ""))
                       (or (ish-status style) "")
                       "~A"             ; code (integer)
                       " "
                       "~A"             ; text (string)
                       (ish-eol style)))
      (let ((key (cons protocol-version style)))
        (or (assoc-ref cache key)
            (let ((rv (trundle)))
              (set! cache (acons key rv cache))
              rv))))))

;; Return a command-delegating closure capable of writing a properly
;; formatted HTTP 1.1 response with @code{Host} header set to @var{host}.
;; The actual status and header format is controlled by @var{style},
;; an opaque object.
;; The actual protocol version is controlled by @var{protocol-version},
;; a pair of integers, such as @code{(1 . 0)} to indicate HTTP 1.0.
;;
;; The returned closure @var{r} accepts commands and args:
;;
;; @table @code
;; @findex set-protocol-version
;; @item #:set-protocol-version @var{pair}
;; Set the major and minor version protocol-version numbers.
;;
;; @findex set-reply-status
;; @item #:set-reply-status @var{number} @var{message}
;; Set the reply status.  @var{message} is a short string.
;;
;; @findex add-header
;; @item #:add-header @var{name} @var{value}
;; @var{name} may be @code{#f}, @code{#t}, a string, symbol or keyword.
;; @var{value} is a string.  If @var{name} is @code{#f} or @code{#t},
;; @var{value} is taken to be a pre-formatted string, "A: B" or "A:
;; B\r\n", respectively.  If @var{name} is not a boolean, @var{value}
;; may also be a tree of strings or a number.
;;
;; @findex add-content
;; @item #:add-content [@var{tree} @dots{}]
;; @var{tree} may be a string, a nested list of strings, or a series of such.
;; Subsequent calls to @code{#:add-content} append their trees to the
;; collected content tree thus far.
;;
;; @findex add-formatted
;; @item #:add-formatted @var{format-string} [@var{args} @dots{}]
;; @var{format-string} may be @code{#f} to mean @code{~S}, @code{#t} to
;; mean @code{~A}, or a normal format string.  It is used to format
;; @var{args}, and the result passed to @code{#:add-content}.
;;
;; @findex add-direct-writer
;; @item #:add-direct-writer @var{len} @var{write}
;; @var{len} is the number of bytes that procedure @var{write} will
;; output to its arg, @var{out-port} (passed back), when called during
;; @code{#:send-reply}.  This is to allow sendfile(2) and related
;; hackery.
;;
;; @findex entity-length
;; @item #:entity-length
;; Return the total number of bytes in the content added thus far.
;;
;; @findex rechunk-content
;; @item #:rechunk-content @var{chunk}
;; @var{chunk} may be @code{#f}, in which case a list of the string
;; lengths collected thus far is returned; @code{#t} which means to use
;; the content length as the chunk size (effectively producing one
;; chunk); or a number specifying the maximum size of a chunk.  The
;; return value is a list of the chunk sizes.
;;
;; It is an error to use @code{#:rechunk-content} with a non-@code{#f}
;; @var{chunk} in the presence of a previous @code{#:add-direct-writer}.
;;
;; @findex inhibit-content!
;; @item #:inhibit-content! @var{bool}
;; Non-@code{#f} @var{bool} arranges for @code{#:send-reply} (below) to
;; compute content length and add the appropriate header, as usual, but
;; no content is actually sent.  This is useful, e.g., when answering a
;; @code{HEAD} request.  If @var{bool} is @code{#f}, @code{#:send-reply}
;; acts normally (i.e., sends both headers and content).
;;
;; @findex send!
;; @item #:send! @var{sock} [@var{flags}]
;; Send the properly formatted response to file-port @var{sock}.  It is
;; an error to invoke @code{#:send-reply} without having first set
;; the reply status.
;;
;; Optional arg @var{flags} are the same as for @code{send-request}.
;; @xref{http}.
;; @end table
;;
(define* (compose-response host #:key
                           (style http-ish)
                           (protocol-version '(1 . 1)))

  (define tree<-header
    (tree<-header-proc style))

  (let* ((status #f)
         (hq (make-q))
         (hlen 0)
         (headers '())
         (body? #t)
         (direct-writers '())
         (entq (make-q))
         (lenq (make-q))
         (final-entity-length #f))

    (define (current-entity-length)
      (or final-entity-length (apply + (car lenq))))

    (define (walk-content proc)
      (walk-tree proc (car entq)))

    (define (set-protocol-version pair)
      (or (and (pair? pair)
               (integer? (car pair))
               (integer? (cdr pair)))
          (error "bad protocol-version:" pair))
      (set! protocol-version pair))

    (define (set-reply-status number msg)
      (and number msg (set! status (list number msg))))

    (define (add-header name value)
      (define (up! new)
        (+! hlen (tree-flat-length! new))
        (enq! hq new))
      (cond ((eq? #f name)
             (up! (list value CRLF)))
            ((eq? #t name)
             (up! value))
            (else
             (up! (tree<-header name value)))))

    (define (add-string s)
      (enq! lenq (string-length s))
      (enq! entq s))

    (define (add-content . tree)
      (add-string (string<-tree tree)))

    (define (add-formatted fstr . args)
      (add-string (apply fs
                         (cond ((eq? #f fstr) "~S")
                               ((eq? #t fstr) "~A")
                               (else fstr))
                         args)))

    (define* (add-direct-writer len write #:optional chunkable?)
      (set! direct-writers (acons write (cons len chunkable?) direct-writers))
      (enq! lenq len)
      (enq! entq (if chunkable?
                     (lambda (port)
                       (write port len))
                     write)))

    (define (x-length x)
      (if (procedure? x)
          (car (assq-ref direct-writers x))
          (string-length x)))

    (define (rechunk-content chunk)
      (cond ((not chunk)
             (car lenq))
            ((not (null? direct-writers))
             (error "cannot rechunk in the presence of direct-writers"))
            ((eq? #t chunk)
             (rechunk-content (current-entity-length)))
            ((not (number? chunk))
             (error "bad #:rechunk-content spec:" chunk))
            ((zero? chunk) ;;; slack
             '())
            (else
             (let* ((entlen (current-entity-length))
                    (extra (remainder entlen chunk))
                    (dreck (make-list (quotient entlen chunk) chunk))
                    (frizz (if (zero? extra)
                               dreck
                               (append! dreck (list extra))))
                    (noise (map (lambda (n) (make-string n)) frizz))
                    (nw noise)
                    (dest (car nw))
                    (dlen (string-length dest))
                    (wpos 0))

               (define (shift s)
                 (let* ((size (string-length s))
                        (dpos (remainder wpos chunk))
                        (left (- dlen dpos)))
                   (let loop ((start 0) (move (min size left)))
                     (substring-move! s start (+ start move)
                                      dest dpos)
                     (+! wpos move)
                     (let ((new-start (+ start move))
                           (new-dpos (remainder wpos chunk)))
                       (cond ((zero? new-dpos)
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

               (walk-content shift)
               (set-car! entq noise) (sync-q! entq)
               (set-car! lenq frizz) (sync-q! lenq)
               frizz))))

    (define (inhibit-content! value)
      (set! body? (not value)))

    (define (entity-length)
      (if body? (current-entity-length) 0))

    (define* (send! sock #:optional (flags '()))
      (or status (error "reply status not set"))
      (set! final-entity-length (current-entity-length))
      (out! sock host
            ;; fkv
            (lambda (k v)
              (string<-headers (acons k v '()) style))
            ;; status
            (apply fs (status-format-string protocol-version style) status)
            ;; neck
            (ish-neck style)
            ;; headers
            (let ((h-str (make-string hlen))
                  (wp 0))
              (walk-tree (lambda (s)
                           (let ((len (string-length s)))
                             (substring-move! s 0 len h-str wp)
                             (+! wp len)))
                         (car hq))
              (list h-str))
            ;; body
            (and body?
                 ;; body
                 (lambda (command)
                   (case command
                     ((content-length) final-entity-length)
                     ((next-chunk) (if (q-empty? entq)
                                       (values #f #f)
                                       (values (deq! lenq)
                                               (deq! entq))))
                     (else #f))))
            flags)
      ;; rv
      (list (car status) (if body? final-entity-length 0)))

    ;; rv
    (lambda (command . args)
      (apply
       (case command
         ((#:set-protocol-version) set-protocol-version)
         ((#:set-reply-status) set-reply-status)
         ((#:add-header) add-header)
         ((#:add-content) add-content)
         ((#:add-formatted) add-formatted)
         ((#:add-direct-writer) add-direct-writer)
         ((#:rechunk-content) rechunk-content)
         ((#:inhibit-content!) inhibit-content!)
         ((#:entity-length) entity-length)
         ((#:send!) send!)
         (else (error "unrecognized command:" command)))
       args))))

;; Return a command-delegating closure capable of writing a properly formatted
;; HTTP 1.0 response to @var{out-port}.  Optional arg @var{status-box} is a
;; list whose @sc{car} is set to the numeric status code given to a
;; @code{#:set-reply-status} command.  If @var{status-box} has length of two
;; or more, its @sc{cadr} is set to the content-length on @code{#:send-reply}.
;; A content-length value of @code{#f} means there have been no calls to
;; @code{#:add-content}.  The commands and their args are:
;;
;; @table @code
;; @findex reset-protocol!
;; @item #:reset-protocol!
;; Reset internal state, including reply status, headers and content.
;; This is called automatically by @code{#:send-reply}.
;;
;; @findex set-reply-status
;; @item #:set-reply-status @var{number} @var{message}
;; Set the reply status.  @var{message} is a short string.
;;
;; @findex set-reply-status:success
;; @item #:set-reply-status:success
;; This is equivalent to @code{#:set-reply-status 200 "OK"}.
;;
;; @findex add-header
;; @item #:add-header @var{name} @var{value}
;; @var{name} may be @code{#f}, @code{#t}, a string, symbol or keyword.
;; @var{value} is a string.  If @var{name} is @code{#f} or @code{#t},
;; @var{value} is taken to be a pre-formatted string, "A: B" or "A:
;; B\r\n", respectively.  If @var{name} is not a boolean, @var{value}
;; may also be a tree of strings or a number.
;;
;; @findex add-content
;; @item #:add-content [@var{tree} @dots{}]
;; @var{tree} may be a string, a nested list of strings, or a series of such.
;; Subsequent calls to @code{#:add-content} append their trees to the
;; collected content tree thus far.
;;
;; @findex add-formatted
;; @item #:add-formatted @var{format-string} [@var{args} @dots{}]
;; @var{format-string} may be @code{#f} to mean @code{~S}, @code{#t} to
;; mean @code{~A}, or a normal format string.  It is used to format
;; @var{args}, and the result passed to @code{#:add-content}.
;;
;; @findex add-direct-writer
;; @item #:add-direct-writer @var{len} @var{write}
;; @var{len} is the number of bytes that procedure @var{write} will
;; output to its arg, @var{out-port} (passed back), when called during
;; @code{#:send-reply}.  This is to allow sendfile(2) and related
;; hackery.
;;
;; @findex content-length
;; @item #:content-length
;; Return the total number of bytes in the content added thus far.
;;
;; @findex rechunk-content
;; @item #:rechunk-content @var{chunk}
;; @var{chunk} may be @code{#f}, in which case a list of the string
;; lengths collected thus far is returned; @code{#t} which means to use
;; the content length as the chunk size (effectively producing one
;; chunk); or a number specifying the maximum size of a chunk.  The
;; return value is a list of the chunk sizes.
;;
;; It is an error to use @code{#:rechunk-content} with a non-@code{#f}
;; @var{chunk} in the presence of a previous @code{#:add-direct-writer}.
;;
;; @findex inhibit-content!
;; @item #:inhibit-content! @var{bool}
;; Non-@code{#f} @var{bool} arranges for @code{#:send-reply} (below) to
;; compute content length and add the appropriate header, as usual, but
;; no content is actually sent.  This is useful, e.g., when answering a
;; @code{HEAD} request.  If @var{bool} is @code{#f}, @code{#:send-reply}
;; acts normally (i.e., sends both headers and content).
;;
;; @findex send-reply
;; @item #:send-reply [close]
;; Send the properly formatted response to @var{out-port}, and reset
;; all internal state (status reset, content discarded, etc).  It is
;; an error to invoke @code{#:send-reply} without having first set
;; the reply status.
;;
;; Optional arg @var{close} means do a @code{shutdown} on @var{out-port}
;; using @var{close} --- directly, if an integer, or called with no
;; arguments, if a thunk --- as the shutdown @code{how} argument.
;; (Note: If @var{out-port} is not a socket, this does nothing silently.)
;; @xref{Network Sockets and Communication,,,guile}.
;;
;; If @var{close} is specified, the closure forgets about @var{out-port}
;; internally; it is an error to call other mouthpiece commands,
;; subsequently.
;; @end table
;;
(define* (mouthpiece out-port #:optional (status-box #f) (style #f))
  (define protocol-version '(1 . 0))

  ;; normalize
  (or (list? status-box)
      (set! status-box #f))
  (or style (set! style http-ish))

  (let ((partial #f))

    (define (reset-protocol!)
      (set! partial (compose-response (or (gethostname) "localhost")
                                      #:protocol-version protocol-version
                                      #:style style)))

    (define (set-reply-status:success)
      (partial #:set-reply-status 200 "OK"))

    (define* (send-reply #:optional close)
      (let ((rv (partial #:send! out-port)))
        (force-output out-port)
        (reset-protocol!)
        (cond (close
               (and (eq? 'socket (port-filename out-port))
                    (shutdown out-port (if (thunk? close)
                                           (close)
                                           close)))
               (set! out-port #f)))
        (cond (status-box
               (set-car! status-box (car rv))
               (cond ((pair? (cdr status-box))
                      (set-car! (cdr status-box) (cadr rv))))))
        rv))

    (reset-protocol!)
    ;; rv
    (lambda (command . args)
      (or (keyword? command) (error "command not a keyword:" command))
      (if (memq command '(#:set-reply-status
                          #:add-header
                          #:add-content
                          #:add-formatted
                          #:add-direct-writer
                          #:content-length
                          #:rechunk-content
                          #:inhibit-content!))
          (apply partial command args)
          (apply
           (case command
             ((#:reset-protocol!) reset-protocol!)
             ((#:set-reply-status:success) set-reply-status:success)
             ((#:send-reply) send-reply)
             (else (error "unrecognized command:" command)))
           args)))))

;;; (www server-utils answer) ends here
