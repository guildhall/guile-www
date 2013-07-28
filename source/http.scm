;;; (www http) --- HTTP client library

;; Copyright (C) 2008, 2009, 2011, 2012 Thien-Thi Nguyen
;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.
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

;;; Code:

(define-module (www http)
  #:export (protocol-version
            http:message-version
            http:message-status-code
            http:message-status-text
            http:message-status-ok?
            http:status-ok?
            http:message-body
            http:message-headers
            http:message-header
            http:post-form
            http:connect
            http:open
            send-request
            receive-response
            http:request)
  #:use-module ((www crlf) #:select (read-three-part-line
                                     hsym-proc
                                     read-headers/get-body
                                     out!))
  #:autoload (www post) (formatted-form-for-http:post-form)
  #:use-module ((srfi srfi-1) #:select (car+cdr))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((www url) #:select (url:host
                                    url:port
                                    url:path))
  #:use-module (ice-9 optargs))

(define (fs s . args)
  (apply simple-format #f s args))


;;; Variables that affect HTTP usage.

;; A pair of integers representing the major and minor
;; portions of the protocol version this module should support.
;; The default value is @code{(1 . 0)}.  Users:
;;
;; @example
;; http:request
;; http:post-form   ; via http:request
;; @end example
;;
;;-category: fluid
;;
(define protocol-version (make-fluid))
(fluid-set! protocol-version '(1 . 0))

;; An HTTP message is represented by a vector:
;;      #(VERSION STATUS-CODE STATUS-TEXT HEADERS BODY)
;;
;; VERSION and STATUS-TEXT are strings.  STATUS-CODE is a number
;; if through ‘receive-response’, a string if through ‘http:request’.
;; HEADERS is an alist with symbolic keys and string values.
;; BODY is #f, a "get body" procedure, a string, a u8 vector, etc.

(define (make-message version statcode stattext headers body)
  (vector version statcode stattext headers body))

;;;; HTTP status predicates.

;; Return the HTTP version in use in HTTP message @var{msg}.
(define (http:message-version msg)     (vector-ref msg 0))
;; Return the status code returned in HTTP message @var{msg}.
(define (http:message-status-code msg) (vector-ref msg 1))
;; Return the text of the status line from HTTP message @var{msg}.
(define (http:message-status-text msg) (vector-ref msg 2))
;; Return @code{#t} iff status code of @var{msg}
;; indicates a successful request.
(define (http:message-status-ok? msg)
  (http:status-ok? (http:message-status-code msg)))

;; Return @code{#t} iff @var{status} (string or integer) is @code{2xx}.
;;
(define (http:status-ok? status)
  (= 2 (quotient (if (string? status)
                     (string->number status)
                     status)
                 100)))

;; Return the body of the HTTP message @var{msg}.
(define (http:message-body msg) (vector-ref msg 4))

;; {HTTP response headers functions}
;;
;; An HTTP message header is represented here by a pair.  The @sc{car}
;; is a symbol representing the header name, and the @sc{cdr} is a string
;; containing the header text.  E.g.:
;;
;; @example
;; ((date . "Thu, 29 May 1997 23:48:27 GMT")
;;  (server . "NCSA/1.5.1")
;;  (last-modified . "Tue, 06 May 1997 18:32:03 GMT")
;;  (content-type . "text/html")
;;  (content-length . "8097"))
;; @end example
;;
;; @noindent
;; Note: these symbols are all lowercase, although the original headers
;; were mixed-case.

;; Return a list of the headers from HTTP message @var{msg}.
;;
(define (http:message-headers msg) (vector-ref msg 3))

;; Return the header field named @var{header} from HTTP message @var{msg},
;; or @code{#f} if no such header is present in the message.
;;
(define (http:message-header header msg)
  (assq-ref (http:message-headers msg) header))

(define (msg-headers! msg alist) (vector-set! msg 3 alist))
(define (msg-body! msg string)   (vector-set! msg 4 string))

(define (msg-string-rcode! msg)
  (vector-set! msg 1 (fs "~A" (vector-ref msg 1))))


;; Submit an http request using the @code{POST} method on the @var{url}.
;; @var{extra-headers} is a list of extra headers, each a string of form
;; "@var{name}: @var{value} @dots{}".
;;
;; The "Content-Type" and "Host" headers are sent automatically and do
;; not need to be specified.  @var{fields} is a list of elements of the
;; form @code{(@var{fkey} . @var{fvalue})}, where @var{fkey} is a symbol
;; and @var{fvalue} is normally a string.
;;
;; @var{fvalue} can also be a list of file-upload specifications, each
;; of which has the form @code{(@var{source} @var{name} @var{mime-type}
;; @var{transfer-encoding})}.  @var{source} can be a string or a thunk
;; that returns a string.
;;
;; The rest of the elements are strings or symbols: @var{name} is the
;; filename (only the non-directory part is used); @var{mime-type} is a
;; type/subtype pair such as "image/jpeg", or @code{#f} to mean
;; "text/plain".  @var{transfer-encoding} is one of the tokens specified
;; by RFC 1521, or @code{#f} to mean "binary".  File-upload spec
;; elements with invalid types result in a "bad upload spec" error prior
;; to the http request.
;;
;; Note that @var{source} is used directly without further processing;
;; it is the caller's responsibility to ensure that the MIME type and
;; transfer encoding specified describe @var{source} accurately.
;;
;; If there are no file-upload specifications in @var{fields}, the
;; @code{Content-Type} is @code{application/x-www-form-urlencoded},
;; and furthermore all the @var{fkey} and @var{fvalue} are transformed
;; by @code{url-coding:encode} (@pxref{url-coding}) with the additional
;; reserved characters @code{#\&} (ampersand) and @code{#\=} (equal sign).
;;
;; Otherwise, the @code{Content-Type} is @code{multipart/form-data},
;; with each field in @var{fields} formatted as a MIME sub-part.
;;
(define (http:post-form url extra-headers fields)
  (let-values (((headers body) (formatted-form-for-http:post-form fields)))
    (http:request
     'POST url
     (append headers extra-headers)
     body)))

;; Connection-oriented functions:

;; Return a TCP stream socket connected to the location specified by
;; protocol @var{proto}, @var{addrfam} and @var{address}.  @var{proto}
;; is @code{PF_INET} or @code{PF_UNIX}, and the other args take
;; corresponding forms:
;;
;; @table @code
;; @item PF_INET
;; @code{(AF_INET @var{ipaddr} @var{portno})}, where @var{ipaddr} is
;; an integer.  Use @code{(car (hostent:addr-list (gethost @var{host})))}
;; to compute the ipaddr of @var{host} (a string).
;;
;; @item PF_UNIX
;; @code{(AF_UNIX @var{filename})}, made, for example, by@*
;; @code{(list AF_UNIX "/tmp/foo-control")}.
;; @end table
;;
;; Note that @code{PF_foo} and @code{AF_foo} are names of variables
;; that have constant values, not symbols.
;;
(define (http:connect proto addrfam address . address-rest)
  (let ((sock (socket proto SOCK_STREAM 0)))
    (apply connect sock addrfam address address-rest)
    sock))

;; Return an HTTP connection (a socket) to @var{host} (a string) on TCP
;; port @var{port} (default 80 if unspecified).
;;
(define* (http:open host #:optional (port 80))
  (http:connect PF_INET AF_INET
                (car (hostent:addr-list (gethost host)))
                port))

;; Submit to socket @var{sock} an HTTP request using @var{method}
;; (a symbol) and @var{url}, an object returned by @code{url:parse},
;; forming the message with additional @var{headers}, a list of strings,
;; each of which should have one of the forms:
;;
;; @example
;; NAME ": " VALUE
;; NAME ": " VALUE CRLF
;; @end example
;;
;; @noindent
;; and @var{body}, which may be @code{#f} (which means no body),
;; a string or list of them, a @code{u8} vector or list of them,
;; or a procedure @var{m} which manages the transmission of the
;; body data by supporting the @dfn{body transmission protocol}.
;; This means @var{m} takes one arg, @var{command} (symbol):
;;
;; @table @code
;; @item content-length
;; This is called if the transfer is not ``chunked'' (see below).
;; @var{m} returns the total length (in bytes) of the data.
;;
;; @item next-chunk
;; Return two values: the length (in bytes) of the next chunk of
;; data to send, and either a string, or a procedure @var{w} that
;; does the actual writing to its port arg (which will be @var{sock}).
;; If there is no more data, both values should be @code{#f}, i.e.,
;; @var{m} should return @code{(values #f #f)}.
;; @end table
;;
;; If @code{flags} contains the symbol @code{chunked}, send the body
;; with ``chunked'' @code{Transfer-Encoding}.  Otherwise, compute and
;; add to the headers its total @code{Content-Length}.
;;
;; If @code{flags} contains the symbol @code{close},
;; add @code{Connection: close} to the headers.
;;
;; The @var{protocol-version} is a pair specifying the major and minor
;; version numbers of HTTP to send as.  It defaults to @code{(1 . 1)}.
;; For HTTP 1.x (x >= 1), automatically add @code{chunked} to @var{flags}
;; as well as the headers:
;;
;; @example
;; TE: trailers
;; Connection: TE
;; @end example
;;
;; Return an unspecified object @var{pending} that can be passed
;; to @code{receive-response}.
;;
(define* (send-request sock method url #:key
                       (headers '())
                       body
                       (flags '())
                       (protocol-version '(1 . 1)))

  (define (chunked!)
    (set! flags (cons 'chunked flags)))

  (let-values (((major minor) (car+cdr protocol-version)))

    (define (h+! . more)
      (set! headers (append more headers)))

    ;; Pile on some version-specific boilerplate; auto-enable chunking.
    (cond
     ;; 1.x, x>0
     ((and (= 1 major) (positive? minor))
      (chunked!)
      (h+! "TE: trailers"
           "Connection: TE")))

    ;; Spew!
    (out! sock (url:host url)
          method
          (fs "/~A" (or (url:path url) ""))
          (fs "HTTP/~A.~A" major minor)
          headers body
          flags))

  ;; rv: pending
  (lambda (s2s)
    (let-values (((rvers rcode rtext) (read-three-part-line sock)))
      (let ((numeric (string->number rcode)))
        (let-values (((headers body) (read-headers/get-body
                                      sock s2s (lambda ()
                                                 (values method numeric)))))
          (make-message rvers numeric rtext headers body))))))

;; Receive the @var{pending} (from @code{send-request}) response.
;; Return an HTTP message object.  The header names are symbols made
;; by @code{(string->symbol (s2s @var{orig}))}, where @var{s2s}
;; defaults to @code{string-titlecase}.  The status code is a 3-digit
;; integer.  The body of the message may be @code{#f} if there is
;; no body possible (per HTTP).  Otherwise, its value depends on
;; @var{intervene} and @var{flags}.
;;
;; @itemize
;; @item If @var{intervene} is specified, it should be a procedure
;; that takes two args, @var{hget} and @var{flags} and returns
;; two values, @var{new-headers} and @var{new-flags}.  It is called
;; after the headers are parsed but before the body is received so
;; that its returned values may influence the body processing.
;;
;; @var{hget} is a procedure that takes one arg @var{sel}.
;; @table @asis
;; @item @code{#f}
;; Return the headers (alist).
;; @item @code{#t}
;; Return the name normalization procedure
;; (involving @var{s2s}, described above).
;; @item @var{string}
;; Normalize it; return the associated header value.
;; @item @var{symbol}
;; Return the associated header value.
;; @end table
;;
;; A @code{#f} value for @var{new-headers} means ``don't change
;; the headers''.  Likewise, for @var{new-flags}.  Otherwise, the
;; respective items are replaced (@strong{NB}: not just added!).
;;
;; @item If @var{flags} is null (the default), the body is a string.
;;
;; @item If @var{flags} contains the symbol @code{u8}, the body is
;; a @code{u8} vector.
;;
;; @item If @var{flags} contains the symbol @code{custom}, the
;; following item in @var{flags} should be a thunk that returns
;; four values (all procedures)
;; that support the @dfn{chunk transfer protocol}.  These are:
;;
;; @table @code
;; @item (mkx @var{len})
;; Create and return a container capable of holding @var{len} bytes.
;; @item (r! @var{x} @var{sock})
;; Fill @var{x}, reading from @var{sock}.
;; Return the number of bytes read (positive integer), or zero on EOF.
;; @item (cat-r @var{list})
;; Return a new container formed by reversing @var{list}
;; and concatenating its elements.
;; @item (subseq @var{x} @var{len})
;; Return a new container that holds the first @var{len}
;; bytes of container @var{x}.
;; @end table
;;
;; The message body is a single container, either constructed from
;; multiple exact chunks (``chunked'' @code{Transfer-Encoding}),
;; or read in one swoop (if @code{Content-Length} is given),
;; or from multiple inexact chunks (the default).
;;
;; For backward compatability, instead of a thunk returning four values,
;; you can also specify the four values directly.
;; @strong{NB: Support for specifying four values directly will
;; NO LONGER BE AVAILABLE after 2013-05-15.}@footnote{Out of an explicit
;; @code{call-with-values} context, Guile 2.x will silently discard all
;; values following the ``expected number'' (one, in this case):
;;
;; @example
;; (list 'custom (values P1 P2 P3 P4))
;; @result{} (custom P1)
;; @end example
;;
;; This is apparently allowed under R5RS and R6RS.}
;;
;; @item If @var{flags} contains the symbol @code{no-cat}, then all
;; multi-chunk transfers are not ``concatenated''; instead, the message
;; body is the list of chunk data (string, @code{u8} or @code{custom}),
;; in order of reception.
;; @end itemize
;;
(define* (receive-response pending #:key
                           (s2s string-titlecase)
                           intervene
                           (flags '()))
  (let ((msg (pending s2s)))

    (define (h-maybe! x)
      (and x (msg-headers! msg x)))

    (define (intervene!)

      (define hget
        (let ((headers (http:message-headers msg))
              (hsym (hsym-proc s2s)))
          ;; hget
          (lambda (sel)
            (cond ((not sel) headers)
                  ((eq? #t sel) hsym)
                  (else (assq-ref headers (if (string? sel)
                                              (hsym sel)
                                              sel)))))))

      (let-values (((new-headers new-flags) (intervene hget flags)))
        (h-maybe! new-headers)
        (and new-flags (set! flags new-flags))))

    (and=> (http:message-body msg)
           (lambda (get-body)
             (and intervene (intervene!))
             (let-values (((new-headers s) (get-body flags)))
               (h-maybe! new-headers)
               (msg-body! msg s))))
    msg))

;; Submit an HTTP request using @var{method} and @var{url}, wait
;; for a response, and return the response as an HTTP message object.
;; The field types and values of this message object are as described in
;; @code{receive-response}, with two exceptions (for backward compatability):
;; the status code is a string; the header names are symbols, all lower-case.
;;
;; @var{method} is the symbolic name of some HTTP method, e.g.,
;; @code{GET} or @code{POST}.  It may also be a string.
;; @var{url} is a url object returned by @code{url:parse}.  Optional
;; args @var{headers} and @var{body} are lists of strings that comprise
;; the lines of an HTTP message.  The header strings should not end with
;; @samp{CR} or @samp{LF} or @samp{CRLF}; @code{http:request} handles
;; that.  Also, the Content-Length header and Host header are calculated
;; automatically and should not be supplied.  Here are two examples:
;;
;; @example
;; (http:request 'GET parsed-url
;;   (list "User-Agent: Anonymous/0.1"
;;         "Content-Type: text/plain"))
;;
;; (http:request 'POST parsed-url
;;   (list "User-Agent: Fred/0.1"
;;         "Content-Type: application/x-www-form-urlencoded")
;;   (list "search=Gosper"
;;         "&case=no"
;;         "&max_hits=50"))
;; @end example
;;
;; In the second example, the @code{Content-Length} header is
;; computed to have value 33 (the sum of 13, 8 and 12).
;;
(define* (http:request method url #:optional (headers '()) (body '()))
  (cond ((symbol? method))
        ;; Handle string ‘method’ for backward compatability.
        ((string? method) (set! method (string->symbol method)))
        (else (error "bad method:" method)))
  (let* ((sock (http:open (url:host url) (or (url:port url)
                                             80)))
         (get (send-request sock method url
                            #:headers headers
                            #:body body
                            #:protocol-version (fluid-ref protocol-version)))
         (ans (receive-response get
                                ;; Backward comptability; blech!
                                #:s2s string-downcase)))
    ;; Backward compatability; blech!
    (msg-string-rcode! ans)
    ;; FIXME: what about keepalives?
    (close-port sock)
    ans))

;;; (www http) ends here
