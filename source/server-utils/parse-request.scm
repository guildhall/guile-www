;;; (www server-utils parse-request) --- Read HTTP first line and headers

;; Copyright (C) 2009, 2012 Thien-Thi Nguyen
;; Copyright (C) 2004 Free Software Foundation, Inc.
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

(define-module (www server-utils parse-request)
  #:export (request?
            request-method
            request-upath
            request-protocol-version
            request-headers
            request-body
            read-first-line
            hqf<-upath alist<-query
            read-headers skip-headers read-body)
  #:use-module ((www crlf) #:select (read-through-CRLF
                                     read-three-part-line
                                     (read-headers . crlf:read-headers)
                                     read-characters))
  #:autoload (www url-coding) (url-coding:decode)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (ice-9 rw)
  #:use-module (ice-9 and-let-star))

;; A request object has five fields.
;;
;; @table @code
;; @item method
;; A symbol, such as @code{GET}.
;;
;; @item upath
;; A string.  You can use @code{hqf<-upath}
;; and @code{alist<-query} to break this down further.
;;
;; @item protocol-version
;; A pair of integers indicating the protocol version.
;; For example, @code{(1 . 1)} corresponds to HTTP 1.1.
;;
;; @item headers
;; A list of pairs @code{(@var{name} . @var{value})}, aka alist,
;; where @var{name} is a symbol and @var{value} is a string.
;; How @var{name} is normalized depends on which @var{s2s}
;; was specified to @code{parse-request-proc}.
;;
;; @item body
;; Either @code{#f} or a procedure @var{get-body}.
;; This should be called with one arg, @var{flags},
;; to retrieve the request body.
;; @c FIXME: Factor into its own node; xref directly.
;; @xref{http}, procedure @code{receive-response},
;; for @var{flags} documentation.
;; @end table
;;
(define-record-type request
    (make-request method upath protocol-version headers body)
    request?
  (method request-method)
  (upath request-upath)
  (protocol-version request-protocol-version)
  (headers request-headers)
  (body request-body))

;; {request procs}
;;
;; @deffn {Procedure} request? obj
;; Return @code{#t} if @var{obj} is a request object.
;; @end deffn
;;
;; @deffn {Procedure} request-method req
;; @deffnx {Procedure} request-upath req
;; @deffnx {Procedure} request-protocol-version req
;; @deffnx {Procedure} request-headers req
;; @deffnx {Procedure} request-body req
;; Return the respective field of request object @var{req}.
;; @end deffn

(define-macro (false-if-eof . body)
  `(catch 'unexpected-eof
          (lambda ()
            ,@body)
          (lambda ignored
            #f)))

;; Parse the first line of the HTTP message from input @var{port} and
;; return a list of the method, URL path and HTTP version indicator, or
;; @code{#f} if the line ends prematurely or is otherwise malformed.  A
;; successful parse consumes the trailing @samp{CRLF} of the line as
;; well.  The method is a symbol with its constituent characters
;; upcased, such as @code{GET}; the other elements are strings.  If the
;; first line is missing the HTTP version, @code{parse-first-line}
;; returns the default "HTTP/1.0".
;;
(define (read-first-line port)
  (false-if-eof
   (let-values (((method url vers) (read-three-part-line port)))
     (list (string->symbol (string-upcase method))
           url
           (if (string-null? vers)
               "HTTP/1.0"
               vers)))))

;; Parse string @var{upath} and return three values representing
;; its hierarchy, query and fragment components.
;; If a component is missing, its value is @code{#f}.
;;
;; @example
;; (hqf<-upath "/aa/bb/cc?def=xyz&hmm#frag")
;; @result{} "/aa/bb/cc"
;; @result{} "def=xyz&hmm"
;; @result{} "frag"
;;
;; (hqf<-upath "/aa/bb/cc#fr?ag")
;; @result{} "/aa/bb/cc"
;; @result{} #f
;; @result{} "fr?ag"
;; @end example
;;
(define hqf<-upath
  (let ((question-mark/number-sign (char-set #\? #\#)))
    (define (bit . x)
      (apply substring/shared upath x))
    ;; hqf<-upath
    (lambda (upath)
      (or (and-let* ((one (string-index upath question-mark/number-sign))
                     (h (bit 0 one))
                     (more (1+ one)))
            (and (string-null? h)
                 (set! h #f))
            (cond ((char=? #\# (string-ref upath one))
                   (values h #f (bit more)))
                  ((string-index upath #\# more)
                   => (lambda (two)
                        (values h (bit more two) (bit (1+ two)))))
                  (else
                   (values h (bit more) #f))))
          (values upath #f #f)))))

(define amp-split
  (let ((not-amp-cs (char-set-complement (char-set #\&))))
    (lambda (s)
      (string-tokenize s not-amp-cs))))

;; Parse urlencoded @var{query-string} and return an alist.
;; For each element @code{(@var{name} . @var{value})} of the alist,
;; @var{name} is a string and @var{value} is either @code{#f} or a string.
;;
(define (alist<-query query-string)
  (map (lambda (pair)
         (define (decode . args)
           (url-coding:decode (apply substring/shared pair args)))
         (let ((mid (string-index pair #\=)))
           (cons (if mid (decode 0 mid) (decode 0))
                 (and mid (decode (1+ mid))))))
       (amp-split query-string)))

;; Parse the headers of the HTTP message from input @var{port} and
;; return a list of key/value pairs, or @code{#f} if the message ends
;; prematurely or is otherwise malformed.  Both keys and values are
;; strings.  Values are trimmed of leading and trailing whitespace and
;; may be empty.  Values that span more than one line have their
;; "continuation whitespace" reduced to a single space.  A successful
;; parse consumes the trailing @samp{CRLF} of the header block as well.
;;
(define (read-headers port)
  (catch 'parse-error (lambda ()
                        (crlf:read-headers port identity))
         (lambda ignored
           #f)))

;; Scan without parsing the headers of the HTTP message from input
;; @var{port}, and return the empty list, or @code{#f} if the message
;; ends prematurely.  A successful scan consumes the trailing
;; @samp{CRLF} of the header block as well.
;;
(define (skip-headers port)
  (false-if-eof
   (let loop ()
     (let ((line (read-through-CRLF port)))
       (if (string-null? line)
           '()
           (loop))))))

;; Return a new string of @var{len} bytes with contents
;; read from input @var{port}.
;;
(define (read-body len port)
  (read-characters len port))

;;; (www server-utils parse-request) ends here
