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
            receive-request
            hqf<-upath alist<-query)
  #:use-module ((www crlf) #:select (read-three-part-line
                                     read-headers/get-body))
  #:use-module ((www url-coding) #:select (url-coding:decode))
  #:use-module ((srfi srfi-2) #:select (and-let*))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (substring/shared
                                         string-index
                                         string-tokenize))
  #:use-module ((srfi srfi-14) #:select (char-set
                                         char-set-complement))
  #:use-module ((ice-9 regex) #:select (match:substring))
  #:use-module (ice-9 optargs))

(define (read-request port s2s)
  (let-values (((method upath pvers) (read-three-part-line port)))
    (let-values (((headers get-body) (read-headers/get-body port s2s)))
      (values method upath pvers headers get-body))))

(define PV-RX (make-regexp "HTTP/([0-9]+)[.]([0-9]+)"))

;; Return a request object read from @var{port}.
;; Use @var{s2s} (defaults to @code{string-titlecase}) to normalize
;; the header names.
;; With @code{#:s2s string-downcase}, for instance, you would
;; see @code{(host . "example.com")} in the @code{headers} field
;; of the request object.
;;
;; Keyword arg @var{style} is an object specifying the syntax of the
;; initial (non-body) portion.  By default, @var{parse} expects a normal
;; HTTP 1.1 request message as per RFC 2616.
;;
(define* (receive-request port #:key (s2s string-titlecase) (style #f))
  (let ((rd-req (or (and style (vector-ref style 0))
                    read-request)))
    (let-values (((method upath pvers headers get-body) (rd-req port s2s)))
      (make-request (string->symbol method)
                    upath
                    (cond ((regexp-exec PV-RX pvers)
                           => (lambda (m)
                                (define (num n)
                                  (string->number (match:substring m n)))
                                (cons (num 1) (num 2))))
                          (else
                           '(1 . 0)))
                    headers
                    get-body))))

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
;; was specified to @code{receive-request}.
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

;;; (www server-utils parse-request) ends here
