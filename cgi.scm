;;; www/cgi.scm --- Common Gateway Interface support for WWW scripts

;; 	Copyright (C) 1997,2001,2002 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA
;;

;;; Commentary:

;; This module exports the following variables and procedures:
;;    cgi-server-software-type
;;    cgi-server-software-version
;;    cgi-server-hostname
;;    cgi-gateway-interface
;;    cgi-server-protocol-name
;;    cgi-server-protocol-version
;;    cgi-server-port
;;    cgi-request-method
;;    cgi-path-info
;;    cgi-path-translated
;;    cgi-script-name
;;    cgi-query-string
;;    cgi-remote-host
;;    cgi-remote-addr
;;    cgi-authentication-type
;;    cgi-remote-user
;;    cgi-remote-ident
;;    cgi-content-type
;;    cgi-content-length
;;    cgi-http-accept-types
;;    cgi-http-user-agent
;;    cgi-http-cookie
;;   (cgi:init)
;;   (cgi:values name)
;;   (cgi:value name)
;;   (cgi:names)
;;   (cgi:form-data?)
;;   (cgi:uploads name)
;;   (cgi:upload name)
;;   (cgi:cookies name)
;;   (cgi:cookie name)
;;   (cgi:make-cookie value #&key path domain expires secure)

;;; Code:

(define-module (www cgi)
  :use-module (www url)
  :use-module (ice-9 regex)
  :use-module (ice-9 optargs))

(define form-variables '())

(define file-uploads '())

(define cookies '())

;;; CGI environment variables.
;;; Should these all be public?

(define-public cgi-server-software-type #f)
(define-public cgi-server-software-version #f)
(define-public cgi-server-hostname #f)
(define-public cgi-gateway-interface #f)
(define-public cgi-server-protocol-name #f)
(define-public cgi-server-protocol-version #f)
(define-public cgi-server-port #f)
(define-public cgi-request-method #f)
(define-public cgi-path-info #f)
(define-public cgi-path-translated #f)
(define-public cgi-script-name #f)
(define-public cgi-query-string #f)
(define-public cgi-remote-host #f)
(define-public cgi-remote-addr #f)
(define-public cgi-authentication-type #f)
(define-public cgi-remote-user #f)
(define-public cgi-remote-ident #f)
(define-public cgi-content-type #f)
(define-public cgi-content-length #f)
(define-public cgi-http-accept-types #f)
(define-public cgi-http-user-agent #f)
(define-public cgi-http-cookie #f)


;;; CGI high-level interface

;; A typical CGI program will first call (cgi:init) to initialize
;; the environment and read in any data returned from a form.  Form
;; data can be extracted conveniently with these functions:
;;
;; (cgi:values NAME)
;;	Fetch any values associated with NAME found in the form data.
;;	Returned value is a list, even if it contains only one element.
;; (cgi:value NAME)
;;	Fetch only the CAR from (cgi:values NAME).  Convenient for when
;;	you are certain that NAME is associated with only one value.
;; (cgi:uploads NAME)
;;     Fetch any files associated with name. Returns list. Can only be
;;     called once per particular name. Subsequent calls will return
;;     #f. The caller had better hang onto the descriptor, lest the
;;     garbage man wisk it away for good. This is done do minimize the
;;     amount of time the file is resident in memory.
;; (cgi:upload NAME)
;;     Fetch the first file associated with form var NAME. Can only be
;;     called once per NAME, so the called had better be sure that
;;     there is only one file associated with NAME. Use (cgi:uploads
;;     NAME) if you are unsure.
;; (cgi:cookies NAME)
;;     Fetch any cookie values associated with NAME. Return a list
;;     of values in the order they were found in the HTTP header,
;;     which should be the order of most specific to least specific
;;     path associated with the cookie.
;; (cgi:cookies NAME)
;;     Fetch the first cookie value associated with NAME.
;; (cgi:make-cookie NAME VALUE #&key path domain expires secure)
;;     Create a cookie suitable for inclusion into an HTTP response
;;     header. Recognize optional parameters path, doman, expires,
;;     (which should be strings) and secure (which is boolean).

(define-public (cgi:init)
  (init-environment)
  (and cgi-content-length
       (string-ci=? cgi-content-type
		    "application/x-www-form-urlencoded")
       (parse-form (read-raw-form-data)))
  (and cgi-content-length
       (string-ci=? (make-shared-substring cgi-content-type 0 19)
		    "multipart/form-data")
       (parse-form-multipart (read-raw-form-data)))
  (and cgi-query-string
       (parse-form cgi-query-string))
  (and cgi-http-cookie
       (get-cookies)))

(define-public (cgi:values name)
  (assoc-ref form-variables name))

(define-public (cgi:value name)
  ;; syntactic sugar for obtaining just one value from a particular key
  (let ((values (cgi:values name)))
    (and values (car values))))

(define-public (cgi:names) (map car form-variables))

(define-public (cgi:form-data?) (not (null? form-variables)))

(define-public (cgi:uploads name)
  (let ((uploads (assoc-ref file-uploads name)))
    (if uploads (assoc-remove! file-uploads name))
    uploads))

(define-public (cgi:upload name)
  (let ((uploads (cgi:uploads name)))
    (and uploads (car uploads))))

(define-public (cgi:cookies name)
  (assoc-ref cookies name))

(define-public (cgi:cookie name)
  (let ((cookie-values (cgi:cookies name)))
    (and cookie-values (car cookie-values))))

(define-public cgi:make-cookie
  (lambda* (name value #&key path domain expires secure)
           (format #f "Set-Cookie: ~A=~A~A~A~A~A"
                   name value
                   (if (bound? path)
                       (format #f "; path=~A" path) "")
                   (if (bound? domain)
                       (format #f "; domain=~A" domain) "")
                   (if (bound? expires)
                       (format #f "; expires=~A" expires) "")
                   (if (and (bound? secure) secure)
                       "; secure" ""))))



;;; Internal functions.

;; (parse-form DATA): parse DATA as raw form response data of enctype
;;  x-www-form-urlencoded, adding values as necessary to `form-variables'.
;; (parse-form-multipart DATA): parse DATA as raw form response data
;;  of enctype multipart/form-data, adding values as necessary to
;;  'form-variables' and file data to 'file-uploads'.
;; (read-raw-form-data): read in `content-length' bytes from stdin
;; (init-environment): initialize CGI environment from Unix env vars.
;; (get-cookies): initialize the cookie list from cgi-http-cookie.

(define (parse-form raw-data)
  ;; get-name and get-value are used to parse individual `name=value' pairs.
  ;; Values are URL-encoded, so url:decode must be called on each one.
  (define (get-name pair)
    (let ((p (string-index pair #\=)))
      (and p (make-shared-substring pair 0 p))))
  (define (get-value pair)
    (let ((p (string-index pair #\=)))
      (and p (url:decode (make-shared-substring pair (+ p 1))))))
  (for-each (lambda (pair)
	      (let* ((name (get-name pair))
		     (value (get-value pair))
		     (old-value (cgi:values name)))
		(set! form-variables
		      (assoc-set! form-variables
				  name
				  (cons value (or old-value '()))))))
	    (separate-fields-discarding-char #\& raw-data)))


(define (parse-form-multipart raw-data)
  (let* ((boundary (format #f "--~A"
                           (match:substring
                            (string-match "boundary=(.*)$" cgi-content-type)
                            1)))
	 (boundary-len (string-length boundary))
	 (name-exp (make-regexp "name=\"([^\"]*)\""))
	 (filename-exp (make-regexp "filename=\"([^\"]*)\""))
	 (type-exp (make-regexp "Content-Type: (.*)\r\n"))
	 (value-exp (make-regexp "\r\n\r\n")))
    (define (get-pair raw-data)
      (define (get-segment str)
	(define (find-bound str)
	  (define (find-bound-h str n)
	    (let ((n-str (string-length str)))
	      (if (< n-str boundary-len)
		  #f
		  (if (string=? boundary (make-shared-substring
                                          str 0 boundary-len))
		      n
		      (find-bound-h (make-shared-substring str 1 n-str)
                                    (+ n 1))))))
	  (find-bound-h str 0))
	(let* ((seg-start (find-bound str))
	       (seg-length (find-bound (make-shared-substring
                                        str (+ seg-start boundary-len)
                                        (string-length str)))))
	  (if (and seg-start seg-length)
	      (cons (make-shared-substring
                     str (+ seg-start boundary-len)
                     (+ seg-start seg-length boundary-len -2))
		    (make-shared-substring
                     str (+ seg-start seg-length boundary-len)
                     (string-length str)))
	      #f)))
      (let ((segment-pair (get-segment raw-data)))
	(if segment-pair
	    (let* ((segment (car segment-pair))
		   (name-match (regexp-exec name-exp segment))
		   (filename-match (regexp-exec filename-exp segment))
		   (type-match (regexp-exec type-exp segment))
		   (value-match (regexp-exec value-exp segment)))
	      (if (and name-match value-match)
		  (if (and filename-match type-match)
		      (let* ((name (match:substring name-match 1))
			     (value (match:substring filename-match 1))
			     (old-value (cgi:values name))
			     (file-data (match:suffix value-match))
			     (old-file-data (assoc-ref file-uploads name)))
			(set! form-variables
			      (assoc-set! form-variables name
					  (cons value (or old-value '()))))
			(set! file-uploads
			      (assoc-set! file-uploads name
					  (cons file-data (or old-file-data
                                                              '())))))
		      (let* ((name (match:substring name-match 1))
			     (value (match:suffix value-match))
			     (old-value (cgi:values name)))
			(set! form-variables
			      (assoc-set! form-variables name
					  (cons value (or old-value '())))))))
	      (get-pair (cdr segment-pair))))))
    (get-pair raw-data)))

(define (read-raw-form-data)
  (and cgi-content-length (read-n-chars cgi-content-length)))

(define (init-environment)

  ;; SERVER_SOFTWARE format: name/version
  (let ((server-software (getenv "SERVER_SOFTWARE")))
    (if server-software
	(let ((slash (string-index server-software #\/)))
	  (set! cgi-server-software-type    (substring server-software
                                                       0 slash))
	  (set! cgi-server-software-version (substring server-software
                                                       (1+ slash))))))

  (set! cgi-server-hostname	   (getenv "SERVER_NAME"))
  (set! cgi-gateway-interface	   (getenv "GATEWAY_INTERFACE")) ;"CGI/revision"

  (let* ((server-protocol (getenv "SERVER_PROTOCOL")))
    (if server-protocol
	(let ((slash (string-index server-protocol #\/)))
	  (set! cgi-server-protocol-name     (substring server-protocol
                                                        0 slash))
	  (set! cgi-server-protocol-version  (substring server-protocol
                                                        (1+ slash))))))

  (let ((port (getenv "SERVER_PORT")))
    (set! cgi-server-port (and port (string->number port))))

  (set! cgi-request-method	   (getenv "REQUEST_METHOD"))
  (set! cgi-path-info		   (getenv "PATH_INFO"))
  (set! cgi-path-translated	   (getenv "PATH_TRANSLATED"))
  (set! cgi-script-name		   (getenv "SCRIPT_NAME"))
  (set! cgi-remote-host		   (getenv "REMOTE_HOST"))
  (set! cgi-remote-addr		   (getenv "REMOTE_ADDR"))
  (set! cgi-authentication-type	   (getenv "AUTH_TYPE"))
  (set! cgi-remote-user		   (getenv "REMOTE_USER"))
  (set! cgi-remote-ident	   (getenv "REMOTE_IDENT"))
  (set! cgi-content-type	   (getenv "CONTENT_TYPE"))
  (set! cgi-query-string	   (getenv "QUERY_STRING"))

  (and cgi-query-string
       (string-null? cgi-query-string)
       (set! cgi-query-string #f))

  (let ((contlen (getenv "CONTENT_LENGTH")))
    (set! cgi-content-length (and contlen (string->number contlen))))

  ;; HTTP_ACCEPT is a list of MIME types separated by commas.
  (let ((types (getenv "HTTP_ACCEPT")))
    (set! cgi-http-accept-types
	  (and types (separate-fields-discarding-char #\, types))))

  ;; HTTP_USER_AGENT format: software/version library/version.
  (set! cgi-http-user-agent		   (getenv "HTTP_USER_AGENT"))
  (set! cgi-http-cookie                    (getenv "HTTP_COOKIE")))

;;; Seting up the cookies
(define (get-cookies)
  (let ((pair-exp (make-regexp "([^=; \t\n]+)=([^=; \t\n]+)")))
    (define (get-pair str)
      (let ((pair-match (regexp-exec pair-exp str)))
	(if (not pair-match) '()
	    (let ((name (match:substring pair-match 1))
		  (value (match:substring pair-match 2)))
	      (if (and name value)
		  (set! cookies
			(assoc-set! cookies name
				    (append (or (cgi:cookies name) '())
                                            (list value)))))
	      (get-pair (match:suffix pair-match))))))
    (get-pair cgi-http-cookie)))


;;; System I/O and low-level stuff.

(define (read-n-chars num . port-arg)
  (let ((p (if (null? port-arg)
	       (current-input-port)
	       (car port-arg)))
	(s (make-string num)))
    (do ((i   0              (+ i 1))
	 (ch  (read-char p)  (read-char p)))
	((or (>= i num) (eof-object? ch)) s)
      (string-set! s i ch))))

;; This is defined in (ice-9 string-fun), but the interface is
;; weird, the semantics perverse, and it doesn't work.  We use
;; a working copy here.
(define (separate-fields-discarding-char ch str)
  (let loop ((fields '())
             (str str))
    (let ((pos (string-rindex str ch)))
      (if pos
	  (loop (cons (make-shared-substring str (+ 1 pos)) fields)
		(make-shared-substring str 0 pos))
	  (cons str fields)))))

;;; www/cgi.scm ends here
