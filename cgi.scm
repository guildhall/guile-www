;;; www/cgi.scm --- Common Gateway Interface support for WWW scripts

;;	Copyright (C) 1997,2001,02,03,2004 Free Software Foundation, Inc.
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

;;; Commentary:

;; The (www cgi) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www cgi)
  #:use-module (www url-coding)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 optargs-kw))

(define subs make-shared-substring)

(define form-variables '())

(define file-uploads '())

(define cookies '())

;;; CGI environment variables.

(define *env-alist*
  (let ((server-sw-info
         (delay (and=> (getenv "SERVER_SOFTWARE")
                       (lambda (sw) (list sw (string-index sw #\/))))))

        (server-pr-info
         (delay (and=> (getenv "SERVER_PROTOCOL")
                       (lambda (pr) (list pr (string-index pr #\/)))))))
    `(;; simple stuff
      (server-hostname . ,(delay (getenv "SERVER_NAME")))
      (gateway-interface . ,(delay (getenv "GATEWAY_INTERFACE")))
      (server-port . ,(delay (and=> (getenv "SERVER_PORT") string->number)))
      (request-method . ,(delay (getenv "REQUEST_METHOD")))
      (path-info . ,(delay (getenv "PATH_INFO")))
      (path-translated . ,(delay (getenv "PATH_TRANSLATED")))
      (script-name . ,(delay (getenv "SCRIPT_NAME")))
      (query-string . ,(delay (getenv "QUERY_STRING")))
      (remote-host . ,(delay (getenv "REMOTE_HOST")))
      (remote-addr . ,(delay (getenv "REMOTE_ADDR")))
      (authentication-type . ,(delay (getenv "AUTH_TYPE")))
      (remote-user . ,(delay (getenv "REMOTE_USER")))
      (remote-ident . ,(delay (getenv "REMOTE_IDENT")))
      (content-type . ,(delay (getenv "CONTENT_TYPE")))
      (content-length . ,(delay (or (and=> (getenv "CONTENT_LENGTH")
                                           string->number)
                                    0)))
      (http-user-agent . ,(delay (getenv "HTTP_USER_AGENT")))
      (http-cookie . ,(delay (getenv "HTTP_COOKIE")))
      ;; complex stuff
      (server-software-type
       . ,(delay (apply-to-args (force server-sw-info)
                                (lambda (sw slash)
                                  (if slash
                                      (subs sw 0 slash)
                                      sw)))))
      (server-software-version
       . ,(delay (apply-to-args (force server-sw-info)
                                (lambda (sw slash)
                                  (and slash (subs sw (1+ slash)))))))
      (server-protocol-name
       . ,(delay (apply-to-args (force server-pr-info)
                                (lambda (pr slash)
                                  (subs pr 0 slash)))))
      (server-protocol-version
       . ,(delay (apply-to-args (force server-pr-info)
                                (lambda (pr slash)
                                  (subs pr (1+ slash))))))
      (http-accept-types
       . ,(delay (and=> (getenv "HTTP_ACCEPT")
                        (lambda (types)
                          (map (lambda (s)
                                 (if (char=? #\space (string-ref s 0))
                                     (subs s 1)
                                     s))
                               (separate-fields-discarding-char
                                #\, types)))))))))

(define (env-look key)                  ; may return #f
  (and=> (or (assq key *env-alist*)
             (error "unrecognized key:" key))
         (lambda (cell)
           (let ((v (cdr cell)))
             (if (promise? v)
                 (force v)
                 v)))))

(define (env-set! key value)
  (set-cdr! (assq key *env-alist*) value))


;;; CGI high-level interface

;; Initialize the environment.
;;
(define-public (cgi:init)
  (and=> (env-look 'query-string)
         (lambda (s)
           (and (string-null? s)
                (env-set! 'query-string #f))))
  (let ((len (env-look 'content-length)))
    (cond ((= 0 len))
          ((string-ci=? (env-look 'content-type)
                        "application/x-www-form-urlencoded")
           (parse-form (read-raw-form-data len)))
          ((string-ci=? (subs (env-look 'content-type) 0 19)
                        "multipart/form-data")
           (parse-form-multipart (read-raw-form-data len)))))
  (and=> (env-look 'query-string) parse-form)
  (and=> (env-look 'http-cookie) get-cookies))

;; Return the value of the environment variable associated with @var{key}, a
;; symbol.  Unless otherwise specified below, the return value is a (possibly
;; massaged, possibly empty) string.  The following keys are recognized:
;;
;; @itemize
;; @item server-software-type
;; @item server-software-version
;; @item server-hostname
;; @item gateway-interface
;; @item server-protocol-name
;; @item server-protocol-version
;; @item server-port (integer)
;; @item request-method
;; @item path-info
;; @item path-translated
;; @item script-name
;; @item query-string
;; @item remote-host
;; @item remote-addr
;; @item authentication-type
;; @item remote-user
;; @item remote-ident
;; @item content-type
;; @item content-length (integer, possibly 0)
;; @item http-accept-types (list of strings)
;; @item http-user-agent
;; @item http-cookie
;; @end itemize
;;
;; Keys not listed above result in an "unrecognized key" error.
;;
(define-public (cgi:getenv key)
  (or (env-look key) ""))

;; Fetch any values associated with @var{name} found in the form data.
;; Return a list, even if it contains only one element.
;;
(define-public (cgi:values name)
  (assoc-ref form-variables name))

;; Fetch only the @sc{car} from @code{(cgi:values NAME)}.  Convenient
;; for when you are certain that @var{name} is associated with only one
;; value.
;;
(define-public (cgi:value name)
  (and=> (cgi:values name) car))

;; Return a list of variable names in the form.
;;
(define-public (cgi:names)
  (map car form-variables))

;; Return #t iff there is form data available.
;;
(define-public (cgi:form-data?)
  (not (null? form-variables)))

;; Fetch any files associated with @var{name}.  Return a list.  Can only
;; be called once per particular @var{name}.  Subsequent calls return
;; #f.  The caller had better hang onto the descriptor, lest the garbage
;; man whisk it away for good.  This is done to minimize the amount of
;; time the file is resident in memory.
;;
(define-public (cgi:uploads name)
  (and=> (assoc name file-uploads)
         (lambda (cell)
           (set! file-uploads (delq cell file-uploads))
           (cdr cell))))

;; Fetch the first file associated with form var @var{name}.  Can only be
;; called once per @var{name}, so the caller had better be sure that
;; there is only one file associated with @var{name}.  Use @code{cgi:uploads}
;; if you are unsure.
;;
(define-public (cgi:upload name)
  (and=> (cgi:uploads name) car))

;; Fetch any cookie values associated with @var{name}.  Return a list of
;; values in the order they were found in the HTTP header, which should
;; be the order of most specific to least specific path associated with
;; the cookie.
;;
(define-public (cgi:cookies name)
  (assoc-ref cookies name))

;; Fetch the first cookie value associated with @var{name}.
;;
(define-public (cgi:cookie name)
  (and=> (cgi:cookies name) car))

;; Return a string suitable for inclusion into an HTTP response header
;; as a cookie with @var{name} and @var{value}.  Recognize and format
;; appropriately the optional keyword parameters @code{#:path},
;; @code{#:domain}, @code{#:expires} (strings); and @code{#:secure}
;; (boolean).
;;
(define*-public (cgi:make-cookie name value #:key (path #f)
                                 (domain #f) (expires #f) (secure #f))
  (format #f "Set-Cookie: ~A=~A~A~A~A~A"
          name value
          (if path (format #f "; path=~A" path) "")
          (if domain (format #f "; domain=~A" domain) "")
          (if expires (format #f "; expires=~A" expires) "")
          (if secure "; secure" "")))



;;; Internal procedures.

;; (parse-form DATA): parse DATA as raw form response data of enctype
;;  x-www-form-urlencoded, adding values as necessary to `form-variables'.
;; (parse-form-multipart DATA): parse DATA as raw form response data
;;  of enctype multipart/form-data, adding values as necessary to
;;  'form-variables' and file data to 'file-uploads'.
;; (read-raw-form-data LEN): read in LEN bytes from stdin
;; (get-cookies RAW): initialize the cookie list.

(define (parse-form raw-data)
  ;; get-name and get-value are used to parse individual `name=value' pairs.
  ;; Values are URL-encoded, so each must be decoded.
  (define (get-name pair)
    (let ((p (string-index pair #\=)))
      (and p (subs pair 0 p))))
  (define (get-value pair)
    (let ((p (string-index pair #\=)))
      (and p (url-coding:decode (subs pair (+ p 1))))))
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

  (define (determine-boundary s)
    (format #f "--~A" (match:substring
                       (string-match "boundary=\"*(.[^\"\r\n]*)\"*" s)
                       1)))

  (define (m1 m)
    (match:substring m 1))

  (define (updated-alist alist name value)
    (assoc-set! alist name (cons value (or (assoc-ref alist name) '()))))

  (define (stash-form-variable! name value)
    (set! form-variables (updated-alist form-variables name value)))

  (define (stash-file-upload! name filename type value raw-headers)
    (stash-form-variable! name filename)
    (set-object-property! value #:guile-www-cgi
                          `((name . ,name)
                            (filename . ,filename)
                            (mime-type . ,type)
                            (raw-mime-headers . ,raw-headers)))
    (set! file-uploads (updated-alist file-uploads name value)))

  (let ((name-exp     (make-regexp "name=\"([^\"]*)\""))
        (filename-exp (make-regexp "filename=\"*([^\"\r]*)\"*"))
        (type-exp     (make-regexp "Content-Type: ([^\r]*)\r\n" regexp/icase))
        (value-exp    (make-regexp "\r\n\r\n")))

    (let level ((str raw-data)
                (boundary (determine-boundary (env-look 'content-type)))
                (parent-name #f))

      (let* ((boundary-len (string-length boundary))
             (find-bound (lambda (start)
                           (string-contains str boundary start))))

        (let get-pair ((start 0))
          (and=> (and=> (find-bound start)
                        (lambda (outer-seg-start)
                          (let ((seg-start (+ outer-seg-start boundary-len)))
                            (and=> (find-bound seg-start)
                                   (lambda (seg-finish)
                                     (cons (subs str seg-start (- seg-finish 2))
                                           seg-finish))))))
                 (lambda (segment-newstart)
                   (let* ((segment (car segment-newstart))
                          (try (lambda (rx extract)
                                 (and=> (regexp-exec rx segment)
                                        extract)))
                          (name (or parent-name
                                    (try name-exp  m1)))
                          (value    (try value-exp match:suffix))
                          (type     (try type-exp  m1)))
                     (and name
                          value
                          (cond ((and type
                                      (not parent-name) ; only recurse once
                                      (string-match "multipart/mixed" type))
                                 (level value
                                        (determine-boundary type)
                                        name))
                                ((and type (try filename-exp m1))
                                 => (lambda (filename)
                                      (stash-file-upload!
                                       name filename type value
                                       (subs (try value-exp match:prefix)
                                             2))))
                                (else
                                 (stash-form-variable! name value)))))
                   (get-pair (cdr segment-newstart)))))))))

(define (read-raw-form-data len)
  (read-n-chars len))

;; Setting up the cookies

(define (get-cookies raw)
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
    (get-pair raw)))


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
          (loop (cons (subs str (+ 1 pos)) fields)
                (subs str 0 pos))
          (cons str fields)))))

;;; www/cgi.scm ends here
