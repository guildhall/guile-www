;;; (www cgi) --- Common Gateway Interface support

;; Copyright (C) 2007, 2008, 2009, 2011, 2012, 2013 Thien-Thi Nguyen
;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005,
;;   2006 Free Software Foundation, Inc.
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

(define-module (www cgi)
  #:export (cgi:init
            cgi:getenv
            cgi:nv-pairs
            cgi:values cgi:value
            cgi:names
            cgi:form-data?
            cgi:uploads cgi:upload
            cgi:cookie-names
            cgi:cookies cgi:cookie)
  #:use-module ((www server-utils parse-request) #:select (alist<-query))
  #:autoload (www server-utils cookies) (simple-parse-cookies)
  #:autoload (www server-utils form-2-form) (parse-form)
  #:autoload (ice-9 rw) (read-string!/partial)
  #:use-module ((srfi srfi-2) #:select (and-let*))
  #:use-module ((srfi srfi-13) #:select (string-join
                                         substring/shared
                                         string-prefix-ci?
                                         string-index
                                         string-upcase
                                         string-tokenize))
  #:use-module ((srfi srfi-14) #:select (char-set
                                         char-set-adjoin
                                         char-set-complement
                                         char-set:whitespace)))

;; NB: This is a copy of ‘(www crlf) read-characters’,
;;     here to avoid (auto)loading that module.
(define (read-body n port)
  (let ((s (make-string n)))
    (let loop ((start 0))
      (or (= start n)
          (and=> (read-string!/partial s port start)
                 (lambda (got)
                   (loop (+ start got))))))
    s))

(define (collate alist)
  (let ((rv '()))
    (for-each (lambda (k v)
                (set! rv (or (and-let* ((old (assoc-ref rv k)))
                               (append! old (list v))
                               rv)
                             (acons k (list v) rv))))
              (map car alist)
              (map cdr alist))
    (reverse! rv)))

(define (split-on cs)
  (let ((not-cs (char-set-complement cs)))
    ;; rv
    (lambda (string)
      (string-tokenize string not-cs))))

(define ws/comma-split
  (split-on (char-set-adjoin char-set:whitespace #\,)))

;;; CGI environment variables.

(define getenv/symbol
  (let ((split-on-hyphen (split-on (char-set #\-))))
    ;; getenv/symbol
    (lambda (symbol)
      (getenv (string-join (map string-upcase
                                (split-on-hyphen
                                 (symbol->string symbol)))
                           "_")))))

(define (env-look key)                  ; may return #f

  (define (server-sw-info)
    (and-let* ((sw (getenv/symbol 'server-software)))
      (list sw (string-index sw #\/))))

  (define (server-pr-info)
    (and-let* ((pr (getenv/symbol 'server-protocol)))
      (list pr (string-index pr #\/))))

  (define (extract make-args proc)
    (apply-to-args (make-args) proc))

  (case key
    ;; no fuss
    ((gateway-interface
      server-name server-software server-protocol
      auth-type
      request-method
      path-info path-translated
      script-name
      query-string
      remote-host remote-addr remote-user remote-ident
      content-type
      http-user-agent http-cookie)
     (getenv/symbol key))
    ;; oblique -- TODO: zonk; move alias target up (into "no fuss")
    ((server-hostname)
     (getenv/symbol 'server-name))
    ((authentication-type)
     (getenv/symbol 'auth-type))
    ;; devious
    ((server-port)
     (and=> (getenv/symbol key)
            string->number))
    ((content-length)
     (or (and=> (getenv/symbol key)
                string->number)
         0))
    ((server-software-type)             ; TODO: zonk; add ‘server-software’
     (extract server-sw-info
              (lambda (sw slash)
                (if slash
                    (substring/shared sw 0 slash)
                    sw))))
    ((server-software-version)          ; TODO: zonk; add ‘server-software’
     (extract server-sw-info
              (lambda (sw slash)
                (and slash (substring/shared sw (1+ slash))))))
    ((server-protocol-name)             ; TODO: zonk; add ‘server-protocol’
     (extract server-pr-info
              (lambda (pr slash)
                (substring/shared pr 0 slash))))
    ((server-protocol-version)          ; TODO: zonk; add ‘server-protocol’
     (extract server-pr-info
              (lambda (pr slash)
                (substring/shared pr (1+ slash)))))
    ((http-accept http-accept-types)    ; TODO: zonk latter
     (or (and=> (getenv/symbol 'http-accept) ws/comma-split)
         ;; SHOULD be set (RFC3875, 4.1.18) but sometimes isn't
         '()))
    (else
     (error "unrecognized key:" key))))

;;; CGI context closure.

(define (make-ccc)
  (let ((P '())                         ; form variables as pairs
        (V '())                         ; form variables collated
        (U '()) (pre-squeezed? #t)      ; file uploads
        (C '()))                        ; cookies

    (define (init! opts)
      (set! P '()) (set! V '()) (set! U '())
      (set! pre-squeezed? (not (memq 'uploads-lazy opts)))
      (and-let* ((len (env-look 'content-length))
                 ((not (zero? len)))
                 (type (env-look 'content-type))
                 (s (read-body len (current-input-port))))
        ;; We check for prefix instead of equality because sometimes
        ;; the server appends other information (e.g., "; charset=UTF-8").
        (cond ((string-prefix-ci? "application/x-www-form-urlencoded" type)
               (set! P (alist<-query s)))
              ((string-prefix-ci? "multipart/form-data" type)
               (let ((alist (parse-form (substring/shared type 19) s)))

                 (define (mogrify m)
                   (or (cdr m) (error "badness from parse-form:" m))
                   (if (string? (cdr m))
                       (set! P (cons m P))
                       (let ((four (cdr m)))

                         (define (handle filename type headers squeeze)
                           (set! P (acons (car m) filename P))
                           (and pre-squeezed?
                                (let ((value (squeeze substring)))
                                  (set-object-property!
                                   value #:guile-www-cgi
                                   `((#:name . ,(car m))
                                     (#:filename . ,filename)
                                     (#:mime-type . ,type)
                                     (#:raw-mime-headers . ,headers)))
                                  (set-cdr! m value)))
                           (set! U (cons m U)))

                         (apply handle four))))

                 (for-each mogrify alist))
               (set! P (reverse! P))
               (set! U (reverse! U)))))
      ;; Include ‘query-string’ pairs, if any.
      (and-let* ((s (getenv/symbol 'query-string))
                 ((not (string-null? s))))
        ;; FIXME: We prefix, but perhaps we should suffix?
        (set! P (append! (alist<-query s) P)))
      (set! V (collate P))
      (set! U (collate U))
      (set! C (cond ((env-look 'http-cookie)
                     => (lambda (raw)
                          (collate (simple-parse-cookies
                                    raw (if (memq 'cookies-split-on-semicolon opts)
                                            #\; #\,)))))
                    (else '()))))

    (define (uploads name)
      (and-let* ((pair (assoc name U)))
        (set! U (delq pair U))
        (cdr pair)))

    ;; rv
    (lambda (command . args)
      (define (one)
        (car args))
      (case command
        ((#:init!) (init! args))
        ((#:nv-pairs) P)
        ((#:values) (assoc-ref V (one)))
        ((#:value) (and=> (assoc-ref V (one)) car))
        ((#:names) (map car V))
        ((#:form-data?) (not (null? P)))
        ((#:uploads) (uploads (one)))
        ((#:upload) (and=> (uploads (one)) car))
        ((#:cookie-names) (map car C))
        ((#:cookies) (assoc-ref C (one)))
        ((#:cookie) (and=> (assoc-ref C (one)) car))
        (else (error "bad command:" command))))))

(define ONE #f)


;;; Public interface.

;; (Re-)initialize internal data structures.  This must be called before
;; calling any other @samp{cgi:foo} procedure.  For FastCGI, call this
;; ``inside the loop'' (that is, for each CGI invocation).
;;
;; @var{opts} are zero or more symbols that configure the module.
;;
;; @table @code
;; @item uploads-lazy
;; This controls how uploaded files, as per @code{cgi:uploads}
;; and @code{cgi:upload}, are represented.
;;
;; @item cookies-split-on-semicolon
;; This causes cookies parsing to use @code{#\;} (semicolon), instead
;; of the default @code{#\,} (comma), for splitting multiple cookies.
;; This is necessary, for example, if the server is configured to
;; provide ``Netscape style'' (i.e., old and deprecated) cookies.
;; @end table
;;
;; Unrecognized options are ignored.
;;
(define (cgi:init . opts)
  (or ONE (set! ONE (make-ccc)))
  (apply ONE #:init! opts))

;; Return the value of the environment variable associated with @var{key}, a
;; symbol.  Unless otherwise specified below, the return value is a (possibly
;; massaged, possibly empty) string.  The following keys are recognized:
;;
;; @example
;; server-software
;; server-software-type     ; @r{part of} server-software @r{before} "/"
;; server-software-version  ; @r{part of} server-software @r{after} "/"
;; server-name
;; server-hostname          ; @r{alias for} server-name
;; gateway-interface
;; server-protocol
;; server-protocol-name     ; @r{part of} server-protocol @r{before} "/"
;; server-protocol-version  ; @r{part of} server-protocol @r{after} "/"
;; server-port @r{(integer)}
;; request-method
;; path-info
;; path-translated
;; script-name
;; query-string
;; remote-host
;; remote-addr
;; auth-type
;; authentication-type      ; @r{alias for} auth-type
;; remote-user
;; remote-ident
;; content-type
;; content-length @r{(integer, possibly 0)}
;; http-accept @r{(list, possibly empty, of strings)}
;; http-accept-types        ; @r{alias for} http-accept-types
;; http-user-agent
;; http-cookie
;; @end example
;;
;; Keys not listed above result in an "unrecognized key" error.
;;
(define (cgi:getenv key)
  (or (env-look key) ""))

;; Fetch the list of @code{(name . value)}, in the same order as found
;; in the form data.  A name may appear more than once.  A value is
;; either a string, or @code{#f}.
;;
(define (cgi:nv-pairs)
  (ONE #:nv-pairs))

;; Fetch any values associated with @var{name} found in the form data.
;; Return a list, even if it contains only one element.  A value is
;; either a string, or @code{#f}.  When there are multiple values, the
;; order is the same as that found in the form.
;;
(define (cgi:values name)
  (ONE #:values name))

;; Fetch only the @sc{car} from @code{(cgi:values @var{name})}.
;; Convenient for when you are certain that @var{name} is associated
;; with only one value.
;;
(define (cgi:value name)
  (ONE #:value name))

;; Return a list of variable names in the form.  The order of the
;; list is the same as that found in the form for the first occurance
;; of each variable and each variable appears at most once.  For example,
;; if the form has variables ordered @code{a b a c d b e}, then the
;; returned list would have order @code{a b c d e}.
;;
(define (cgi:names)
  (ONE #:names))

;; Return @code{#t} iff there is form data available.
;;
(define (cgi:form-data?)
  (ONE #:form-data?))

;; Return a list of file contents associated with @var{name},
;; or @code{#f} if no files are available.
;;
;; Uploaded files are parsed by @code{parse-form} (@pxref{form-2-form}).
;; If the @code{uploads-lazy} option is specified to @code{cgi:init}, then
;; the file contents are those directly returned by @code{form-2-form}.
;; If unspecified, the file contents are strings with the object property
;; @code{#:guile-www-cgi} whose value is an alist with the following keys:
;;
;; @table @code
;; @item #:name
;; identical to @var{name} (sanity check)
;;
;; @item #:filename
;; original/suggested filename for this bunch of bits
;;
;; @item #:mime-type
;; something like "image/jpeg"
;;
;; @item #:raw-mime-headers
;; the MIME headers before parsing
;; @end table
;;
;; Note that the string's object property and the keys are all keywords.
;; The associated values are strings.
;;
;; Unless @code{uploads-lazy} is specified (to @code{cgi:init}),
;; @code{cgi:uploads} can only be called once per particular @var{name}.
;; Subsequent calls return @code{#f}.  Caller had better hang onto the
;; information, lest the garbage man whisk it away for good.  This is
;; done to minimize the amount of time the file is resident in memory.
;;
(define (cgi:uploads name)
  (ONE #:uploads name))

;; Fetch the first file associated with form var @var{name}.  Can only be
;; called once per @var{name}, so the caller had better be sure that
;; there is only one file associated with @var{name}.  Use @code{cgi:uploads}
;; if you are unsure.
;;
(define (cgi:upload name)
  (ONE #:upload name))

;; Return a list of cookie names.
;;
(define (cgi:cookie-names)
  (ONE #:cookie-names))

;; Fetch any cookie values associated with @var{name}.  Return a list of
;; values in the order they were found in the HTTP header, which should
;; be the order of most specific to least specific path associated with
;; the cookie.  If no cookies are associated with @var{name}, return
;; @code{#f}.
;;
(define (cgi:cookies name)
  (ONE #:cookies name))

;; Fetch the first cookie value associated with @var{name}.
;;
(define (cgi:cookie name)
  (ONE #:cookie name))

;;; (www cgi) ends here
