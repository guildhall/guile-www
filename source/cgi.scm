;;; (www cgi) --- Common Gateway Interface support

;; Copyright (C) 2007, 2008, 2009, 2011, 2012, 2013, 2014 Thien-Thi Nguyen
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
;; You should have received a copy of the GNU General Public License
;; along with Guile-WWW.  If not, see <http://www.gnu.org/licenses/>.

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
  #:autoload (www mime-headers) (parse-type)
  #:autoload (www server-utils cookies) (simple-parse-cookies)
  #:autoload (www mime-multipart) (parse-multipart)
  #:autoload (www server-utils form-2-form) (parse-form)
  #:autoload (www crlf) (read-characters)
  #:use-module ((srfi srfi-2) #:select (and-let*))
  #:use-module ((srfi srfi-13) #:select (string-map!
                                         substring/shared
                                         string-index
                                         string-upcase
                                         string-tokenize))
  #:use-module ((srfi srfi-14) #:select (char-set
                                         char-set-adjoin
                                         char-set-complement
                                         char-set:whitespace)))

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
  (let ((ht (make-hash-table)))

    (define (squash-hyphen c)
      (if (char=? #\- c)
          #\_
          c))

    (define (string<- symbol)
      (or (hashq-ref ht symbol)
          (let ((str (string-upcase (symbol->string symbol))))
            (string-map! squash-hyphen str)
            (hashq-set! ht symbol str)
            str)))

    ;; getenv/symbol
    (lambda (symbol)
      (getenv (string<- symbol)))))

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

(define (parse-form/move type len opt?)

  (define string-text/plain-please (opt? 'move-simple-text/plain))

  (define (reflow pair)

    (define move (car pair))

    (define (hget header)
      (p-ref pair header))

    (define (d key)
      (p-ref (hget 'Content-Disposition)
             key))

    (let* ((filename (d 'filename))
           (type     (hget 'Content-Type))
           (value    (if (and string-text/plain-please
                              (typed? type 'text 'plain)
                              (not filename))
                         (call-with-output-string move)
                         (list (if (procedure? move)
                                   move
                                   (map reflow move))
                               (hget 'Content-Length)
                               type))))
      (list (d 'name)
            (or filename value)
            (and filename (cons filename value)))))

  (map reflow (parse-multipart type (current-input-port) len)))

(define (parse-form/squeeze-maybe type len opt?)
  (let ((pre-squeezed? (not (opt? 'uploads-lazy)))
        (alist (parse-form type len)))

    (define (linear name rest)

      (define (upload filename type headers squeeze)
        (list filename
              (if pre-squeezed?
                  (let ((value (squeeze substring)))
                    (set-object-property!
                     value #:guile-www-cgi
                     `((#:name . ,name)
                       (#:filename . ,filename)
                       (#:mime-type . ,type)
                       (#:raw-mime-headers . ,headers)))
                    value)
                  rest)))

      (cons* name (if (string? rest)
                      (list rest #f)
                      (apply upload rest))))

    (map linear
         (map car alist)
         (map cdr alist))))

(define (make-ccc)
  (let ((P '())                         ; form variables as pairs
        (V '())                         ; form variables collated
        (U '())                         ; file uploads
        (C '()))                        ; cookies

    (define (P! x) (set! P x))
    (define (V! x) (set! V x))
    (define (U! x) (set! U x))

    (define (init! opts)

      (define (opt? symbol)
        (memq symbol opts))

      (define (alist<-qs s)
        (alist<-query s (opt? 'u8-qs)))

      (P! '()) (V! '()) (U! '())
      (and-let* ((len (env-look 'content-length))
                 ((not (zero? len)))
                 (type (parse-type (env-look 'content-type))))
        (cond ((typed? type 'application 'x-www-form-urlencoded)
               (P! (alist<-qs (read-characters len))))
              ((typed? type 'multipart 'form-data)
               (let ((full ((if (opt? 'move)
                                parse-form/move
                                parse-form/squeeze-maybe)
                            type len opt?)))

                 (define (extract-P name value upload)
                   (cons name value))

                 (define (extract-U name value upload)
                   (and upload (cons name upload)))

                 (define (map-ent proc)
                   (map (lambda (ent)
                          (apply proc ent))
                        full))

                 (P! (map-ent extract-P))
                 (U! (delq #f (map-ent extract-U)))))))
      ;; Include ‘query-string’ pairs, if any.
      (and-let* ((s (getenv/symbol 'query-string))
                 ((not (string-null? s))))
        ;; FIXME: We prefix, but perhaps we should suffix?
        (P! (append! (alist<-qs s) P)))
      (V! (collate P))
      (U! (collate U))
      (set! C (cond ((env-look 'http-cookie)
                     => (lambda (raw)
                          (collate (simple-parse-cookies
                                    raw (if (opt? 'cookies-split-on-semicolon)
                                            #\; #\,)))))
                    (else '()))))

    (define (uploads name)
      (and-let* ((pair (assoc name U)))
        (U! (delq pair U))
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
;; @item u8-qs
;; This causes parsing of @code{query-string} and form data posted
;; with MIME type @code{application/x-www-form-urlencoded} to return
;; an alist with u8vector keys and values.
;;
;; @item uploads-lazy
;; This controls how uploaded files, as per @code{cgi:uploads}
;; and @code{cgi:upload}, are represented.
;;
;; @item move
;; This is like @code{uploads-lazy} but additionally affects all
;; parameters in a form with MIME type @code{multipart/form-data}.
;; If both @code{uploads-lazy} and @code{move} are specified,
;; then @code{move} takes precedence.
;;
;; @item move-simple-text/plain
;; This simplifies the value of form parameters with MIME type
;; @code{text/plain} to a string, discarding other MIME information.
;; It is only meaningful in conjunction with @code{move}.
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
;; If the @code{move} option is specified to @code{cgi:init},
;; each file contents element has the form:
;;
;; @example
;; (FILENAME MOVE LENGTH TYPE)
;; @end example
;;
;; where @var{filename} is a string, @var{length} is an integer,
;; @var{type} is a ``MIME type form'' [[[TODO: this is really
;; a PITA to document piecemeal --- might as well redirect the
;; effort towards making @code{(www mime-headers)} public.]]],
;; and @var{move} is either a sub-list (from a sub-multipart part),
;; or, more typically the case a procedure that takes one arg @var{to}:
;;
;; @c NB: This is taken from @code{(www mime-multipart)}.
;; @table @asis
;; @item @var{port}
;; Send the part contents to @var{port}.
;;
;; @item @code{#t}
;; Return a u8vector of the part contents.
;;
;; @item @code{#f}
;; Discard the part contents and return @code{#f}.
;; @end table
;;
;; NB: @code{move-simple-text/plain} does not apply to file
;; contents, even for file with MIME type @code{text/plain}.
;;
;; If @code{move} is @strong{not} specified to @code{cgi:init}, read on.
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
