;;; www/url.scm --- URL manipulation tools

;; 	Copyright (C) 1997,2001,02,03,2004 Free Software Foundation, Inc.
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

;; The (www url) module is fully documented in the guile-www.info file.

;;; Code:


;; TODO:
;;   * support `user:password@' strings where appropriate in URLs.
;;   * make URL parsing smarter.  This is good for most TCP/IP-based
;;	 URL schemes, but parsing is actually specific to each URL scheme.
;;   * fill out url:encode, include facilities for URL-scheme-specific
;;     encoding methods (e.g. a url-scheme-reserved-char-alist)

(define-module (www url)
  #:use-module (ice-9 regex))

;; Extract and return the "scheme" portion of a @var{url} object.
;; @code{url:scheme} is an unfortunate term, but it is the technical
;; name for that portion of the URL according to RFC 1738.  Sigh.
;;
(define-public (url:scheme url)  (vector-ref url 0))

;; Extract and return the "address" portion of the @var{url} object.
;;
(define-public (url:address url) (vector-ref url 1))

;; Extract and return the "unknown" portion of the @var{url} object.
;;
(define-public (url:unknown url) (vector-ref url 1))

;; Extract and return the "user" portion of the @var{url} object.
;;
(define-public (url:user url)    (vector-ref url 1))

;; Extract and return the "host" portion of the @var{url} object.
;;
(define-public (url:host url)    (vector-ref url 2))

;; Extract and return the "port" portion of the @var{url} object.
;;
(define-public (url:port url)    (vector-ref url 3))

;; Extract and return the "path" portion of the @var{url} object.
;;
(define-public (url:path url)    (vector-ref url 4))

;; Construct a url object with specific @var{scheme} and other @var{args}.
;; The number and meaning of @var{args} depends on the @var{scheme}.
;;
(define-public (url:make scheme . args)
  (apply vector scheme args))

;; Construct a HTTP-specific url object with
;; @var{host}, @var{port} and @var{path} portions.
;;
(define-public (url:make-http host port path)
  (vector 'http #f host port path))

;; Construct a FTP-specific url object with
;; @var{user}, @var{host}, @var{port} and @var{path} portions.
;;
(define-public (url:make-ftp user host port path)
  (vector 'ftp user host port path))

;; Construct a mailto-specific url object with
;; an @var{address} portion.
;;
(define-public (url:make-mailto address)
  (vector 'mailto address))

(define http-regexp (make-regexp "^http://([^:/]+)(:([0-9]+))?(/(.*))?$"))
(define ftp-regexp
  (make-regexp "^ftp://(([^@:/]+)@)?([^:/]+)(:([0-9]+))?(/(.*))?$"))
(define mailto-regexp (make-regexp "^mailto:(.*)$"))

;; Parse @var{string} and return a url object, with one of the
;; following "schemes": HTTP, FTP, mailto, unknown.
;;
;;-sig: (string)
;;
(define-public (url:parse url)
  (cond
   ((regexp-exec http-regexp url)
    => (lambda (m)
	 (url:make-http (match:substring m 1)
			(cond ((match:substring m 3) => string->number)
			      (else #f))
			(match:substring m 5))))

   ((regexp-exec ftp-regexp url)
    => (lambda (m)
	 (url:make-ftp (match:substring m 2)
		       (match:substring m 3)
		       (cond ((match:substring m 5) => string->number)
			     (else #f))
		       (match:substring m 7))))

   ((regexp-exec mailto-regexp url)
    => (lambda (m)
	 (url:make-mailto (match:substring m 1))))

   (else
    (url:make 'unknown url))))

;; Return the @var{url} object formatted as a string.
;; Note: The username portion is not included!
;;
(define-public (url:unparse url)
  (define (pathy scheme username url)   ; username not used!
    (format #f "~A://~A~A~A"
            scheme
            (url:host url)
            (cond ((url:port url) => (lambda (port) (format #f ":~A" port)))
                  (else ""))
            (cond ((url:path url) => (lambda (path) (format #f "/~A" path)))
                  (else ""))))
  (case (url:scheme url)
    ((http) (pathy 'http #f url))
    ((ftp)  (pathy 'ftp (url:user url) url))
    ((mailto) (format #f "mailto:~A" (url:address url)))
    ((unknown) (url:unknown url))))


;; Return a new string made from url-decoding @var{str}.  Specifically,
;; turn @code{+} into space, and hex-encoded @code{%XX} strings into
;; their eight-bit characters.
;;
(define-public (url:decode str)
  ;; Implementation Questions: Is a regexp faster than character scanning?
  ;; Does it incur more overhead (which may be more important for code that
  ;; frequently gets restarted)?
  (regexp-substitute/global
   #f "\\+|%([0-9A-Fa-f][0-9A-Fa-f])" str
   'pre
   (lambda (m)
     (cond ((string=? "+" (match:substring m 0)) " ")
           (else (integer->char
                  (string->number
                   (match:substring m 1)
                   16)))))
   'post))


;; Return a new string made from url-encoding @var{str},
;; unconditionally transforming those in @var{reserved-chars}, a list
;; of characters, in addition to those in the standard (internal) set.
;;
(define-public (url:encode str reserved-chars)
  ;; Can't be done easily with a regexp: we would have to construct a
  ;; regular expression like "[\277-\377]", for example, and Guile
  ;; strings don't let you interpolate character literals.  Pity.
  (with-output-to-string
    (lambda ()
      (for-each (lambda (ch)
		  (if (and (safe-char? ch)
			   (not (memv ch reserved-chars)))
		      (display ch)
		      (begin
			(display #\%)
			(display (number->string (char->integer ch) 16)))))
		(string->list str)))))

(define safe-chars (append (string->list "$-_.+!*'(),")
                           ;; reserved
                           (string->list ";/?:@&=")))

(define (safe-char? ch)
  ;; ``Thus, only alphanumerics, the special characters "$-_.+!*'(),", and
  ;; reserved characters used for their reserved purposes may be used
  ;; unencoded within a URL.'' RFC 1738, #2.2.
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch safe-chars)))

;;; www/url.scm ends here
