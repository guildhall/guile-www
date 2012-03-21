;;; (www server-utils modlisp) --- Handlers for Apache mod_lisp protocol

;; Copyright (C) 2010, 2011, 2012 Thien-Thi Nguyen
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

(define-module (www server-utils modlisp)
  #:export (modlisp-hgrok
            modlisp-ish)
  #:use-module ((www crlf) #:select (get-body-proc))
  #:use-module ((srfi srfi-1) #:select (lset-difference))
  #:use-module ((ice-9 rdelim) #:select (read-line)))

(define (read-until-body port hsym)
  (let loop ((acc '()))
    (let ((k (read-line port)))
      (if (string=? "end" k)
          (reverse! acc)
          (loop (acons (hsym k) (read-line port) acc))))))

(define (read-request port s2s)
  (define (hsym string)
    (string->symbol (s2s string)))
  (let* ((headers (read-until-body port hsym))
         (method (assq (hsym "method") headers))
         (upath (assq (hsym "url") headers))
         (pvers (assq (hsym "server-protocol") headers)))
    (set! headers (lset-difference eq? headers (list method upath pvers)))
    (values (cdr method)
            (cdr upath)
            (cdr pvers)
            headers
            (get-body-proc port hsym headers))))

(define LF "\n")

;; An object suitable as the optional @code{style} argument for
;; @code{string<-headers}, @code{compose-response} and @code{mouthpiece}.
;; @xref{answer}.
;;
;;-category: object
;;
(define modlisp-ish (vector LF
                            #f
                            (string-append "Status" LF)
                            LF
                            (string-append "end" LF)))

;; An object suitable for the @code{#:style} argument to
;; both @code{make-big-dishing-loop} (@pxref{big-dishing-loop})
;; and @code{receive-request} (@pxref{parse-request}).
;;
;;-category: object
;;
(define modlisp-hgrok (vector read-request
                              modlisp-ish))

;;; modlisp.scm ends here
