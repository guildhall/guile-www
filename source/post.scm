;;; (www post)

;; Copyright (C) 2012 Thien-Thi Nguyen
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

;;; Commentary:

;; This module is "internal": distributed, installed,
;; but not documented (at least, for now).
;;
;; It contains procedures to prepare data for HTTP ‘POST’.

;;; Code:

(define-module (www post)
  #:export (formatted-form-for-http:post-form)
  #:use-module ((www url-coding) #:select (url-coding:encode))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (string-concatenate-reverse
                                         string-join))
  #:use-module (ice-9 optargs))

(define (fs s . args)
  (apply simple-format #f s args))

(define (string<-tree tree)
  (let ((acc '()))
    (define (walk x)
      (cond ((string? x)
             (set! acc (cons x acc)))
            ((pair? x)
             (for-each walk x))))
    (walk tree)
    (string-concatenate-reverse acc)))

(define (formatted-form-for-http:post-form fields)

  (define (simple? field)
    (not (pair? (cdr field))))

  (define (urlenc x)
    (url-coding:encode (cond ((string? x) x)
                             ((symbol? x) (symbol->string x))
                             (else (object->string x)))
                       '(#\& #\=)))

  (define* (c-type type #:optional boundary)
    (fs "Content-Type: ~A~A"
        type
        (if boundary
            (fs "; boundary=~S" boundary)
            "")))

  (or (and-map pair? fields)
      (error "bad fields:" fields))

  (if (and-map simple? fields)
      ;; simple
      (values
       ;; headers
       (list (c-type "application/x-www-form-urlencoded"))
       ;; body
       (string-join (map (lambda (pair)
                           (string-append (urlenc (car pair))
                                          "="
                                          (urlenc (cdr pair))))
                         fields)
                    "&"))
      ;; uploads
      (let ((boundary "gUiLeWwWhTtPpOsTfOrM"))

        (define* (c-disp disp name #:optional f?)
          (fs "Content-Disposition: ~A; ~Aname=\"~A\""
              disp (if f? "file" "") name))

        (values
         ;; headers
         (list (c-type "multipart/form-data" boundary))
         ;; body
         (let* ((sub-b (string-append "SuB" boundary)))

           (define (do-simple pair)
             (values
              ;; headers
              (list (c-disp "form-data" (car pair)))
              ;; body
              (cdr pair)))

           (define (source: spec)        (list-ref spec 0))
           (define (name: spec)          (list-ref spec 1))
           (define (mime-type: spec) (or (list-ref spec 2) "text/plain"))
           (define (xfer-enc: spec)  (or (list-ref spec 3) "binary"))

           (define (validate-upload-spec spec)

             (define (string-or-symbol? obj)
               (or (string? obj)
                   (symbol? obj)))

             (or (and (list? spec)
                      (= 4 (length spec))
                      (and=> (source: spec) (lambda (source)
                                              (or (thunk? source)
                                                  (string? source))))
                      (and=> (name: spec) string-or-symbol?)
                      (and=> (mime-type: spec) string-or-symbol?)
                      (and=> (xfer-enc: spec) string-or-symbol?))
                 (error "bad upload spec:" spec)))

           (define (emit-parts boundary ls proc)
             (let ((b-line (string-append "--" boundary)))
               (define (term x)
                 (list x "\r\n"))
               (define (part x)
                 (let-values (((headers body) (proc x)))
                   (map term (list b-line
                                   (map term headers)
                                   body))))
               `(,@(map part ls)
                 ,(string-append b-line "--"))))

           (define (do-upload name-spec)
             (values
              ;; headers
              (list (c-disp "form-data" (car name-spec))
                    (c-type "multipart/mixed" sub-b))
              ;; body
              (emit-parts
               sub-b (cdr name-spec)
               (lambda (spec)
                 (validate-upload-spec spec)
                 (values
                  ;; headers
                  (list (c-disp "attachment"
                                (basename (name: spec))
                                #t)
                        (c-type (mime-type: spec))
                        (fs "Content-Transfer-Encoding: ~A"
                            (xfer-enc: spec)))
                  ;; body
                  (list (let ((s (source: spec)))
                          (if (thunk? s)
                              (s)
                              s))))))))

           (string<-tree
            (emit-parts
             boundary fields
             (lambda (field)
               ((if (simple? field)
                    do-simple
                    do-upload)
                field)))))))))

;;; (www post) ends here
