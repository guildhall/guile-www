;;; (www url-coding) --- URL character coding (decode/encode)

;; Copyright (C) 2009, 2012 Thien-Thi Nguyen
;; Copyright (C) 2004, 2005, 2007 Free Software Foundation, Inc.
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

(define-module (www url-coding)
  #:export (url-coding:decode
            url-coding:encode)
  #:use-module ((srfi srfi-13) #:select (string-index
                                         string-concatenate-reverse
                                         substring/shared))
  #:use-module ((srfi srfi-14) #:select (char-set
                                         char-set-intersection
                                         char-set-union
                                         char-set-difference
                                         list->char-set
                                         string->char-set
                                         char-set:ascii
                                         char-set:letter+digit)))

;; Return a new string made from url-decoding @var{str}.  Specifically,
;; turn @code{+} into space, and hex-encoded @code{%XX} strings into
;; their eight-bit characters.
;;
(define url-coding:decode
  (let ((both (char-set #\+ #\%)))
    ;; url-coding:decode
    (lambda (str)
      (define (subs b . e)
        (apply substring/shared str b e))
      (let ((last-start (- (string-length str) 3)))
        (let loop ((acc '()) (start 0))
          (define (until . e)
            (apply subs start e))
          (cond ((string-index str both start)
                 => (lambda (pos)
                      (define (with processed x)
                        (loop (cons* x (until pos) acc)
                              (+ processed pos)))
                      (case (string-ref str pos)
                        ((#\+) (with 1 " "))
                        (else (and (< last-start pos)
                                   (error "% too late:" str))
                              (with 3 (string
                                       (integer->char
                                        (string->number
                                         (subs (1+ pos) (+ 3 pos))
                                         16))))))))
                (else
                 (string-concatenate-reverse acc (until)))))))))

;; Return a new string made from url-encoding @var{str},
;; unconditionally transforming those in @var{reserved-chars}, a list
;; of characters, in addition to those in the standard (internal) set.
;;
(define url-coding:encode
  ;; “Thus, only alphanumerics, the special characters "$-_.+!*'(),", and
  ;; reserved characters used for their reserved purposes may be used
  ;; unencoded within a URL.” RFC 1738, #2.2.
  (let ((safe (char-set-union
               ;; Unicode-based ‘char-set:letter+digit’ is huge.
               ;; Hew to the original RFC world view.
               (char-set-intersection char-set:letter+digit
                                      char-set:ascii)
               (string->char-set "$-_.+!*'(),")
               (string->char-set ";/?:@&="))))

    (define percent
      (let ((v (list->vector (map (lambda (i)
                                    (string-append
                                     "%"
                                     (if (> 16 i) "0" "")
                                     (number->string i 16)))
                                  (iota 256)))))
        ;; percent
        (lambda (ch)
          (vector-ref v (char->integer ch)))))

    ;; url-coding:encode
    (lambda (str reserved-chars)
      (let ((ok (if (pair? reserved-chars)
                    (char-set-difference safe (list->char-set reserved-chars))
                    safe)))
        (let loop ((acc '()) (start 0))
          (define (until . end)
            (apply substring/shared str start end))
          (cond ((string-skip str ok start)
                 => (lambda (pos)
                      (loop (cons* (percent (string-ref str pos))
                                   (until pos)
                                   acc)
                            (1+ pos))))
                (else
                 (string-concatenate-reverse acc (until)))))))))

;;; (www url-coding) ends here
