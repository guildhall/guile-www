;;; (www url-coding) --- URL character coding (decode/encode)

;; Copyright (C) 2009, 2012, 2013 Thien-Thi Nguyen
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
  #:use-module (ice-9 optargs)
  #:use-module ((srfi srfi-4) #:select (make-u8vector
                                        u8vector-set!))
  #:use-module ((srfi srfi-11) #:select (let*-values))
  #:use-module ((srfi srfi-13) #:select (string-index
                                         string-skip
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

 
;;; These three are from Guile-BAUX: (info "(guile-baux) bv")

(define make-bv
  (cond-expand (guile-2 make-bitvector)
               (else (if (defined? 'make-bitvector)
                         make-bitvector
                         (lambda (sz init)
                           (make-uniform-vector sz #t init))))))

(define bv-set!
  (cond-expand (guile-2 bitvector-set!)
               (else (if (defined? 'bitvector-set!)
                         bitvector-set!
                         uniform-vector-set!))))

(define bv-ref
  (cond-expand (guile-2 bitvector-ref)
               (else (if (defined? 'bitvector-ref)
                         bitvector-ref
                         uniform-vector-ref))))


(define PLUS/PERCENT (char-set #\+ #\%))

(define ZERO (char->integer #\0))

(define SPACE (char->integer #\space))

(define (particulars str)
  (let* ((slen (string-length str))
         (hmmm (make-bv slen #f))
         (plus (make-bv slen #f)))

    (define (yep! bv pos)
      (bv-set! bv pos #t))

    (let scan ((len (string-length str))
               (start 0))
      (cond ((string-index str PLUS/PERCENT start)
             => (lambda (pos)
                  (yep! hmmm pos)
                  (let ((dec (char=? #\% (string-ref str pos))))
                    (or dec (yep! plus pos))
                    (scan (if dec
                              (- len 2)
                              len)
                          (+ pos (if dec 3 1))))))
            (else
             (values hmmm
                     plus
                     slen
                     len))))))

(define (c-hex c)
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (- (char->integer c)
                                                  ZERO))
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)))

;; Return a new string made from url-decoding @var{str}.  Specifically,
;; turn @code{+} into space, and hex-encoded @code{%XX} strings into
;; their eight-bit characters.
;;
;; If optional arg @var{u8} is non-@code{#f}, return u8vector instead
;; of string, useful for further processing in the case when the desired
;; ``character set'' is not ISO-8859-1.  For example:
;;
;; @example
;; (url-coding:decode "%E2%98%A1" #t)
;; @result{} #u8(226 152 161) ; aka U+2621 CAUTION SIGN in UTF-8
;; @end example
;;
(define* (url-coding:decode str #:optional (u8 #f))

  (define (w/string len)
    (let ((s (make-string len))
          (wx 0))

      (define (one! n)
        (string-set! s wx (integer->char n))
        (set! wx (1+ wx)))

      (define (many! beg end)
        (substring-move! str beg end s wx)
        (set! wx (+ wx (- end beg))))

      (values s one! many!)))

  (define (w/u8vector len)
    (let ((v (make-u8vector len))
          (wx 0))

      (define (one! n)
        (u8vector-set! v wx n)
        (set! wx (1+ wx)))

      (define (many! beg end)
        (do ((i beg (1+ i)))
            ((= end i))
          (one! (char->integer (string-ref str i)))))

      (values v one! many!)))

  (let*-values (((hmmm plus slen len) (particulars str))
                ((rv one! many!) ((if u8 w/u8vector w/string) len)))

    (let transfer ((rx 0))

      (define (copy-up-to! end)
        (many! rx end))

      (cond ((bit-position #t hmmm rx)
             => (lambda (pos)

                  (define (n-at ofs)
                    (c-hex (string-ref str (+ ofs pos))))

                  (copy-up-to! pos)
                  (transfer
                   (+ pos (cond ((bv-ref plus pos)
                                 (one! SPACE)
                                 1)
                                (else
                                 (one! (logior (ash (n-at 1)
                                                    4)
                                               (n-at 2)))
                                 3))))))
            (else
             (copy-up-to! slen)
             rv)))))

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
