;;; (www https) --- HTTPS client library

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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

;;; Commentary:

;; This module exports the following variables and procedures:
;;     https:version
;;     https:user-agent
;;    (https-via-lynx host ip-port path)
;;
;; TODO: clean up (no longer export data structures) and release

;;; Code:

(define-module (www https)
  #:export (https:version
            https:user-agent
            https-via-lynx)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen))

(define https:version "HTTP/1.0")  ; bump up to 1.1 when ready
(define https:user-agent "GuileHTTP 0.1")

;; BUG -- I don't really want this function to be public; I want
;; people to use ‘www:get’ instead.  But if I use ‘define’ here,
;; instead of ‘define-public’, then when main.scm calls
;; www:set-protocol-handler!, https-via-lynx isn't defined.  This
;; means that I don't understand Guile's module system.

(define (https-via-lynx host ip-port path)
  (let ((p (open-pipe
            (format
             ;; We set TERM to prevent lynx from complaining about an
             ;; unknown terminal type.
             "TERM=vt100 lynx -source https://~a~a/~a"
             host
             (if ip-port (format ":~a" ip-port) "")
             path)
            OPEN_READ)))
    (let loop ((one-line (read-line p 'concat))
               (lines '()))
      (if (eof-object? one-line)
          (apply string-append (reverse lines))
        (loop (read-line p 'concat)
              (cons one-line lines))))))

;;; (www https) ends here
