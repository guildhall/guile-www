;;; (www data content-type) --- Don't use this module!

;; Copyright (C) 2008 Thien-Thi Nguyen
;; Copyright (C) 2004 Free Software Foundation, Inc.
;; Copyright (C) 2001 Martin Grabmueller
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

(define-module (www data content-type)
  #:export (*content-type-by-filename-extension*-META
            *content-type-by-filename-extension*))

(define *content-type-by-filename-extension*-META
  '((#:source . "mgrabmue-pers-scheme 0.2.0")
    (#:layout . "list of pairs (EXT . STRING), EXT is a symbol")))

(define *content-type-by-filename-extension*
  '((mp2 . "audio/x-mpeg")
    (mpa . "audio/x-mpeg")
    (abs . "audio/x-mpeg")
    (mpega . "audio/x-mpeg")
    (mpeg . "video/mpeg")
    (mpg . "video/mpeg")
    (mpe . "video/mpeg")
    (mpv . "video/mpeg")
    (vbs . "video/mpeg")
    (mpegv . "video/mpeg")
    (bin . "application/octet-stream")
    (com . "application/octet-stream")
    (dll . "application/octet-stream")
    (xml . "application/xml")
    (bmp . "image/x-MS-bmp")
    (exe . "application/octet-stream")
    (mid . "audio/x-midi")
    (midi . "audio/x-midi")
    (css . "text/css")
    (htm . "text/html")
    (html . "text/html")
    (txt . "text/plain")
    (gif . "image/gif")
    (tar . "application/x-tar")
    (jpg . "image/jpeg")
    (jpeg . "image/jpeg")
    (png . "image/png")
    (svg . "image/svg+xml")
    (ra . "audio/x-pn-realaudio")
    (ram . "audio/x-pn-realaudio")
    (sys . "application/octet-stream")
    (wav . "audio/x-wav")
    (xbm . "image/x-xbitmap")
    (zip . "application/x-zip")
    (deb . "application/x-debian-package")
    (ps . "application/postscript")))

;;; (www data content-type) ends here
