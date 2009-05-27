;;; (www server-utils filesystem) --- Work with the local filesystem

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.
;; Copyright (C) 2000, 2001 Martin Grabmueller
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

;; The (www server-utils filesystem) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils filesystem)
  #:export (access-forbidden?-proc
            cleanup-filename
            upath->filename-proc
            filename->content-type)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 and-let-star)
  #:autoload (www data content-type) (*content-type-by-filename-extension*)
  #:autoload (www data mime-types) (put-mime-types!))

(define subs make-shared-substring)

;; Create and return a filesystem-access procedure based on
;; @var{docroot} and @var{forbid-rx}.  The returned procedure @var{p}
;; takes a @var{filename} and returns @code{#t} if access to that file
;; should be denied for any of the following reasons:
;;
;; @itemize
;; @item @var{filename} is the null string
;; @item @var{filename} does not begin with slash
;; @item @var{filename} is not longer than @var{docroot}
;; @item @var{filename} does not begin with @var{docroot}
;; @item @var{filename} matches regular expression @var{forbid-rx}
;; @end itemize
;;
;; If @var{forbid-rx} is @code{#f}, the regular expression check is
;; skipped.  @var{p} returns @code{#f} if access should be granted.
;;
(define (access-forbidden?-proc docroot forbid-rx)
  (let ((docroot-length (string-length docroot))
        (rx (and forbid-rx (make-regexp forbid-rx))))
    ;; rv
    (lambda (filename)
      (or (string-null? filename)
          (not (char=? (string-ref filename 0) #\/))
          (<= (string-length filename) docroot-length)
          (not (string=? docroot (substring filename 0 docroot-length)))
          (and rx (regexp-exec rx filename))))))

(define clean-parent-rx       (make-regexp "[^/]*/\\.\\./"))
(define clean-dot-rx          (make-regexp "/\\./"))
(define clean-double-slash-rx (make-regexp "//"))

;; Return a new filename made from cleaning up filename @var{name}.
;; Cleaning removes "@var{foo}/../", and collapses both "/./"
;; and "//" into "/".
;;
(define (cleanup-filename name)
  (cond ((regexp-exec clean-parent-rx name)
         => (lambda (m)
              (cleanup-filename
               (string-append (match:prefix m)
                              (match:suffix m)))))
        ((or (regexp-exec clean-dot-rx name)
             (regexp-exec clean-double-slash-rx name))
         => (lambda (m)
              (cleanup-filename
               (string-append (match:prefix m)
                              "/"
                              (match:suffix m)))))
        (else name)))

;; Create and return a url-path-to-filename mapping procedure based on
;; @var{docroot}.  The returned procedure @var{p} takes a (string)
;; @var{upath} and returns a valid local filename path for the requested
;; resource, or @code{#f} if that file cannot be found.  Optional arg
;; @var{dir-indexes} specifies an ordered list of filenames to try if
;; the resolved filename path turns out to be a directory.
;;
;; If no such files exist, return the directory name.  As a special
;; case, when @var{p} encounters a value of @code{#f} during iteration
;; over @var{dir-indexes}, it returns @code{#f} immediately.
;;
;; For example, presuming files @file{/a/b/c.txt} and
;; @file{/a/b/index.html} both exist and are readable:
;;
;; @example
;; (define resolve (upath->filename-proc
;;                    "/a/b/"
;;                    '("index.shtml" "index.html")))
;;
;; (resolve "/random") @result{} #f
;; (resolve "/c.txt") @result{} "/a/b/c.txt"
;; (resolve "/") @result{} "/a/b/index.html"
;; @end example
;;
;; Directory names are always returned with a trailing slash.
;;
;;-sig: (docroot [dir-indexes])
;;
(define (upath->filename-proc docroot . idx)
  (or (null? idx) (set! idx (car idx)))
  ;; rv
  (lambda (upath)
    (let ((filename (cleanup-filename (in-vicinity docroot upath))))
      (and (file-exists? filename)
           (case (stat:type (stat filename))
             ((regular) filename)
             ((directory)
              (set! filename (in-vicinity filename "")) ; ensure trailing /
              (let loop ((ls idx))
                (cond ((null? ls)
                       filename)
                      ((eq? #f (car ls))
                       #f)
                      (else
                       (let ((full (string-append filename (car ls))))
                         (or (and (file-exists? full)
                                  (eq? 'regular (stat:type (stat full)))
                                  full)
                             (loop (cdr ls))))))))
             (else #f))))))

;; Return a valid Content-Type string which matches @var{filename} best.
;; Matching is done by comparing the extension (part of @var{filename} after
;; the last "." if available) against a table.  If none match, return
;; "application/octet-stream".  Optional arg @var{default} specifies another
;; value to use instead of "application/octet-stream".
;;
;; @strong{NOTE}@* As of Guile-WWW 2.24, the internal table is populated from
;; @code{*content-type-by-filename-extension*} (@pxref{content-type}) the
;; first time @code{filename->content-type} is called.  This initialization
;; will be dropped after 2009-12-31; you will need to populate it yourself
;; after that (@pxref{mime-types}).
;;
;; If there are multiple MIME types associated with the extension,
;; return the first one.
;;
;;-sig: (filename [default])
;;
(define (filename->content-type filename . default)
  (or TABLE-OK? ;; TODO: ZONK after 2009-12-31.
      (let* ((alist *content-type-by-filename-extension*)
             (exts (map car alist))
             (mime-types (map string->symbol (map cdr alist))))
        (apply put-mime-types! 'stomp
               (apply append (map list exts mime-types)))
        (set! TABLE-OK? #t)))
  (or (and-let* ((cut (string-rindex filename #\.))
                 (mt (mime-types<-extension (subs filename (1+ cut)))))
        (symbol->string (if (pair? mt)
                            (car mt)
                            mt)))
      ;; use ‘if’ here to allow #f for ‘default’
      (if (not (null? default))
          (car default)
          "application/octet-stream")))

(define TABLE-OK? #f) ;; TODO: ZONK after 2009-12-31.

;;; (www server-utils filesystem) ends here
