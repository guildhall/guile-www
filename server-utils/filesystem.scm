;;; www/server-utils/filesystem.scm --- Work with the local filesystem

;; 	Copyright (C) 2004 Free Software Foundation, Inc.
;;      Copyright (C) 2000, 2001 Martin Grabmueller
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

;;; Commentary:

;; The (www server-utils filesystem) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils filesystem)
  #:use-module (ice-9 regex)
  #:autoload (www data content-type) (*content-type-by-filename-extension*)
  #:export (access-forbidden?-proc
            cleanup-filename
            upath->filename-proc
            filename->content-type))

(define subs make-shared-substring)

;; Create and return a filesystem-access procedure based on @var{docroot}
;; and @var{forbid-rx}.  The returned procedure @var{p} takes a @var{filename}
;; and returns #t if access to that file should be denied for any of the
;; following reasons:
;;
;; @itemize
;; @item @var{filename} is the null string
;; @item @var{filename} does not begin with slash
;; @item @var{filename} is not longer than @var{docroot}
;; @item @var{filename} does not begin with @var{docroot}
;; @item @var{filename} matches regular expression @var{forbid-rx}
;; @end itemize
;;
;; If @var{forbid-rx} is #f, the regular expression check is skipped.
;; @var{p} returns #f if access should be granted.
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
;; Cleaning removes "FOO/../", and collapses both "/./" and "//" into "/".
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
;; @var{docroot}.  The returned procedure @var{p} takes a (string) @var{upath}
;; and returns a valid local filename path for the requested resource, or #f
;; if that file cannot be found.  Optional arg @var{dir-indexes} specifies an
;; ordered list of filenames to try if the resolved filename path turns out
;; to be a directory.  If no such files exist, return the directory name.
;; As a special case, when @var{p} encounters a value of #f during iteration
;; over @var{dir-indexes}, it returns #f immediately.
;;
;; For example, presuming files @file{/a/b/c.txt} and @file{/a/b/index.html}
;; both exist and are readable:
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
;;-sig: (filename [default])
;;
(define (filename->content-type filename . default)
  (or (and=> (string-rindex filename #\.)
             (lambda (cut)
               (assq-ref *content-type-by-filename-extension*
                         (string->symbol (subs filename (1+ cut))))))
      ;; use `if' here to allow #f for `default'
      (if (not (null? default))
          (car default)
          "application/octet-stream")))

;;; www/server-utils/filesystem.scm ends here
