(define-module (www https))
(use-modules (ice-9 format))
(use-modules (ice-9 popen))

(define-public https:version "HTTP/1.0")  ; bump up to 1.1 when ready
(define-public https:user-agent "GuileHTTP 0.1")

;; BUG -- I don't really want this function to be public; I want
;; people to use `www:get' instead.  But if I use `define' here,
;; instead of `define-public', then when main.scm calls
;; www:set-protocol-handler!, https-via-lynx isn't defined.  This
;; means that I don't understand Guile's module system.

(define-public (https-via-lynx host ip-port path)
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
