#!/usr/bin/env racket

#|
Copyright (C) 2020  Brett Boston

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
|#

#lang typed/racket/base

(require racket/format
         racket/match
         racket/string
         typed/net/url
         "src/renderer.rkt")

(define REPL_PROMPT "dive> ")

(: repl (-> Void))
(define (repl)
  (display REPL_PROMPT)
  (flush-output)
  (define expr (read-line))
  (cond
    [(eof-object? expr) (exit 0)]
    [else
      (match expr
        ; Open URL
        [(regexp #rx"^o ") (handle-url (string-trim (substring expr 2)))]
        ; Follow a link
        [(regexp #rx"^l ") (handle-link (string-trim (substring expr 2)))]
        ; Display raw gemtext for current page
        ["raw" (display-gemtext #t)]
        ; Re-display pretty printed gemtext for current page
        ["pretty" (display-gemtext #f)]
        ; Go back one page (if possible)
        ["b" (handle-history current-pages current-forwards #t)]
        ; For forward one page (if possible)
        ["f" (handle-history current-forwards current-pages #f)]
        ; Save current page to a file
        [(regexp #rx"^w ") (write-gemtext (string-trim (substring expr 2)))]
        ; Scroll to the top of the page
        ["t" (goto-top)]
        ; Print the current page's URL
        ["p" (displayln (url->string (caar (current-pages))))]
        ; Quit
        ["q" (exit 0)]
        ; Treat everything else as links
        [_ (handle-link expr)])
      (repl)]))

; TODO: Main function

(: main-loop (-> Void))
(define (main-loop)
  (with-handlers ([exn:fail?
                   (λ ([x : exn])
                     ; Write out gemtext, print error, and try to continue
                     (write-gemtext "exception.gmi")
                     (displayln (~a "An exception occurred.  Attempting to "
                                    "continue, but internal state may be "
                                    "inconsistant!")
                                (current-error-port))
                     ((error-display-handler) (exn-message x) x))])
    (repl))
  (main-loop))

(main-loop)

;(display-gemtext (transact (string->url "gemini://gemini.circumlunar.space/")))
