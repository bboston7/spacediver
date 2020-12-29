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

(define-syntax command-list
  (syntax-rules ()
    [(command-list) null]
    ; For commands that don't take an argument
    [(command-list [name f help-body] binding ...)
     (cons (list (λ ([expr : String]) (equal? name expr))
                 (λ ([expr : String]) f)
                 name
                 help-body)
           (command-list binding ...))]
    ; For command that do take an argument
    [(command-list [starts-with expr f arg-name help-body] binding ...)
     (cons (list (λ ([expr : String]) (string-prefix? expr starts-with))
                 (λ ([expr : String]) f)
                 (~a starts-with
                     (if (non-empty-string? starts-with) " " "")
                     "<" arg-name ">")
                 help-body)
           (command-list binding ...))]))

(define-type Command (List (-> String Boolean) (-> String Void) String String))

(: COMMANDS (Listof Command))
(define COMMANDS
  (command-list
    ["o" expr (handle-url (string-trim (substring expr 2)))
     "URL" "Open URL"]
    ["raw" (display-gemtext #t) "Display raw gemtext for current page"]
    ["pretty" (display-gemtext #f)
     "Re-display pretty printed gemtext for current page"]
    ["b" (handle-history current-pages current-forwards #t)
     "Go back one page (if possible)"]
    ["f" (handle-history current-forwards current-pages #f)
     "Go forward one page (if possible)"]
    ["w" expr (write-gemtext (string-trim (substring expr 2)))
     "file" "Save current page to <file>"]
    ["l" expr (load-gemtext (string-trim (substring expr 2)))
     "file" "Load <file>"]
    ["t" (goto-top)
     "Scroll to the top of the page (must be running in tmux)"]
    ["p" (displayln (url->string (caar (current-pages))))
     "Print the current page's URL"]
    ["q" (exit 0) "Quit"]
    ["h" (display-help) "Display this help message"]
    ["m" expr (handle-mark (if (> (string-length expr) 2)
                               (string-trim (substring expr 2))
                               ""))
     "subcommand" "Manage bookmarks.  Leave <subcommand> empty to open bookmarks list and display bookmark help."]
    ; Treat everything else as links
    ["" expr (handle-link expr)
     "link number" "Follow a link"]))

(define HELP_LHS_LEN
  (+ 2
     (foldl (λ ([cmd : Command] [res : Integer])
               (max (string-length (caddr cmd)) res))
            0
            COMMANDS)))

#|
The EP part of REPL
|#
(: eval-print (-> String Void))
(define (eval-print expr)
  (set! expr (string-trim expr))
  (define cmd
    (assert (memf (λ ([elem : Command]) ((car elem) expr)) COMMANDS)))
  ((cadar cmd) expr))

(: display-help (-> Void))
(define (display-help)
  (for ([cmd : Command COMMANDS])
    (: lhs String)
    (define lhs (caddr cmd))
    (displayln (~a "  "
                   lhs
                   (make-string (- HELP_LHS_LEN (string-length lhs)) #\ )
                   (cadddr cmd)))))

(: repl (-> Void))
(define (repl)
  (display REPL_PROMPT)
  (flush-output)
  ; R
  (define expr (read-line))
  (cond
    [(eof-object? expr) (exit 0)]
    ; EP
    [else (eval-print expr)
          ; L
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

(load-bookmarks)
(main-loop)

;(display-gemtext (transact (string->url "gemini://gemini.circumlunar.space/")))
