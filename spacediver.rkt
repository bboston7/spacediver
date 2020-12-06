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

(require racket/file
         racket/format
         racket/match
         racket/string
         racket/system
         typed/net/mime
         typed/net/url
         "gemini.rkt")

(define REPL_PROMPT "dive> ")

; TODO: Control these with command line flags
(define DEBUG (make-parameter #f))
(define current-link-number (make-parameter 1))
(define LINE_WIDTH 80)

; A history of a list of pairs of urls and page content.  The URLS should
; always be absolute paths.
(define-type History (Listof (Pair URL (Listof String))))

; Gemtext list for page history.  In theory this could get large, but in
; practice I doubt it'll be an issue unless gemsites become as large as
; websites
(: current-pages (Parameterof History))
(define current-pages (make-parameter null))

; Like current-pages, but to enable forward history
(: current-forwards (Parameterof History))
(define current-forwards (make-parameter null))

(: debug (-> Any Void))
(define (debug msg) (when (DEBUG) (displayln (~a "DEBUG: " msg))))

(: links (Mutable-HashTable Integer String))
(define links (make-hasheq))

(: reset-state (-> Void))
(define (reset-state)
  ; Clear link state
  (hash-clear! links)
  (current-link-number 1)

  ; Clear terminal
  (display "\033c"))

(: display-link (-> String Void))
(define (display-link link)
  (define tokens (string-split link))

  ; If link to a gemini page, give it a number.  Otherwise, display the scheme
  ; instead of a number to make it unselectable.
  (define link-scheme (url-scheme (string->url (cadr tokens))))
  (define gemini? (or (not link-scheme) (equal? link-scheme "gemini")))
  (define link-id (if gemini? (current-link-number) link-scheme))

  ; Display the link line
  (displayln (~a
    ; link number
    "\033[1m[" link-id "]\033[0m  \033[4;36m"
    (if (null? (cddr tokens))
      ; No description, show url
      (cadr tokens)
      ; Show description
      (string-join (cddr tokens)))
    ; Reset formatting
    "\033[0m"))

  (when gemini?
    ; Store link data
    (hash-set! links (current-link-number) (cadr tokens))
    (current-link-number (add1 (current-link-number)))))

(: display-header (-> String Void))
(define (display-header header)
  ; Write out color based on type of header
  (display
    (match header
      ; h3
      [(regexp #rx"^###") "\033[1;35m"]
      ; h2
      [(regexp #rx"^##")  "\033[1;32m"]
      ; h1
      [_                  "\033[1;31m"]))
  (displayln (~a header "\033[0m")))

#|
Display a line of text, with some smart wrapping for readability
|#
(: display-text (-> String Void))
(define (display-text line)
  (: display-token (-> (Listof String) Integer Void))
  (define (display-token tokens chars)
    (unless (null? tokens)
      (define token (car tokens))
      (define token-len (string-length token))
      (cond
        [(> token-len chars)
         ; Add newline, followed by token, then recurse
         (display (~a "\n" token " "))
         (display-token (cdr tokens) (- LINE_WIDTH token-len 1))]
        [(= token-len chars)
         ; Omit trailing space
         (display token)
         ; Recurse with 0 as argument, in case (null? (cdr tokens))
         (display-token (cdr tokens) 0)]
        [else
         (display (~a token " "))
         (display-token (cdr tokens) (- chars token-len 1))])))

  (display-token (string-split line) LINE_WIDTH)
  (displayln ""))

#|
Given a status line, builds a redirect page to render
|#
(: build-redirect-page (-> String (Listof String)))
(define (build-redirect-page status-line)
  (define dest (cadr (string-split status-line)))
  `("# Redirect"
    ""
    ,(~a "This page would like to redirect you.  Accept the redirect by "
         "following the link below, or use the back command (b) to go back.")
    ""
    ,(~a "=> " dest)))



#|
Display the current page

Parameters:
  raw - True to display raw gemtext, false to pretty print
|#
(: display-gemtext (-> Boolean Void))
(define (display-gemtext raw)
  (reset-state)
  (: page (Listof String))
  (define page (cdar (current-pages)))

  ; Track whether or not the parser is in a pre block
  (: current-pre (Parameterof Boolean))
  (define current-pre (make-parameter #f))

  ; Skip first line (status code) if pretty printing and status code is 20, but
  ; otherwise include it
  (define lines
    (cond
      ; No pre-rendering of raw text
      [raw page]
      ; Success code, parse mime type to figure out what to do next
      [(string-prefix? (car page) "20")
       (define mime-str (string-trim (substring (car page) 3)))
       (match (parse-mime mime-str)
         ; Strip status line and render gemini
         [(cons 'text 'gemini) (cdr page)]
         ; For all other forms of text, put renderer in raw mode and print
         [(cons 'text _) (set! raw #t)
                         (cdr page)]
         ; Should only be text mime types in the history lists
         [_ (error (~a "Unexpected non-text mime type "
                       mime-str
                       " in display-gemtext"))])]
      ; Build redirect gemtext on redirect status code
      [(string-prefix? (car page) "3") (build-redirect-page (car page))]
      ; Display all other status codes as-is
      [else page]))

  ; Parse lines
  (for ([line lines])
    (cond
      ; Raw mode does no processing
      [raw (displayln line)]
      ; Start or end of a pre block
      [(regexp-match #rx"^```" line) (current-pre (not (current-pre)))]
      ; In a pre block, ignore formatting
      [(current-pre) (displayln line)]
      ; Link
      [(regexp-match #rx"^=>" line) (display-link line)]
      ; Header
      [(regexp-match #rx"^#" line) (display-header line)]
      ; Normal text
      [else (display-text line)]))

  ; Add an extra newline before the prompt
  (displayln ""))

(: handle-bytes (-> (Pair (Pair Symbol Symbol) Bytes) Void))
(define (handle-bytes page)
  (match (car page)
    ; Open images in feh
    [(cons 'image _)
     (process/ports #f
                    (open-input-bytes (cdr page))
                    #f
                    "feh -")
     (void)]
    [_
     (displayln (~a "Unsupported mime type: " (car page)))
     (display "Enter path to save file to (leave blank to discard): ")
     (define path (read-line))
     (when (non-empty-string? path)
       (display-to-file (cdr page) path))]))

#|
Given a url string, transacts with the server and displays rendered gemtext.
Handles both relative and absolute paths.
|#
(: handle-url (-> String Void))
(define (handle-url urlstr)
  ; Check URL
  (define url (string->url urlstr))
  ; TODO: Add gemini:// if missing

  ; Translate to absolute path if necessary
  (define absolute-url
    (if (url-host url)
      ; Check for unsupported protocols
      (cond
        [(and (url-scheme url) (not (equal? (url-scheme url) "gemini")))
         (displayln (~a "Unsupported URL scheme: " (url-scheme url)))
         #f]
        [else url])
      (combine-url/relative (caar (current-pages)) urlstr)))

  (when absolute-url
    ; Transact and display
    (define page (transact absolute-url))
    (cond
      [(list? page)
       ; This is a text page
       (current-pages (cons `(,absolute-url . ,page) (current-pages)))
       (display-gemtext #f)
       ; clear forwards
       (current-forwards null)]
      [(pair? page)
       ; This is a non-text page
       (handle-bytes page)])))

(: handle-link (-> String Void))
(define (handle-link key)
  (define link-number (string->number (string-trim key)))
  (if (fixnum? link-number)
    (let ([url (hash-ref links link-number (λ () #f))])
      (if url
        (handle-url url)
        (displayln (~a "No such link number: " link-number))))
    (displayln (~a "Illegal link number: " key))))

#|
Handle history, moving from `from` to `to`, unless `from` is empty, in which
case do nothing.  If `buffer-from` is #t, then `from` will not be popped if it
has only 1 element.
|#
(: handle-history (-> (Parameterof History) (Parameterof History) Boolean Void))
(define (handle-history from to buffer-from?)
  (unless (or (null? (from)) (and buffer-from? (null? (cdr (from)))))
    (to (cons (car (from)) (to)))
    (from (cdr (from)))
    (display-gemtext #f)))

#|
Writes the gemtext for the current page to `path`
|#
(: write-gemtext (-> String Void))
(define (write-gemtext path)
  (display-lines-to-file (cddar (current-pages))
                         path
                         #:exists 'truncate/replace))

#|
Use tmux to go to the top of the page
|#
(: goto-top (-> Void))
(define (goto-top)
  (system "tmux copy-mode")
  (system "tmux send-keys g")
  (void))


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
