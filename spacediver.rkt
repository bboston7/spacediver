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
         "gemini.rkt")

(define REPL_PROMPT "dive> ")

; TODO: Control these with command line flags
(define DEBUG (make-parameter #f))
(define current-link-number (make-parameter 1))

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
  (displayln (~a
    ; link number
    "\033[1m[" (current-link-number) "]\033[0m  \033[4;36m"
    (if (null? (cddr tokens))
      ; No description, show url
      (cadr tokens)
      ; Show description
      (string-join (cddr tokens)))
    ; Reset formatting
    "\033[0m"))

  ; Store link data
  (hash-set! links (current-link-number) (cadr tokens))
  (current-link-number (add1 (current-link-number))))

(: display-header (-> String Void))
(define (display-header header)
  ; Write out color based on type of header
  (display
    (match header
      ; h3
      [(regexp #rx"^###") "\033[1;33m"]
      ; h2
      [(regexp #rx"^##")  "\033[1;32m"]
      ; h1
      [_                  "\033[1;31m"]))
  (displayln (~a header "\033[0m")))

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
    (if (or raw (not (regexp-match #rx"^20" (car page)))) page (cdr page)))

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
      [else (displayln line)]))

  ; Add an extra newline before the prompt
  (displayln ""))

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
    (if (url-path-absolute? url)
      ; Check for unsupported protocols
      (cond
        [(and (url-scheme url) (not (equal? (url-scheme url) "gemini")))
         (displayln (~a "Unsupported URL scheme: " (url-scheme url)))
         #f]
        [else url])
      (combine-url/relative (caar (current-pages)) urlstr)))

  (when absolute-url
    ; Transact and display
    (current-pages (cons `(,absolute-url . ,(transact absolute-url))
                         (current-pages)))
    (display-gemtext #f)

    ; clear forwards
    (current-forwards null)))

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
case do nothing
|#
(: handle-history (-> (Parameterof History) (Parameterof History) Void))
(define (handle-history from to)
  (unless (or (null? (from)) (null? (cdr (from))))
    (to (cons (car (from)) (to)))
    (from (cdr (from)))
    (display-gemtext #f)))

(: repl (-> Void))
(define (repl)
  (display REPL_PROMPT)
  (flush-output)
  (define expr (read-line))
  (cond
    [(eof-object? expr) (void)]
    [else
      (match expr
        ; Open URL
        [(regexp #rx"^o ") (handle-url (substring expr 2))]
        ; Follow a link
        [(regexp #rx"^l ") (handle-link (substring expr 2))]
        ; Display raw gemtext for current page
        ["raw" (display-gemtext #t)]
        ; Re-display pretty printed gemtext for current page
        ["pretty" (display-gemtext #f)]
        ; Go back one page (if possible)
        ["b" (handle-history current-pages current-forwards)]
        ; For forward one page (if possible)
        ["f" (handle-history current-forwards current-pages)]
        ; Treat everything else as links
        [_ (handle-link expr)])
      (repl)]))

; TODO: Main function

(repl)

;(display-gemtext (transact (string->url "gemini://gemini.circumlunar.space/")))