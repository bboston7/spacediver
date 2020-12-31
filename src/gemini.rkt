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

(require typed/net/url
         typed/openssl
         racket/format
         racket/match
         racket/port
         racket/string)

(provide parse-mime transact)

(: GEMINI_PORT Positive-Integer)
(define GEMINI_PORT 1965)

#|
Parse a mime of form type/subtype to a pair of (type . subtype).  Ignores
parameters.
|#
(: parse-mime (-> String (Pair Symbol Symbol)))
(define (parse-mime mime)
  (define base (car (string-split mime ";")))
  (define split (string-split base "/"))
  `(,(string->symbol (car split)) . ,(string->symbol (cadr split))))

(: transact (-> URL
                (Option SSL-Client-Context)
                (U (Listof String) (Pair (Pair Symbol Symbol) Bytes))))
(define (transact resource context)
  ; Connect
  (define resource-port (url-port resource))
  (: port Positive-Integer)
  (define port
    (if (and resource-port (positive? resource-port))
        resource-port
        GEMINI_PORT))
  (define-values (input output)
    (ssl-connect (assert (url-host resource))
                 port
                 (if context context 'tls12)))
  ; Make request
  (write-string (~a (url->string resource) "\r\n") output)
  (flush-output output)
  ; Read header
  (define header (read-line input))
  (cond
    [(eof-object? header) null]
    [(string-prefix? header "20")
     ; Parse mime type
     (define mime-type (parse-mime (string-trim (substring header 3))))
     (match mime-type
       [(cons 'text _)
        ; Append remaining page as list and return
        (cons header (port->lines input #:close? #t))]
       [_
        ; Process remaining page as bytes.  Strip header and return mime type
        ; as a pair of symbols
        (cons mime-type (port->bytes input #:close? #t))])]
    [else
     ; Read response as list, with header prepended
     (cons header (port->lines input #:close? #t))]))
