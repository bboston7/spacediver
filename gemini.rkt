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
         racket/format)

(provide transact)

(: GEMINI_PORT Positive-Integer)
(define GEMINI_PORT 1965)

(: read-lines (-> Input-Port (Listof String)))
(define (read-lines input)
  (define line (read-line input))
  (cond
    [(eof-object? line) null]
    [else (cons line (read-lines input))]))

(: transact (-> URL (Listof String)))
(define (transact resource)
  ; Connect
  (define resource-port (url-port resource))
  (: port Positive-Integer)
  (define port
    (if (and resource-port (positive? resource-port))
        resource-port
        GEMINI_PORT))
  (define-values (input output)
    (ssl-connect (assert (url-host resource)) port 'tls12))
  ; Make request
  (write-string (~a (url->string resource) "\r\n") output)
  (flush-output output)
  ; Read response
  (read-lines input))
