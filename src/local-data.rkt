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
         racket/string
         racket/system
         typed/net/url)

(provide BOOKMARKS_PATH
         HISTORY_PATH
         add-bookmark
         add-history
         edit-bookmarks
         init-bookmarks
         init-history)

; Default editor to use if $EDITOR is not set
(define DEFAULT_EDITOR "vi")

; Directory to store local data in.  Follows XDG conventions
(define DATA_DIR
  (let* ([xdg-data-home (getenv "XDG_DATA_HOME")]
         [data-home (if (and xdg-data-home (not (non-empty-string?
                                                 xdg-data-home)))
                        xdg-data-home
                        (~a (assert (getenv "HOME") non-empty-string? )
                            "/.local/share"))])
    (~a data-home "/spacediver")))

; File to store bookmarks in.  This is just a normal gemtext file.
(define BOOKMARKS_PATH (~a DATA_DIR "/bookmarks.gmi"))

; File to store history in.  This is a normal gemtext file
(define HISTORY_PATH (~a DATA_DIR "/history.gmi"))

#|
Creates the data directory, if it doesn't already exist
|#
(: make-data-dir (-> Void))
(define (make-data-dir)
  (unless (directory-exists? DATA_DIR)
    (make-directory DATA_DIR)))

#|
Creates the initial bookmark file, if it doesn't already exist
|#
(: init-bookmarks (-> Void))
(define (init-bookmarks)
  (make-data-dir)
  (unless (file-exists? BOOKMARKS_PATH)
    (display-lines-to-file
      '("# Spacediver"
        ""
        "Welcome to Spacediver!  Use the 'h' command for help."
        ""
        "## Bookmarks"
        ""
        "=> gemini://gemini.circumlunar.space/ Project Gemini Homepage")
      BOOKMARKS_PATH)))

#|
Append a link to a gmi file
|#
(: add-to-link-file (-> URL (Option String) String Void))
(define (add-to-link-file url description path)
  (display-to-file
    (~a "=> " (url->string url) (if description (~a " " description) "") "\n")
    path
    #:exists 'append))

#|
Add a bookmark to the bookmark file
|#
(: add-bookmark (-> URL (Option String) Void))
(define (add-bookmark url description)
  (init-bookmarks)
  (add-to-link-file url description BOOKMARKS_PATH))

#|
Open bookmarks file in $EDITOR (or vi if $EDITOR is unset)
|#
(: edit-bookmarks (-> Void))
(define (edit-bookmarks)
  (init-bookmarks)
  (define editor (getenv "EDITOR"))
  (system (~a (if (and editor (non-empty-string? editor))
                  editor
                  DEFAULT_EDITOR)
              " "
              BOOKMARKS_PATH))
  (void))

#|
Creates the initial history file, if it doesn't already exist
|#
(: init-history (-> Void))
(define (init-history)
  (make-data-dir)
  (unless (file-exists? HISTORY_PATH)
    (display-to-file "# History\n\n" HISTORY_PATH)))

#|
Add an entry to the history file
|#
(: add-history (-> URL Void))
(define (add-history url)
  (init-history)
  (add-to-link-file url #f HISTORY_PATH))
