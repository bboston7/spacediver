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
         add-bookmark
         edit-bookmarks
         init-bookmarks)

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
      `("# Spacediver"
        ""
        "Welcome to Spacediver!  Use the 'h' command for help."
        ""
        "## Bookmarks"
        ""
        "```"
        "To use the bookmark manager:"
        "* 'm' opens this page."
        "* 'm a [name]' or 'm add [name]' adds a bookmark."
        "   Optionally, provide [name] to give the bookmark a name."
        "* 'm e' or 'm edit' edits the bookmarks file.  This is"
        "   the only way to modify or remove bookmarks.  The file will be"
        "   opened in $EDITOR.  If $EDITOR is unset, it will be opened in"
        "   vi.  Be sure to end the file with a newline.  Most editors do"
        "   this by default."
        "```"
        ""
        "=> gemini://gemini.circumlunar.space/ Project Gemini Homepage")
      BOOKMARKS_PATH)))

#|
Add a bookmark to the bookmark file
|#
(: add-bookmark (-> URL (Option String) Void))
(define (add-bookmark url description)
  (init-bookmarks)
  (display-to-file
    (~a "=> " (url->string url) (if description (~a " " description) "") "\n")
    BOOKMARKS_PATH
    #:exists 'append))

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
