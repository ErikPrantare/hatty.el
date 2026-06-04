;;; hatty.el --- Query positions through hats        -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 2.0.0
;; Homepage: https://github.com/ErikPrantare/hatty.el
;; Package-Requires: ((emacs "29.1"))
;; Created: 05 Jul 2024

;; hatty.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; hatty.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality for drawing and locating
;; "hats", small colored symbols used for indexing the text in the
;; visible portion of the buffer in the current window.

;; Setup: Call ‘global-hatty-mode’ to start displaying hats.  The
;; token of a hat may then be queried using the function
;; ‘hatty-locate-token’.

;;; Code:

(require 'subr-x)
(require 'svg)  ; Hat rendering
(require 'term) ; Default colors

(defgroup hatty nil
  "Index buffer locations through character hats."
  :group 'convenience
  :prefix "hatty-"
  :link '(emacs-commentary-link :tag "Commentary" "hatty.el"))

(defcustom hatty-colors
  (let ((default-colors
         `((default . ,(face-attribute 'term :foreground nil t))
           (yellow . ,(face-attribute 'term-color-yellow :foreground nil t))
           (red . ,(face-attribute 'term-color-red :foreground nil t))
           (blue . ,(face-attribute 'term-color-blue :foreground nil t))
           (pink . ,(face-attribute 'term-color-magenta :foreground nil t))
           (green . ,(face-attribute 'term-color-green :foreground nil t)))))

    (if (fboundp 'modus-themes-color) ; Guard to make byte compiler happy
        (cond
         ((memq 'modus-vivendi custom-enabled-themes)
          `((default . ,(modus-themes-color 'fg-dim))
            (yellow . ,(modus-themes-color 'yellow-graph-0-bg))
            (red . ,(modus-themes-color 'red-intense))
            (blue .  ,(modus-themes-color 'blue-graph-0-bg))
            (pink . ,(modus-themes-color 'magenta-graph-0-bg))
            (green . ,(modus-themes-color 'green))))

         ;; No yellow here, as it is not very visible.
         ((memq 'modus-operandi custom-enabled-themes)
          `((default . ,(modus-themes-color 'fg-main))
            (red . ,(modus-themes-color 'red-intense))
            (blue .  ,(modus-themes-color 'blue-intense-bg))
            (pink . ,(modus-themes-color 'magenta-graph-0-bg))
            (green . ,(modus-themes-color 'green-graph-0-bg))))

         (t default-colors))
      default-colors))
  "Alist of colors used in rendering hats, indexed by identifier.

Identifiers must to be symbols.

The identifier symbol `default' indicates the default color."
  :type '(alist :key-type symbol :value-type color)
  :group 'hatty)

;; TODO: Use proper svgs here?

;; NOTE: "cross" does not render with the original opening in the
;; middle.  However, I quite like it filled in, so I might try to keep
;; it that way if I implement the above TODO.
(defcustom hatty-shapes
  '((default  . "M6 9C9.31371 9 12 6.98528 12 4.5C12 2.01472 9.31371 0 6 0C2.68629 0 0 2.01472 0 4.5C0 6.98528 2.68629 9 6 9Z")
    (bolt  . "M 12,4 V 0 C 12,0 9,4 8,4 7,4 3,0 3,0 L 0,5 V 9 C 0,9 3,5 4,5 5,5 9,9 9,9 Z")
    (curve  . "M6.00016 3.5C10 3.5 12 7.07378 12 9C12 4 10.5 0 6.00016 0C1.50032 0 0 4 0 9C0 7.07378 2.00032 3.5 6.00016 3.5Z")
    (fox . "M6.00001 9L0 0C0 0 3.71818 2.5 6 2.5C8.28182 2.5 12 0 12 0L6.00001 9Z")
    (frame . "M0 0.000115976V8.99988H12V0L0 0.000115976ZM9.5 6.5H6H2.5V4.5V2.5H6H9.5V4.5V6.5Z")
    (play . "M12 4.49999L0 9C0 9 3 6.2746 3 4.49999C3 2.72537 0 0 0 0L12 4.49999Z")
    (wing . "M6 0C6 0 7 3 8.5 4.5C10 6 12 7 12 7V9C12 9 8.5 7 6 7C3.5 7 0 9 0 9V7C0 7 2 6 3.5 4.5C5 3 6 0 6 0Z")
    (hole . "M1.5 4.5L0 7H2.5L3.5 9L6 7.5L8.5 9L9.5 7H12L10.5 4.5L12 2H9.5L8.5 0L6 1.5L3.5 0L2.5 2H0Z M6 5.5L4 6.5L3 4.5L4 2.5L6 3.5L8 2.5L9 4.5L8 6.5L6 5.5Z")
    (ex . "M9.99997 9C9.99997 9 7.5 6.5 6 6.5C4.5 6.5 2 9 2 9C2 9 0.999999 9 0 9C0 9 2.5 6 2.5 4.5C2.5 3 6.5473e-05 0 6.5473e-05 0C6.5473e-05 0 1 0 2 0C2 0 4.5 2.5 6 2.5C7.5 2.5 9.99997 0 9.99997 0C11 0 12 0 12 0C12 0 9.5 3 9.5 4.5C9.5 6 12 9 12 9C12 9 11 9 9.99997 9Z")
    (cross ."M5.25 0C5.25 0 4.5 1.5 3.5 2.5C2.49483 3.50517 0 3.75 0 3.75V5.25C0 5.25 2.49483 5.49483 3.5 6.5C4.5 7.5 5.25 9 5.25 9H6.75C6.75 9 7.5 7.5 8.5 6.5C9.50517 5.49483 12 5.25 12 5.25V3.75C12 3.75 9.50517 3.50517 8.5 2.5C7.5 1.5 6.75 0 6.75 0H5.25ZM5.75 6.5H6.25C6.25 6.5 6.58435 5.25599 7 5C7.41565 4.74401 8.75 4.75 8.75 4.75V4.25C8.75 4.25 7.41565 4.25599 7 4C6.58435 3.74401 6.25 2.5 6.25 2.5H5.75C5.75 2.5 5.41565 3.74401 5 4C4.58435 4.25599 3.25 4.25 3.25 4.25V4.75C3.25 4.75 4.58435 4.74401 5 5C5.41565 5.25599 5.75 6.5 5.75 6.5Z" )
    (eye . "M12 4L6.5 0H5.5L0 4V5L5.5 9H6.5L12 5V4ZM6 7.5C6 7.5 4.5 6.5 4.5 4.5C4.5 2.5 6.01103 1.5 6 1.5C6 1.5 7.5 2.5 7.5 4.5C7.5 6.5 6 7.5 6 7.5Z"))
  "Alist of shapes used in rendering hats, indexed by identifier.

Identifiers must be symbols.  The shapes must specify valid svg
paths.

The identifier symbol `default' indicates the default ."
  :type '(alist :key-type symbol :value-type string)
  :group 'hatty
  ;; The following notice is included for compliance with the license of
  ;; the cursorless project, from which the above hat designs were
  ;; copied.  The "bolt" path has been modified to be symmetric.
  ;;
  ;; MIT License
  ;;
  ;; Copyright (c) 2021 Brandon Virgil Rule
  ;;
  ;; Permission is hereby granted, free of charge, to any person
  ;; obtaining a copy of this software and associated documentation
  ;; files (the "Software"), to deal in the Software without
  ;; restriction, including without limitation the rights to use, copy,
  ;; modify, merge, publish, distribute, sublicense, and/or sell copies
  ;; of the Software, and to permit persons to whom the Software is
  ;; furnished to do so, subject to the following conditions:
  ;;
  ;; The above copyright notice and this permission notice shall be
  ;; included in all copies or substantial portions of the Software.
  ;;
  ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  ;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  ;; SOFTWARE.
  )

(defcustom hatty-scale-factor 1.0
  "Scale factor for hats.
The size of the hats get multiplied by this number: A factor of 2.0
will double hat sizes, while a factor of 0.5 will halve them."
  :type 'number
  :group 'hatty)

(defcustom hatty-color-default-penalty 1
  "Penalty for colors not in `hatty-color-penalties'."
  :type 'number
  :group 'hatty)

(defcustom hatty-shape-default-penalty 1
  "Penalty for shapes not in `hatty-shape-penalties'."
  :type 'number
  :group 'hatty)

(defcustom hatty-color-penalties
  '((default . 0)
    (yellow . 2)
    (red . 1)
    (blue . 1)
    (pink . 1)
    (green . 1))
  "Penalty for using a specific color.

Used in `hatty--penalty' to calculate the penalty for given style."
  :type '(alist :key-type symbol :value-type number)
  :group 'hatty)

(defcustom hatty-shape-penalties
  '((default . 0)
    (bolt . 1)
    (curve . 1)
    (fox . 1)
    (frame . 1)
    (play . 1)
    (wing . 1)
    (hole . 1)
    (ex . 1)
    (cross . 1)
    (eye . 1))
  "Penalty for using a specific shape.

Used in `hatty--penalty' to calculate the penalty for given style."
  :type '(alist :key-type symbol :value-type number)
  :group 'hatty)

(defun hatty--penalty (style)
  "Return penalty for using STYLE.

Hats with lower penalty will have a higher priority for better spots.

Penalties are looked up from `hatty-color-penalties' and
`hatty-shape-penalties' and summed.  If a color or shape is not found
in those, `hatty-color-default-penalty' or
`hatty-shape-default-penalty' is used instead."
  (+ (alist-get (car style)
                hatty-color-penalties
                hatty-color-default-penalty)
     (alist-get (cdr style)
                hatty-shape-penalties
                hatty-shape-default-penalty)))

(defvar hatty--hat-styles nil
  "List of hat styles to choose from, ordered by priority.

This is recalculated at the beginning of ‘hatty-reallocate’ to
create all combinations from ‘hatty-colors’ and ‘hatty-shapes’")

;; TODO: Define the structure with EIEIO instead?  For constructors.
(cl-defstruct hatty--hat
  "A hat with COLOR and SHAPE at MARKER over CHARACTER."
  marker
  character
  color
  shape
  token-region
  (space-deprived-p t))

(defvar-local hatty--hats '()
  "All hats located in the current buffer.")

(defvar hatty--normalize-character-function
  #'downcase
  "Function for normalizing characters.

The function should take one argument, CHARACTER, and return a
normalized representation of it.  The representation needs not be a
character itself.

When locating hats, the lookup will be made on the normalized version
of the characters.  Thus, one character matches another if their
normalized representations are `eq' to each other.

The function should be idempotent, or in other words,

  (funcall hatty--normalize-character-function
           (funcall hatty--normalize-character-function x))}

should be `eq' to

  (funcall hatty--normalize-character-function x)")

(defun hatty--normalize-character (character)
  "Return normalized version of CHARACTER.

The behavior of this function can be changed by setting
`hatty--normalize-character-function'."
  (funcall hatty--normalize-character-function character))

(defun hatty--locate-hat (character &optional color shape)
  "Get the hat over CHARACTER matching COLOR and SHAPE."
  (setq color (or color 'default))
  (setq shape (or shape 'default))
  (seq-find (lambda (hat) (and (eq color (hatty--hat-color hat))
                               (eq (hatty--normalize-character character)
                                   (hatty--hat-character hat))
                               (eq shape (hatty--hat-shape hat))))
            (apply #'append
                   (mapcar (lambda (window)
                             (with-current-buffer (window-buffer window)
                               hatty--hats))
                           (window-list-1 nil nil 'visible)))))

(defun hatty-locate-token (character &optional color shape)
  "Get token region of the hat over CHARACTER with COLOR and SHAPE.

COLOR and SHAPE should be identifiers as they occur in
‘hatty-colors’ and ‘hatty-shapes’.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used.

If there is no token corresponding to CHARACTER, COLOR and SHAPE, this
function returns nil."
  (when-let* ((hat (hatty--locate-hat character color shape)))
    (hatty--hat-token-region hat)))

(defun hatty--buffer-hats (&optional buffer-or-name)
  "Return all hats in BUFFER-OR-NAME.

If BUFFER-OR-NAME is omitted or nil, it defaults to the current buffer."
  (seq-filter (lambda (hat) (eq (marker-buffer (hatty--hat-marker hat))
                                (window-normalize-buffer buffer-or-name)))
              hatty--hats))

(defun hatty-token-at (&optional position buffer-or-name prefer-after)
  "Get the token at POSITION.
If POSITION is nil or not given, get the token at point.

BUFFER-OR-NAME denotes the buffer to look in.  If it is nil or not
given, but POSITION is a marker associated to a buffer, that buffer is
used instead.  Otherwise, the current buffer is used.

If POSITION is at the boundary of two tokens, the preceding token is
preferred unless PREFER-AFTER is non-nil."
  (setq position (or position (point)))
  (setq buffer-or-name (or buffer-or-name
                           (when (markerp position) (marker-buffer position))))
  (let ((candidates
         (thread-last
           (hatty--buffer-hats buffer-or-name)
           (seq-map #'hatty--hat-token-region)
           (seq-filter (lambda (token) (and (<= (car token) position)
                                            (>= (cdr token) position)))))))
    (when candidates
      (seq-first (seq-sort-by #'car (if prefer-after #'> #'<) candidates)))))

(put 'hatty-token 'bounds-of-thing-at-point #'hatty-token-at)

(defun hatty-forward-token (n)
  "Move point forward N tokens (backward if N is negative)."
  (let* ((buffer-hats (hatty--buffer-hats))
         (backward (< n 0))
         (hats-to-skip (cond
                        ((= n 0) nil)
                        (backward (seq-filter (lambda (hat) (> (point) (car (hatty--hat-token-region hat))))
                                              buffer-hats))
                        (t (seq-filter (lambda (hat) (< (point) (cdr (hatty--hat-token-region hat))))
                                       buffer-hats))))
         (regions-to-skip (seq-sort-by (if backward #'car #'cdr) (if backward #'> #'<)
                                       (seq-map #'hatty--hat-token-region hats-to-skip)))
         (iterations (abs n)))
    (while (and regions-to-skip (> iterations 0))
      (goto-char (if backward
                     (car (pop regions-to-skip))
                   (cdr (pop regions-to-skip))))
      (cl-decf iterations))))

(put 'hatty-token 'forward-op #'hatty-forward-token)

(defun hatty--make-hat (position token-region style)
  "Create a hat at POSITION with STYLE.
Associate the hat with the buffer given by `window-buffer'.

TOKEN-REGION denotes the region of the token that the hat indicates.

If COLOR or SHAPE is nil or unspecified, the default color or shape
will be used."
  (make-hatty--hat
   :character (hatty--normalize-character (char-after position))
   :color (car style)
   :marker (set-marker (make-marker) position (window-buffer))
   :shape (cdr style)
   :token-region (cons
                  (set-marker (make-marker) (car token-region))
                  (set-marker (make-marker) (cdr token-region)))))

(defvar hatty--free-styles '()
  "Alist mapping characters to list of free styles.")

(defun hatty--materialize-free-styles (character)
  "Create free style list for CHARACTER if not already present."
  (when (eq (alist-get character hatty--free-styles 'not-present) 'not-present)
    (push (cons character
                (copy-sequence hatty--hat-styles))
          hatty--free-styles)))

(defun hatty--next-style (character)
  "Get the next style applicable to CHARACTER."
  (hatty--materialize-free-styles character)
  (car (alist-get character hatty--free-styles)))

(defun hatty--claim-style (character style)
  "Mark STYLE for CHARACTER as taken."
  (hatty--materialize-free-styles character)
  (setf (alist-get character hatty--free-styles)
        (delete style (alist-get character hatty--free-styles))))

(defun hatty--style-free-p (character style)
  "Return non-nil if STYLE is free for CHARACTER."
  (hatty--materialize-free-styles character)
  (member style (alist-get character hatty--free-styles)))

(defun hatty--next-style-penalty (character)
  "Get penalty of the next style applicable to CHARACTER."
  (hatty--penalty (hatty--next-style character)))

(defun hatty--reset-styles ()
  "Free up all styles for usage.
Done before hat reallocation is made."
  (setq hatty--hat-styles (hatty--compute-styles))
  (setq hatty--free-styles '()))

(defun hatty--select-hat-character (characters)
  "Return the character with highest style priority of CHARACTERS."
  (car (seq-sort-by #'hatty--next-style-penalty #'< characters)))

(defun hatty--create-hat (token)
  "Create a hat for TOKEN.
Return the hat if successful, otherwise return nil.

TOKEN is a cons cell of the bounds of the token."
  (when-let* ((characters
               (thread-last
                 (buffer-substring (car token) (cdr token))
                 string-to-list
                 (mapcar #'hatty--normalize-character)
                 delete-dups
                 ;; Remove control characters.  In ASCII, they are before #x20.
                 (seq-filter (lambda (c) (>= c #x20)))))
              (selected-character
               (hatty--select-hat-character characters))
              (style (hatty--next-style selected-character)))
    (let* ((overlays-in-token (overlays-in (car token) (cdr token)))
           (hats-in-token (seq-keep (lambda (overlay)
                                      (overlay-get overlay 'hatty--hat))
                                    overlays-in-token))
           (previous-hat (car hats-in-token))
           (previous-style (when previous-hat
                             (cons (hatty--hat-color previous-hat)
                                   (hatty--hat-shape previous-hat)))))
      (when (and previous-hat
                 (hatty--style-free-p (hatty--hat-character previous-hat)
                                      previous-style)
                 (<= (hatty--penalty previous-style)
                     (hatty--penalty style))
                 (member (hatty--hat-character previous-hat) characters))
        (setq selected-character (hatty--hat-character previous-hat))
        (setq style previous-style))
      (hatty--claim-style selected-character style)
      (save-excursion
        (goto-char (car token))
        (while (not (eq (hatty--normalize-character (char-after))
                        selected-character))
          (forward-char))
        (hatty--make-hat (point) token style)))))

(defvar hatty--tokenize-region-function
  #'hatty--tokenize-region-default
  "Function to use for tokenizing the contents of the buffer.

The function should take two parameters, BEG and END, and return
a list of token boundaries for tokens occurring within BEG and
END in the current buffer.

A token boundary is a cons cell of the beginning position and end
position.

Token boundaries may occur outside BEG and END, but must be
inside `point-min' and `point-max'.")

(defun hatty--tokenize-region (beg end)
  "Return token boundaries within BEG and END.

The behavior of this function can be changed by setting
  `hatty--tokenize-region-function'."
  (funcall hatty--tokenize-region-function beg end))

(defun hatty--tokenize-region-default (beg end)
  "Return token boundaries within BEG and END.

Tokens are delimited by white space.  Non-word characters are regarded
single tokens."
  (let* (token
         (tokens '())
         (next-token
          (lambda ()
            (skip-chars-forward "[:space:]\n")
            (let ((start (point)))
              (if (zerop (skip-syntax-forward "w_'"))
                  (skip-syntax-forward "^w_' " (1+ (point))))
              (setq token
                    (if (= start (point))
                        nil
                      (cons start (point))))))))
    (save-excursion
      (goto-char beg)
      (while (and (<= (point) end)
                  (funcall next-token))
        (push token tokens)))
    tokens))

(defun hatty--get-tokens ()
  "Return bounds of tokens in the visible buffer.
Order tokens by importance."
  (thread-last
    (funcall hatty--tokenize-region-function
             (window-start) (window-end))
    (seq-filter
     (lambda (token)
       (not (or (invisible-p (car token))
                (invisible-p (1- (cdr token)))))))
    (seq-sort-by
     (lambda (token)
       (abs (- (point) (car token))))
     #'<)))

(defvar hatty--preferred-spacing-method nil
  "Preferred spacing method.
Should be one of `line-height' or `svg-prefix'.")

(defvar-local hatty--current-spacing-method nil
  "Spacing method used for this buffer.
Possible values are `line-height' and `svg-prefix'.")

(defvar hatty--svg-prefix-height nil
  "Size of the svg prefix image.
Only applicable when `hatty--current-spacing-method' is `svg-prefix'.")

(defun hatty--compute-space-deprivation-truncated (hats)
  "Populate HATS with space deprivation information.
Assume that long lines are truncated."
  ;; When a line gets truncated, all hats on it get deprived of space.
  (setq hats (seq-sort-by #'hatty--hat-marker #'< hats))
  (save-excursion
    (goto-char (window-start))
    (let ((worklist hats))
      (while worklist
        (let (visual-end real-end)
          (end-of-visual-line)
          (setq visual-end (point))
          (end-of-line)
          (setq real-end (point))
          (forward-line)
          (while (and worklist
                      (< (hatty--hat-marker (car worklist)) (point)))
            (setf (hatty--hat-space-deprived-p (pop worklist))
                  (/= visual-end real-end))))))))

(defun hatty--compute-space-deprivation-wrapped (hats)
  "Populate HATS with space deprivation information.
Assume that long lines are not truncated."
  ;; When a line is wrapped, only the final wrapping gets the extra
  ;; line space (because it contains the newline).  All other lines
  ;; are space deprived.
  (setq hats (seq-sort-by #'hatty--hat-marker #'< hats))
  (save-excursion
    (goto-char (window-start))
    (end-of-line)
    (let ((worklist hats)
          ;; HACK: Sometimes the scan got stuck.  Keeping track of
          ;; maximum position allows us to more directly ensure
          ;; progress.
          (maximum-position (point)))
      (while worklist
        (let (last-visual-begin
              last-visual-end)
          (end-of-line)
          (setq maximum-position (max maximum-position (point)))
          (setq last-visual-end (point))
          (beginning-of-visual-line)
          (setq last-visual-begin (point))
          ;; Mark all hats that occur before the last visual line...
          (while (and worklist
                      (< (hatty--hat-marker (car worklist)) last-visual-begin))
            (setf (hatty--hat-space-deprived-p (car worklist)) t)
            (pop worklist))
          ;; ... and all hats within it.
          (while (and worklist
                      (< (hatty--hat-marker (car worklist)) last-visual-end))
            (setf (hatty--hat-space-deprived-p (car worklist)) nil)
            (pop worklist)))
        (goto-char maximum-position)    ; HACK (cont): This is to
                                        ; ensure progress.
        (vertical-motion 1)
        (setq maximum-position (max (point) maximum-position))))))

(defun hatty--compute-space-deprivation (hats)
  "Populate HATS with space deprivation information.

When the extra line spacing for a hat is not applied visually, the hat
is deprived of space.  The computation of when this happens is
expensive, but is made quicker by computing it in bulk with this
function."
  ;; The reason that it is expensive is because we need to use visual
  ;; line movements, which are slower than regular movement
  ;; operations.  We only need this when line-height is calculated
  ;; through line-height overlay properties.
  (when (eq hatty--current-spacing-method 'line-height)
    (if truncate-lines
        (hatty--compute-space-deprivation-truncated hats)
      (hatty--compute-space-deprivation-wrapped hats))))

(defun hatty--create-hats ()
  "Create and return hats in the buffer given by `window-buffer'.

Tokens are queried from `hatty--get-tokens'."
  (let ((hats
         (with-current-buffer (window-buffer)
           (seq-keep #'hatty--create-hat (hatty--get-tokens)))))
    (hatty--compute-space-deprivation hats)
    hats))

(defun hatty--get-raise-display-property (position)
  "Get value of the `raise' display property at POSITION.

This function looks at both text properties and overlay
properties."
  (let ((display-property (get-char-property position 'display)))
    (cl-flet ((raise-property-p (property) (eq (car-safe property) 'raise)))
      (pcase display-property
        ((pred raise-property-p) (cadr display-property))
        ((pred seqp)
         (let ((property (seq-find #'raise-property-p display-property)))
           (if property (cadr property) 0.0)))
        (_ 0.0)))))

(defvar hatty--svg-cache
  (make-hash-table
   :test 'equal))

(defun hatty--svg-parameters (hat)
  "Return plist of parameters required to make SVG for HAT.

If no hat could sensibly be rendered for the hat, this function
returns nil."

  ;; HACK: If the font doesn't have a glyph for a specific character,
  ;; font-at will return nil.  For now, we just bail out if this
  ;; occurs.  Should probably be done somewhere else...
  (when (font-at (marker-position (hatty--hat-marker hat)))
    (pcase-let* ((position (marker-position (hatty--hat-marker hat)))
                 (text (buffer-substring-no-properties position (1+ position)))
                 ;; I will pretend that get-char-property yields all the faces
                 ;; used in the deduction of the face properties for display.
                 ;; I will also pretend that anything not a face or list of
                 ;; faces does not contribute to the display.  These
                 ;; assumptions might not be true; Consult Properties with
                 ;; Special Meanings in the emacs manual.
                 (faces (append (let ((face-spec (get-char-property position 'face)))
                                  (cond
                                   ((facep face-spec) (list face-spec))
                                   ((consp face-spec)
                                    ;; Only handle named faces for now
                                    (seq-filter #'facep face-spec))
                                   (t '())))
                                (list 'default)))
                 (font-family (face-attribute (car faces) :family nil (cdr faces)))
                 (font-weight (face-attribute (car faces) :weight nil (cdr faces)))

                 (font (font-at position))
                 (font-metrics (query-font font))
                 (glyph-metrics (elt (font-get-glyphs font position (1+ position)) 0))

                 (font-size (elt font-metrics 2))
                 (ascent (elt font-metrics 4))
                 (descent (elt font-metrics 5))
                 (char-width (elt glyph-metrics 4))
                 (char-height (+ ascent descent))
                 (raise (truncate
                         (* char-height
                            (hatty--get-raise-display-property position))))

                 (`(,line-height ,default-char-height)
                  (if (eq hatty--current-spacing-method 'line-height)
                      (save-excursion
                        (goto-char position)
                        (skip-chars-forward "^\n" (+ 1000 position))
                        (if (char-after)
                            (list (get-char-property (point) 'line-height)
                                  (+ (elt (query-font (font-at (point))) 4)
                                     (elt (query-font (font-at (point))) 5)))
                          (list nil char-height)))
                    (list nil hatty--svg-prefix-height)))
                 (default-line-height
                  (cond
                   ((hatty--hat-space-deprived-p hat) default-char-height)
                   ((integerp line-height) (max default-char-height line-height))
                   ((floatp line-height) (* default-char-height line-height))
                   (t default-char-height)))

                 (svg-height (max default-line-height char-height))
                 (svg-width char-width)
                 ;; TODO: We should probably calculate the bounding box of
                 ;; the empty space above the typical char, and fit the
                 ;; curve inside that, instead of using this equation
                 ;; derived from trial-and-error.
                 (scale (* hatty-scale-factor
                           ;; Magic number 200.0 was picked to look good.
                           (/ (face-attribute 'default :height) 200.0)))

                 ;; Convert from emacs color to 6 letter svg hexcode.
                 (svg-hat-color
                  (let ((color
                         (color-values
                          (alist-get (hatty--hat-color hat) hatty-colors))))
                    (format "#%02X%02X%02X"
                            (/ (nth 0 color) 256)
                            (/ (nth 1 color) 256)
                            (/ (nth 2 color) 256)))))

      (list
       :svg-hat-color svg-hat-color
       :svg-width svg-width
       :svg-height svg-height
       :text text
       :font-family font-family
       :font-size font-size
       :font-weight font-weight
       :descent descent
       :raise raise
       :path (alist-get (hatty--hat-shape hat) hatty-shapes)
       :svg-hat-color svg-hat-color
       :scale scale))))

(defun hatty--compute-svg (parameters)
  "Return SVG image of a hat over a glyph according to PARAMETERS."
  (let* ((svg-hat-color (plist-get parameters :svg-hat-color))
         (svg-width (plist-get parameters :svg-width))
         (svg-height (plist-get parameters :svg-height))
         (text (plist-get parameters :text))
         (font-family (plist-get parameters :font-family))
         (font-size (plist-get parameters :font-size))
         (font-weight (plist-get parameters :font-weight))
         (descent (plist-get parameters :descent))
         (raise (plist-get parameters :raise))
         (path (plist-get parameters :path))
         (scale (plist-get parameters :scale))
         (svg (svg-create svg-width svg-height)))

    (svg-text svg text
              :stroke-width 0
              :font-family font-family
              :font-size font-size
              :font-weight font-weight
              :x 0
              :y (- svg-height descent)
              :fill "currentcolor")

    (svg-node svg 'path
              ;; Transformations are applied right-to-left
              :transform (format "translate(%s,0) scale(%s) translate(%s,0)"
                                 (/ svg-width 2)
                                 scale
                                 (- 5.5))
              :fill svg-hat-color
              :d path)

    (svg-image svg
               :ascent
               (ceiling (* 100 (- svg-height descent (- raise)))
                        svg-height)
               :scale 1.0)))

(defun hatty--get-svg (hat)
  "Get the SVG for HAT, or nil if no SVG could be sensibly rendered.

Because creating the SVG is computationally heavy, the SVG is cached
in `hatty--svg-cache' to avoid recomputation of visually equivalent
SVGs in the future."
  (when-let* ((parameters (hatty--svg-parameters hat)))
    (with-memoization (gethash parameters hatty--svg-cache)
      (hatty--compute-svg parameters))))

(defun hatty--draw-svg-hat (hat)
  "Overlay character of HAT with image of it having the hat."
  (when-let* ((svg (hatty--get-svg hat))
              (overlay (make-overlay (marker-position (hatty--hat-marker hat))
                                     (1+ (marker-position (hatty--hat-marker hat)))
                                     nil t nil)))
    (remove-overlays (car (hatty--hat-token-region hat))
                     (cdr (hatty--hat-token-region hat))
                     'hatty--old t)
    (with-silent-modifications
      (overlay-put overlay 'display svg))
    (overlay-put overlay 'hatty t)
    (overlay-put overlay 'hatty--hat-p t)
    (overlay-put overlay 'hatty--hat hat)))

(defun hatty--current-buffer-font ()
  "Return font for current buffer."
  (if (and (get-buffer-window) (/= (point-min) (point-max)))
      (font-at (point-min) (get-buffer-window))
    (face-attribute 'default :font)))

(defun hatty--desired-line-height ()
  "Return the minimum desired line height in pixels."
  (let* ((font (hatty--current-buffer-font))
         (font-metrics (query-font font))
         (ascent (elt font-metrics 4))
         (descent (elt font-metrics 5))
         (char-height (+ ascent descent))
         (target-height (ceiling (* char-height 1.2))))
    target-height))

(defun hatty--increase-line-height ()
  "Add space between lines through line-height overlays."
  (let ((modify-line-spacing (make-overlay (point-min) (point-max) nil nil t)))
    (overlay-put modify-line-spacing 'line-height (hatty--desired-line-height))
    (overlay-put modify-line-spacing 'evaporate nil)
    (overlay-put modify-line-spacing 'hatty t)
    (overlay-put modify-line-spacing 'hatty--modified-spacing t)
    (overlay-put modify-line-spacing 'hatty--modified-line-height t))
  (setq hatty--current-spacing-method 'line-height))

(defun hatty--increase-spacing-svg-prefix ()
  "Add space between lines for hats through svg prefix overlays."
  (let* ((modify-line-spacing (make-overlay (point-min) (point-max) nil nil t))
         (font (hatty--current-buffer-font))
         (descent (elt (query-font font) 5))
         (target-height (hatty--desired-line-height))
         (svg-ascent-pct (ceiling (* 100 (- target-height descent))
                                  target-height))
         ;; HACK: Images get the same 'raise property as the first
         ;; visual char when a line-prefix.  We concat image with
         ;; zero-width space, so neither gets 'raise set.  Does not
         ;; work for display properties.
         ;; FIXME: For display properties (upstream bug?)
         (prefix (concat (propertize
                          "\N{ZERO WIDTH SPACE}"
                          'display
                          (svg-image (svg-create 1 target-height)
                                     :ascent svg-ascent-pct
                                     :scale 1.0)
                          'raise 0)
                         "\N{ZERO WIDTH SPACE}")))
    (setq hatty--svg-prefix-height target-height)
    (overlay-put modify-line-spacing 'line-prefix prefix)
    (overlay-put modify-line-spacing 'wrap-prefix prefix)
    (overlay-put modify-line-spacing 'evaporate nil)
    (overlay-put modify-line-spacing 'hatty t)
    (overlay-put modify-line-spacing 'hatty--modified-spacing t)
    (overlay-put modify-line-spacing 'hatty--modified-prefix prefix))
  (setq hatty--current-spacing-method 'svg-prefix))

(defun hatty--increase-line-spacing ()
  "Add space between lines for hats to render in the current buffer."
  (cond
   ((eq hatty--preferred-spacing-method 'line-height)
    (hatty--increase-line-height))
   ((eq hatty--preferred-spacing-method 'svg-prefix)
    (hatty--increase-spacing-svg-prefix))
   (t
    (if (or (seq-some (lambda (overlay)
                        (and (not (overlay-get overlay 'hatty))
                             (or (overlay-get overlay 'line-prefix)
                                 (overlay-get overlay 'wrap-prefix))))
                      (overlays-in (point-min) (point-max)))
            (text-property-not-all (point-min) (point-max) 'line-prefix nil)
            (text-property-not-all (point-min) (point-max) 'wrap-prefix nil))
        (hatty--increase-line-height)
      (hatty--increase-spacing-svg-prefix)))))

(defun hatty--render-hats (hats)
  "Display HATS."
  (dolist (hat hats)
    ;; We must avoid rendering if there is already a display property
    ;; replacing the glyph that we want to override.
    (let* ((position (hatty--hat-marker hat))
           (display-properties
            (cons (get-text-property position 'display)
                  (seq-keep (lambda (overlay)
                              ;; We keep around old overlays to avoid
                              ;; flickering if rendering gets
                              ;; interrupted by input.  Do not let
                              ;; those overlays block rendering.
                              (unless (overlay-get overlay 'hatty)
                                (overlay-get overlay 'display)))
                            (overlays-in position (1+ position))))))
      (unless (seq-some (lambda (display-property)
                          (or (stringp display-property)
                              (and (consp display-property)
                                   (or (eq (car display-property) 'image)
                                       (assq 'image display-property)))))
                        display-properties)
        (hatty--draw-svg-hat hat)))))

;; Declare now, will be set later along the minor mode.
(defvar hatty-mode)

(defun hatty--compute-styles ()
  "Return all possible styles in increasing order of penalty.

The penalty is computed using `hatty--penalty'."
  (let ((styles (cl-loop
                 for shape in (seq-uniq (mapcar #'car hatty-shapes))
                 append (cl-loop
                         for color in (seq-uniq (mapcar #'car hatty-colors))
                         collect (cons color shape)))))
    (sort styles (lambda (style1 style2)
                   (< (hatty--penalty style1)
                      (hatty--penalty style2))))))

(defun hatty-reallocate ()
  "Reallocate hats."
  (interactive)
  (hatty--reset-styles)
  (while-no-input ; This computation is heavy and gets invoked often.  Do not block input.
    (redisplay)
    (let ((gc-cons-threshold most-positive-fixnum) ; Wait with gc until we have redisplayed.
          (visited-buffers '())) ; Avoid reallocating same buffer in different windows.
      (dolist (window (window-list-1 nil nil 'visible))
        (with-current-buffer (window-buffer window)
          (when (and hatty-mode (not (member (current-buffer) visited-buffers)))
            (push (current-buffer) visited-buffers)
            (with-selected-window window
              ;; We wait with removing previous overlays: If the
              ;; computation gets interrupted by input, removing them
              ;; first causes flickering.
              (hatty--mark-old-overlays)
              (setq hatty--hats (hatty--create-hats))
              (hatty--increase-line-spacing)
              (hatty--render-hats hatty--hats)
              (hatty--remove-old-overlays)))))
      (redisplay)))
  (when (> (hash-table-count hatty--svg-cache) 10000)
    (clrhash hatty--svg-cache)))

(defun hatty--clear ()
  "Clean up all resources of hatty in the current buffer.

This should restore the buffer state as it was before hatty was enabled."
  (remove-overlays nil nil 'hatty t)
  (dolist (hat hatty--hats)
    (set-marker (hatty--hat-marker hat) nil))
  (setq hatty--hats nil))

(defun hatty--mark-old-overlays ()
  "Mark each hatty overlay for removal.

Marked overlays may be removed with `hatty--clear-old'."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'hatty)
      (overlay-put overlay 'hatty--old t))))

(defun hatty--remove-old-overlays ()
  "Remove each hatty overlay from previous generation."
  (remove-overlays nil nil 'hatty--old t))

(defvar hatty--hat-reallocate-timer (timer-create))
;; Not sure what the best way to disable this whenever hatty-mode is
;; disabled in all buffers.  Currently I just let it always run, as
;; hatty-reallocate bails if it is not enabled anyway.
(defvar hatty--hat-reallocate-idle-timer
  (run-with-idle-timer 0.05 t #'hatty-reallocate))

(defun hatty-request-reallocation (&optional seconds-delay)
  "Signal that the current buffer will need hat reallocation.

The function will try to avoid rapid consecutive reallocations by
deferring reallocation by SECONDS-DELAY and cancelling
any previously unperformed reallocations.

To reallocate immediately, use `hatty-reallocate' instead."
  (setq seconds-delay (or seconds-delay 0.05))
  (cancel-timer hatty--hat-reallocate-timer)
  (setq hatty--hat-reallocate-timer
        (run-with-timer seconds-delay nil #'hatty-reallocate)))

;;;###autoload
(define-minor-mode hatty-mode
  "Minor mode for querying buffer positions through hats."
  :init-value nil
  :lighter nil
  :group 'hatty
  :after-hook
  (if hatty-mode
      (hatty--increase-line-height)
    (hatty--clear)))

;;;###autoload
(define-globalized-minor-mode global-hatty-mode hatty-mode hatty-mode
  :group 'hatty)

(provide 'hatty)
;;; hatty.el ends here
