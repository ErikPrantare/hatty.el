;;; hatty.el --- Query positions through hats        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.2.1
;; Homepage: https://github.com/ErikPrantare/hatty.el
;; Package-Requires: ((emacs "26.1"))
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
;; position of a hat may then be queried using the function
;; ‘hatty-locate’.

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

   (t
    `((default . ,(face-attribute 'term :foreground nil t))
      (yellow . ,(face-attribute 'term-color-yellow :foreground nil t))
      (red . ,(face-attribute 'term-color-red :foreground nil t))
      (blue . ,(face-attribute 'term-color-blue :foreground nil t))
      (pink . ,(face-attribute 'term-color-magenta :foreground nil t))
      (green . ,(face-attribute 'term-color-green :foreground nil t)))))
  "Alist of colors used in rendering hats, indexed by identifier.

Identifiers must to be symbols.

The identifier symbol `default' indicates the default color."
  :type '(alist :key-type symbol :value-type color)
  :group 'hatty)

;; TODO: Use proper svgs here.  Also add the other hats.
(defcustom hatty-shapes
  '((default  . "M6 9C9.31371 9 12 6.98528 12 4.5C12 2.01472 9.31371 0 6 0C2.68629 0 0 2.01472 0 4.5C0 6.98528 2.68629 9 6 9Z")
    (bolt  . "M12 4V0C12 0 9 5 8 5C7 5 3 0 3 0L0 5V9C0 9 3 5 4 5C5 5 9 9 9 9L12 4Z")
    (curve  . "M6.00016 3.5C10 3.5 12 7.07378 12 9C12 4 10.5 0 6.00016 0C1.50032 0 0 4 0 9C0 7.07378 2.00032 3.5 6.00016 3.5Z")
    (fox . "M6.00001 9L0 0C0 0 3.71818 2.5 6 2.5C8.28182 2.5 12 0 12 0L6.00001 9Z")
    (frame . "M0 0.000115976V8.99988H12V0L0 0.000115976ZM9.5 6.5H6H2.5V4.5V2.5H6H9.5V4.5V6.5Z"))
  "Alist of shapes used in rendering hats, indexed by identifier.

Identifiers must be symbols.  The shapes must specify valid svg
paths.

The identifier symbol `default' indicates the default ."
  :type '(alist :key-type symbol :value-type string)
  :group 'hatty
;; The following notice is included for compliance with the license of
;; the cursorless project, from which the above hat designs were
;; copied.
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

(defvar hatty--hat-styles nil
  "List of hat styles to choose from.

This is recalculated at the beginning of ‘hatty-reallocate’ to
create all combinations from ‘hatty-colors’ and ‘hatty-shapes’")

;; TODO: Define the structure with EIEIO instead?  For constructors.
(cl-defstruct hatty--hat
  "A hat with COLOR and SHAPE at MARKER over CHARACTER."
  marker
  character
  color
  shape)

(defvar-local hatty--hats '()
  "All hats located in the current buffer.")

(defun hatty--normalize-character (character)
  "Return normalized version of CHARACTER.
This function is equivalent to ‘downcase’."
  (downcase character))

(cl-defun hatty-locate (character &optional color shape)
  "Get the position of hat over CHARACTER matching COLOR and SHAPE.

COLOR and SHAPE should be identifiers as they occur in
‘hatty-colors’ and ‘hatty-shapes’.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used."
  (setq color (or color 'default))
  (setq shape (or shape 'default))
  (marker-position
   (hatty--hat-marker
    (seq-find (lambda (hat) (and (eq color (hatty--hat-color hat))
                                 (eq (hatty--normalize-character character)
                                     (hatty--hat-character hat))
                                 (eq shape (hatty--hat-shape hat))))
              hatty--hats))))

(cl-defun hatty--make-hat (position &key color shape)
  "Create a hat at POSITION with color COLOR and shape SHAPE.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used. "
  (unless color (setq color 'default))
  (unless shape (setq shape 'default))
  (make-hatty--hat
   :character (hatty--normalize-character (char-after position))
   :color color
   :marker (set-marker (make-marker) position (window-buffer))
   :shape shape))

(defvar hatty--next-styles '()
  "Alist mapping characters to index of next free style.")

(defun hatty--next-style-index (character)
  "Get index of the next style applicable to CHARACTER."
  (let* ((normalized (hatty--normalize-character character))
         (entry (assq normalized hatty--next-styles)))
    (if entry
        (cdr entry)
      (add-to-list 'hatty--next-styles (cons normalized 0))
      0)))

(defun hatty--request-style (character)
  "Get the next applicable style of CHARACTER.
Return nil if none is applicable."
  (let* ((normalized (hatty--normalize-character character))
        (entry (assq normalized hatty--next-styles)))
    (unless entry
      (add-to-list 'hatty--next-styles (cons normalized 0))
      (setq entry (car hatty--next-styles)))
    (let ((index (cdr entry)))
      (if (>= index (length hatty--hat-styles))
          nil
        (setf (cdr entry) (1+ index))
        (elt hatty--hat-styles index)))))

(defun hatty--reset-styles ()
  "Clear ‘hatty--next-styles’.
Done before hat reallocation is made."
  (setq hatty--next-styles '()))

(defun hatty--select-hat-character (characters)
  "Return the character with highest priorities style from CHARACTERS."
  (car (seq-sort-by #'hatty--next-style-index #'< characters)))

(defun hatty--create-hat (token)
  "Create a hat for TOKEN.
Return the hat if successful, otherwise return nil.

TOKEN is a cons cell of the bounds of the token."
  (let* ((characters
          (thread-last
            (buffer-substring (car token) (cdr token))
            string-to-list
            seq-uniq))
         (selected-character
          (hatty--select-hat-character characters))
         (requested-style
          (hatty--request-style selected-character))
         (position
          (cl-loop
           with position = (car token)
           until (equal (char-after position) selected-character)
           do (setq position (1+ position))
           finally return position)))

    (if requested-style
        (hatty--make-hat position
                              :color (car requested-style)
                              :shape (cdr requested-style))
      nil)))

(defun hatty--get-tokens ()
  "Return bounds of tokens in the visible buffer.
Order tokens by importance."
  (let ((previous-point (point)))
    (save-excursion
      (goto-char (window-start))
      (forward-thing 'symbol)
      (thread-last
        (cl-loop
         ;; If we are at the end of the window, collect token before
         ;; exiting the loop.  If we would use <= in the while loop
         ;; instead, we would risk an infinite loop if we ended up at
         ;; the end of the buffer.
         if (and (equal (point) (window-end))
                 (bounds-of-thing-at-point 'symbol))
         collect (bounds-of-thing-at-point 'symbol)

         while (< (point) (window-end))
         collect (bounds-of-thing-at-point 'symbol)
         do (forward-thing 'symbol))

        ;; Move to hat assignment algorithm?
        (seq-filter (lambda (token) (not (or (invisible-p (car token))
                                             (invisible-p (1- (cdr token)))))))

        (seq-sort-by (lambda (token)
                       (abs (- previous-point (car token))))
                     #'<)))))

(defun hatty--create-hats ()
  "Create hats in the buffer given by ‘window-buffer’.
Return the created hats.

Tokens are queried from `hatty--get-tokens'"
  (hatty--reset-styles)
  (setq hatty--hats
        (with-current-buffer (window-buffer)
          (let ((tokens (hatty--get-tokens)))
            (seq-filter
             #'identity
             (seq-map #'hatty--create-hat tokens))))))

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

(defun hatty--draw-svg-hat (hat)
  "Overlay character of HAT with with image of it having the hat."

  (let* ((position (marker-position (hatty--hat-marker hat)))
         (text (buffer-substring position (1+ position)))
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
         (family (face-attribute (car faces) :family nil (cdr faces)))
         (weight (face-attribute (car faces) :weight nil (cdr faces)))

         (font (font-at position))
         (font-metrics (query-font font))
         (glyph-metrics (elt (font-get-glyphs font position (1+ position)) 0))

         (font-size (elt font-metrics 2))
         (ascent (elt font-metrics 4))
         (descent (elt font-metrics 5))
         (char-width (elt glyph-metrics 4))
         (char-height (+ ascent descent))
         (raise (round (* char-height (hatty--get-raise-display-property position))))

         ;; Should probably look at the final newline for this property
         (line-height (get-char-property position 'line-height))
         (default-char-height (frame-char-height))
         (default-line-height (cond
                               ((integerp line-height) (max default-char-height line-height))
                               ((floatp line-height) (* default-char-height line-height))
                               (t default-char-height)))

         (svg-height (max default-line-height char-height))
         (svg-width char-width)
         (svg (svg-create svg-width svg-height))

         ;; Convert from emacs color to 6 letter svg hexcode.
         (svg-hat-color
          (let ((color
                 (color-values
                  (alist-get (hatty--hat-color hat) hatty-colors))))
            (format "#%02X%02X%02X"
                    (/ (nth 0 color) 256)
                    (/ (nth 1 color) 256)
                    (/ (nth 2 color) 256))))

         (overlay (make-overlay position (1+ position) nil t nil)))

    (svg-text svg text
              :stroke-width 0
              :font-family family
              :font-size font-size
              :font-weight weight
              :x 0
              :y (- svg-height descent))

    (svg-node svg 'path
              ;; Transformations are applied in reverse order
              :transform (format "translate(%s,0) scale(%s) translate(%s,0)"
                                 (/ svg-width 2)
                                 0.6
                                 (- 6))
              :fill svg-hat-color
              :d (alist-get (hatty--hat-shape hat) hatty-shapes))

    (with-silent-modifications
      (overlay-put overlay
                   'display
                   (svg-image svg
                              :ascent
                              (round (* 100 (- svg-height descent (- raise)))
                                     svg-height)
                              :scale 1.0)))
    (overlay-put overlay 'hatty t)
    (overlay-put overlay 'hatty-hat t)))

(defun hatty--increase-line-height ()
  "Create more space for hats to render in current buffer."
  (remove-overlays (point-min) (point-max) 'hatty-modified-line-height t)
  (let ((modify-line-height (make-overlay (point-min) (point-max) nil nil t)))
    (overlay-put modify-line-height 'line-height  1.2)
    (overlay-put modify-line-height 'evaporate nil)
    (overlay-put modify-line-height 'hatty t)
    (overlay-put modify-line-height 'hatty-modified-line-height t)))

(defun hatty--render-hats (hats)
  "Display HATS."
  (dolist (hat hats)
    (let ((display-property
           (get-char-property (hatty--hat-marker hat) 'display)))
      ;; Do not render if there already is a string or image display
      ;; property
      (unless (or (stringp display-property)
                  (and (consp display-property)
                       (or (eq (car display-property) 'image)
                           (assq 'image display-property))))
        (hatty--draw-svg-hat hat)))))

;; Declare now, will be set later along the minor mode.
(defvar hatty-mode)

(defun hatty-reallocate ()
  "Reallocate hats."
  (interactive)
  (with-current-buffer (window-buffer)
    (when hatty-mode
      (setq hatty--hat-styles
            (cl-loop
             for shape in (seq-uniq (mapcar #'car hatty-shapes))
             append (cl-loop
                     for color in (seq-uniq (mapcar #'car hatty-colors))
                     collect (cons color shape))))
      (hatty--clear)
      (hatty--increase-line-height)
      (hatty--render-hats (hatty--create-hats)))))

(defun hatty--clear ()
  "Clean up all resources of hatty.

This should restore the buffer state as it was before hatty was enabled."
  (remove-overlays nil nil 'hatty t)
  (dolist (hat hatty--hats)
    (set-marker (hatty--hat-marker hat) nil))
  (setq hatty--hats nil))

(defvar hatty--hat-reallocate-timer (timer-create))
;; Not sure what the best way to disable this whenever hatty-mode is
;; disabled in all buffers.  Currently I just let it always run, as
;; hatty-reallocate bails if it is not enabled anyway.
(defvar hatty--hat-reallocate-idle-timer
  (run-with-idle-timer 0.2 t #'hatty-reallocate))

(defun hatty-request-reallocation ()
  "Signal that the current buffer will need hat reallocation.

The function will try to avoid rapid consecutive reallocations by
deferring reallocation by a small amount of time and counselling
any previously unperformed reallocations.

To reallocate immediately, use `hatty-reallocate' instead."
  (cancel-timer hatty--hat-reallocate-timer)
  (setq hatty--hat-reallocate-timer
        (run-with-timer 0.1 nil #'hatty-reallocate)))

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
