;;; hatty.el --- Query positions through hats        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Erik Präntare

;; This file is part of hatty.el.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test that the different deduced face properties for the rendered
;; SVGs are correct.

;;; Code:

(require 'hatty)

;; TODO Change so it doesn't need to modify the default face.  This
;; could straightforwardly be done when anonymous faces are properly
;; supported.
(ert-deftest hatty-test-text-scaling ()
  "Size of the character is retained at different scales."
  (dolist (height '(120 240 60          ;Nice values
                        173 37          ;Not nice values
                        1 1337          ;Extremes
                        ))
    (let ((previous-height (face-attribute 'default :height))
          (previous-size)
          (current-size))
      (unwind-protect
          (progn
            (set-face-attribute 'default nil :height height)
            (with-temp-buffer
              (switch-to-buffer (current-buffer))
              (insert "i")
              (setq previous-size (buffer-text-pixel-size))
              (hatty--draw-svg-hat (hatty--make-hat (point-min)))
              (setq current-size (buffer-text-pixel-size))))
        (set-face-attribute 'default nil :height previous-height)
        (should (equal previous-size current-size))))))

(ert-deftest hatty-test-variable-width-font ()
  "Variable width fonts have the right size."
  (let ((previous-size)
        (current-size))
    (with-temp-buffer
      (insert "i")
      (switch-to-buffer (current-buffer))
      (put-text-property (point-min) (point-max) 'face 'variable-pitch)
      (setq previous-size (window-text-pixel-size))
      (hatty--draw-svg-hat (hatty--make-hat (point-min)))
      (setq current-size (window-text-pixel-size))
      (should (equal previous-size current-size)))))

(ert-deftest hatty-test-extra-line-height ()
  "If extra line height is present, use it."
  (let ((previous-size)
        (current-size))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "i")
      (let ((previous-height (cdr (window-text-pixel-size))))
        ;; TODO: Less nice test numbers
        (put-text-property (point-min) (point-max) 'line-height 2.0)
        (hatty--draw-svg-hat (hatty--make-hat (point-min)))
        (should (= (* 2.0 previous-height) (cdr (window-text-pixel-size))))))))

(defface hatty-test-face-large
  '((t . (:height 2.0 :inherit default)))
  "Remove this when anonymous faces are properly supported.")

(ert-deftest hatty-test-line-height-large-face ()
  "Do not use extra line height if character is larger than
default height."
  (let ((previous-size)
        (current-size))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "i")
      (let ((previous-height (cdr (window-text-pixel-size))))
        ;; TODO: Less nice test numbers
        (put-text-property (point-min) (point-max) 'line-height 2.0)
        (put-text-property (point-min) (point-max) 'face 'hatty-test-face-large)
        (hatty--draw-svg-hat (hatty--make-hat (point-min)))
        (should (= (* 2.0 previous-height) (cdr (window-text-pixel-size))))))))

(ert-deftest hatty-test-invisible-text ()
  "Invisible text should not contribute tokens."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc")
    (should (equal 3 (length (hatty--get-tokens))))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'invisible t)
      (should (equal 0 (length (hatty--get-tokens)))))))

(ert-deftest hatty-test-buffer-end-space ()
  "Tokenize buffer ending in space."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc  ")
    (should (equal 3 (length (hatty--get-tokens))))))

(ert-deftest hatty-test-readonly-buffer ()
  "Adding hats should be possible in read-only mode."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc")
    (read-only-mode 1)
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty-test-readonly-text ()
  "Adding hats should be possible for read-only text."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'read-only t)
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty-test-anonymous-face ()
  "Do not explode when encountering anonymous faces."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'face '(:foreground "red"))
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty-test-multiple-anonymous-faces ()
  "Do not explode when encountering multiple anonymous faces."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'face '((:background "black")
                                                       (:foreground "red")))
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty-test-image-text-property ()
  "Do not add hats if an image is displaying as a text property."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert-image (svg-image (svg-create 100 100)) "a b c")
    (let ((previous-size (window-text-pixel-size)))
      (hatty-mode)
      (hatty-reallocate)
      (should (equal previous-size (window-text-pixel-size))))))

(ert-deftest hatty-test-image-overlay ()
  "Do not add hats if an image is displaying as an overlay."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    (overlay-put (make-overlay (point-min) (point-max))
                 'display
                 (svg-image (svg-create 200 200)))
    (let ((previous-size (window-text-pixel-size)))
      (hatty-mode)
      (hatty-reallocate)
      (should (equal previous-size (window-text-pixel-size))))))

(ert-deftest hatty-test-string-property ()
  "Do not add hats if a string is displaying as a text property.

This is crucial to not reveal characters of password prompts."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    (put-text-property (point-min) (point-max) 'display "*****")
    (hatty-mode)
    (hatty-reallocate)
    (should (null (seq-filter (lambda (overlay)
                                (overlay-get overlay 'hatty-hat))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest hatty-test-string-overlay ()
  "Do not add hats if an image is displaying as an overlay.

This is crucial to not reveal characters of password prompts."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    (overlay-put (make-overlay (point-min) (point-max)) 'display "*****")
    (should (null (seq-filter (lambda (overlay)
                                (overlay-get overlay 'hatty-hat))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest hatty-test-raise-display-text-property ()
  "The 'raise text display property raises hatted characters."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    (add-display-text-property (+ (point-min) 2) (+ (point-min) 3) 'raise 0.23)
    (add-display-text-property (+ (point-min) 4) (+ (point-min) 5) 'raise -0.3)
    (let ((previous-size (window-text-pixel-size)))
      (hatty--draw-svg-hat (hatty--make-hat (+ (point-min) 2)))
      (hatty--draw-svg-hat (hatty--make-hat (+ (point-min) 4)))
      (should (equal previous-size (window-text-pixel-size))))))

(ert-deftest hatty-test-raise-display-overlay-property ()
  "The 'raise overlay display property raises hatted characters."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    ;; Test with different display property formats: Single property
    ;; and vector of properties.
    (overlay-put (make-overlay (+ (point-min) 2) (+ (point-min) 3))
                 'display '(raise 0.23))
    (overlay-put (make-overlay (+ (point-min) 4) (+ (point-min) 5))
                 'display [(raise -0.3)])
    (let ((previous-size (window-text-pixel-size)))
      (hatty--draw-svg-hat (hatty--make-hat (+ (point-min) 2)))
      (hatty--draw-svg-hat (hatty--make-hat (+ (point-min) 4)))
      (should (equal previous-size (window-text-pixel-size))))))

(ert-deftest hatty-test-deleted-buffer-content-line-height ()
  "Deleting buffer contents should preserve line height overlay."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "a b c")
    (hatty--increase-line-height)
    (should (and "check 1" (overlays-in (point-min) (point-max))))
    (delete-region (point-min) (point-max))
    ;; The line height overlay should remain, so the overlays at the
    ;; remaining position should be non-nil.
    (should (and "check 2" (overlays-in (point-min) (point-max))))))

(ert-deftest hatty-test-links ()
  "Hats should render over links."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "[abc]")
    (add-display-text-property (point-min) (1+ (point-min)) 'invisible t)
    (add-display-text-property (1- (point-max)) (point-max) 'invisible t)
    (hatty-mode)
    (hatty-reallocate)
    (thread-last
      (overlays-in (point-min) (point-max))
      (seq-filter (lambda (overlay) (overlay-get overlay 'hatty-hat)))
      null
      should-not)))

;;; tests.el ends here
