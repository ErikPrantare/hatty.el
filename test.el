;;; test.el --- Tests for hatty                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Erik Präntare

;; This file is part of hatty.el.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test that the different deduced face properties for the rendered
;; SVGs are correct.

;;; Code:

(require 'hatty)
(require 'cl-lib)

(defmacro hatty-test (&rest body)
  "Evaluate BODY in a fresh hatty test environment.

BODY is evaluated once for each available line spacing methods."
  (declare (indent 0))
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     (redisplay t)
     ,@body))

(cl-defmacro hatty-test-preserves-pixel-size (&key content setup allocation
                                                   setup-retains-pixel-size
                                                   (line-height-methods '(line-height svg-prefix)))
  "Verify hat rendering preserves pixel size.

CONTENT is a form producing a string to insert as buffer content.  SETUP
is a form that sets up text properties.  ALLOCATION is a form that
performs hat allocation.

Checks that SETUP changes pixel size (unless SETUP-RETAINS-PIXEL-SIZE is
non-nil) and that ALLOCATION does not."
  `(dolist (method ',line-height-methods)
     (hatty-test
       (let ((hatty--preferred-spacing-method method))
         (insert ,content)
         (redisplay t)
         (let ((content-size (window-text-pixel-size)))
           ,setup
           (hatty--increase-line-spacing)
           (redisplay t)
           (let ((setup-size (window-text-pixel-size)))
             ,(unless setup-retains-pixel-size
                '(should-not (equal content-size setup-size)))
             ,allocation
             (should (equal setup-size (window-text-pixel-size)))))))))

(defun hatty-test--draw-hat-at (position)
  (hatty--draw-svg-hat
   (hatty--make-hat position
                    (cons position (1+ position))
                    '(default . default))))

;; TODO Change so it doesn't need to modify the default face.  This
;; could straightforwardly be done when anonymous faces are properly
;; supported.
(ert-deftest hatty--text-scaling ()
  "Size of the character is retained at different scales."
  (dolist (height '( 120 240 60         ;Nice values
                     173 37             ;Not nice values
                     1 1337             ;Extremes
                     ))
    (let ((previous-height (face-attribute 'default :height)))
      (unwind-protect
          (hatty-test-preserves-pixel-size
           :content "i"
           :setup (set-face-attribute 'default nil :height height)
           :allocation (hatty-test--draw-hat-at (point-min)))
        (set-face-attribute 'default nil :height previous-height)))))

(ert-deftest hatty--variable-width-font ()
  "Variable width fonts have the right size."
  (hatty-test-preserves-pixel-size
   :content "i\n"
   :setup (buffer-face-set 'variable-pitch)
   :allocation (hatty-test--draw-hat-at (point-min))))

(ert-deftest hatty--variable-width-font-no-newline ()
  "Variable width fonts have the right size."
  (hatty-test-preserves-pixel-size
   :content "i"
   :setup (buffer-face-set 'variable-pitch)
   :allocation (hatty-test--draw-hat-at (point-min))))

(ert-deftest hatty--extra-line-height ()
  "If extra line height is present, use it."
  (dolist (line-height '( 2.0 1.5       ;Nice values
                          1.73 2.37     ;Not nice values
                          ))
    (hatty-test-preserves-pixel-size
     :content "i\n"
     :setup (put-text-property (point-min) (point-max)
                               'line-height line-height)
     :allocation (hatty-test--draw-hat-at (point-min)))))

(defface hatty--test-face-large
  '((t . (:height 2.0 :inherit default)))
  "TODO: Remove this when anonymous faces are properly supported.")

(ert-deftest hatty--line-height-large-face ()
  "Do not use extra line height if character is larger than default height."
  (dolist (line-height '( 2.0 1.5       ;Nice values
                          1.73 2.37     ;Not nice values
                          ))
    (hatty-test-preserves-pixel-size
     :content "i\n"
     :setup (progn
              (put-text-property (point-min) (point-max)
                                 'line-height line-height)
              (put-text-property (point-min) (point-max)
                                 'face 'hatty--test-face-large))
     :allocation (hatty-test--draw-hat-at (point-min)))))

(ert-deftest hatty--invisible-text ()
  "Invisible text should not contribute tokens."
  (hatty-test
    (insert "aaa bbb ccc")
    (should (equal 3 (length (hatty--get-tokens))))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'invisible t)
      (should (equal 0 (length (hatty--get-tokens)))))))

(ert-deftest hatty--buffer-end-space ()
  "Tokenize buffer ending in space."
  (hatty-test
    (insert "aaa bbb ccc  ")
    (should (equal 3 (length (hatty--get-tokens))))))

(ert-deftest hatty--readonly-buffer ()
  "Adding hats should be possible in read-only mode."
  (hatty-test
    (insert "aaa bbb ccc")
    (read-only-mode 1)
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty--readonly-text ()
  "Adding hats should be possible for read-only text."
  (hatty-test
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'read-only t)
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty--anonymous-face ()
  "Do not explode when encountering anonymous faces."
  (hatty-test
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'face '(:foreground "red"))
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty--multiple-anonymous-faces ()
  "Do not explode when encountering multiple anonymous faces."
  (hatty-test
    (insert "aaa bbb ccc")
    (put-text-property (point-min) (point-max) 'face '((:background "black")
                                                       (:foreground "red")))
    (hatty-mode)
    (hatty-reallocate)))

(ert-deftest hatty--image-text-property ()
  "Do not add hats if an image is displaying as a text property."
  (hatty-test-preserves-pixel-size
   :content "a b c"
   :setup (put-text-property (point-min) (point-max)
                             'display (svg-image (svg-create 100 100)))
   :allocation (progn (hatty-mode) (hatty-reallocate))))

(ert-deftest hatty--image-overlay ()
  "Do not add hats if an image is displaying as an overlay."
  (hatty-test-preserves-pixel-size
   :content "a b c"
   :setup (overlay-put (make-overlay (point-min) (point-max))
                       'display
                       (svg-image (svg-create 200 200)))
   :allocation (progn (hatty-mode) (hatty-reallocate))))

(ert-deftest hatty--string-property ()
  "Do not add hats if a string is displaying as a text property.

This is crucial to not reveal characters of password prompts."
  (hatty-test
    (insert "a b c")
    (put-text-property (point-min) (point-max) 'display "*****")
    (hatty-mode)
    (hatty-reallocate)
    (should (null (seq-filter (lambda (overlay)
                                (overlay-get overlay 'hatty--hat))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest hatty--string-overlay ()
  "Do not add hats if an image is displaying as an overlay.

This is crucial to not reveal characters of password prompts."
  (hatty-test
    (insert "a b c")
    (overlay-put (make-overlay (point-min) (point-max)) 'display "*****")
    (should (null (seq-filter (lambda (overlay)
                                (overlay-get overlay 'hatty--hat))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest hatty--raise-display-text-property ()
  "The 'raise text display property raises hatted characters."
  (hatty-test-preserves-pixel-size
   :content "a b c"
   :setup (progn
            (add-display-text-property (+ (point-min) 2) (+ (point-min) 3)
                                       'raise 0.23)
            (add-display-text-property (+ (point-min) 4) (+ (point-min) 5)
                                       'raise -0.3))
   :allocation (progn
                 (hatty-test--draw-hat-at (+ (point-min) 2))
                 (hatty-test--draw-hat-at (+ (point-min) 4)))
   ;; FIXME: Make it work for all methods
   :line-height-methods (line-height)))

(ert-deftest hatty--raise-display-overlay-property ()
  "The 'raise overlay display property raises hatted characters."
  (hatty-test-preserves-pixel-size
   :content "a b c"
   ;; Test with different display property formats: Single property
   ;; and vector of properties.
   :setup (progn
            (overlay-put (make-overlay (+ (point-min) 2) (+ (point-min) 3))
                         'display '(raise 0.23))
            (overlay-put (make-overlay (+ (point-min) 4) (+ (point-min) 5))
                         'display [(raise -0.3)]))
   :allocation (progn
                 (hatty-test--draw-hat-at (+ (point-min) 2))
                 (hatty-test--draw-hat-at (+ (point-min) 4)))
   ;; FIXME: Make it work for all methods
   :line-height-methods (line-height)))

(ert-deftest hatty--deleted-buffer-content-line-height ()
  "Deleting buffer contents should preserve line height overlay."
  (hatty-test
    (insert "a b c")
    (hatty--increase-line-height)
    (should (and "check 1" (overlays-in (point-min) (point-max))))
    (delete-region (point-min) (point-max))
    ;; The line height overlay should remain, so the overlays at the
    ;; remaining position should be non-nil.
    (should (and "check 2" (overlays-in (point-min) (point-max))))))

(ert-deftest hatty--test-linewrap ()
  "Linewrapping should retain correct height.

Only the last visual line is affected by line height.  Check that
this quality is retained when rendering hats."
  (hatty-test-preserves-pixel-size
   :content (concat (apply #'concat (make-list 100 "aaaaaaaaa bbbbbbbbbbbbb cccccccccccc "))
                    "\n")
   :setup (hatty-mode)
   :setup-retains-pixel-size t
   :allocation (hatty-reallocate)))

;;; test.el ends here
