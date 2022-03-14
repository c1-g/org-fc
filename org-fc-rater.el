;;; org-fc-rater.el --- Rating button in the minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'org-fc-algo)

(defvar org-fc-rater-buffer nil
  "The current Rater buffer, or nil.")

(defvar org-fc-rater-origin-buffer nil
  "The original buffer before calling rater.")

(defvar org-fc-rater-inactive-minibuffer nil
  "The original minibuffer before calling rater.")

(defvar org-fc-rater-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-map org-fc-review-rate-mode-map))
    (define-key map "\C-f" 'org-fc-rater-next-button)
    (define-key map "\C-b" 'org-fc-rater-previous-button)
    map))

(defun org-fc-rater-set-up ()
  (setq org-fc-rater-buffer (get-buffer-create "*Rater*"))
  (setq org-fc-rater-origin-buffer (current-buffer))
  (save-window-excursion
    (message nil)

    (let* ((echo-keystrokes 0)

           (inhibit-message t)
           (garbage-collection-messages nil)
           (algo (org-fc-algorithm))
           (numbered-key-p
            (seq-find (lambda (rater)
                        (not (plist-get rater :key)))
                      (org-fc-algo-rating algo))))

      (set-window-buffer (minibuffer-window) org-fc-rater-buffer)
      (select-window (minibuffer-window))
      (let ((new-local-map (make-sparse-keymap)))
        (set-keymap-parent new-local-map org-fc-rater-map)
        (seq-map-indexed (lambda (rater i)
                           (define-key new-local-map
                             (if numbered-key-p
                                 (number-to-string i)
                               (plist-get rater :key))
                             (lambda ()
                               (interactive)
                               (org-fc-rater-button-action (plist-get rater :rate)))))
                         (org-fc-algo-rating algo))
        (org-fc-rater-redisplay algo new-local-map)
        (use-local-map new-local-map)
        (catch 'rating-done
          (Electric-command-loop
           'rating-done
           ;; Avoid `noprompt' due to
           ;; a bug in electric.el.
           (lambda () 'noprompt)
           t
           (lambda (x y) (when org-fc-rater-buffer (set-window-buffer (minibuffer-window) org-fc-rater-buffer)))))))))

(defun org-fc-rater-redisplay (algo map)
  (let ((inhibit-read-only t)
        (numbered-key-p (seq-find (lambda (rater)
                                    (not (plist-get rater :key)))
                                  (org-fc-algo-rating algo))))
    (read-only-mode 1)
    (set-buffer org-fc-rater-buffer)
    (delete-region (point-min) (point-max))
    (seq-map-indexed
     (lambda (rater i)
       (let* ((label (or (plist-get rater :tag)
                         (symbol-name (plist-get rater :rate))))
              (label (concat (if numbered-key-p
                                 (format "(%d) " i)
                               (format "(%s) " (key-description (plist-get rater :key))))
                             label))
              (width (- (/ (frame-width) (length (org-fc-algo-rating algo))) 1))
              (offset (/ (- width (string-width label)) 2)))
         (apply #'insert-text-button (concat (make-string offset ? ) label (make-string offset ? ))
                (when (plist-get rater :face)
                  (list 'face (plist-get rater :face)
                        'action (apply-partially #'org-fc-rater-button-action (plist-get rater :rate)))))
         (insert " ")))
     (org-fc-algo-rating algo))
    (put-text-property (point-min) (point-max) 'keymap map)))

(defun org-fc-rater-next-button ()
  "Interactive Wrapper for `next-button'."
  (interactive)
  (ignore-errors (goto-char (next-button (point)))))

(defun org-fc-rater-previous-button ()
  "Interactive Wrapper for `previous-button'."
  (interactive)
  (ignore-errors (goto-char (previous-button (point)))))

(defun org-fc-rater-kill-rater ()
  (interactive)
  (when org-fc-rater-buffer
    (kill-buffer org-fc-rater-buffer)
    (setq org-fc-rater-buffer nil))
  (if org-fc-rater-inactive-minibuffer
      (set-window-buffer (minibuffer-window) org-fc-rater-inactive-minibuffer)
    (setq org-fc-rater-inactive-minibuffer (window-buffer (minibuffer-window))))
  (when (window-live-p (get-buffer-window org-fc-rater-origin-buffer))
    (select-window (get-buffer-window org-fc-rater-origin-buffer))))

(provide 'org-fc-rater)
;;; org-fc-rater.el ends here
