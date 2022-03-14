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

(defvar org-fc-rater-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-map org-fc-review-rate-mode-map))
    (define-key map "\C-f" 'org-fc-rater-next-button)
    (define-key map "\C-b" 'org-fc-rater-previous-button)
    map))

(defun org-fc-rater-setup-button ()
  (setq org-fc-rater-buffer (get-buffer-create "*Rater*"))
  (unwind-protect
      (save-window-excursion
        (require 'electric) (message nil)
        (let ((echo-keystrokes 0)
              (garbage-collection-messages nil))
          (set-window-buffer (minibuffer-window) org-fc-rater-buffer)
          (select-window (minibuffer-window))
          (org-fc-rater-redisplay)))
    (when org-fc-rater-buffer
      (kill-buffer org-fc-rater-buffer)
      (setq org-fc-rater-buffer nil))))

(defun org-fc-rater-redisplay ()
  (let ((inhibit-read-only t))
    (with-current-buffer org-fc-rater-buffer
      (delete-region (point-min) (point-max))
      )))


(provide 'org-fc-rater)
;;; org-fc-rater.el ends here
