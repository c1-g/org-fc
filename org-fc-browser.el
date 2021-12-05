;;; org-fc-browser.el --- Card browser for org-fc    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  c1-g

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
(defcustom org-fc-browser-buffer-name "*org-fc Browser*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)


(define-derived-mode org-fc-browser-mode tabulated-list-mode "org-fc browser"
  "Major mode for browsing flashcards created by org-fc.")

(defvar org-fc-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map)
  "Keymap for `org-fc-browser-mode'.")

(defun org-fc-browser-draw-buffer (context)
  "Draw a buffer with a list of all cards for CONTEXT.")


;;;###autoload
(defun org-fc-browser (context)
  "Open a buffer showing a list of all cards from CONTEXT."
  (interactive (list (org-fc-select-context)))
  (switch-to-buffer org-fc-browser-buffer-name)
  (org-fc-browser-draw-buffer context)
  (goto-char (point-min))
  (org-fc-browser-mode))

(provide 'org-fc-browser)
;;; org-fc-browser.el ends here
