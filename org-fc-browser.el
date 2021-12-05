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

(defcustom org-fc-browser-list-entries-function #'org-fc-browser-list-entries-default
  "A function which lists cards in the format proper for `tabulated-list-entries'

The function will be passed an index from `org-fc-index' and must return
a list of vector for it.")


(define-derived-mode org-fc-browser-mode tabulated-list-mode "org-fc browser"
  "Major mode for browsing flashcards created by org-fc.")

(defvar org-fc-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map)
  "Keymap for `org-fc-browser-mode'.")

(defun org-fc-browser-draw-buffer (context)
  "Draw a buffer with a list of all cards for CONTEXT."
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (index (org-fc-index context))
         (stats (org-fc-dashboard-stats index))
         (created-stats (plist-get stats :created))
         (due-stats (plist-get stats :due))
         (reviews-stats (org-fc-awk-stats-reviews)))
    (with-current-buffer buf
      (erase-buffer)
      (setq tabulated-list-format `[("No." 5 t)
                                    ("Title" 80 t)
                                    ("Intrv" 4 t)
                                    ("Due" 20 t)
                                    ("Type" 10 nil)])
      (setq tabulated-list-entries (funcall org-fc-browser-list-entries-function
                                            index)))))


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
