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
a list of vector for it."
  :type 'function
  :group 'org-fc)

(defcustom org-fc-browser-title-length 70
  "Length for the string of a title."
  :type 'integer
  :group 'org-fc)

(define-derived-mode org-fc-browser-mode tabulated-list-mode "org-fc browser"
  "Major mode for browsing flashcards created by org-fc."
  (set (make-local-variable 'revert-buffer-function) #'org-fc-browser-revert))

(defvar org-fc-browser-context org-fc-context-all
  "Context of the current dashboard view.")

(defvar org-fc-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "<return>") #'org-fc-browser-open-id)
    map)
  "Keymap for `org-fc-browser-mode'.")

(defun org-fc-browser-revert (_ignore-auto _noconfirm)
  "Reload the current dashboard."
  (interactive)
  (org-fc-browser-draw-buffer org-fc-browser-context))

(defun org-fc-browser--get-current-card ()
  "docstring"
  (nth (1- (line-number-at-pos)) tabulated-list-entries))

(defun org-fc-browser-open-id ()
  (interactive)
  (org-id-open (car (org-fc-browser--get-current-card))  nil))

(defun org-fc-browser-draw-buffer (context)
  "Draw a buffer with a list of all cards for CONTEXT."
  (let* ((buf (get-buffer-create org-fc-browser-buffer-name))
         (inhibit-read-only t)
         (index (org-fc-index context)))
    (with-current-buffer buf
      (erase-buffer)
      (setq tabulated-list-format `[("No." 5 t)
                                   ("Title" ,org-fc-browser-title-length nil)
                                   ("Intrv" 10 t)
                                   ("Due" 20 t)
                                   ("Type" 10 nil)])
      (setq tabulated-list-entries (funcall org-fc-browser-list-entries-function
                                            index))
      (setq tabulated-list-padding 1)
      (tabulated-list-init-header)
      (tabulated-list-print))))

(defun org-fc-browser-list-entries-default (index)
  "Return a list in the form of (CARD-ID [NUMBER TITLE INTERVAL DUE-DATE TYPE])"
  (let (result)
    (dotimes (i (length index))
      (let* ((card-plist (nth i index))
             (positions (car (plist-get card-plist :positions))))
        (push (list (plist-get card-plist :id)
                    (vector (number-to-string i)
                            (or (if (string-empty-p (plist-get card-plist :title))
                                    (plist-get card-plist :filetitle)
                                  (plist-get card-plist :title))
                                "No title")
                            (number-to-string (plist-get positions :interval))
                            (format-time-string
                             "%FT%TZ"
                             (plist-get positions :due)
                             "UTC0")
                            (symbol-name (plist-get card-plist :type))))
              result)))
    (nreverse result)))


;;;###autoload
(defun org-fc-browser (context)
  "Open a buffer showing a list of all cards from CONTEXT."
  (interactive (list (org-fc-select-context)))
  (setq org-fc-browser-context context)
  (org-fc-browser-draw-buffer context)
  (switch-to-buffer org-fc-browser-buffer-name)
  (unless (eq major-mode 'org-fc-browser-mode)
    (org-fc-browser-mode))
  (goto-char (point-min)))

(provide 'org-fc-browser)
;;; org-fc-browser.el ends here
