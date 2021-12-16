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
(require 'org-fc-core)
(require 'tablist)

(defcustom org-fc-browser-buffer-name "*org-fc Browser*"
  "Name of the buffer to use for displaying the browser view."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-browser-list-entries-function #'org-fc-browser-list-entries-default
  "A function which lists cards in the format proper for `tabulated-list-entries'

The function will be passed an index from `org-fc-index' and must return
a list of vector for it."
  :type 'function
  :group 'org-fc)

(define-derived-mode org-fc-browser-mode tablist-mode "org-fc browser"
  "Major mode for browsing flashcards created by org-fc."
  (setq-local revert-buffer-function #'org-fc-browser-revert)
  (setq tabulated-list-format
        `[("Title" 120 nil)
          ("Intrv" 8 t)
          ("Due" 20 t :read-only)
          ("Type" 10 nil)])
  (hl-line-mode)
  (setq tabulated-list-entries (funcall org-fc-browser-list-entries-function))
  (setq tabulated-list-printer #'org-fc-browser-print)
  (setq tablist-operations-function #'org-fc-browser-operations)
  (setq tabulated-list-padding 0)
  (tabulated-list-init-header))

(defvar org-fc-browser-context org-fc-context-all
  "Context of the current browser view.")

(defun org-fc-browser-revert (_ignore-auto _noconfirm)
  "Reload the browser."
  (interactive)
  (setq tabulated-list-entries (funcall org-fc-browser-list-entries-function))
  (org-fc-browser-draw-buffer org-fc-browser-context))

(defun org-fc-browser--get-current-card ()
  "Get entry at point from `tabulated-list-entries'."
  (nth (1- (line-number-at-pos)) tabulated-list-entries))

(defun org-fc-browser-draw-buffer (context)
  "Draw a buffer with a list of all cards for CONTEXT."
  (let* ((buf (get-buffer-create org-fc-browser-buffer-name))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (tabulated-list-print t))))

(defun org-fc-browser-print (id cols)
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
        (insert (make-string x ? )))
    (let ((tabulated-list--near-rows ; Bind it if not bound yet (Bug#25506).
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (point-at-bol 0)) cols) cols))))
      (dotimes (n ncols)
        (setq x
              (let* ((format (aref tabulated-list-format n))
                     (name (nth 0 format))
                     (width (nth 1 format))
                     (props (nthcdr 3 format))
                     (pad-right (or (plist-get props :pad-right)
                                    1))
                     (right-align (plist-get props :right-align))
                     (label (if (stringp (aref cols n))
                                (aref cols n)
                              (car (aref cols n))))
                     (label-width (string-width label))
                     (help-echo (concat
                                 (car format)
                                 ": "
                                 label))
                     (opoint (point))
                     (not-last-col (< (1+ n)
                                      (length tabulated-list-format)))
                     (available-space (and not-last-col
                                           (if right-align
                                               width
                                             (tabulated-list--available-space
                                              width
                                              n)))))
                ;; Truncate labels if necessary (except last column).
                ;; Don't truncate to `width' if the next column is align-right
                ;; and has some space left, truncate to `available-space' instead.
                (when (and not-last-col
                           (> label-width available-space))
                  (setq label (truncate-string-to-width label available-space nil nil t t)
                        label-width available-space))
                (setq label (bidi-string-mark-left-to-right label))
                (when (and right-align
                           (> width label-width))
                  (let ((shift (- width label-width)))
                    (insert (propertize
                             (make-string shift ?\s)
                             'face
                             'header-line
                             'display
                             `(space :align-to ,(+ x shift))))
                    (setq width (- width shift))
                    (setq x (+ x shift))))
                (if (stringp (aref cols n))
                    (insert (if (get-text-property
                                 0
                                 'help-echo
                                 label)
                                label
                              (propertize
                               label
                               'help-echo
                               help-echo)))
                  (apply 'insert-text-button label (cdr (aref cols n))))
                (let ((next-x (1- (+ x pad-right width))))
                  ;; No need to append any spaces if this is the last column.
                  (when not-last-col
                    (when (> pad-right 0)
                      (insert (make-string pad-right ?\s)))
                    (insert (propertize
                             ;; We need at least one space to align correctly.
                             (make-string
                              (1- (- width (min 1 width label-width)))
                              ?\s)
                             'display
                             `(space :align-to ,next-x)))
                    (insert (propertize
                             ;; We need at least one space to align correctly.
                             (make-string
                              1
                              ?\s)
                             'face 'header-line
                             'display `(space))))
                  (put-text-property opoint (point)
                   'tabulated-list-column-name
                   name)
                  next-x)))))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols))))

(defun org-fc-browser-list-entries-default ()
  "Return a list with each element in the form of (CARD-ID [NUMBER TITLE INTERVAL DUE-DATE TYPE])
from calling `org-fc-index' with `org-fc-browser-context' as its argument."
  (let ((index (org-fc-index org-fc-browser-context))
        res)
    (while (progn
             (let* ((card-plist (pop index))
                    (positions (car (plist-get card-plist :positions))))
               (push (list (plist-get card-plist :id)
                           (vector (or (if (string-empty-p (plist-get card-plist :title))
                                           (plist-get card-plist :filetitle)
                                         (plist-get card-plist :title))
                                       "No title")
                                   (number-to-string (plist-get positions :interval))
                                   (format-time-string
                                    "%FT%TZ"
                                    (plist-get positions :due)
                                    "UTC0")
                                   (symbol-name (plist-get card-plist :type))))
                     res)
               index)))
    (nreverse res)))

(defun org-fc-browser-operations (op &rest args)
  (setq org-fc-browser-current-entry (tabulated-list-get-entry))
  (cl-case op
    (supported-operations '(find-entry edit-column))
    (find-entry
     (apply #'org-id-goto args))))


;;;###autoload
(defun org-fc-browser (context)
  "Open a buffer showing a list of all cards from CONTEXT."
  (interactive (list (org-fc-select-context)))
  (setq org-fc-browser-context context)
  (switch-to-buffer org-fc-browser-buffer-name)
  (unless (eq major-mode 'org-fc-browser-mode)
    (org-fc-browser-mode)
    (goto-char (point-min)))
  (org-fc-browser-draw-buffer context))

(provide 'org-fc-browser)
;;; org-fc-browser.el ends here
