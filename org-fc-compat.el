;;; org-fc-compat.el --- Compatibility Code -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.1.0

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
;; Code needed for backward compatibility with previous versions of org-fc.
;;
;;; Code:
;;;; Obsolete Aliases

(define-obsolete-function-alias
  'org-fc-review-rate-card
  'org-fc-review-rate "0.0.1")

(define-obsolete-function-alias
  'org-fc-show-all
  'org-fc-remove-overlays "0.0.1")

(define-obsolete-function-alias
  'org-fc-stats
  'org-fc-dashboard-stats "0.0.1")

(define-obsolete-function-alias
  'org-fc--hashtable-to-alist
  'org-fc-dashboard--hashtable-to-alist "0.0.1")

(define-obsolete-function-alias
  'org-fc-sm2-fuzz
  'org-fc-algo-sm2-fuzz "0.0.1")

(define-obsolete-function-alias
  'org-fc-sm2-next-parameters
  'org-fc-algo-sm2-next-parameters "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-changes
  'org-fc-algo-sm2-changes "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fixed-intervals
  'org-fc-algo-sm2-intervals "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-min
  'org-fc-algo-sm2-ease-min "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-max
  'org-fc-algo-sm2-ease-max "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-initial
  'org-fc-algo-sm2-ease-initial "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-min
  'org-fc-algo-sm2-fuzz-min "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-max
  'org-fc-algo-sm2-fuzz-max "0.0.1")

(define-obsolete-variable-alias
  'org-fc-audio-property-after
  'org-fc-audio-after-setup-property "0.1.0")

(define-obsolete-variable-alias
  'org-fc-audio-property-before
  'org-fc-audio-before-setup-property "0.1.0")

(define-obsolete-function-alias
  'org-fc-audio-set-before
  'org-fc-audio-set-before-setup "0.1.0")

(define-obsolete-function-alias
  'org-fc-audio-set-after
  'org-fc-audio-set-after-setup "0.1.0")

(define-obsolete-function-alias
  'org-fc-review-data-position
  'org-fc-review-data-location
  "0.2.0"
  "The function actually returns the beginning and the end points in
which the review data property drawer resides i.e. its location,
naming it \"position\" might cause confusion with the \"position\" in
the review data e.g. the \"front\" or the \"back\" of a card etc.")


;; TODO: doc
(defun org-fc-insert-hline-review-data ()
  "Insert hline under every position in review data drawer except the last one.


For example, the function will turn a review data drawer like this,

:REVIEW_DATA:
| position | ease | box | interval | due                  |
|----------+------+-----+----------+----------------------|
|        0 | 2.50 |   6 |    80.76 | 2000-01-01T00:00:00Z |
|        1 | 2.50 |   6 |    72.76 | 2000-01-01T00:00:00Z |
|        2 | 2.50 |   6 |    91.28 | 2000-01-01T00:00:00Z |
|        3 | 2.50 |   6 |    95.75 | 2000-01-01T00:00:00Z |
:END:

to this,

:REVIEW_DATA:
| position | ease | box | interval | due                  |
|----------+------+-----+----------+----------------------|
|        0 | 2.50 |   6 |    80.76 | 2000-01-01T00:00:00Z |
|----------+------+-----+----------+----------------------|
|        1 | 2.50 |   6 |    72.76 | 2000-01-01T00:00:00Z |
|----------+------+-----+----------+----------------------|
|        2 | 2.50 |   6 |    91.28 | 2000-01-01T00:00:00Z |
|----------+------+-----+----------+----------------------|
|        3 | 2.50 |   6 |    95.75 | 2000-01-01T00:00:00Z |
:END:
"
  (interactive)
  (when-let ((location (org-fc-review-data-location)))
    (org-with-point-at (car location)
      (let* ((review-data (org-fc-review-data-get))
             (line-index (length review-data)))
        (when (> line-index 1)
          (while (progn (org-table-goto-line line-index)
                        (cl-incf line-index -1)
                        (org-table-insert-hline)
                        (not (= 1 (1- (org-table-current-line)))))))))))

(defun org-fc-rename-cloze-position-to-zero ()
  (interactive)
  (when-let ((cloze-p (org-fc-entry-cloze-p))
             (location (org-fc-review-data-location)))
    (org-with-point-at (car location)
      (when-let ((review-data (org-fc-review-data-get)))
        (let* ((review-data (org-fc-review-data-get))
               (line-index (1+ (length review-data))))
          (while (progn (org-table-goto-line line-index)
                        (cl-incf line-index -1)
                        (org-table-get-field 1 "0")
                        (not (= 1 (1- (org-table-current-line))))))
          (org-table-align))))))


(defun org-fc-import-history-from-file ()
  (interactive)
  (when-let ((id (org-id-get))
             (positions (mapcar #'car (org-fc-review-data-get))))
    (dolist (pos positions)
      (let ((history (org-fc-awk-history-for-id id pos)))
        (seq-do-indexed
         (lambda (history i)
           (org-fc-review-history-set
            (list (plist-get history :position)
                  (plist-get history :ease)
                  (plist-get history :box)
                  (plist-get history :interval)
                  (plist-get history :date))
            (1+ i)))
         history)))))

(defun org-fc-migrate-wizard ()
  (interactive)
  (let* ((index (org-fc-index '(:paths all)))
         (ids (delete-dups
               (mapcar (lambda (plist)
                         (plist-get plist :id))
                       index))))
    (dolist (id ids)
      (org-id-goto id)
      (org-fc-migrate))))

(defun org-fc-migrate ()
  ""
  (org-with-wide-buffer
   (org-show-all)
   (org-fc-rename-cloze-position-to-zero)
   (org-fc-insert-hline-review-data)
   (org-fc-import-history-from-file)))

;;; Footer

(provide 'org-fc-compat)

;;; org-fc-compat.el ends here
