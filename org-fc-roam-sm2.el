;;; org-fc-roam-sm2.el --- Variant of SM2 algorithm with extra parameters  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: extensions

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
(require 'org-fc-algo-sm2)

(defun org-fc-roam-sm2-priority-get (&optional ease)
  "Return a float based on the content of this buffer.
EASE will help with the computation."
  (setq ease (or ease (org-fc-algo-sm2-ease-initial)))
  (let ((org-export-with-toc nil)
        (org-export-with-author nil)
        (org-export-with-emphasize nil)
        (org-export-with-tables nil)
        (org-export-show-temporary-export-buffer nil)
        (priority) (buf) (winconf (current-window-configuration)))
    (if (org-before-first-heading-p)
	(setq buf (org-ascii-export-as-ascii))
      (setq buf (org-ascii-export-as-ascii nil t t t)))

    (setq priority (or (ignore-errors
                         (with-current-buffer buf
                           (/ (org-fc-roam-sm2-lix-region (point-min)
                                                          (point-max))
                              (* 40 ease))))
                       80))
    (kill-buffer buf)
    (set-window-configuration winconf)
    priority))

(defun org-fc-roam-sm2-inital-review-data ()
  (let ((priority (org-fc-roam-sm2-priority-get)))
    (list "front"
          priority
          (org-fc-algo-sm2-ease-initial)
          0 0 0
          (if (string= "cloze" (org-entry-get nil org-fc-type-property))
              (org-fc-timestamp-in (org-fc-roam-sm2-cloze-interval priority))
            (format-time-string "%FT%TZ" (org-get-scheduled-time nil) "UTC0")))))

(defun org-fc-roam-sm2-lix-region (start end)
  "The Lasbarhetsindex Swedish Readability test score for region in START to END.

LIX is a readability test that performs well on most of the Western
European languages. The test focuses on total words, the number of
sentences and number of long words (more than 6 characters). A
score is returned which gives an indication of reading ease.

The LIX readability formula is as follows:

LIX = A/B + (C x 100)/A, where

A = Number of words
B = Number of periods (defined by period, colon or capital first letter)
C = Number of long words (More than 6 letters)"
  (let ((words (how-many (rx (+ word)) start end))
        (sentences (how-many (sentence-end) start end))
        (long-words (how-many (rx (>= 7 word)) start end)))
    ;; Prevent dividing by 0
    (when (= words 0)
      (setq words 1))
    (when (= sentences 0)
      (setq sentences 1))
    ;; Lix = wds/sent+100*(wds >= 6 char)/wds
    (+ (/ words sentences)
       (/ (* long-words 100.0)
          words))))

(defun org-fc-roam-sm2-cloze-interval (priority)
  (require 'calc)
  (let* ((min-bound (calc-eval (format "%f*%s" 0.0023 (calc-eval (format "%f^2" priority)))))
         (max-bound (calc-eval (format "0.4*%f" priority))))
    (string-to-number (calc-eval (format "%s+%f"
                                         min-bound
                                         (cl-random (string-to-number (calc-eval (format "%s-%s" max-bound min-bound)))))))))

(defun org-fc-roam-sm2-next-parameters (rating position prior ease box interval postp due)
  (cl-destructuring-bind (position next-ease next-box next-interval next-due)
      (org-fc-algo-sm2-next-parameters rating position ease box interval due)
    (list position (org-fc-roam-sm2-priority-get next-ease) next-ease next-box next-interval postp next-due)))

(defun org-fc-roam-sm2-format-data (where position prior ease box interval postp due)
  (let ((formatted-params (list (if (stringp position)
                                    position
                                  (format "%s" position)) 
                                (format "%.3f" prior)
                                (format "%.2f" ease)
                                (format "%d" box)
                                (format "%.2f" interval)
                                (format "%d" postp)
                                due)))
    (if (eq where 'history)
        (butlast formatted-params)
      formatted-params)))

(org-fc-register-algo
 'roam-sm2
 '("position" "prior" "ease" "box" "interval" "postp" "due")
 '((:rate again :tag "Again" :key "a" :face (:background "red" :foreground "black"))
   (:rate hard :tag "Hard" :key "h" :face (:background "orange" :foreground "black"))
   (:rate good :tag "Good" :key "g" :face (:background "green" :foreground "black"))
   (:rate easy :tag "Easy" :key "e" :face (:background "lime green" :foreground "black")))
 'org-fc-roam-sm2-inital-review-data
 'org-fc-roam-sm2-next-parameters
 'org-fc-roam-sm2-format-data)

(provide 'org-fc-roam-sm2)
;;; org-fc-roam-sm2.el ends here
