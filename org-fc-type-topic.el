;;; org-fc-type-topic.el --- Special card type for passive reading.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  c1-g

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
(require 'org-fc-core)

(defun org-fc-type-topic-init ()
  "Mark headline as card of the topic type."
  (interactive)
  (org-fc--init-card "topic")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-topic-setup (_position)
  "Prepare a normal card for review."
  (interactive)
  ;; Make sure the properties drawer is collapsed
  (outline-hide-body))

(org-fc-register-type
 'topic
 'org-fc-type-topic-setup
 'ignore
 'org-fc-noop)




(provide 'org-fc-type-topic)
;;; org-fc-type-topic.el ends here
