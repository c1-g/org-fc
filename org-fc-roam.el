;;; org-fc-roam.el --- Query Org-roam for org-fc     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: data, multimedia

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

;; Query org roam's database instead of awk file.
;; The awk indexer and the history file will act as "backup".

;; All the codes here is based on Vulpea, a library of org-roam extensions
;; that insert its own table into org-roam's database.
;; See: https://github.com/d12frosted/vulpea.

;;; Code:
(require 'org-roam)
(require 'org-roam-db)

(defconst org-fc-roam--schemata
  '((review-history
     ([(node-id :not-null)
       (title :not-null)
       (pos :not-null)
       (ease :not-null)
       (box :not-null)
       (intrv :not-null)
       (date :not-null)]
      (:foreign-key
       [node-id]
       :references
       nodes [id]
       :on-delete
       :cascade))))
  "Org fc db schemata.")

(defconst org-fc-roam-db--indices
  '((review-history-node-id review-history [node-id]))
  "Org fc db indices.")

(defvar org-fc-roam-db--initalized nil
  "Non-nil when database was initialized.")

(defun org-fc-roam-db--init (get-db)
  "Initialize database by creating missing tables if needed.

GET-DB is a function that returns connection to database."
  (when-let ((db (funcall get-db)))
    (unless org-fc-roam-db--initalized
      (emacsql-with-transaction db
        (pcase-dolist (`(,table ,schema) org-fc-roam-db--schemata)
          (unless (emacsql db
                           [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name $r1))]
                           (emacsql-escape-identifier table))
            (emacsql db [:create-table $i1 $S2] table schema)))
        (pcase-dolist (`(,index-name ,table ,columns)
                       org-fc-roam-db--indices)
          (unless (emacsql db
                           [:select name
                            :from sqlite_master
                            :where (and (= type 'index)
                                        (= name $r1))]
                           (emacsql-escape-identifier index-name))
            (emacsql db [:create-index $i1 :on $i2 $S3]
                     index-name table columns))))
      (setq org-fc-roam-db--initalized t))
    db))

(provide 'org-fc-roam)
;;; org-fc-roam.el ends here
