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
(require 'org-fc-core)
(require 'org-fc-review)
(require 'org-fc-algo)
(require 'org-fc-algo-sm2)

(require 'org-roam)
(require 'org-roam-db)

(defconst org-fc-roam-db--schemata
  '((cards
     ([(node-id :not-null)
       (title :not-null)
       (pos :not-null)
       (prior :integer :not-null)
       (ease :not-null)
       (box :not-null)
       (ivl :integer :not-null)
       (postp :integer :not-null)
       (due :integer :not-null)
       (reps :integer :not-null)
       (lapses :integer :not-null)
       (type :not-null)
       (queue :integer :not-null)]
      (:foreign-key
       [node-id]
       :references nodes
       [id]
       :on-delete :cascade)))
    (revlog
     ([(cid :not-null)
       (time :integer :not-null)
       (rating :not-null)
       (type :not-null)
       (pos :not-null)
       (prior)
       (ease :integer :not-null)
       (box :integer :not-null)
       (ivl :integer :not-null)]
      ;; (queue :integer :not-null)
      (:foreign-key
       [cid]
       :references nodes
       [id]
       :on-delete :cascade))))
  "Org fc db schemata.")

(defconst org-fc-roam-db--indices
  '((cards-node-id cards [node-id])
    (revlog-cid revlog [cid]))
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

;;;###autoload
(define-minor-mode org-fc-roam-db-autosync-mode
  "Global minor mode to automatically synchronise org-fc-roam db."
  :global t
  :group 'org-fc
  :init-value nil
  (let ((enabled org-fc-roam-db-autosync-mode))
    (cond (enabled
           (setq org-fc-roam-db--initalized nil)
           (add-hook 'org-roam-find-file-hook #'org-fc-roam-update)
           (advice-add 'org-roam-node-find-noselect
                       :filter-return
                       #'org-fc-roam-update)
           ;; attach custom schemata
           (seq-each
            (lambda (schema)
              (add-to-list 'org-roam-db--table-schemata schema 'append))
            org-fc-roam-db--schemata)

           ;; attach custom indices
           (seq-each
            (lambda (index)
              (add-to-list 'org-roam-db--table-indices index 'append))
            org-fc-roam-db--indices)

           ;; make sure that extra tables exist table exists
           (advice-add 'org-roam-db :around #'org-fc-roam-db--init)

           ;; make sure that all data is inserted into table
           (advice-add
            'org-roam-db-insert-file-node
            :after
            #'org-fc-roam-db-insert-file-review-history)
           (advice-add
            'org-roam-db-insert-node-data
            :after
            #'org-fc-roam-db-insert-outline-review-history))
          (t
           (setq org-fc-roam-db--initalized nil)
           ;; (advice-remove 'org-roam-db-map-links #'org-fc-roam-db-insert-links)
           (advice-remove
            'org-roam-db-insert-node-data #'org-fc-roam-db-insert-outline-review-history)
           (advice-remove
            'org-roam-db-insert-file-node #'org-fc-roam-db-insert-file-review-history)
           (advice-remove 'org-roam-db #'org-fc-roam-db--init)
           (seq-each
            (lambda (schema)
              (setq org-roam-db--table-schemata
                    (delete schema org-roam-db--table-schemata)))
            org-fc-roam-db--schemata)
           (seq-each
            (lambda (index)
              (setq org-roam-db--table-indices
                    (delete index org-roam-db--table-indices)))
            org-fc-roam-db--indices)))))

;;;###autoload
(defun org-fc-roam-db-autosync-enable ()
  "Activate function `org-fc-roam-db-autosync-mode'."
  (org-fc-roam-db-autosync-mode +1))

(defun org-fc-roam-db-autosync-disable ()
  "Deactivate function `org-fc-roam-db-autosync-mode'."
  (org-fc-roam-db-autosync-mode -1))

(defun org-fc-roam-db-autosync-toggle ()
  "Toggle status of function `org-fc-roam-db-autosync-mode'."
  (org-fc-roam-db-autosync-mode 'toggle))

(defun org-fc-roam-update (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (org-roam-file-p)
      (when-let* ((id (org-id-get))
                  (data (org-roam-db-query "SELECT pos, prior, ease, box, ivl,
postp, '\"' || strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ', due, 'unixepoch') || '\"'
FROM cards WHERE node_id = $s1" id)))
        (org-fc-review-data-set data)
        (save-buffer)))
    (current-buffer)))


(defun org-fc-roam-db-insert-file-review-history ()
  "Insert file level review history into `org-roam' database."
  (org-with-point-at (point-min)
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-link-display-format
                       (or (cadr
                            (assoc "TITLE"
                                   (org-collect-keywords '("title"))
                                   #'string-equal))
                           (file-relative-name
                            file org-roam-directory))))
               (review-data (org-fc-review-data-get))
               (history (org-fc-awk-history-for-id id))
               (type (org-entry-get nil org-fc-type-property))
               (tags org-file-tags))

          (when (and (member org-fc-flashcard-tag org-file-tags)
                     review-data)
            (org-roam-db-query
             [:delete :from cards
                      :where (= node-id $s1)]
             id)
            (when history
              (org-roam-db-query
               [:insert :into revlog
                        :values $v1]
               (seq-map (lambda (hist)
                          (let ((params (plist-get hist :params)))
                            (vector id
                                    (string-to-number (plist-get hist :time))
                                    (plist-get hist :rating)
                                    (intern type)
                                    (plist-get params :position)
                                    (and (plist-get params :prior)
                                         (string-to-number (plist-get params :prior)))
                                    (string-to-number (plist-get params :ease))
                                    (string-to-number (plist-get params :box))
                                    (string-to-number (plist-get params :interval)))))
                        history)))
              (org-roam-db-query
               [:insert :into cards
                        :values $v1]
             (seq-map (lambda (datum)
                        (cl-destructuring-bind (pos prior ease box intrv postp due) datum
                    (vector id
                            title
                            pos
                            (string-to-number prior)
                            (string-to-number ease)
                            (string-to-number box)
                            (string-to-number intrv)
                            (string-to-number postp)
                            (string-to-number (format-time-string "%s" (date-to-time due)))
                            (length history)
                            (cl-count "again" history :test (lambda (rating elt)
                                                              (string= rating (plist-get elt :rating))))
                            (intern type)
                            (cond ((member org-fc-suspended-tag tags) -1)
                                  ((member org-fc-pending-tag tags) 0)
                                  (t 1)))))
                      review-data))))))))

(defun org-fc-roam-db-insert-outline-review-history ()
  "Insert outline level review history into `org-roam' database."
  (when-let ((id (org-id-get)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (heading-components (org-heading-components))
           (level (nth 1 heading-components))
           (title
            (or (nth 4 heading-components)
                (progn (lwarn
                        'org-roam
                        :warning
                        "Node in %s:%s:%s has no title, skipping..."
                        file
                        (line-number-at-pos)
                        (1+ (- (point) (line-beginning-position))))
                       (cl-return-from
                           org-roam-db-insert-node-data))))
           (title (org-link-display-format title))
           (review-data (org-fc-review-data-get))
           (history (org-fc-awk-history-for-id id))
           (type (org-entry-get nil org-fc-type-property))
           (tags (org-get-tags)))
      (when (and (member org-fc-flashcard-tag (org-get-tags))
                 review-data)
        (org-roam-db-query
         [:delete :from cards
                  :where (= node-id $s1)]
         id)
        (when history
          (org-roam-db-query
           [:insert :into revlog
                    :values $v1]
           (seq-map (lambda (hist)
                      (let ((params (plist-get hist :params)))
                        (vector id
                                (plist-get hist :time)
                                (plist-get hist :rating)
                                (intern type)
                                (plist-get params :position)
                                (and (plist-get params :prior)
                                     (string-to-number (plist-get params :prior)))
                                (plist-get params :ease)
                                (plist-get params :box)
                                (plist-get params :interval))))
                    history)))
        (org-roam-db-query
         [:insert :into cards
                  :values $v1]
         (seq-map (lambda (datum)
                    (cl-destructuring-bind (pos prior ease box intrv postp due) datum
                      (vector id
                              title
                              pos
                              (string-to-number prior)
                              (string-to-number ease)
                              (string-to-number box)
                              (string-to-number intrv)
                              (string-to-number postp)
                              (string-to-number (format-time-string "%s" (date-to-time due)))
                              (length history)
                              (cl-count "again" history :test (lambda (rating elt)
                                                                (string= rating (plist-get elt :rating))))
                              (intern type)
                              (cond ((member org-fc-suspended-tag tags) -1)
                                    ((member org-fc-pending-tag tags) 0)
                                    (t 1)))))
                  review-data))))))

(defun org-fc-roam-index-pending ()
  (org-roam-db-query "SELECT * FROM
(SELECT
':num', rowid,
':id', id,
':title', title,
':type', type,
':position', pos,
':prior', prior,
':ease', ease,
':box', box,
':interval', ivl,
':tags', '(' || group_concat(tags, ' ') || ')' as tags,
':due', '\"' || strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ', due, 'unixepoch') || '\"',
':path', path,
':filetitle', filetitle
FROM
(SELECT * FROM
(SELECT rowid, id, title, pos, prior, ease, box, ivl, due, postp, type, queue, tags, path, filetitle
FROM
(SELECT
cards.rowid as rowid,
cards.node_id as id,
cards.title as title,
cards.pos as pos,
cards.prior as prior,
cards.ease as ease,
cards.box as box,
cards.ivl as ivl,
cards.due as due,
cards.postp as postp,
cards.type as type,
cards.queue as queue,
tags.tag as tags,
nodes.file as path,
files.title as filetitle
FROM cards
LEFT JOIN tags ON tags.node_id = cards.node_id
LEFT JOIN nodes ON nodes.id = cards.node_id
LEFT JOIN files ON files.file = nodes.file
GROUP BY id, cards.pos, tags)
GROUP BY id, pos)
WHERE date(due, 'unixepoch', 'utc') <= date('now', 'localtime') AND queue = 0)
GROUP BY id)"))

(defun org-fc-roam-index (paths &optional filter)
  (cl-remove-if-not (or filter #'always)
                    (if (= 1 (length paths))
                        (org-roam-db-query
                         "SELECT * FROM
(SELECT
':num', rowid,
':id', id,
':title', title,
':type', type,
':suspended', 'nil',
':positions' || '((',
':position', pos,
':prior', prior,
':ease', ease,
':box', box,
':interval', ivl,
':due', '\"' || strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ', due, 'unixepoch') || '\"',
':rating', '\"' || 'Future' || '\"))',
':tags', '(' || group_concat(tags, ' ') || ')' as tags,
':path', path,
':filetitle', filetitle
FROM
(SELECT rowid, id, title, pos, prior, ease, box, ivl, postp, due, type, queue, tags, path, filetitle
FROM
(SELECT
cards.rowid as rowid,
cards.node_id as id,
cards.title as title,
cards.pos as pos,
cards.prior as prior,
cards.ease as ease,
cards.box as box,
cards.ivl as ivl,
cards.postp as postp,
cards.due as due,
cards.type as type,
cards.queue as queue,
tags.tag as tags,
nodes.file as path,
files.title as filetitle
FROM cards
LEFT JOIN tags ON tags.node_id = cards.node_id
LEFT JOIN nodes ON nodes.id = cards.node_id
LEFT JOIN files ON files.file = nodes.file
GROUP BY id, cards.pos, tags)
GROUP BY id, pos)
WHERE id IN $v1 AND date(due, 'unixepoch', 'utc') <= date('now', 'localtime') AND queue = 1
GROUP BY id, pos)
ORDER BY prior" (vconcat (org-roam-with-file (car paths) t
                           (org-element-cache-map (lambda (el)
                                                    (when (org-fc-entry-p) (org-id-get)))
                                                  :granularity 'element :restrict-elements '(property-drawer)))))
                      (org-roam-db-query "SELECT * FROM
(SELECT
':num', rowid,
':id', id,
':title', title,
':type', type,
':suspended', 'nil',
':positions' || '((',
':position', pos,
':prior', prior,
':ease', ease,
':box', box,
':interval', ivl,
':due', '\"' || strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ', due, 'unixepoch') || '\"',
':rating', '\"' || 'Future' || '\"))',
':tags', '(' || group_concat(tags, ' ') || ')' as tags,
':path', path,
':filetitle', filetitle
FROM
(SELECT rowid, id, title, pos, prior, ease, box, ivl, postp, due, type, queue, tags, path, filetitle
FROM
(SELECT
cards.rowid as rowid,
cards.node_id as id,
cards.title as title,
cards.pos as pos,
cards.prior as prior,
cards.ease as ease,
cards.box as box,
cards.ivl as ivl,
cards.postp as postp,
cards.due as due,
cards.type as type,
cards.queue as queue,
tags.tag as tags,
nodes.file as path,
files.title as filetitle
FROM cards
LEFT JOIN tags ON tags.node_id = cards.node_id
LEFT JOIN nodes ON nodes.id = cards.node_id
LEFT JOIN files ON files.file = nodes.file
GROUP BY id, cards.pos, tags)
GROUP BY id, pos)
WHERE date(due, 'unixepoch', 'utc') <= date('now', 'localtime') AND queue = 1
GROUP BY id, pos)
ORDER BY prior"))))



;; Roam-SM2 algorithm
(defun org-fc-priority (&optional ease)
  "Return a float based on the content of this buffer.
EASE will help with the computation."
  (save-excursion
    (let* ((content (abs (progn (org-back-to-heading-or-point-min)
                                (org-fc-end-of-meta-data t)
                                (- (point)
                                   (progn (outline-next-visible-heading 1) (point)))))))
      (* 100 (/ (float (or ease (org-fc-algo-sm2-ease-initial)))
                (if (zerop content)
                    100
                  content))))))

(defun org-fc-roam-sm2-inital-review-data ()
  (list "front"
        (org-fc-priority (org-fc-algo-sm2-ease-initial))
        (org-fc-algo-sm2-ease-initial)
        0
        0
        0
        (org-fc-timestamp-in 0)))

(defun org-fc-roam-sm2-next-parameters (rating position prior ease box interval postp due)
  (cl-destructuring-bind (position next-ease next-box next-interval next-due)
      (org-fc-algo-sm2-next-parameters rating position ease box interval due)
    (list position (org-fc-priority next-ease) next-ease next-box next-interval postp next-due)))

(defun org-fc-roam-sm2-format-data (where position prior ease box interval postp due)
  (let ((formatted-params (list position
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
 '(again hard good easy)
 'org-fc-roam-sm2-inital-review-data
 'org-fc-roam-sm2-next-parameters
 'org-fc-roam-sm2-format-data)


(provide 'org-fc-roam)
;;; org-fc-roam.el ends here
