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
(require 'org-fc-roam-sm2)

(require 'org-roam)
(require 'org-roam-db)

;;; Options
(defcustom org-fc-roam-auto-sort t
  "Whether to automatically run `org-fc-roam-auto-sort' when Emacs closes."
  :group 'org-fc
  :type 'boolean)

(defcustom org-fc-roam-auto-postpone t
  "Whether to automatically run `org-fc-roam-auto-postpone' when Emacs closes."
  :group 'org-fc
  :type 'boolean)

(defcustom org-fc-roam-postpone-skip-following-number-of-cards 50
  "How many cards should be left after the Postpone.

If you choose 50, Postpone will first execute a standard
postpone procedure, and then it will postpone more, if needed,
until only 50 cards remain un-postponed. This option is useful
in auto-postpone if you want to ensure that you inherit no more
than a given number of outstanding cards from prior days of
learning."
  :group 'org-fc
  :type 'integer)

(defcustom org-fc-roam-postpone-delay-factor '(1.2 . 1.5)
  "How much cards should be delayed.

This variable is a cons cell of (OTHER-DELAY-FACTOR . TOPIC-DELAY-FACTOR).
with OTHER-DELAY-FACTOR being the delay factor for cards with other types
and TOPIC-DELAY-FACTOR being the delay factor for cards with `topic' type.

For example, if you choose the delay of 1.1 (10%) on a card with
the interval of 100 days, it will be delayed by ten days, i.e.
rescheduled to the interval of 110 days. Postpone will always
increase intervals by no less than one day from the present day.
This way, all items on which Postpone is executed fall out of the
outstanding subset."
  :group 'org-fc
  :type '(cons (float :tag "Delay factor for other types of cards")
               (float :tag "Delay factor for topic cards")))

(defcustom org-fc-roam-postpone-maximum-interval '(50 . 100)
  "A ceiling on the length of the delay interval.

This variable is a cons cell of (OTHER-MAX-IVL . TOPIC-MAX-IVL).
with OTHER-MAX-IVL being the maximum interval for cards with other types
and TOPIC-MAX-IVL being the maximum interval for cards with `topic' type.

For example, if you choose the delay of 1.1 on a card with
the interval of 200 days, and the maximum interval is 5 days, the
card will be delayed only by 5 days (instead of the 20 days
produced by multiplying the original interval by the delay
factor)."
  :group 'org-fc
  :type '(cons (integer :tag "Maximum interval for other types of cards")
               (integer :tag "Maximum interval for topic cards")))

(defcustom org-fc-roam-postpone-minimum-interval '(1 . 6)
  "A floor on the length of the delay interval.

This variable is a cons cell of (OTHER-MIN-IVL . TOPIC-MIN-IVL).
with OTHER-MIN-IVL being the minimum interval for cards with other types
and TOPIC-MIN-IVL being the minimum interval for cards with `topic' type.

For example, if you set it to (3 . 5), cards with other types will be delayed by no
less than 3 days, and with `topic' type, no less than 5."
  :group 'org-fc
  :type '(cons (integer :tag "Minimum interval for other types of cards")
               (integer :tag "Minimum interval for topic cards")))

(defcustom org-fc-shuffled-others-proportion 80
  "Proportion of randomized cards in comparision to prioritized cards.
Cards are of other types than topic."
  :group 'org-fc
  :type 'integer)

(defcustom org-fc-shuffled-topic-proportion 80
  "Proportion of randomized topic in comparision to prioritized topics."
  :group 'org-fc
  :type 'integer)


;;; Database
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
           (advice-add 'org-roam-node-open
                       :after
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
          
          (when (member org-fc-flashcard-tag tags)
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
                                    (string-to-number (plist-get params :ease))
                                    (string-to-number (plist-get params :box))
                                    (string-to-number (plist-get params :interval)))))
                        history)))
            (when review-data
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
                                          (t 1)))))
                        review-data)))))))))

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

      (when (member org-fc-flashcard-tag tags)
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
        (when review-data
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
                                      (t 1)))))
                    review-data)))))))

(defun org-fc-roam-review-history-add (history)
  (let ((current-card (oref org-fc-review--session current-item)))
    (org-roam-db-query
     [:insert :into revlog
              :values $v1]
     (cl-destructuring-bind (date path id algo delta rating pos prior ease box ivl postp)
         history
       (vector id
               (string-to-number delta)
               rating
               (plist-get current-card :type)
               pos
               (string-to-number prior)
               (string-to-number ease)
               (string-to-number box)
               (string-to-number ivl))))))

(advice-add 'org-fc-roam-review-history-add :before 'org-fc-review-history-add)


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

(defun org-fc-roam-auto-sort ()
  "Sort cards by their priority.

When `org-fc-roam-auto-sort' is non-nil, this function will be called by
`org-fc-roam-maybe-postpone-then-sort' when Emacs closes."
  (org-roam-db-query [:create :table $i1 $S2]
                     'sorted_cards
                     (cadr (assq 'cards org-roam-db--table-schemata)))
  (org-roam-db-query "INSERT INTO sorted_cards SELECT * FROM cards ORDER BY prior")
  (org-roam-db-query "DROP TABLE cards")
  (org-roam-db-query "ALTER TABLE sorted_cards RENAME TO cards"))


(defun org-fc-roam-auto-postpone ()
  "Postpone low-priority cards.

When `org-fc-roam-auto-postpone' is non-nil, this function will be called by
`org-fc-roam-maybe-postpone-then-sort' when Emacs closes."
  (org-roam-db-query (org-roam-db-query [:create :table $i1 $S2]
                                        'postponed_cards
                                        (cadr (assq 'cards org-roam-db--table-schemata))))

  (org-roam-db-query "INSERT INTO postponed_cards
SELECT * FROM cards
WHERE due >= strftime('%%s','now', 'utc')
ORDER BY prior")

  (org-roam-db-query "INSERT INTO postponed_cards
SELECT * FROM cards
WHERE due < strftime('%%s','now', 'utc') AND  queue = -1
ORDER BY prior")
  
  (org-roam-db-query "INSERT INTO postponed_cards
SELECT * FROM cards
WHERE due < strftime('%%s','now', 'utc') AND queue = 1
ORDER BY prior
LIMIT $s1" org-fc-roam-postpone-skip-following-number-of-cards)

  (org-roam-db-query "INSERT INTO postponed_cards
SELECT node_id, title, pos, prior, ease, box, new_ivl,
-- Due
CASE
WHEN new_ivl > ivl
THEN due + ((new_ivl - ivl) * 60 * 60 * 24)
ELSE due + (new_ivl * 60 * 60 * 24)
END,
-- Postpone count
postp + 1,
reps,
lapses,
type,
queue

FROM (SELECT node_id, title, pos, prior, ease, box,
CASE
WHEN type = 'topic' THEN min($s3, max($s2, ivl * $s1)) 
ELSE min($s6, max($s5, ivl * $s4))
END AS new_ivl, ivl, due, postp, reps, lapses, type, queue

FROM
(SELECT * FROM cards
WHERE due < strftime('%%s','now', 'utc') AND queue = 1
ORDER BY prior
LIMIT -1 OFFSET $s7))"
                     (cdr org-fc-roam-postpone-delay-factor)
                     (cdr org-fc-roam-postpone-minimum-interval)
                     (cdr org-fc-roam-postpone-maximum-interval)

                     (car org-fc-roam-postpone-delay-factor)
                     (car org-fc-roam-postpone-minimum-interval)
                     (car org-fc-roam-postpone-maximum-interval)
                     
                     org-fc-roam-postpone-skip-following-number-of-cards)
  
  (org-roam-db-query "DROP TABLE cards")
  (org-roam-db-query "ALTER TABLE postponed_cards RENAME TO cards"))

(defun org-fc-roam-maybe-postpone-then-sort ()
  (when org-fc-roam-auto-postpone (org-fc-roam-auto-postpone))
  (when org-fc-roam-auto-sort (org-fc-roam-auto-sort)))

(add-hook 'kill-emacs-hook 'org-fc-roam-maybe-postpone-then-sort)

(defun org-fc-roam-index (paths &optional filter)
  (cl-remove-if
   filter
   (progn
     (setq paths (--group-by (or (f-same? it org-roam-directory)
                                 (f-descendant-of? it org-roam-directory)) paths))
     (cond ((and (null (assq t paths)) (assq nil paths))
            (mapc (lambda (path)
                    (warn "%s is not managed by org-roam, will index it with org-fc-awk-index instead." path))
                  (cdr (assq nil paths)))
            (require 'org-fc-awk)
            (org-fc-awk-index (cdr (assq nil paths)) filter))

           ((and (assq t paths) (assq nil paths))
            (mapc (lambda (path)
                    (warn "%s is not managed by org-roam, will index it with org-fc-awk-index instead." path))
                  (cdr (assq nil paths)))
            (require 'org-fc-awk)
            (append (org-fc-awk-index (cdr (assq nil paths)))
                    (org-fc-roam-index (cdr (assq t paths)))))

           (t
            (org-roam-db-query  "SELECT * FROM
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
WHERE due <= strftime('%%s','now', 'utc') AND queue = 1
GROUP BY id, pos)
ORDER BY prior"))))))

;;; Sorting

(defun org-fc-roam-index--sort-others (index)
  "Sort INDEX by interleaving prioritized cards with randomized cards by `org-fc-shuffled-others-proportion'."
  (when index
    (let ((cards-count (length index))
          (ratio (org-fc-ratio-simplify-round (- 100 org-fc-shuffled-others-proportion) org-fc-shuffled-others-proportion))
          priority-index)
      (cond ((not org-fc-shuffle-positions) (org-fc-index-positions index))
            ((eq org-fc-shuffled-others-proportion 0) (org-fc-index-positions index))
            ((eq org-fc-shuffled-others-proportion 100) (org-fc-index-shuffled-positions index))
            (t (sort index org-fc-index-sort-predicate)
               (dotimes (i (round (* cards-count
                                     (/ (- 100 org-fc-shuffled-others-proportion)
                                        100.0))))
                 (push (pop (nthcdr (random (length index)) index)) priority-index))
               (setq priority-index (org-fc-index-positions priority-index))
               (sort priority-index (lambda (card1 card2)
                                      (< (or (plist-get card1 :prior) 0)
                                         (or (plist-get card2 :prior) 0))))
               (org-fc-interleave (seq-partition priority-index (cl-first ratio))
                                  (seq-partition (org-fc-index-shuffled-positions index)
                                                 (cl-second ratio))))))))

(defun org-fc-roam-index--sort-topic (index)
  "Sort INDEX by interleaving prioritized cards with randomized cards by `org-fc-shuffled-topic-proportion'."
  (when index
    (let ((cards-count (length index))
          (ratio (org-fc-ratio-simplify-round (- 100 org-fc-shuffled-topic-proportion) org-fc-shuffled-topic-proportion))
          priority-index)
      (cond ((not org-fc-shuffle-positions) (org-fc-index-positions index))
            ((eq org-fc-shuffled-topic-proportion 0) (org-fc-index-positions index))
            ((eq org-fc-shuffled-topic-proportion 100) (org-fc-index-shuffled-positions index))
            (t (sort index org-fc-index-sort-predicate)
               (dotimes (i (round (* cards-count
                                     (/ (- 100 org-fc-shuffled-topic-proportion)
                                        100.0))))
                 (push (pop (nthcdr (random (length index)) index)) priority-index))
               (setq priority-index (org-fc-index-positions priority-index))
               (sort priority-index (lambda (card1 card2)
                                      (< (or (plist-get card1 :prior) 0)
                                         (or (plist-get card2 :prior) 0))))
               (org-fc-interleave (seq-partition priority-index (cl-first ratio))
                                  (seq-partition (org-fc-index-shuffled-positions index)
                                                 (cl-second ratio))))))))

(defun org-fc-roam-index-sort-cards (index)
  "Sort INDEX by interleaving topic cards with others like `org-fc-index-sort-cards'

One difference between this and `org-fc-index-sort-cards' is that this function
further interleave prioritized cards with randomized cards for topic and other types."
  (let ((alist (seq-group-by (lambda (it) (eq (plist-get it :type) 'topic)) index))
        (ratio (org-fc-ratio-simplify-round (- 100 org-fc-topic-proportion) org-fc-topic-proportion))
        others topic)

    (setq topic (org-fc-roam-index--sort-topic (cdr (assq t alist))))
    (setq others (org-fc-roam-index--sort-others (cdr (assq nil alist))))

    (cond ((not org-fc-shuffle-positions) (org-fc-index-positions index))
          ((eq org-fc-topic-proportion 100) (append topic others))
          ((eq org-fc-topic-proportion 0) (append others topic))
          (t (org-fc-interleave (seq-partition others (cl-first ratio))
                                (seq-partition topic (cl-second ratio)))))))

;;;###autoload
(define-minor-mode org-fc-roam-mode
  "Minor mode for caching org-fc card data.

This mode sets up several hooks to ensure the case updated when files change,
are renamed or deleted."
  :lighter " org-fc roam"
  :group 'org-fc
  :require 'org-fc
  :global t
  (if org-fc-roam-mode
      (progn (customize-set-variable 'org-fc-index-function #'org-fc-roam-index)
             (customize-set-variable 'org-fc-index-filter-function #'identity
                                     "The index function does the filtering already so just return them as-is.")
             (customize-set-variable 'org-fc-index-sort-function #'org-fc-roam-index-sort-cards))
    (cl-labels ((original-value (sym)
                                (ignore-errors
                                  (eval (car (get sym 'standard-value))))))
      (customize-set-variable 'org-fc-index-function (original-value 'org-fc-index-function))
      (customize-set-variable 'org-fc-index-filter-function (original-value 'org-fc-index-filter-function))
      (customize-set-variable 'org-fc-index-sort-function (original-value 'org-fc-index-sort-function)))))

(provide 'org-fc-roam)
;;; org-fc-roam.el ends here
