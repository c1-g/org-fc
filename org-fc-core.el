;;; org-fc-core.el --- Core functions of org-fc -*- lexical-binding: t; -*-

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
;;; Code:

(require 'outline)

(require 'org-id)
(require 'org-indent)
(require 'org-element)

(require 'subr-x)

;;; Customization

(defgroup org-fc nil
  "Manage and review flashcards with Emacs."
  :group 'external
  :group 'text)

(defcustom org-fc-directories '("~/org/")
  "Directories to search for flashcards."
  :type 'string
  :group 'org-fc)

(defvar org-fc-source-path
  (file-name-directory
   (file-truename (or load-file-name (buffer-file-name))))
  "Location of the org-fc sources.
Used to generate absolute paths to the awk scripts.")

(defcustom org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" user-emacs-directory)
  "File to store review results in."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-shuffle-positions t
  "Shuffle positions before review."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-append-failed-cards t
  "Add failed cards to the end of the review session."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-index-function #'org-fc-awk-index
  "Function used to index cards in a list of paths."
  :type 'function
  :group 'org-fc)

(defcustom org-fc-index-filter-function #'org-fc-index-filter-due
  "Function used to filter cards."
  :type 'function
  :group 'org-fc)

(defcustom org-fc-index-sort-function #'org-fc-index-sort-cards
  "Function used to sort cards."
  :type 'function
  :group 'org-fc)

(defcustom org-fc-index-sort-predicate #'ignore
  "Predicate function used to sort cards."
  :type 'function
  :group 'org-fc)

;;;; Org Tags / Properties

(defcustom org-fc-type-property "FC_TYPE"
  "Property used to store the cards type."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-algorithm-property "FC_ALGO"
  "Property used to store the card algorithm."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-created-property "FC_CREATED"
  "Property used to store the cards creation time."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX"
  "Name of the property to use for storing the max hole index."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-suspended-tag "suspended"
  "Tag for marking suspended cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-pending-tag "pending"
  "Tag for marking pending cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-flashcard-tag "fc"
  "Tag for marking headlines as flashcards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-demo-tag "fc-demo"
  "Tag for marking headlines as demo flashcards.
When demo flashcards are reviewed, their review data is not
updated.  This is used for the `org-fc-demo' and for testing card
types."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-drawer-whitelist '()
  "Drawers that are not hidden during review."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-stats-review-min-box 0
  "Minimum box for reviews to include in the review stats."
  :type 'integer
  :group 'org-fc)

;;;; Spacing Parameters

(defcustom org-fc-algorithm 'sm2-v1
  "Algorithm for spacing reviews of cards."
  :type '(choice (const sm2-v1) (const sm2-v2))
  :group 'org-fc)

(defcustom org-fc-bury-siblings nil
  "If non-nil, show at most one position of a card per review.
Does not apply to cloze single and cloze enumeration cards."
  :type 'boolean
  :group 'org-fc)

;;;; Sorting Parameters
(defcustom org-fc-topic-proportion 20
  "Proportion of topic type cards in comparision to others.
For the default value of 20, if you have 100 cards due, the
proportion of topic vs other cards is 20:80 which is in the ratio
of 1:4 which means that for every 4 cards of other types there
will be 1 topic card that comes after for review.")

;;; Helper Functions

(defun org-fc-interleave (&rest lists)
  "Return a new list of the first item in each list, then the second etc."
  (when lists
    (let (result)
      (while (flatten-list lists)
        (let ((list lists) it)
          (while list
            (setq it (pop list))
            (if (not (car-safe (car it)))
                (setq result (cons (car it) result))
              (mapc (lambda (l)
                      (setq result (cons l result)))
                    (car it)))))
        (setq lists (mapcar 'cdr lists)))
      (delete nil (nreverse result)))))

(defun org-fc-ratio-simplify-round (n1 n2)
  (mapcar (lambda (it)
            (round (/ it (float (min n1 n2)))))
          (list n1 n2)))

(defun org-fc-member-p (path)
  "Check if PATH is member of one of the `org-fc-directories'."
  (setq path (expand-file-name path))
  (and (string= (file-name-extension path) "org")
       (cl-some
        (lambda (dir) (string-prefix-p (expand-file-name dir) path))
        org-fc-directories)))

(defun org-fc-noop ()
  "Noop-function.")

(defun org-fc-timestamp-in (interval)
  "Generate an `org-mode' timestamp INTERVAL days from now."
  (let ((seconds (* interval 60 60 24))
        (now (time-to-seconds)))
    (format-time-string
     "%FT%TZ"
     (seconds-to-time (+ now seconds))
     "UTC0")))

(defun org-fc-show-latex ()
  "Show latex fragments of heading at point."
  (org-latex-preview 4))

(defun org-fc-back-heading-position ()
  "Return point at the beginning of an entries 'Back' subheading.
Return nil if there is no such heading.
This is expected to be called on an card entry heading."
  (let ((found nil)
        (level (cl-first (org-heading-components))))
    (org-map-entries
     (lambda ()
       (when (let ((comps (org-heading-components)))
               (and
                (string= (cl-fifth comps) "Back")
                (= (cl-first comps) (1+ level))))
         (setq found (point))))
     t 'tree)
    found))

(defun org-fc-has-back-heading-p ()
  "Check if the entry at point has a 'Back' subheading.
Used to determine if a card uses the compact style."
  (not (null (org-fc-back-heading-position))))

(defun org-fc-sorted-random (n)
  "Generate a list of N sorted random numbers."
  (sort (cl-loop for i below n collect (cl-random 1.0)) #'>))

(defun org-fc-zip (as bs)
  "Zip two lists AS and BS."
  (cl-loop for a in as for b in bs collect (cons a b)))

;; File-scoped variant of `org-id-goto'
(defun org-fc-id-goto (id file)
  "Go to the heading with ID in FILE."
  (let ((position (org-id-find-id-in-file id file)))
    (if position
        (goto-char (cdr position))
      (error "ID %s not found in %s" id file))))

(defun org-fc-deemphasize (string)
  "Remove org emphasis markers from STRING.
Returns a pair (marker . body)."
  (if (or (string-match org-emph-re string)
          (string-match org-verbatim-re string))
      (cons (match-string 3 string) (match-string 4 string))
    (cons nil string)))

(defun org-fc-emphasize (string)
  "Apply org emphasis faces to STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (org-do-emphasis-faces (point-max))
    (buffer-string)))

(defun org-fc-indent ()
  "Run `org-indent' on the current headline.
Usually org-indent runs with a delay, so when reviewing a card in
a new file, the cards contents jump to the right (are indented)
during the review.  We can get around this by synchronously
indenting the current heading."
  (if org-indent-mode
      (let ((el (org-element-at-point)))
        (org-indent-add-properties
         (org-element-property :begin el)
         (org-element-property :end el)))))

(defmacro org-fc-with-point-at-entry (&rest body)
  "Execute BODY with point at the card heading.
If point is not inside a flashcard entry, an error is raised."
  (declare (debug (body)))
  `(save-excursion
     (unless (org-before-first-heading-p)
        (org-fc-goto-entry-heading))
     ,@body))

(defmacro org-fc-with-point-at-back-heading (&rest body)
  "Execute BODY with point at the card's back heading.
If point is not inside a flashcard entry, an error is raised."
  (declare (debug (body)))
  `(if-let ((pos (org-fc-back-heading-position)))
       (save-excursion
         (goto-char pos)
         ,@body)))

;;; Checking for / going to flashcard headings

(defun org-fc-entry-p ()
  "Check if the current heading is a flashcard."
  (member org-fc-flashcard-tag (org-fc--get-tags)))

(defun org-fc-suspended-entry-p ()
  "Check if the current heading is a suspended flashcard."
  (let ((tags (org-fc--get-tags)))
    (and (member org-fc-flashcard-tag tags)
         (member org-fc-suspended-tag tags))))

(defun org-fc-part-of-entry-p ()
  "Check if the current heading belongs to a flashcard."
  (member org-fc-flashcard-tag (org-fc--get-tags)))

(defun org-fc-up-heading-or-point-min ()
  "Fixed version of Org's `org-up-heading-or-point-min'."
  (ignore-errors (org-back-to-heading t))
  (let ((p (point)))
    (if (< 1 (funcall outline-level))
        (progn
          (org-up-heading-safe)
          (when (= (point) p)
            (goto-char (point-min))))
      (unless (bobp) (goto-char (point-min))))))

(defun org-fc-goto-entry-heading ()
  "Move up to the parent heading marked as a flashcard."
  (unless (org-fc-part-of-entry-p)
    (error "Not inside a flashcard entry"))
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (while (not (org-fc-entry-p))
    (unless (org-up-heading-safe)
      (error "Cannot find a parent heading that is marked as a flashcard"))))

;;; Adding / Removing Tags

(defun org-fc--get-tags ()
  "Get tags of heading at point or the file tags if there're no local tags."
  (if (org-before-first-heading-p)
      org-file-tags
    (org-get-tags nil 'local)))

(defun org-fc--add-tags (tags)
  "Add TAG to the heading at point."
  (org-with-wide-buffer
   (if (org-before-first-heading-p)
       (org-fc-set-keyword "FILETAGS" (org-make-tag-string
                                       (cl-remove-duplicates
                                        (append tags (org-fc--get-tags))
                                        :test #'string=)))
     (org-back-to-heading)
     (org-set-tags
      (cl-remove-duplicates
       (append tags (org-fc--get-tags))
       :test #'string=)))))

(defun org-fc--remove-tags (tags)
  "Add TAG to the heading at point."
  (org-with-wide-buffer
   (if (org-before-first-heading-p)
       (org-fc-set-keyword "FILETAGS" (org-make-tag-string
                                       (seq-difference (org-fc--get-tags) tags #'string=)))
     (org-back-to-heading)
     (org-set-tags
      (seq-difference (org-fc--get-tags) tags #'string=)))))

;;; Dealing with keywords
;; Thank you, org-roam.

(defun org-fc-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (org-roam-end-of-meta-data 'drawers)
        (if (save-excursion (end-of-line) (eobp))
            (progn
              (end-of-line)
              (insert "\n"))
          (beginning-of-line))
        (insert "#+" key ": " value "\n")))))

(defun org-fc-get-keyword (name &optional file bound)
  "Return keyword property NAME from an org FILE.
FILE defaults to current file.
Only scans up to BOUND bytes of the document."
  (unless bound
    (setq bound 1024))
  (if file
      (with-temp-buffer
        (insert-file-contents file nil 0 bound)
        (org-fc--get-keyword name))
    (org-fc--get-keyword name bound)))

(defun org-fc--get-keyword (name &optional bound)
  "Return keyword property NAME in current buffer.
If BOUND, scan up to BOUND bytes of the buffer."
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun org-fc-end-of-meta-data (&optional full)
  "Like `org-end-of-meta-data', but supports file-level metadata.

When FULL is non-nil but not t, skip planning information,
properties, clocking lines and logbook drawers.

When optional argument FULL is t, skip everything above, and also
skip keywords."
  (org-back-to-heading-or-point-min t)
  (when (org-at-heading-p) (forward-line))
  ;; Skip planning information.
  (when (looking-at-p org-planning-line-re) (forward-line))
  ;; Skip property drawer.
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line))
  ;; When FULL is not nil, skip more.
  (when (and full (not (org-at-heading-p)))
    (catch 'exit
      (let ((end (save-excursion (outline-next-heading) (point)))
            (re (concat "[ \t]*$" "\\|" org-clock-line-re)))
        (while (not (eobp))
          (cond ;; Skip clock lines.
           ((looking-at-p re) (forward-line))
           ;; Skip logbook drawer.
           ((looking-at-p org-logbook-drawer-re)
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                (forward-line)
              (throw 'exit t)))
           ((looking-at-p org-drawer-regexp)
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                (forward-line)
              (throw 'exit t)))
           ;; When FULL is t, skip keywords too.
           ((and (eq full t)
                 (looking-at-p org-keyword-regexp))
            (forward-line))
           (t (throw 'exit t))))))))

(defun org-fc-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (org-fc-end-of-meta-data 'drawers)
        (if (save-excursion (end-of-line) (eobp))
            (progn
              (end-of-line)
              (insert "\n"))
          (forward-line)
          (beginning-of-line))
        (insert "#+" key ": " value "\n")))))

(defun org-fc-erase-keyword (keyword)
  "Erase the line where the KEYWORD is, setting line from the top of the file."
  (let ((case-fold-search t))
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" keyword ":") nil t)
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (delete-char 1)))))

;;; Card Initialization

(defun org-fc--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card TYPEs."
  (if (org-fc-entry-p)
      (error "Headline is already a flashcard"))
  (org-back-to-heading-or-point-min t)
  (org-set-property
   org-fc-created-property
   (org-fc-timestamp-in 0))
  (org-set-property org-fc-type-property type)
  (org-id-get-create)
  (org-fc--add-tags (list org-fc-flashcard-tag)))

(defun org-fc-deinit-card ()
  "Deinitialize the current flashcard.

This is the opposite of `org-fc--init-card' that is,
remove `org-fc-created-property', remove `org-fc-type-property', remove `org-fc-review-data-drawer'
and remove any tags related to org-fc."
  (interactive)
  (if (not (org-fc-entry-p))
      (error "Headline is not a flashcard"))
  (org-back-to-heading-or-point-min)
  (org-delete-property org-fc-created-property)
  (org-delete-property org-fc-type-property)
  (org-fc--remove-tags (list org-fc-suspended-tag org-fc-flashcard-tag))
  (let* ((data-location (org-fc-review-data-position))
         (drawer-begin (org-with-point-at (car data-location) (forward-line -1) (point)))
         (drawer-end (org-with-point-at (cdr data-location) (forward-line 1) (point))))
    (delete-region drawer-begin drawer-end))
  (save-buffer))

;;; Card Types
;;;; Type Management

(defvar org-fc-types '()
  "Alist for registering card types.
Entries should be lists (name handler-fn update-fn).
Use `org-fc-register-type' for adding card types.")

(defun org-fc-register-type (name setup-fn flip-fn update-fn)
  "Register a new card type.
Argument NAME Name of the new type.
Argument SETUP-FN Function for initializing a new card of this type.
Argument FLIP-FN Function for flipping a card during review.
Argument UPDATE-FN Function to update a card when it's contents have changed."
  (push
   (list name setup-fn flip-fn update-fn)
   org-fc-types))

(defun org-fc-type-setup-fn (type)
  "Get the review function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-first entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-flip-fn (type)
  "Get the flip function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-second entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-update-fn (type)
  "Get the update function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-third entry)
      (error "No such flashcard type: %s" type))))

;;; Working with Overlays / Hiding Text
;;;; Showing / Hiding Overlays

(defun org-fc-remove-overlays ()
  "Remove all org-fc overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'org-fc))

;; Based on `outline-flag-region'
(defun org-fc-hide-region (from to &optional text face)
  "Hide region FROM ... TO, optionally replacing it with TEXT.
FACE can be used to set the text face of the overlay, e.g. to
make it bold."
  ;; (remove-overlays from to 'category 'org-fc)
  (let ((o (make-overlay from to nil 'front-advance)))
    (overlay-put o 'category 'org-fc)
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (if (stringp text)
        (progn
          (overlay-put o 'invisible nil)
          (overlay-put o 'display text))
      (overlay-put o 'invisible t))
    o))

(defun org-fc-make-overlay (begin end &rest props)
  "Create an overlay from BEGIN to END with PROPS."
  (let ((o (make-overlay begin end)))
    (overlay-put o 'category 'org-fc)
    (cl-loop for (prop value) on props by #'cddr do
             (overlay-put o prop value))
    o))

(defun org-fc-overlay-surround (o before after &optional face)
  "Surround O with strings BEFORE and AFTER with optional FACE."
  (overlay-put o 'before-string (propertize before 'face face))
  (overlay-put o 'after-string (propertize after 'face face))
  o)

;;;; Hiding Drawers

(defun org-fc-hide-keyword-times ()
  "Hide all timestamp keywords (e.g. DEADLINE) after point."
  (save-excursion
    (while (re-search-forward org-keyword-time-regexp nil t)
      (let ((start (1- (match-beginning 0)))
            (end (match-end 0)))
        (org-fc-hide-region start end)))))

(defun org-fc-hide-drawers ()
  "Hide all drawers except ones in `org-fc-drawer-whitelist' after point."
  (let ((bound (org-element-property :end (org-element-at-point))))
    (save-excursion
      (while (re-search-forward org-drawer-regexp bound t)
        (let ((start (1- (match-beginning 0)))
              (name (match-string 1))
              (end))
          (if (re-search-forward ":END:" bound t)
              (setq end (point))
            (error "No :END: found for drawer"))
          (if (member name org-fc-drawer-whitelist)
              (org-hide-drawer-toggle 'off t)
            (org-fc-hide-region start end)))))))

;;;; Hiding Headings / Section Contents

(defun org-fc-hide-heading (&optional text)
  "Hide the title of the headline at point.
If TEXT is non-nil, the heading is replaced with TEXT."
  ;; Case sensitive search
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (looking-at org-complex-heading-regexp)
          (org-fc-hide-region (match-beginning 4) (match-end 4) (or text "..."))
        (error "Point is not on a heading")))))

(defun org-fc-hide-content (&optional text)
  "Hide the main text of a heading *before* the first subheading.
If TEXT is non-nil, the content is replaced with TEXT."
  (let (start end)
    (save-excursion
      (org-back-to-heading)
      (forward-line)
      (setq start (point)))
    (save-excursion
      (outline-next-heading)
      (setq end (point)))
    (org-fc-hide-region start end text)))

;;;; Outline Trees

(defcustom org-fc-narrow-visibility 'ancestors
  "Visibility of the current heading during review.
See `org-show-set-visibility' for possible values"
  :group 'org-fc
  :type 'symbol
  :options '(ancestors lineage minimal local tree canonical))

(defun org-fc-narrow ()
  "Narrow the outline tree.
Only parent headings of the current heading remain visible."
  (interactive)
  (let* ((tags (org-fc--get-tags))
         (parent-fc-p))
    ;; Find the first heading with a :narrow: tag or the top level
    ;; ancestor of the current heading and narrow to its region
    (save-excursion
      (while (org-up-heading-safe)
        (setq parent-fc-p (or (org-fc-entry-p) parent-fc-p)))
      (org-narrow-to-subtree)
      (outline-hide-subtree))
    ;; Show only the ancestors of the current card if there is no flashcards in the ancestors
    (when (and parent-fc-p (not (org-before-first-heading-p)))
      (org-narrow-to-subtree))
    (org-show-set-visibility org-fc-narrow-visibility)
    (if (member "noheading" tags) (org-fc-hide-heading))))

;;; Updating Cards

(defun org-fc-map-cards (fn &optional scope)
  "Call FN for each flashcard headline in SCOPE.
FN is called with point at the headline and no arguments.
If SCOPE is nil, it defaults to the full buffer.
Other useful values are:
- tree
- region"
  (org-map-entries
   (lambda () (if (org-fc-entry-p) (funcall fn)))
   nil
   scope))

;;;###autoload
(defun org-fc-update ()
  "Re-process the current flashcard."
  (interactive)
  (org-fc-with-point-at-entry
   (let ((type (org-entry-get (point) "FC_TYPE")))
     (funcall (org-fc-type-update-fn type)))))

;;;###autoload
(defun org-fc-update-all ()
  "Re-process all flashcards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-update))

;;; Suspending / Unsuspending Cards

;;;###autoload
(defun org-fc-suspend-card ()
  "Suspend the headline at point if it is a flashcard."
  (interactive)
  (org-fc-with-point-at-entry
   (org-fc--add-tags (list org-fc-suspended-tag))))

;;;###autoload
(defun org-fc-suspend-tree ()
  "Suspend all cards in the subtree at point."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card 'tree))

;;;###autoload
(defun org-fc-suspend-buffer ()
  "Suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card))

;;;###autoload
(defun org-fc-suspend-region ()
  "Suspend all cards in the current region."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card 'region))

;;;###autoload
(defun org-fc-unsuspend-card ()
  "Unsuspend the headline at point.
Checks if the headline is a suspended card first."
  (interactive)
  (org-fc--remove-tags (list org-fc-suspended-tag)))

;;;###autoload
(defun org-fc-unsuspend-tree ()
  "Un-suspend all cards in the subtree at point."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card 'tree))

;;;###autoload
(defun org-fc-unsuspend-buffer ()
  "Un-suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card))

;;;###autoload
(defun org-fc-unsuspend-region ()
  "Un-suspend all cards in the current region."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card 'region))

;;; Indexing Cards
;;;; Card Filters

(defun org-fc--compile-filter (filter)
  "Compile FILTER into a lambda function.
Filters can be combinations of the following expressions:

- `(and ex1 ex2 ...)'
- `(or ex1 ex2 ...)'
- `(not ex)'
- `(tag \"tag\")'
- `(type card-type)' or `(type \"card-type\")'

For example, to match all double cards with tag \"math\",
use `(and (type double) (tag \"math\"))'."
  (let ((card-var (gensym)))
    (cl-labels
        ((check-arity-exact
          (filter n)
          (unless (= (length filter) (1+ n))
            (error
             (format "Filter '%s' expects %d argument(s)" filter n))))
         (compile-inner
          (filter)
          (cl-case (car filter)
            ('and `(and ,@(mapcar #'compile-inner (cdr filter))))
            ('or `(or ,@(mapcar #'compile-inner (cdr filter))))
            ('not
             (check-arity-exact filter 1)
             `(not ,(compile-inner (cadr filter))))
            ('tag
             (check-arity-exact filter 1)
             `(member ,(cadr filter) (plist-get ,card-var :tags)))
            ('type
             (check-arity-exact filter 1)
             `(eq ',(if (stringp (cadr filter))
                        (intern (cadr filter))
                      (cadr filter))
                  (plist-get ,card-var :type))))))
      `(lambda (,card-var)
         ,(compile-inner filter)))))

(defun org-fc-index (context)
  "Create an index for review CONTEXT."
  (let ((paths (plist-get context :paths))
        (filter (plist-get context :filter)) ;; Handle path formats / symbols
        (non-recursive (plist-get context :non-recursive)))
    (cond
     ((or (null paths) (eq paths 'all)) (setq paths org-fc-directories))
     ((eq paths 'buffer) (setq paths (list (buffer-file-name))))
     ((stringp paths) (setq paths (list paths))))

    (if filter (setq filter (org-fc--compile-filter filter)))

    (funcall (or (plist-get context :indexer) org-fc-index-function) paths filter non-recursive)))

(defun org-fc-index-flatten-card (card)
  "Flatten CARD into a list of positions.
Relevant data from the card is included in each position
element."
  (mapcar
   (lambda (pos)
     (list
      :filetitle (plist-get card :filetitle)
      :tags (plist-get card :tags)
      :path (plist-get card :path)
      :id (plist-get card :id)
      :type (plist-get card :type)
      :due (plist-get pos :due)
      :position (plist-get pos :position)))
   (plist-get card :positions)))

(defun org-fc-index-filter-due (index)
  "Filter INDEX to include only unsuspended due positions.
Cards with no positions are removed from the index."
  (let (res (now (current-time)))
    (dolist (card index)
      (unless (plist-get card :suspended)
        (let ((due
               (cl-remove-if-not
                (lambda (pos)
                  (time-less-p (plist-get pos :due) now))
                (plist-get card :positions))))
          (unless (null due)
            (plist-put
             card :positions
             (if (or (not org-fc-bury-siblings)
                     (member (plist-get card :cloze-type) '(single enumeration)))
                 due (list (car due))))
            (push card res)))))
    res))

(defun org-fc-index-positions (index)
  "Return all positions in INDEX."
  (mapcan (lambda (card) (org-fc-index-flatten-card card)) index))

(defun org-fc-index-shuffled-positions (index)
  "Return all positions in INDEX in random order.
Positions are shuffled in a way that preserves the order of the
  positions for each card."
  ;; 1. assign each position a random number
  ;; 2. flatten the list
  ;; 3. sort by the random number
  ;; 4. remove the random numbers from the result
  (let ((positions
         (mapcan
          (lambda (card)
            (let ((pos (org-fc-index-flatten-card card)))
              (org-fc-zip
               (org-fc-sorted-random (length pos))
               pos)))
          index)))
    (mapcar
     #'cdr
     (sort positions (lambda (a b) (> (car a) (car b)))))))

;;;; Cards sorter

(defun org-fc-index-sort-cards (index)
  "Sort INDEX by interleaving topic cards with others by `org-fc-topic-proportion'

These are the conditions:
`org-fc-topic-proportion' is 100, topic cards will always come first.
`org-fc-topic-proportion' is 0, cards of other types will always come first.
`org-fc-shuffle-positions' is nil, index the cards as-is i.e. the order they appear in files.
Else, shuffle topic cards and other cards then interleave them by the
ratio from `org-fc-topic-proportion'."
  (let ((alist (seq-group-by (lambda (it) (eq (plist-get it :type) 'topic)) index))
        (ratio (org-fc-ratio-simplify-round (- 100 org-fc-topic-proportion) org-fc-topic-proportion))
        others topic)

    (setq topic (org-fc-index-shuffled-positions (cdr (assq t alist))))
    (setq others (org-fc-index-shuffled-positions (cdr (assq nil alist))))

    (cond ((not org-fc-shuffle-positions) (org-fc-index-positions index))
          ((eq org-fc-topic-proportion 100) (append topic others))
          ((eq org-fc-topic-proportion 0) (append others topic))
          (t (org-fc-interleave (seq-partition others (cl-first ratio))
                                (seq-partition topic (cl-second ratio)))))))

;;; Demo Mode

;;;###autoload
(defun org-fc-demo ()
  "Start a review of the demo file."
  (interactive)
  (let ((path (expand-file-name "demo.org" org-fc-source-path)))
    (with-current-buffer (find-file path)
      (org-fc-review-buffer))))

;;; Contexts

(defvar org-fc-custom-contexts '()
  "User-defined review contexts.")

(defvar org-fc-context-all '(:paths all)
  "Default context for all cards.")
(defvar org-fc-context-buffer '(:paths buffer)
  "Default context for the current buffer.")

(defun org-fc-contexts ()
  "List of all contexts."
  (cl-list*
   (cons 'all org-fc-context-all)
   (cons 'buffer org-fc-context-buffer)
   org-fc-custom-contexts))

(defun org-fc-select-context ()
  "Select a review context."
  (let ((context (completing-read
                  "Context: "
                  (mapcar (lambda (c) (car c)) (org-fc-contexts))
                  nil
                  :require-match)))
    (unless (string= context "")
      (alist-get (intern context) (org-fc-contexts)))))

;;; Footer

(provide 'org-fc-core)

;;; org-fc-core.el ends here
