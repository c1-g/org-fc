(require 'hydra)

(require 'org-fc-overlay)
(require 'org-fc-review)
(require 'org-fc-awk)
(require 'org-fc-dashboard)

;;; Configuration

(defgroup org-fc nil
  "Manage and review flashcards with emacs"
  :group 'external
  :group 'text)

;; TODO: a combination of (load-path) and (buffer-file-name) could be
;; used for this
(defcustom org-fc-source-path "~/src/org-fc/"
  "Location of the org-fc sources, used to generate absolute
  paths to the awk scripts"
  :type 'string
  :group 'org-fc)

(defcustom org-fc-review-history-file "~/org/fc_reviews.tsv"
  "File to store review results in."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-property "FC_TYPE"
  "Property used to store the cards type."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-created-property "FC_CREATED"
  "Property used to store the cards creation time."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-suspended-tag "suspended"
  "Tag for marking suspended cards"
  :type 'string
  :group 'org-fc)

(defcustom org-fc-flashcard-tag "fc"
  "Tag for marking headlines as flashcards"
  :type 'string
  :group 'org-fc)

(defcustom org-fc-prefix-key (kbd "C-f")
  "Prefix key for all flashcard key bindings"
  :type 'key-sequence
  :group 'org-fc)

(defcustom org-fc-directories '("~/org")
  "Directories to search for flashcards"
  :type 'string
  :group 'org-fc)

(defcustom org-fc-unsuspend-overdue-percentage 0.1
  "Cards overdue by this percentage of their interval keep their
  spacing parameters when they are unsuspended. Cards overdue by
  more than that are reset."
  :type 'float
  :group 'org-fc)

;; TODO: Allow customizing this, currently that's not possible because
;; the indexers / filters expect a ISO8601 format.
(defvar org-fc-timestamp-format "%FT%H:%M:%S"
  "Format to use for storing timestamps.
Defaults to ISO8601")

;; TODO: Allow customizing this once different indexers are supported
(defvar org-fc-indexer
  'awk
  "Indexer to use for finding cards / positions.
Only 'awk is supported at the moment.")

(defvar org-fc-demo-mode nil
  "If set to a non-nil value, a cards review data is not
  updated. Used by `org-fc-demo'")
(make-variable-buffer-local 'org-fc-demo-mode)

;;; Helper Functions

(defun org-fc-timestamp-now ()
  "ISO8601 timestamp of the current time in the UTC0 timezone"
  (format-time-string org-fc-timestamp-format nil "UTC0"))

(defun org-fc-days-overdue (ts)
  "Number of days between now and the ISO8601 timestamp TS."
  (/ (- (time-to-seconds)
        (time-to-seconds (date-to-time ts)))
     (* 24 60 60)))

(defun org-fc-show-latex ()
  "Show / re-display latex fragments."
  (org-clear-latex-preview)
  (org-latex-preview))

;; TODO: Rewrite using skip parameter
(defun org-fc-has-back-heading-p ()
  "Check if the entry at point has a 'Back' subheading.
Used to determine if a card uses the compact style."
  (let ((found nil))
    (org-map-entries
     (lambda ()
       (when (string= (fifth (org-heading-components)) "Back")
         (setq found t)))
     t 'tree)
    found))

;;; Checking for / going to flashcard headings

(defun org-fc-entry-p ()
  "Check if the current heading is a flashcard"
  (member org-fc-flashcard-tag (org-get-tags-at nil 'local)))

(defun org-fc-suspended-entry-p ()
  "Check if the current heading is a suspended flashcard"
  (let ((tags (org-get-tags-at nil 'local)))
    (and (member org-fc-flashcard-tag tags)
         (member org-fc-suspended-tag tags))))

(defun org-fc-part-of-entry-p ()
  "Check if the current heading belongs to a flashcard"
  (member org-fc-flashcard-tag (org-get-tags-at)))

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

(defun org-fc-add-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags-to (remove-duplicates
                    (cons tag (org-get-tags-at (point) t))
                    :test #'string=)))

(defun org-fc-remove-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags-to
   (remove tag (org-get-tags-at (point) t))))

;;; Registering Card Types

(defvar org-fc-types '()
  "Alist for registering card types.
Entries should be lists (name handler-fn update-fn).
Use `org-fc-register-type' for adding card types.")

(defun org-fc-register-type (name setup-fn flip-fn update-fn)
  "Register a new card type."
  (push
   (list name setup-fn flip-fn update-fn)
   org-fc-types))

(defun org-fc-type-setup-fn (type)
  "Get the review function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (first entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-flip-fn (type)
  "Get the flip function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (second entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-update-fn (type)
  "Get the update function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (third entry)
      (error "No such flashcard type: %s" type))))

;;; Card Initialization

(defun org-fc--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card types."
  (if (org-fc-part-of-entry-p)
      (error "Headline is already a flashcard"))
  (org-back-to-heading)
  (org-set-property
   org-fc-created-property
   (org-fc-timestamp-now))
  (org-set-property org-fc-type-property type)
  (org-id-get-create)
  (org-fc-add-tag org-fc-flashcard-tag))

;;; Default Card Types

(require 'org-fc-type-normal)
(require 'org-fc-type-text-input)
(require 'org-fc-type-double)
(require 'org-fc-type-cloze)

;;; Updating Cards

(defun org-fc-map-cards (fn)
  "Call FN for each flashcard headline in the current buffer.
FN is called with point at the headline and no arguments."
  (org-map-entries
   (lambda () (if (org-fc-entry-p) (funcall fn)))))

(defun org-fc-update ()
  "Re-process the current flashcard"
  (interactive)
  (unless (org-fc-part-of-entry-p)
      (error "Not part of a flashcard entry"))
  (save-excursion
    (org-fc-goto-entry-heading)
    (let ((type (org-entry-get (point) "FC_TYPE")))
      (funcall (org-fc-type-update-fn type)))))

(defun org-fc-update-all ()
  "Re-process all flashcards in the current buffer"
  (interactive)
  (org-fc-map-cards 'org-fc-update))

;;; Suspending / Unsuspending Cards

(defun org-fc-suspend-card ()
  "Suspend the headline at point if it is a flashcard."
  (interactive)
  (if (org-fc-entry-p)
      (progn
        (org-fc-goto-entry-heading)
        (org-fc-add-tag org-fc-suspended-tag))
    (message "Entry at point is not a flashcard")))

(defun org-fc-suspend-buffer ()
  "Suspend all cards in the current buffer"
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card))

(defun org-fc--unsuspend-card ()
  "If a position is overdue by more than
`org-fc-unsuspend-overdue-percentage' of its interval, reset it to box 0,
if not, keep the current parameters."
  (when (org-fc-suspended-entry-p)
    (org-fc-remove-tag org-fc-suspended-tag)
    (-as-> (org-fc-get-review-data) data
           (--map
            (let* ((pos (first it))
                   (interval (string-to-number (fourth it)))
                   (due (fifth it))
                   (days-overdue (org-fc-days-overdue due)))
              (print days-overdue)
              (if (< days-overdue (* org-fc-unsuspend-overdue-percentage interval))
                  it
                  (org-fc-review-data-default pos)))
            data)
           (org-fc-set-review-data data))))

(defun org-fc-unsuspend-card ()
  "Un-suspend the headline at point if it is a suspended
flashcard."
  (interactive)
  (if (org-fc-suspended-entry-p)
      (progn (org-fc-goto-entry-heading)
             (org-fc--unsuspend-card))
    (message "Entry at point is not a suspended flashcard")))

(defun org-fc-unsuspend-buffer ()
  "Un-suspend all cards in the current buffer"
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card))

;;; Indexing Cards

(defun org-fc-due-positions-for-paths (paths)
  (if (eq org-fc-indexer 'awk)
      (org-fc-awk-due-positions-for-paths paths)
    (error
     'org-fc-indexer-error
     (format "Indexer %s not implemented yet" org-fc-indexer-error))))

(defun org-fc-due-positions (context)
  "Return a shuffled list of elements (file id position) of due cards."
  (case context
    ('all (org-fc-due-positions-for-paths org-fc-directories))
    ('buffer (org-fc-due-positions-for-paths (list (buffer-file-name))))
    (t (error "Unknown review context %s" context))))

;;; Demo Mode

(defun org-fc-demo ()
  "Start a review of the demo file."
  (interactive)
  (let ((path (expand-file-name "demo.org" org-fc-source-path)))
    (with-current-buffer (find-file path)
      (setq-local org-fc-demo-mode t)
      (org-fc-review-buffer))))

;;; Exports

(provide 'org-fc)
