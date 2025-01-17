;;; org-fc-review.el --- Review mode for org-fc -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.0.1

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
;; During review, due cards are presented one after another and the
;; user is asked to rate each card.
;;
;; Cards are reviewed by
;; 1. opening the file they are in
;; 2. calling the setup function for the card type
;; 3. switch to review-flip-mode
;; 4. calling the flip function for the card type
;; 5. switch to review-rate-mode
;; 6. updating the review data based on the rating
;;
;;; Code:

(require 'eieio)

(require 'org-fc-core)

;;; Hooks

(defcustom org-fc-before-setup-hook '()
  "Functions run before a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-setup-hook '()
  "Functions run after a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-flip-hook '()
  "Functions run after a card is flipped during review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-before-review-hook '()
  "Functions run when a review session is started."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-review-hook '()
  "Functions run when a review session ends / is quit."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-review-hide-title-in-header-line nil
  "Whether or not to hide the file title in review header line.

Hide title for individual cards by adding the :notitle: tag."
  :type 'boolean
  :group 'org-fc)

;;; Variables

(defcustom org-fc-review-show-remaining-cards t
  "If non-nil, the number of the remaining cards will show on the header line."
  :type 'boolean
  :group 'org-fc)

(defvar org-fc-review--session nil
  "Current review session.")

(defvar org-fc-review--timestamp nil
  "Time the last card was flipped.
Used to calculate the time needed for reviewing a card.")

(defvar org-fc-reviewing-existing-buffer nil
  "Track if the current buffer was open before the review.")
(make-variable-buffer-local 'org-fc-reviewing-existing-buffer)

;;; Main Review Functions

;;;###autoload
(defun org-fc-review (context)
  "Start a review session for all cards in CONTEXT.
Called interactively, prompt for the context.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
  (interactive (list (org-fc-select-context)))
  (if org-fc-review--session
      (when (yes-or-no-p "Flashcards are already being reviewed. Resume? ")
        (org-fc-review-resume))
    (let* ((index (org-fc-index context))
           (cards (funcall (or (plist-get context :filterer) org-fc-index-filter-function) index)))
      (setq cards (funcall (or (plist-get context :sorter) org-fc-index-sort-function) cards))
      (if (null cards)
          (message "No cards due right now")
        (setq org-fc-review--session (org-fc-make-review-session cards))
        (run-hooks 'org-fc-before-review-hook)
        (org-fc-review-next-card)))))

(defun org-fc-review-resume ()
  "Resume review session, if it was paused."
  (interactive)
  (if org-fc-review--session
      (progn
        (org-fc-review-edit-mode -1)
        (org-fc-review-next-card 'resuming))
    (message "No session to resume to")))

;;;###autoload
(defun org-fc-review-buffer ()
  "Review due cards in the current buffer."
  (interactive)
  (org-fc-review org-fc-context-buffer))

;;;###autoload
(defun org-fc-review-all ()
  "Review all due cards."
  (interactive)
  (org-fc-review org-fc-context-all))

(defun org-fc-review-next-card (&optional resuming)
  "Review the next card of the current session.
If RESUMING is non-nil, some parts of the buffer setup are skipped."
  (if (not (null (oref org-fc-review--session cards)))
      (condition-case err
          (let* ((card (pop (oref org-fc-review--session cards)))
                 (path (plist-get card :path))
                 (id (plist-get card :id))
                 (type (plist-get card :type))
                 (position (plist-get card :position)))
            (setf (oref org-fc-review--session current-item) card)
            (let ((buffer (find-buffer-visiting path)))
              (with-current-buffer (find-file path)
                (unless resuming
                  ;; If buffer was already open, don't kill it after rating the card
                  (if buffer
                      (setq-local org-fc-reviewing-existing-buffer t)
                    (setq-local org-fc-reviewing-existing-buffer nil))
                  (org-fc-set-header-line))

                (goto-char (point-min))
                (org-fc-id-goto id path)

                (org-fc-indent)
                ;; Make sure the headline the card is in is expanded
                (org-reveal)
                (unless (org-before-first-heading-p)
                  (org-fc-narrow)
                  (org-fc-hide-drawers))
                (org-fc-hide-keyword-times)
                (org-fc-show-latex)
                (org-display-inline-images)
                (run-hooks 'org-fc-before-setup-hook)

                (setq org-fc-review--timestamp (time-to-seconds (current-time)))
                (let ((step (funcall (org-fc-type-setup-fn type) position)))
                  (run-hooks 'org-fc-after-setup-hook)

                  ;; If the card has a no-noop flip function,
                  ;; skip to rate-mode
                  (let ((flip-fn (org-fc-type-flip-fn type)))
                    (if (or (eq step 'rate)
                            (null flip-fn)
                            (eq flip-fn #'org-fc-noop))
                        (org-fc-review-rate-mode 1)
                      (org-fc-review-flip-mode 1)))))))
        (error
         (org-fc-review-quit)
         (signal (car err) (cdr err))))
    (message "Review Done")
    (org-fc-review-quit)))

(defmacro org-fc-review-with-current-item (var &rest body)
  "Evaluate BODY with the current card bound to VAR.
Before evaluating BODY, check if the heading at point has the
same ID as the current card in the session."
  (declare (indent 0) (debug t))
  `(if org-fc-review--session
       (if-let ((,var (oref org-fc-review--session current-item)))
           (if (string= (plist-get ,var :id) (org-id-get))
               (progn ,@body)
             (message "Flashcard ID mismatch"))
         (message "No flashcard review is in progress"))))

(defun org-fc-review-flip ()
  "Flip the current flashcard."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item card
        (let ((type (plist-get card :type)))
          (funcall (org-fc-type-flip-fn type))
          (run-hooks 'org-fc-after-flip-hook)
          (org-fc-review-rate-mode)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(defun org-fc-review-rate (rating)
  "Rate the card at point with RATING."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item card
        (let* ((path (plist-get card :path))
               (id (plist-get card :id))
               (position (plist-get card :position))
               (now (time-to-seconds (current-time)))
               (delta (- now org-fc-review--timestamp)))
          (org-fc-review-add-rating org-fc-review--session rating)
          (org-fc-review-update-data path id position rating delta)
          (org-fc-review-reset)

          (if (and (eq rating 'again) org-fc-append-failed-cards)
              (with-slots (cards) org-fc-review--session
                (setf cards (append cards (list card)))))

          (save-buffer)
          (if org-fc-reviewing-existing-buffer
              (org-fc-review-reset)
            (kill-buffer))
          (org-fc-review-next-card)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(defun org-fc-review-skip-card ()
  "Skip card and proceed to next."
  (interactive)
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-suspend-card ()
  "Suspend card and proceed to next."
  (interactive)
  (if (buffer-modified-p)
      (org-fc-suspend-card)
    (org-fc-suspend-card)
    (save-buffer))
  ;; Remove all other positions from review session
  (with-slots (current-item cards) org-fc-review--session
    (let ((id (plist-get current-item :id)))
      (setf cards
            (cl-remove-if
             (lambda (card)
               (string= id (plist-get card :id))) cards))))
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-update-data (path id position rating delta)
  "Update the review data of the card.
Also add a new entry in the review history file.  PATH, ID,
POSITION identify the position that was reviewed, RATING is a
review rating and DELTA the time in seconds between showing and
rating the card."
  (org-fc-with-point-at-entry
   ;; If the card is marked as a demo card, don't log its reviews and
   ;; don't update its review data
   (unless (member org-fc-demo-tag (org-fc--get-tags))
     (let* ((data (org-fc-review-data-get))
            (current (assoc position data #'string=))
            (algo (org-fc-algorithm))
            new-current)
       (unless current
         (error "No review data found for this position"))
       (setq new-current (mapcar (lambda (s)
                                   (if (numberp (read s))
                                       (read s)
                                     s))
                                 current))
       (org-fc-review-history-add
        (append
         (list
          (org-fc-timestamp-in 0)
          path
          id
          (symbol-name algo)
          (format "%.2f" delta)
          (symbol-name rating))
         (org-fc-algo-format-params algo 'history new-current)))
       (setq new-current (org-fc-algo-next-params algo rating new-current))
       (setq new-current (org-fc-algo-format-params algo 'drawer new-current))
       (setcar current (car new-current))
       (setcdr current (cdr new-current))
       (org-fc-review-data-set data)))))

(defun org-fc-review-reset ()
  "Reset the buffer to its state before the review."
  (org-fc-review-rate-mode -1)
  (org-fc-review-flip-mode -1)
  (org-fc-review-edit-mode -1)
  (org-fc-reset-header-line)
  (org-fc-remove-overlays)
  (widen))

;;;###autoload
(defun org-fc-review-quit ()
  "Quit the review, remove all overlays from the buffer."
  (interactive)
  (org-fc-review-reset)
  (run-hooks 'org-fc-after-review-hook)
  (org-fc-review-history-save)
  (setq org-fc-review--session nil))

;;;###autoload
(defun org-fc-review-edit ()
  "Edit current flashcard.
Pauses the review, unnarrows the buffer and activates
`org-fc-edit-mode'."
  (interactive)
  (widen)
  (org-fc-remove-overlays)
  ;; Queue the current flashcard so it's reviewed a second time
  (push
   (oref org-fc-review--session current-item)
   (oref org-fc-review--session cards))
  (setf (oref org-fc-review--session paused) t)
  (setf (oref org-fc-review--session current-item) nil)
  (org-fc-review-edit-mode 1))

;;; Managing Review Data

;; Based on `org-log-beginning'
(defun org-fc-review-data-position (&optional create)
  "Return (BEGINNING . END) points of the review data drawer.
When optional argument CREATE is non-nil, the function creates a
drawer, if necessary.  Returned position ignores narrowing.

BEGINNING is the start of the first line inside the drawer,
END is the start of the line with :END: on it."
  (org-with-wide-buffer
   (org-fc-end-of-meta-data)
   (let ((regexp (concat "^[ \t]*:" (regexp-quote org-fc-review-data-drawer) ":[ \t]*$"))
         (end (if (org-at-heading-p) (point)
                (save-excursion (outline-next-heading) (point))))
         (case-fold-search t))
     (catch 'exit
       ;; Try to find existing drawer.
       (while (re-search-forward regexp end t)
         (let ((element (org-element-at-point)))
           (when (eq (org-element-type element) 'drawer)
             (throw 'exit
                    (cons (org-element-property :contents-begin element)
                          (org-element-property :contents-end element))))))
       ;; No drawer found.  Create one, if permitted.
       (when create
         (unless (bolp) (insert "\n"))
         (let ((beg (point)))
           (insert ":" org-fc-review-data-drawer ":\n:END:\n")
           (org-indent-region beg (point)))
         (cons
          (line-beginning-position 0)
          (line-beginning-position 0)))))))

(defun org-fc-review-data-get ()
  "Get a cards review data as a Lisp object."
  (if-let ((position (org-fc-review-data-position)))
      (org-with-point-at (car position)
        (mapcar (lambda (datum)
                  (mapcar (lambda (string) (substring-no-properties string))
                          datum))
                (cddr (org-table-to-lisp))))))

(defun org-fc-review-data-set (data)
  "Set the cards review data to DATA."
  (save-excursion
    (let ((position (org-fc-review-data-position 'create))
          (params (org-fc-algo-params (org-fc-algorithm))))
      (kill-region (car position) (cdr position))
      (goto-char (car position))
      (insert "| " (mapconcat #'identity params "|") " |\n")
      (insert "|-" "\n")
      (dolist (datum data)
        (insert
         "| "
         (mapconcat (lambda (x) (format "%s" x)) datum " | ")
         " |\n"))
      (org-table-align))))

(defun org-fc-review-data-default (&rest params)
  "Override the initial function with PARAMS."
  (append params (nthcdr (length params)
                         (org-fc-algo-initial-params (org-fc-algorithm)))))

(defun org-fc-review-data-update (first-columns)
  "Update review data to FIRST-COLUMNS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in FIRST-COLUMNS are
removed."
  (let ((old-data (org-fc-review-data-get)))
    (org-fc-review-data-set
     (mapcar
      (lambda (column)
        (or (assoc column old-data #'string=)
            (org-fc-algo-format-params (org-fc-algorithm)
                                       'drawer
                                       (org-fc-review-data-default column))))
      first-columns))))

;;; Sessions

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (paused :initform nil :initarg :paused)
   (history :initform nil)
   (ratings :initform nil :initarg :ratings)
   (cards :initform nil :initarg :cards)))

(defun org-fc-make-review-session (cards)
  "Create a new review session with CARDS."
  (make-instance
   'org-fc-review-session
   :ratings
   (if-let ((stats (org-fc-awk-stats-reviews)))
       (plist-get stats :day)
     '(:total 0 :again 0 :hard 0 :good 0 :easy 0))
   :cards cards))

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to review history."
  (push
   elements
   (oref org-fc-review--session history)))

(defun org-fc-review-history-save ()
  "Save all history entries in the current session."
  (when (and org-fc-review--session (oref org-fc-review--session history))
    (append-to-file
     (concat
      (mapconcat
       (lambda (elements) (mapconcat #'identity elements "\t"))
       (reverse (oref org-fc-review--session history))
       "\n")
      "\n")
     nil
     org-fc-review-history-file)
    (setf (oref org-fc-review--session history) nil)))

;; Make sure the history is saved even if Emacs is killed
(add-hook 'kill-emacs-hook #'org-fc-review-history-save)

(defun org-fc-review-add-rating (session rating)
  "Store RATING in the review history of SESSION."
  (with-slots (ratings) session
    (cl-case rating
      (again (cl-incf (cl-getf ratings :again) 1))
      (hard (cl-incf (cl-getf ratings :hard) 1))
      (good (cl-incf (cl-getf ratings :good) 1))
      (easy (cl-incf (cl-getf ratings :easy) 1)))
    (cl-incf (cl-getf ratings :total 1))))

;;; Header Line

(defvar org-fc-original-header-line-format nil
  "`header-line-format' before it was set by org-fc.")

(defun org-fc-set-header-line ()
  "Set the header-line for review."
  (let* ((remaining (when org-fc-review-show-remaining-cards (1+ (length (oref org-fc-review--session cards)))))
         (current (oref org-fc-review--session current-item))
         (title
          (unless (or org-fc-review-hide-title-in-header-line
                      (member "notitle" (plist-get current :tags)))
            (plist-get current :filetitle))))
    (setq org-fc-original-header-line-format header-line-format)
    (setq-local
     header-line-format
     `((org-fc-review-flip-mode "Flip")
       (org-fc-review-rate-mode "Rate")
       (org-fc-review-edit-mode "Edit")
       ,(if org-fc-review-show-remaining-cards (format " (%d) " remaining) " ")
       ,title))))

(defun org-fc-reset-header-line ()
  "Reset the header-line to its original value."
  (setq-local header-line-format org-fc-original-header-line-format))

;;; Modes

(defvar org-fc-review-flip-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'org-fc-review-flip)
    (define-key map (kbd "q") 'org-fc-review-quit)
    (define-key map (kbd "p") 'org-fc-review-edit)
    (define-key map (kbd "s") 'org-fc-review-suspend-card)
    map)
  "Keymap for `org-fc-flip-mode'.")

(define-minor-mode org-fc-review-flip-mode
  "Minor mode for flipping flashcards.

\\{org-fc-review-flip-mode-map}"
  :init-value nil
  :lighter " fc-flip"
  :keymap org-fc-review-flip-mode-map
  :group 'org-fc
  (when org-fc-review-flip-mode
    ;; Make sure only one of the modes is active at a time
    (org-fc-review-rate-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (if (and (derived-mode-p 'org-mode) org-fc-review--session)
        (progn (require 'org-fc-rater)
               (org-fc-rater-kill-rater))
      (org-fc-review-flip-mode -1))))

;;;;; Rate Mode

(defvar org-fc-review-rate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'org-fc-review-suspend-card)
    (define-key map (kbd "p") 'org-fc-review-edit)
    (define-key map (kbd "q") 'org-fc-review-quit)
    map)
  "Keymap for `org-fc-rate-mode'.")

(define-minor-mode org-fc-review-rate-mode
  "Minor mode for rating flashcards.

\\{org-fc-review-rate-mode-map}"
  :init-value nil
  :lighter " fc-rate"
  :keymap org-fc-review-rate-mode-map
  :group 'org-fc
  (when org-fc-review-rate-mode
    ;; Make sure only one of the modes is active at a time
    (org-fc-review-flip-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (if (and (derived-mode-p 'org-mode) org-fc-review--session)
        (if (= 1 (length (org-fc-algo-rating (org-fc-algorithm))))
            (org-fc-review-rate (plist-get (car (org-fc-algo-rating (org-fc-algorithm))) :rate))
            (org-fc-rater-set-up))
      (org-fc-review-rate-mode -1))))

(defvar org-fc-review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'org-fc-review-resume)
    (define-key map (kbd "C-c C-k") 'org-fc-review-quit)
    map)
  "Keymap for `org-fc-edit-mode'.")

(define-minor-mode org-fc-review-edit-mode
  "Minor mode for editing flashcards.

\\{org-fc-review-edit-mode-map}"
  :init-value nil
  :lighter " fc-edit"
  :keymap org-fc-review-edit-mode-map
  :group 'org-fc
  (when org-fc-review-edit-mode
    (org-fc-review-flip-mode -1)
    (org-fc-review-rate-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (unless (and (derived-mode-p 'org-mode) org-fc-review--session)
      (org-fc-review-edit-mode -1))))

;;; Footer

(provide 'org-fc-review)

;;; org-fc-review.el ends here
