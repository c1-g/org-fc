;;; org-fc-algo.el --- Algorithm management for org-fc  -*- lexical-binding: t; -*-

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

(defmacro org-fc-property (symbol standard doc &rest args)
  (let (defcustom-args property)
    (while args
      (let ((keyword (pop args)))
        (unless (symbolp keyword)
          (error "Junk in args %S" args))
        (unless args
          (error "Keyword %s is missing an argument" keyword))
        (let ((value (pop args)))
          (cl-case keyword
            (:property (setq property value))
            (t
             (push value defcustom-args)
             (push keyword defcustom-args))))))
    (unless property
      (error "Missing keyword :property"))
    (let ((property-symbol (intern (concat (symbol-name symbol) "-property"))))
      `(progn
         (defcustom
           ,symbol
           ,standard
           ,doc
           ,@defcustom-args)
         (defcustom
           ,property-symbol
           ,property
           ,(format "Headline property for `%s'" symbol)
           :type 'string
           :group ,(plist-get defcustom-args :group))
         (defun ,symbol ()
           ,(format "Getter for `%s'" symbol)
           (if-let ((value (or (org-entry-get (point) ,property-symbol t)
                               (cadar (org-collect-keywords (list ,property-symbol))))))
               ;; TODO: Switch on possible types
               (read value)
             ;; ,(case (plist-get defcustom-args :type)
             ;;    ('string 'value)
             ;;    ('float '(string-to-number value))
             ;;    ('list '(read value))
             ;;    (t (error "Unsupported property type %s"
             ;; (plist-get defcustom-args :type)

             ,symbol))))))

(org-fc-property org-fc-algorithm
                 org-fc-algorithm
                 "Spacing algorithm."
                 :type 'const
                 :group 'org-fc
                 :property org-fc-algorithm-property)

(defvar org-fc-algos '()
  "Alist for registering spacing algorithms.
Entries should be lists (NAME INIT-FN NEXT-FN PARAMS RATING).
See `org-fc-register-algo' to see what those lists are.

This variable should only be modified with `org-fc-register-algo'
since there are some sanity checks in that function.")

(defun org-fc-register-algo (name params rating init-fn next-fn &optional format-fn)
  "Register a new spacing algorithm

Argument `NAME': Name of the new algorithm.
Argument `PARAMS': A list of the name of the parameters that INIT-FN
will give out. Must have a \"due\" parameter.
Argument `RATING': A list of all possible ratings.
Argument `INIT-FN': A list of initial value for PARAMS, can also be a
function that returns the list.
Argument `NEXT-FN': Function that takes one of the RATING and PARAMS then give
out a new list of value for PARAMS.

Optional argument `FORMAT-FN': Function that takes two arguments, WHERE
and PARAMS, then format PARAMS to insert in
`org-fc-review-history-file' and in the review data drawer. the
WHERE argument is given as 'history when params will be inserted
in the history file and given as 'drawer when they will be
inserted in the drawer. If nil, the default is formatting every
parameter to a string."
  ;; Basic check, PARAMS & RATING can't be nil.
  (cond ((null params) (error "The parameters of the algorithm %s can not be nil." name))
        ((null rating) (error "The possible ratings of the algorithm %s can not be nil." name))
        ((not (member "due" params)) (error "No \"due\" column in %S" params)))

  (let* ((init-values (if (functionp init-fn)
                          (funcall init-fn)
                        init-fn))
         (next-values (if (functionp next-fn)
                          ;; This simulates a real scenario, user select one of the RATING,
                          ;; NEXT-FN is passed with that rating and the current parameters to
                          ;; return new parameters.
                          (apply next-fn (append
                                          (list (plist-get
                                                 (nth (random (length rating)) rating) :rate))
                                          init-values))
                        next-fn)))
    
    (cond ((not (= (length params) (length init-values)))
           (error "The number of initialized values (%d) is not equal to the number of possible parameters (%d)."
                  (length init-values) (length params)))
          
          ((not (= (length params) (length next-values)))
           (error "The number of new values of %S (%d) is not equal to the number of possible parameters (%d)."
                  next-fn (length next-values) (length params)))
          
          (t (push (list name params rating init-fn next-fn format-fn) org-fc-algos)))))

(defun org-fc-algo-params (algo)
  "Get initial review data for ALGO"
  (let* ((entry (alist-get algo org-fc-algos))
         (params (cl-first entry)))
    (cond ((null entry) (error "No such algorithm: %s" algo))
          (t (if (functionp params)
                 (funcall params)
               params)))))

(defun org-fc-algo-rating (algo)
  "Get initial review data for ALGO"
  (let* ((entry (alist-get algo org-fc-algos))
         (rating (cl-second entry)))
    (cond ((null entry) (error "No such algorithm: %s" algo))
          (t (if (functionp rating)
                 (funcall rating)
               rating)))))

(defun org-fc-algo-initial-params (algo)
  "Get initial review data for ALGO"
  (let* ((entry (alist-get algo org-fc-algos))
         (init-fn (cl-third entry)))
    (cond ((null entry) (error "No such algorithm: %s" algo))
          (t (if (functionp init-fn)
                 (funcall init-fn)
               init-fn)))))

(defun org-fc-algo-next-params (algo rating current-params)
  "Get initial review data for ALGO"
  (let* ((entry (alist-get algo org-fc-algos))
         (next-fn (cl-fourth entry)))
    (cond ((null entry) (error "No such algorithm: %s" algo))
          (t (if (functionp next-fn)
                 (apply next-fn (append (list rating) current-params))
               next-fn)))))

(defun org-fc-algo-format-params (algo where params)
  "Get initial review data for ALGO"
  (let* ((entry (alist-get algo org-fc-algos))
         (format-fn (cl-fifth entry)))
    (cond ((null entry) (error "No such algorithm: %s" algo))
          (t (if (functionp format-fn)
                 (apply format-fn (append (list where) params))
               (mapcar (lambda (elt)
                         (if (stringp elt)
                             elt
                           (format "%s" elt)))
                       params))))))


(provide 'org-fc-algo)
;;; org-fc-algo.el ends here
