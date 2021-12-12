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
           (if-let ((value (org-entry-get (point) ,property-symbol t)))
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

(provide 'org-fc-algo)
;;; org-fc-algo.el ends here
