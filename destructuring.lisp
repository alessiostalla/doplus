;;; Copyright (C) 2011 Alessio Stalla
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :doplus)

(defun extract-variables (lambda-list)
  (flet ((extract-variable (form)
           (cond ((symbolp form)
                  (unless (find form lambda-list-keywords)
                    form))
                 ((symbolp (car form)) ;;&optional/&key (var default)
                  (car form))
                 (t (cadar form))))) ;;&key with explicit keyword name ((key var) default)
    (let ((rest (cdr lambda-list)))
      (remove nil
              (list* (extract-variable (car lambda-list))
                     (if (symbolp rest)
                         (list (extract-variable rest))
                         (extract-variables rest)))))))

(defun make-destructuring-form (lambda-list expression)
  `(destructuring-bind ,lambda-list ,expression))