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

(defstruct (fset-iterator (:constructor %make-fset-iterator)
                          (:conc-name #.(string '#:%fset-iterator-)))
  function index element endp)

(defun make-fset-iterator (iterator-function)
  (multiple-value-bind (elt not-done)
      (funcall iterator-function :get)
    (%make-fset-iterator :function iterator-function :index 0
                         :element elt :endp (not not-done))))

(defun fset-iterator-step (seq iter from-end)
  (declare (ignore seq from-end))
  (multiple-value-bind (elt not-done)
      (funcall (%fset-iterator-function iter) :get)
    (setf (%fset-iterator-element iter) elt
          (%fset-iterator-endp iter) (not not-done))
    (incf (%fset-iterator-index iter)))
  iter)

(defun fset-iterator-endp (seq iter limit from-end)
  (declare (ignore seq limit from-end))
  (%fset-iterator-endp iter))

(defun fset-iterator-element (seq iter)
  (declare (ignore seq))
  (%fset-iterator-element iter))

(defun fset-iterator-index (seq iter)
  (declare (ignore seq))
  (%fset-iterator-index iter))

(defun (setf fset-iterator-element) (new-value seq iter)
  (declare (ignore new-value seq iter))
  (error "FSet collections are immutable"))

(defun fset-iterator-copy (seq iter)
  (declare (ignore seq iter))
  (error "FSet iterators cannot be copied"))

(defmethod make-sequence-iterator ((sequence fset:seq) &key from-end start end)
  (when (or from-end start end)
    (error "Not implemented"))
  (let ((iterator (make-fset-iterator (fset:iterator sequence))))
    (values iterator nil nil ;;limit from-end
              #'fset-iterator-step #'fset-iterator-endp
              #'fset-iterator-element #'(setf fset-iterator-element)
              #'fset-iterator-index #'fset-iterator-copy)))