;;; Copyright (C) 2011-2012 Alessio Stalla
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

(asdf:defsystem :doplus
  :name "doplus"
  :description "DO+ (doplus) is a high-level, extensible iteration construct for Common Lisp with a reasonably simple implementation, which in particular does not use a code walker."
  :author "Alessio Stalla <alessiostalla@gmail.com>"
  :licence "GPLv3"
  :serial t
  :depends-on (:parse-declarations-1.0)
  :components
  ((:static-file "doplus.asd")
   (:file "packages")
   ;;Could be conditionalized to provide different destructuring implementations
   (:file "destructuring")
   #-(or abcl sbcl)
   (:file "sequence-iterator")
   (:file "doplus")))

(defmethod perform ((o asdf:test-op) (s (eql (asdf:find-system :doplus))))
  (asdf:load-system :doplus-tests)
  (eval (read-from-string "(doplus-tests:run-all-tests)"))
  t)