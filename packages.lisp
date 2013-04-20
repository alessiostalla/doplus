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

#+abcl
(require :extensible-sequences)

(defpackage :doplus
  (:use :cl)
  (:shadow #:declaration #:step)
  #+(or abcl sbcl)
  (:import-from :sequence #:make-sequence-iterator)
  (:export
   ;;User-level API
   #:do+ #:for #:generating #:being #:in #:in-list #:in-vector #:across #:list-tails
   #:hash-entries-of #:symbols-in #:from #:to
   #:finding #:with #:initially #:finally #:previous #:stop-when #:returning
   #:counting #:summing-to #:accumulating-to #:collecting-into
   #:summing-to-and-returning #:accumulating-to-and-returning #:collecting-into-and-returning
   #:maximizing #:minimizing #:optimizing
   #:options
   ;;Body-local macros
   #:collect #:sum #:skip #:terminate #:update #:try-update

   ;;Extensions API
   #:defclause

   ;;Low-level API
   #:make-binding #:make-termination #:make-result #:make-initialization #:make-finalization
   #:make-accumulation #:make-prologue #:make-epilogue #:make-step
   #:make-iteration #:make-simple-iteration
   #:*iteration-variable*))