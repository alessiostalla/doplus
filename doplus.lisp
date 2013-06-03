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

(in-package :doplus)

(defstruct generator name clauses)
(defstruct termination condition)
(defstruct result form)
(defstruct (binding (:constructor %make-binding)) var default default-provided?)
(defstruct declaration form)
(defstruct initialization form)
(defstruct step form)
(defstruct finalization form)
(defstruct accumulator var function)
(defstruct prologue form)
(defstruct epilogue form)
(defstruct wrapper function)
(defstruct options map)

(defun make-binding (var &key (default nil default-provided?))
  (%make-binding :var var :default default :default-provided? default-provided?))

(defvar *iteration-variable*)

(defun make-iteration (&key bindings initializations steps preconditions postconditions)
  "General iteration clause generator."
  `(,@(mapcar (lambda (binding) `(with ,binding)) bindings)
    ,@(mapcar (lambda (precondition)
                (make-initialization :form `(unless ,precondition (terminate))))
              preconditions)
    ,@(mapcar (lambda (initialization)
                (make-initialization :form initialization))
              initializations)
    ,@(mapcar (lambda (postcondition)
                (make-initialization :form `(unless ,postcondition (terminate))))
              postconditions)
    ,@(mapcar (lambda (precondition)
                (make-step :form `(unless ,precondition (terminate))))
              preconditions)
    ,@(mapcar (lambda (step)
                (make-step :form step))
              steps)
    ,@(mapcar (lambda (postcondition)
                (make-step :form `(unless ,postcondition (terminate))))
              postconditions)))

(defun make-simple-iteration (&key (var *iteration-variable*) init (step nil step-p)
                              precondition postcondition)
  "Specialized iteration clause generator, where the iteration clause refers to a single variable, possibly with an initial value, step form, precondition, or postcondition."
  (unless var
    (error "var is required"))
  (make-iteration :bindings (list var) :initializations (when init `((setf ,var ,init)))
                  :steps (when step-p `((setf ,var ,step)))
                  :preconditions (when precondition (list precondition))
                  :postconditions (when postcondition (list postcondition))))

(defvar *clauses* nil "A list of known symbols that name clause macros, useful for introspection and documentation purposes.")

(defmacro defclause (name arglist &body body)
  "Same as cl:defmacro, but additionally records <name> in *clauses* as a known clause macro."
  `(progn
     (defmacro ,name ,arglist ,@body)
     (pushnew ',name *clauses*)))

(defclause declaring (thing)
  "Causes the emission of a (declare <thing>) form in the declaration section of the loop."
  (make-declaration :form thing))

(defclause for (var-or-vars iteration &environment env)
  "General iteration clause. Its actual behaviour is controlled by ITERATION, a macro form that FOR expands with *ITERATION-VARIABLE* bound to VAR. Example: in (for x (in '(1 2 3))), IN is a macro that expands into clauses that make use of *ITERATION-VARIABLE*, which is bound to the symbol X in the example. For certain iteration macros, VAR can be a lambda-list as well, in which case destructuring is applied. Example: (for (x &optional y) (in-list '((1 2) (3))))."
  (let ((*iteration-variable* var-or-vars))
    (macroexpand iteration env)))

(defclause generating (var iteration)
  "Lazy version of FOR. The user must call UPDATE or TRY-UPDATE in the body of the DO+ form in order to compute new values for the variable(s). Initialization, instead, is *always* performed eagerly."
  (make-generator :name (if (symbolp var) var (extract-variables var))
                  :clauses `(for ,var ,iteration)))

(defclause finding (var condition)
  "Same as FOR, but perhaps nicer to read when used in conjunction with macros like MAXIMIZING."
  `(for ,var ,condition))

(defclause being (form &key (then form))
  "Assigns to the iteration variable a value computed by evaluating FORM on each iteration, including the first. Optionally, the variable can be updated evaluating a differen form (the value of the `then' parameter). Examples: (for x (being (random))), (for y (being 'quiet :then (if (> x 0.5) 'angry 'quiet)))."
  (make-simple-iteration :init form :step then))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun ensure-list (obj)
    "(if (listp obj) obj (list obj))"
    (if (listp obj) obj (list obj)))
  (defun expand-with-simple-destructuring (form env)
    "Expand `form' in `env' as an iteration form that assigns to `*iteration-variable*' destructuring as by `destructuring-bind'."
    (let* ((lambda-list *iteration-variable*)
           (variables (extract-variables lambda-list))
           (*iteration-variable* (gensym "VAR"))
           (assignment-form `(with-atomic-updates* ,variables
                               ,(make-destructuring-form lambda-list *iteration-variable*))))
      (list*
       (macroexpand form env)
       (make-initialization :form assignment-form)
       (make-step :form assignment-form)
       (mapcar (lambda (var) (make-binding var)) variables)))))

(defclause in (&whole form seq &rest args &key &allow-other-keys &environment env)
  "Iterates over a sequence. IN must be used in combination with FOR, GENERATING and similar macros (those that bind *ITERATION-VARIABLE*). In implementations with extensible sequences (currently ABCL and SBCL), native sequence iterators are used, and all sequence types are supported, not just lists and vectors. In other implementations, an iterator specialized for lists or vectors is used depending on the type of sequence. All `args` are passed down to make-sequence-iterator (see the extensible sequences API paper for details [Rhodes2007]). IN can perform destructuring."
  (if (symbolp *iteration-variable*)
      (let ((state (gensym "STATE")) (limit (gensym "LIMIT"))
            (from-end (gensym "FROM-END")) (step (gensym "STEP"))
            (endp (gensym "ENDP")) (elt (gensym "ELT"))
            (sequence (gensym "SEQUENCE")))
        `((with (,sequence ,seq) ,state ,limit ,from-end ,step ,endp ,elt)
          (initially (multiple-value-setq (,state ,limit ,from-end ,step ,endp ,elt)
                       (make-sequence-iterator ,sequence ,@args)))
          ,(make-step :form `(setf ,state (funcall ,step ,sequence ,state ,from-end)))
          ,(make-simple-iteration
            :precondition `(not (funcall ,endp ,sequence ,state ,limit ,from-end))
            :init `(funcall ,elt ,sequence ,state)
            :step `(funcall ,elt ,sequence ,state))))
      (expand-with-simple-destructuring form env)))

(defclause in-list
    (&whole form list &key (by '(function cdr)) (rest (gensym "REST")) &environment env)
  "Like IN, but specialized for lists. Successive lists are obtained by calling the function BY (which by default is #'CDR) on the previous list. REST, if specified, is the variable holding the remaining elements to be processed; REST initially is bound to the entire list, then to successive lists obtained by funcalling BY."
  (if (symbolp *iteration-variable*)
      (list
       (make-simple-iteration :var rest :init list :step `(funcall ,by ,rest))
       (make-simple-iteration :init `(car ,rest) :step `(car ,rest)
                              :precondition `(not (null ,rest))))
      (expand-with-simple-destructuring form env)))

(defclause list-tails
    (&whole form list &key (by '(function cdr)) &environment env)
  "Loops over the successive tails of a list, checking for the end of the list as if by ATOM. Can perform destructuring."
  (if (symbolp *iteration-variable*)
      (let ((g (gensym)))
        (list
         (make-simple-iteration :var *iteration-variable* :init list :step `(funcall ,by ,*iteration-variable*))
         (make-simple-iteration :var g :init nil :step nil
                                :precondition `(not (atom ,*iteration-variable*)))))
      (expand-with-simple-destructuring form env)))

(defclause in-vector (vector &key (index (gensym "INDEX")) (start 0) end (by +1))
  "Loops across a vector. INDEX is bound to the index of the current element in the vector. The vector is traversed starting from index START (0 by default) to index END (the end of the vector if not specified); the index is incremented by BY (1 by default) on each iteration."
  ;;From a contribution by Tamas Papp
  (let ((tmp-var (gensym "VECTOR")))
    `((with (,tmp-var ,vector) (,index 0))
      (declaring (type (integer 0 ,(1- array-total-size-limit)) ,index))
      (for ,index (from ,start :to ,(or end `(1- (length ,tmp-var))) :by ,by))
      (for ,*iteration-variable* (being (aref ,tmp-var ,index))))))

(defclause across (vector &rest args &key index start end by)
  "Synonym for in-vector."
  (declare (ignore index start end by))
  `(in-vector ,vector ,@args))

(defclause hash-entries-of (hash-table)
  "Iterates over the entries of a hash table. The iteration variables must be specified as (key value), for example: (for (lemma definitions) (hash-entries-of the-english-vocabulary))."
  (unless (and (listp *iteration-variable*) (= 2 (length *iteration-variable*)))
    (error "Invalid variable specification for hash-entry, expected (key value) but got ~S" *iteration-variable*))
  (let ((iterator (gensym "HASH-TABLE-ITERATOR")) (test (gensym "TEST"))
        (key (car *iteration-variable*)) (value (cadr *iteration-variable*)))
    `(,(make-wrapper :function (lambda (body)
                                 `(with-hash-table-iterator (,iterator ,hash-table) ,body)))
      ,(make-iteration
        :bindings (list test key value)
        :initializations `((setf (values ,test ,key ,value) (,iterator)))
        :steps `((setf (values ,test ,key ,value) (,iterator)))
        :postconditions `((not (null ,test)))))))

(defclause symbols-in (package-or-packages &rest symbol-types)
  "Iterates over the symbols in one or more packages. Symbol-types follows the specification of cl:with-package-iterator."
  (let ((variables (ensure-list *iteration-variable*))
        (package-list (ensure-list package-or-packages))
        (symbol-types (or symbol-types '(:external)))
        (iterator (gensym "PACKAGE-ITERATOR")) (test (gensym "TEST")))
    (unless (<= 1 (length variables) 3)
      (error "Invalid variable specification for package entry, expected (symbol &optional accessibility package) but got ~S" *iteration-variable*))
    `(,(make-wrapper
        :function
        (lambda (body)
          `(with-package-iterator (,iterator (list ,@package-list) ,@symbol-types)
             ,body)))
      ,(make-iteration
        :bindings `(,test ,@variables)
        :initializations `((setf (values ,test ,@variables) (,iterator)))
        :steps `((setf (values ,test ,@variables) (,iterator)))
        :postconditions `((not (null ,test)))))))

(defclause from (initial-value &key to by while (using ''+))
  "Iterates from a given initial value to an optional maximum. Every iteration increments/decrements the value applying the <using> function (by default +) to the previous value and <by> (by default 1 or -1, depending on whether to >= from or not). Similarly, the <while> function is used to determine if <to> has been reached or surpassed. <while> is a function of two arguments, the current value and the value of <to>, and as soon as it returns false, the loop is terminated. <while> defaults to a function that returns true only if <to> has been specified and, letting cur be the current value, (<= (min to from) cur (max to from))."
  (let* ((diff (gensym "FROM-DIFF")) (to-evaled (gensym "FROM-TO"))
         (initial-evaled (gensym "FROM-FROM"))
         (while (or while
                    (when to
                      `(lambda (cur to &aux (from ,initial-evaled))
                         (let ((max (max to from)) (min (min to from)))
                           (<= min cur max)))))))
    (list
     (make-simple-iteration :var to-evaled :init to)
     (make-simple-iteration :var initial-evaled :init initial-value)
     (make-simple-iteration
      :var diff
      :init (if by by
                `(if ,to-evaled
                     (let ((signum (signum (- ,to-evaled ,initial-evaled))))
                       (if (zerop signum) +1 signum))
                     +1)))
     (make-initialization
      :form `(when (and ,to-evaled (not (zerop (- ,to-evaled ,initial-evaled)))
                        (not (= (signum (- ,to-evaled ,initial-evaled))
                                (signum ,diff))))
               (terminate)))
     (make-simple-iteration
      :init initial-evaled
      :step `(funcall ,using ,*iteration-variable* ,diff)
      :postcondition (when while
                       `(funcall ,while ,*iteration-variable* ,to-evaled))))))

(defclause to (final-value &key (from 0) by while using)
  "Variation over from."
  `(from ,from :to ,final-value :by ,by
         ,@(when while `(:while ,while))
         ,@(when using `(:using ,using))))

(defclause previous (expr)
  "On every iteration, the for-variable is set to the value <expr> had at the end of the previous iteration (initially NIL)."
  (let ((tmp (gensym (format nil "~A-~A" '#:previous expr))))
    (list
     (make-simple-iteration :step tmp)
     (make-simple-iteration :var tmp)
     (make-epilogue :form `(setf ,tmp ,expr)))))

(defclause with (&rest bindings)
  "Establishes bindings which are in effect for the whole loop."
  (mapcar (lambda (binding)
            (if (listp binding)
                (make-binding (car binding) :default (cadr binding))
                (make-binding binding)))
          bindings))

(defclause until (condition)
  "Specifies a termination clause for the loop. When, at the beginning of an iteration, the clause evaluates to true, the loop stops."
  (make-termination :condition condition))

(defclause while (condition)
  "Same as (until (not <condition>))."
  (make-termination :condition `(not ,condition)))

(defclause stop-when (condition)
  "Synonym for until."
  `(until ,condition))

(defclause returning (&rest forms)
  "Returns each form in <forms> as the value of the do+ form when the loop ends. If multiple forms are specified, by one or more RETURNING clauses, multiple values are returned, in the order the corresponding forms appear lexically. If one of the <forms> is (cl:values ...), then each value will be returned as if the values were spelled as direct arguments of the RETURNING form."
  (let (result-clauses)
    (dolist (form forms)
      (if (and (listp form) (eq 'values (car form)))
          (dolist (form (cdr form))
            (push (make-result :form form) result-clauses))
          (push (make-result :form form) result-clauses)))
    (nreverse result-clauses)))

(defclause initially (&rest forms)
  "Evaluates <form> before the first iteration."
  (mapcar (lambda (form) (make-initialization :form form)) forms))

(defclause finally (&rest forms)
  "Evaluates <form> after the last iteration, before returning a value."
  (mapcar (lambda (form) (make-finalization :form form)) forms))

(defclause accumulating-to (vars &key initially by finally)
  "Specifies an accumulation clause. Refer to the manual for details."
  (labels ((make-finally-form (var function)
             `(setf ,var (funcall ,function ,var)))
           (make-one-accumulation (var)
             (cons (make-simple-iteration :var var :init initially)
                   (if by
                       (list*
                        (make-accumulator :var var :function by)
                        (when finally
                          (list (make-finalization :form (make-finally-form var finally)))))
                       (list
                        (make-accumulator :var var :function '(function cons))
                        (make-finalization
                         :form (make-finally-form var (or finally '(function nreverse)))))))))
    (mapcan #'make-one-accumulation (ensure-list vars))))

(defclause collecting-into (vars &rest args &key initially by finally)
  "Synonym for accumulating-to."
  (declare (ignore initially by finally))
  `(accumulating-to ,vars ,@args))

;;TODO maybe retain keys but different defaults?
(defclause summing-to (vars &optional result-processor)
  "Specifies an accumulation strategy with 0 as the default value and + as the accumulator function."
  `(accumulating-to ,vars :initially 0 :by #'+ :finally ,result-processor))

(defclause collecting-into-and-returning (vars &rest args &key initially by finally)
  "Same as collecting-into, but also returns the value of each <vars> at the end of the loop."
  (declare (ignore initially by finally))
  `((collecting-into ,vars ,@args)
    ,@(mapcar (lambda (var) `(returning ,var)) (ensure-list vars))))

(defclause accumulating-to-and-returning (vars &rest args &key initially by finally)
  "Same as accumulating-into, but also returns the value of each <vars> at the end of the loop."
  (declare (ignore initially by finally))
  `((accumulating-to ,vars ,@args)
    ,@(mapcar (lambda (var) `(returning ,var)) (ensure-list vars))))

(defclause summing-to-and-returning (vars &optional result-processor)
  "Same as summing-to, but also returns the value of each <vars> at the end of the loop."
  `((summing-to ,vars ,result-processor)
    ,@(mapcar (lambda (var) `(returning ,var)) (ensure-list vars))))

(defclause counting (var)
  "Counts the number of iterations, starting from 0."
  (make-simple-iteration :var var :init 0 :step `(1+ ,var)))

(defclause maximizing (form &key (test ''>) saving in)
  "Synonym for optimizing."
  `(optimizing ,form :test ,test :saving ,saving :in ,in))

(defclause minimizing (form &key (test ''<) saving in)
  "Same as optimizing but with < as the default test."
  `(optimizing ,form :test ,test :saving ,saving :in ,in))

(defclause optimizing (form &key (test ''>) saving in)
  "Finds the optimum value of an expression across the iteration. Refer to the manual for details."
  (let* ((candidate (gensym "CANDIDATE-MAX"))
         (test-form `(funcall ,test ,candidate ,*iteration-variable*))
         (saving (ensure-list saving))
         (in (ensure-list in)))
    (when (not (= (length saving) (length in)))
      (error ":saving and :in have different lengths!"))
    (list*
     (make-simple-iteration :init form)
     (make-epilogue :form `(let ((,candidate ,form))
                             (when ,test-form
                               (setf ,*iteration-variable* ,candidate
                                     ,@(loop
                                          :for x :in saving :for y :in in
                                          :collect y :collect x)))))
     (loop
        :for x :in saving :for y :in in
        :collect (make-simple-iteration :var y :init x)))))

(defclause options (&rest args &key name atomic-updates)
  (declare (ignore name atomic-updates))
  (make-options :map args))

(defmacro with-atomic-updates* ((&rest variables) binding-form &body body)
  (let ((temps (mapcar (lambda (var) (list (gensym (symbol-name var)) var)) variables)))
    `(let ,temps
       (,@binding-form
        ,@body
        ,@(mapcar (lambda (mapping) `(setf ,@mapping)) temps))
       ,@(mapcar (lambda (mapping) `(setf ,(cadr mapping) ,(car mapping))) temps))))

(defmacro with-atomic-updates ((&rest variables) &body body)
  ;;Self-binding is necessary to avoid losing the previous value
  `(with-atomic-updates* ,variables (let ,(mapcar (lambda (var) (list var var)) variables)) ,@body))

(defmacro without-atomic-updates ((&rest variables) &body body)
  (declare (ignore variables))
  `(progn ,@body))

(defstruct doplus-env
  loop-name skip-label terminate-label default-collect-var
  accumulators generators parent)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-loop-in-env (name env)
    (when env
      (if (eq name (doplus-env-loop-name env))
          env
          (find-loop-in-env name (doplus-env-parent env))))))

(defun find-generator (name env)
  (loop
     :for loop := env :then (doplus-env-parent loop)
     :while loop
     :do (let ((gen (assoc name (doplus-env-generators loop))))
           (when gen (return (cons loop gen))))))

(defmacro with-doplus-body (env &body body)
  (let ((no-return-value (gensym)))
    `(macrolet ((collect (value &key (into ',(doplus-env-default-collect-var env)))
                  (let ((accumulator
                         (loop
                            :for loop := ,env :then (doplus-env-parent loop)
                            :while loop
                            :do (let ((acc (cdr (assoc into (doplus-env-accumulators loop)))))
                                  (when acc (return acc))))))
                    (if accumulator
                        `(setf ,into (funcall ,accumulator ,value ,into))
                        (error "~A is not a known accumulator" into))))
                (update (name)
                  (let ((generator (find-generator name ,env)))
                    (if generator
                        (if (eql (car generator) ,env)
                            `(progn ,@(cdr generator) ,name)
                            `(with-doplus-body ,(car generator)
                               ,@(cdr generator) ,name))
                        (error "~S is not a known generator" name))))
                (try-update (name &key (on-error (lambda (obj condition) (declare (ignore condition)) (values obj nil))))
                  (let ((generator (find-generator name ,env))
                        (block-name (gensym "TRY-UPDATE-BLOCK"))
                        (error-fn (gensym "ON-ERROR")))
                    (if generator
                        ;;The following binding is to fix try-update on ECL (tested on 12.12.1).
                        ;;Using ,on-error directly in the funcall form below results in
                        ;;wrong values for captured variables in compiled code.
                        `(let ((,error-fn ,on-error))
                           (block ,block-name
                             (handler-bind ((error (lambda (cond)
                                                     (return-from ,block-name
                                                       (funcall ,error-fn ,name cond)))))
                               (with-doplus-body ,(car generator)
                                 (macrolet ((terminate (&rest args)
                                              (declare (ignore args))
                                              `(return-from ,',block-name (values ,',name nil))))
                                   ,@(cdr generator) (values ,name t))))))
                        (error "~S is not a known generator" name))))
                (skip (&optional (loop-name ',(doplus-env-loop-name env)))
                  (let ((the-loop (find-loop-in-env loop-name ,env)))
                    (if the-loop
                        `(go ,(doplus-env-skip-label the-loop))
                        (error "~S is not the name of a do+ loop in scope" loop-name))))
                (terminate (&optional loop-name (return-value ',no-return-value))
                  (when (null loop-name)
                    (setf loop-name ',(doplus-env-loop-name env)))
                  (let ((the-loop (find-loop-in-env loop-name ,env)))
                    (if the-loop
                        (if (eq return-value ',no-return-value)
                            `(go ,(doplus-env-terminate-label the-loop))
                            `(return-from ,loop-name ,return-value))
                        (error "~S is not the name of a do+ loop in scope" loop-name))))
                (do+ ((&rest iter-forms) &body body)
                  `(do+/internal ,,env ,iter-forms ,@body)))
       ,@body)))

(defmacro do+ ((&rest iter-forms) &body body)
  "High-level, extensible iteration construct. Refer to the manual for syntax and semantics."
  `(do+/internal nil ,iter-forms ,@body))

(defmacro do+/internal (doplus-env iter-forms &body body &environment env)
  (let (bindings declarations iterations termination-conditions result-forms generators
        initializations finalizations accumulators (default-collect-var (gensym "COLLECT"))
        prologue epilogue (wrapper #'identity) loop-name
        (loop-label (gensym "LOOP")) (return-label (gensym "RETURN"))
        (update-label (gensym "UPDATE")) (with-atomic-updates 'with-atomic-updates))
    (if (and (car iter-forms) (symbolp (car iter-forms)))
        (setf loop-name (car iter-forms) iter-forms (cdr iter-forms))
        (setf loop-name (gensym "DOPLUS")))
    (labels ((process-form (raw-form &optional iterations)
               (let ((form (macroexpand raw-form env)))
                 (cond
                   ((and (listp form) (not (symbolp (car form))))
                    (dolist (x form)
                      (setf iterations (process-form x iterations))))
                   ((options-p form)
                    (when (cadr (member :name (options-map form)))
                      (setf loop-name (cadr (member :name (options-map form)))))
                    (when (member :atomic-updates (options-map form))
                      (unless (cadr (member :atomic-updates (options-map form)))
                        (setf with-atomic-updates 'without-atomic-updates))))
                   ((generator-p form)
                    (let ((generator (process-form (generator-clauses form))))
                      (dolist (name (ensure-list (generator-name form)))
                        (push (cons name (reverse generator)) generators))))
                   ((binding-p form)
                    (let ((existing-binding (find (binding-var form) bindings :key #'binding-var)))
                      (if existing-binding
                          (if (binding-default-provided? existing-binding)
                              ;;It is an error to replace a binding with a default with a binding with a different default,
                              ;;though it is not an error to redeclare an existing binding without respecifying its default.
                              (unless (or (not (binding-default-provided? form))
                                          (eql (binding-default form) (binding-default existing-binding)))
                                (error "There is already a binding for ~S with default ~S"
                                       (binding-var form) (binding-default existing-binding)))
                              (setf bindings (substitute form existing-binding bindings)))
                          (push form bindings))))
                   ((declaration-p form) (push `(declare ,(declaration-form form)) declarations))
                   ((termination-p form) (push (termination-condition form) termination-conditions))
                   ((result-p form) (push (result-form form) result-forms))
                   ((step-p form) (push (step-form form) iterations))
                   ((initialization-p form) (push (initialization-form form) initializations))
                   ((finalization-p form) (push (finalization-form form) finalizations))
                   ((accumulator-p form)
                    (push (cons (accumulator-var form) (accumulator-function form)) accumulators))
                   ((prologue-p form) (push (prologue-form form) prologue))
                   ((epilogue-p form) (push (epilogue-form form) epilogue))
                   ((wrapper-p form)
                    (let ((old-wrapper wrapper))
                      (setf wrapper
                            (lambda (body)
                              (funcall (wrapper-function form) (funcall old-wrapper body))))))
                   (t (error "Unsupported iteration form: ~S (macroexpands to ~S)" raw-form form))))
               iterations))
      (setf iterations (process-form (cons `(accumulating-to ,default-collect-var) iter-forms)))
      (when (null result-forms)
        (setf iterations (process-form `(returning ,default-collect-var) iterations))))
    (setf bindings (nreverse (mapcar (lambda (binding) (list (binding-var binding) (binding-default binding)))
                                     bindings)))
    (setf doplus-env (make-doplus-env
                      :loop-name loop-name :parent doplus-env :accumulators accumulators
                      :generators generators :default-collect-var default-collect-var
                      :skip-label update-label :terminate-label return-label))
    (multiple-value-bind (body decls)
        (tcr.parse-declarations-1.0::parse-body body :documentation nil)
      `(let ,bindings
         ,@(nreverse declarations)
         ,@decls
         ,(funcall
           wrapper
           `(with-doplus-body ,doplus-env
              (block ,loop-name
                (tagbody
                   (,with-atomic-updates ,(mapcar #'car bindings)
                     ,@(nreverse initializations))
                   ,loop-label
                   ,(when termination-conditions
                          `(when (or ,@(nreverse termination-conditions))
                             (go ,return-label)))
                   ,@(nreverse prologue)
                   ,@body
                   ,@(nreverse epilogue)
                   ,update-label
                   (,with-atomic-updates ,(mapcar #'car bindings)
                     ,@(nreverse iterations))
                   (go ,loop-label)
                   ,return-label
                   ,@(nreverse finalizations)
                   ;;result-forms is never nil
                   (return-from ,loop-name (values ,@(nreverse result-forms)))))))))))

;;Top-level macros to aid autocompletion and store documentation
(defmacro sum (value &key (to (error "sum requires a target accumulator (:to key)")))
  "Syntactic sugar for collect, intended to be used when the accumulator computes a sum."
  `(collect ,value :into ,to))

(defmacro collect (value &key into)
  "Collects a value into an accumulator. If no accumulator is provided, the default one for the current do+ loop is used."
  (declare (ignore value into))
  (error "collect can only be called inside the body of a do+"))

(defmacro skip (&optional loop-name)
  "Skips the current iteration. Refers to the current loop or, if loop-name is specified, to the closest surrounding loop with that name."
  (declare (ignore loop-name))
  (error "skip can only be called inside the body of a do+"))

(defmacro terminate (&optional loop-name return-value)
  "Immediately terminates the loop. Refers to the current loop or, if loop-name is specified and non-NIL, to the closest surrounding loop with that name. Can optionally specify a return value for the loop, which, if provided, will override the loop's ordinary return value."
  (declare (ignore loop-name return-value))
  (error "terminate can only be called inside the body of a do+"))

(defmacro update (var)
  "Updates the generator named <var>, calculating new value(s) for its variable(s) and evaluating its pre and post conditions, terminating the loop if they fail.
If successful, the update returns the value of <var>."
  (declare (ignore var))
  (error "update can only be called inside the body of a do+"))

(defmacro try-update (var &key on-error)
  "Tries to update the generator named <var> as by UPDATE. If the loop would be terminated as a result of the update operation, it is not terminated.

Any errors occurring during the update are ignored by default; however, it is possible to provide a function to be invoked in case of error to determine what to do. This is the value of the <on-error> parameter, which, if provided, must be a function of two arguments: the first is the value to be returned as the result of the try-update form if the error is suppressed; the second is the condition object itself, which can be resignaled.
Calling TERMINATE inside the function, if it is defined lexically inside the DO+ body, will regularly terminate the loop."
  (declare (ignore var on-error))
  (error "try-update can only be called inside the body of a do+"))