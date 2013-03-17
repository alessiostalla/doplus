(in-package :doplus)

;;;;The following code is adapted from ABCL which in turn adapted it from SBCL.
;;;;Credit goes to the original authors!

;;;; iterator protocol

;;; The general protocol

(defgeneric make-sequence-iterator (sequence &key from-end start end)
  (:method ((s sequence) &key from-end (start 0) end)
    (multiple-value-bind (iterator limit from-end)
        (make-simple-sequence-iterator
         s :from-end from-end :start start :end end)
      (values iterator limit from-end
              #'iterator-step #'iterator-endp
              #'iterator-element #'(setf iterator-element)
              #'iterator-index #'iterator-copy)))
  (:method ((s t) &key from-end start end)
    (declare (ignore from-end start end))
    (error 'type-error
           :datum s
           :expected-type 'sequence)))

;;; the simple protocol: the simple iterator returns three values,
;;; STATE, LIMIT and FROM-END.

;;; magic termination value for list :from-end t
(defvar *exhausted* (cons nil nil))

(defgeneric make-simple-sequence-iterator
    (sequence &key from-end start end)
  (:method ((s list) &key from-end (start 0) end)
    (if from-end
        (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) s)))
               (init (if (<= (or end (length s)) start)
                         termination
                         (if end (last s (- (length s) (1- end))) (last s)))))
          (values init termination t))
        (cond
          ((not end) (values (nthcdr start s) nil nil))
          (t (let ((st (nthcdr start s)))
               (values st (nthcdr (- end start) st) nil))))))
  (:method ((s vector) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) t)
          (values start end nil))))
  (:method ((s sequence) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) from-end)
          (values start end nil)))))

(defgeneric iterator-step (sequence iterator from-end)
  (:method ((s list) iterator from-end)
    (if from-end
        (if (eq iterator s)
            *exhausted*
            (do* ((xs s (cdr xs)))
                 ((eq (cdr xs) iterator) xs)))
        (cdr iterator)))
  (:method ((s vector) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:method ((s sequence) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator))))

(defgeneric iterator-endp (sequence iterator limit from-end)
  (:method ((s list) iterator limit from-end)
    (eq iterator limit))
  (:method ((s vector) iterator limit from-end)
    (= iterator limit))
  (:method ((s sequence) iterator limit from-end)
    (= iterator limit)))

(defgeneric iterator-element (sequence iterator)
  (:method ((s list) iterator)
    (car iterator))
  (:method ((s vector) iterator)
    (aref s iterator))
  (:method ((s sequence) iterator)
    (elt s iterator)))

(defgeneric (setf iterator-element) (new-value sequence iterator)
  (:method (o (s list) iterator)
    (setf (car iterator) o))
  (:method (o (s vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s sequence) iterator)
    (setf (elt s iterator) o)))

(defgeneric iterator-index (sequence iterator)
  (:method ((s list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))

(defgeneric iterator-copy (sequence iterator)
  (:method ((s list) iterator) iterator)
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))

(defmacro with-sequence-iterator
    ((&rest vars) (s &rest args &key from-end start end) &body body)
  (declare (ignore from-end start end))
  `(multiple-value-bind (,@vars) (make-sequence-iterator ,s ,@args)
    (declare (type function ,@(nthcdr 3 vars)))
    ,@body))

(defmacro with-sequence-iterator-functions
    ((step endp elt setf index copy)
     (s &rest args &key from-end start end)
     &body body)
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")))
    `(with-sequence-iterator
         (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
       (,s ,@args)
       (flet ((,step () (setq ,nstate (funcall ,nstep ,s ,nstate ,nfrom-end)))
              (,endp () (funcall ,nendp ,s ,nstate ,nlimit ,nfrom-end))
              (,elt () (funcall ,nelt ,s ,nstate))
              (,setf (new-value) (funcall ,nsetf new-value ,s ,nstate))
              (,index () (funcall ,nindex ,s ,nstate))
              (,copy () (funcall ,ncopy ,s ,nstate)))
         (declare (truly-dynamic-extent #',step #',endp #',elt
                                  #',setf #',index #',copy))
         ,@body))))
