(defpackage :doplus-tests
  (:use :cl :doplus :eos)
  (:shadowing-import-from :doplus #:skip)
  (:export #:run-all-tests))

(in-package :doplus-tests)

(def-suite doplus-suite)

(in-suite doplus-suite)

(test test-collect
  (is (equal '(5 15 25)
             (do+ ((for x (from 0 :to 30 :by 5)))
               (when (oddp x)
                 (collect x))))))

(test test-destructuring
  (is (equal '(1 a 2 b 3 c) (do+ ((for (x . y) (in '((1 . a) (2 . b) (3 . c)))))
                              (collect x)
                              (collect y))))
  (is (equal '((1 a nil nil k2 k3) (2 y nil nil k2 k3) (3 c (:k1 a :kk b :k2 c) a c b))
             (do+ ((for (x &optional (y 'y) &rest z &key k1 (k2 'k2) ((:kk k3) 'k3))
                     (in '((1 a) (2) (3 c :k1 a :kk b :k2 c)))))
               (collect (list x y z k1 k2 k3))))))

(test test-in-list
  (is (null (do+ ((for x (in-list ()))) (collect x))))
  (is (equal '(1 2 3) (do+ ((for x (in-list '(1 2 3)))) (collect x))))
  (is (equal '(1 2 3) (do+ ((for x (in-list '(1 a 2 b 3 c) :by #'cddr))) (collect x))))
  (is (equal '((1 2 3) (2 3) (3)) (do+ ((for _ (in-list '(1 2 3) :rest x))) (collect x))))
  (is (equal '(1 a 2 b 3 c) (do+ ((for (x . y) (in-list '((1 . a) (2 . b) (3 . c)))))
                              (collect x)
                              (collect y)))))

(test test-in-vector
  ;;across expands to in-vector
  (is (null (do+ ((for x (across #()))) (collect x))))
  (is (equal '(1 2 3) (do+ ((for x (across #(1 2 3)))) (collect x))))
  (is (null (do+ ((for x (across #(3 2 1) :start 2 :end 0))) (collect x))))
  (is (equal '(1 2 3) (do+ ((for x (across #(3 2 1) :start 2 :end 0 :by -1))) (collect x))))
  (is (equal '(1 2 3)
             (do+ ((for x (across #(0 1 a 2 b 3 c) :by 2 :start 1)))
               (collect x))))
  (is (equal '(1 2 3)
             (do+ ((for x (across #(0 1 a 2 b 3 c 4) :by 2 :start 1 :end 6)))
               (collect x))))
  (is (equal '(1 2 3 4)
             (do+ ((for x (across #(0 1 a 2 b 3 c 4) :by 2 :start 1 :end 7)))
               (collect x)))))

(test test-list-tails
  (is (equal '(1 2 3 4) (do+ ((for x (list-tails '(1 a 2 b 3 c 4 d) :by #'cddr)))
                          (collect (car x))))))

(test test-hash-entries
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 'b)
    (setf (gethash 3 ht) 4)
  (is (equal '((a . b) (3 . 4))
             (sort
              (do+ ((for (k v) (hash-entries-of ht)))
                (collect (cons k v)))
              (lambda (x1 x2)
                (declare (ignore x2))
                (symbolp (car x1))))))))

(test test-summing
  (is (equal '(12 7)
             (do+ ((for x (from 0 :to 10))
                   (summing-to total)
                   (finding max (maximizing x))
                   (returning (list total max))
                   (stop-when (eql x 8)))
               (when (evenp x)
                 (sum x :to total))))))

(test test-previous
  (is (equal '((1 nil nil) (2 1 nil) (3 2 1) (4 3 2))
             (do+ ((for x (in '(1 2 3 4)))
                   (for y (previous x))
                   (for z (previous y)))
               (collect (list x y z)))))
  (is (equal '((1 nil nil) (2 1 nil) (3 2 1) (4 3 2))
             (do+ ((for x (in '(1 2 3 4)))
                   (for z (previous y))
                   (for y (previous x)))
               (collect (list x y z))))))

(test test-nesting
  "Tests nesting and accumulator sharing"
  (is (equal
       '((1 A) (1 B) (2 A) (2 B) (3 A) (3 B) (1 A) (1 B) (2 A) (2 B) (3 A) (3 B))
       (do+ ((accumulating-to foo)
             (stop-when (> (length foo) 10))
             (returning foo))
         ;;(print (list 'outer foo))
         (do+ ((for x (in '(1 2 3))))
           ;;(print (list 'inner1 x))
           (do+ ((for y (in '(a b))))
             ;;(print (list 'inner2 x y))
             (collect (list x y) :into foo))))))
  (is (equal
       '((0) (0 1) (0 1 2))
       (do+ ((generating x (from 0 :to 3)))
         (collect (do+ ((for y (to x)))
                    (collect y)
                    (when (= x y)
                      (update x))))))))

(test test-terminate
  (is (equal 1 (do+ ((returning 1)) (terminate))))
  (is (equal 1 (do+ ((with (x 1))) (terminate nil x)))))

(test test-named-loop
  "Tests nested named loops"
  ;;Test old-style name
  (is (equal
       3
       (do+ (outer-loop)
         (do+ (inner-loop)
           (do+ ()
             (terminate outer-loop 3))))))
  ;;Test options :name
  (is (equal
       3
       (do+ ((options :name outer-loop))
         (do+ ((options :name inner-loop))
           (do+ ()
             (terminate outer-loop 3)))))))

(test test-atomic-updates
  (is (equal
       (do+ ((for x (across #(1 2 3 4) :index index))
             (for k (in '(a b c d e)))
             (returning (list x k :index index))))
       (do+ ((for k (in '(a b c d e)))
             (for x (across #(1 2 3 4) :index index))
             (returning (list x k :index index))))))
  (is (not (equal
       (do+ ((options :atomic-updates nil)
             (for x (across #(1 2 3 4) :index index))
             (for k (in '(a b c d e)))
             (returning (list x k :index index))))
       (do+ ((options :atomic-updates nil)
             (for k (in '(a b c d e)))
             (for x (across #(1 2 3 4) :index index))
             (returning (list x k :index index))))))))

(test monster-test
  (let ((*standard-output* (make-broadcast-stream)))
    (is (equal '(X 7 Y 42 Z NIL K 7 M 15 W (1 3 5) P -9 LOOPS 3 MAX-LENGTH 3 LONGEST-LIST (4 5 6))
               (do+ ((for x (in '(1 3 5 7 9)))
                     (for k (from 10 :to 0))
                     (initially (format t "initially k = ~S~%" k))
                     (for m (to 100 :by 5))
                     (initially (format t "another init form~%"))
                     (with (y 42) z)
                     (stop-when (eql x 7))
                     (counting loops)
                     (finally (format t "finally k = ~S~%" k))
                     (accumulating-to w)
                     (accumulating-to p :initially 0 :by #'+ :finally #'-)
                     (returning (list 'x x 'y y 'z z 'k k 'm m 'w w 'p p
                                      'loops loops 'max-length max-length 'longest-list longest-list))
                     (finally (format t "another final form~%"))
                     (finding max-x (maximizing x))
                     (for list (in '((1 2) (3) (4 5 6) (7 8) (9))))
                     (finding max-length (maximizing (length list) :saving list :in longest-list)))
                 (let ((foo "look ma, body is nested!"))
                   (declare (ignore foo))
                   (format t "~%loop no. ~A~%" loops)
                   (format t "before collect, x ~A y ~A z ~A k ~A m ~A w ~A ~% list ~A max-length ~A longest-list ~A ~%"
                           x y z k m w list max-length longest-list)
                   (collect x :into w)
                   (collect x :into p)
                   (format t "after collect, x ~A y ~A z ~A k ~A m ~A w ~A ~% list ~A max-length ~A longest-list ~A ~%"
                           x y z k m w list max-length longest-list)))))))

(test test-generators-1
  (is (equal '((0 0) (1 0) (2 1) (3 1))
             (do+ ((for x (from 0))
                   (generating y (across #(0 1))))
               (collect (list x y))
               (when (oddp x)
                 (is (= (update y) y)))))))

(test test-generators-2
  (is (equal '((0 0) (1 0) (2 1) (3 1) (4 1) (5 1))
             (do+ ((for x (to 5))
                   (generating y (in #(0 1))))
               (collect (list x y))
               (when (oddp x)
                 (is (eql (try-update y) y)))))))

(test test-generators-3
  (let (error)
    (is (equal '((0 0) (1 0) (2 1) (3 1) (4 1) (5 1))
               (do+ ((for x (to 5))
                     (generating y (in #(0 1))))
                 (collect (list x y))
                 (when (oddp x)
                   (is (eql (try-update y :on-error (lambda (value cond) (setf error cond) value)) y)))
                 (when error
                   (multiple-value-bind (value success?) (try-update y)
                     (is (eql value y))
                     (is (null success?)))))))
    (is (not (null error)))))

(test test-skip
  (is (null (do+ ((for x (from 0 :to 3))) (skip) (collect x)))))

(test test-macrolet
  (is (equal '(1 2 3 4)
             (macrolet ((among (&rest args) `(in ,@args))
                        (pick (it) `(collect ,it :into foo)))
               (do+ ((for x (among '(1 2 3 4)))
                     (accumulating-to-and-returning foo))
                 (pick x))))))

(test test-in
  (is (equal '(3 . B)
             (do+ ((for y (in #(A B C D) :from-end t))
                   (for x (in '(1 2 3))) (returning (cons x y))))))
  (is (equal '((2 . 1) (4 . 3))
             (do+ ((for (a b) (in '((1 2) (3 4))))) (collect (cons b a)))))
  (is (equal '((2 . 1) (4 . 3))
             (do+ ((for (a b) (in #((1 2) (3 4))))) (collect (cons b a))))))

(test test-package-iterator
  (is (equal 'truly-the-symbol-with-the-longest-name
             (do+ ((for s (symbols-in :doplus-tests :internal))
                   (for l (maximizing (length (symbol-name s))
                                      :saving s :in symbol-with-longest-name))
                   (returning symbol-with-longest-name))))))

(test test-being
  (is (equal '(2 4 8 16 32 64)
             (do+ ((for x (being 2 :then (* x 2)))
                   (stop-when (> x 100)))
               (collect x)))))

(test test-multiple-accumulations
  (is (equal '(1 2 2 4)
             (do+ ((for x (in '(1 2)))
                   (for y (collecting-into-and-returning list)))
                  (collect x :into list)
                  (collect (* 2 x) :into list)))))

(test test-multiple-returns
  (is (equal '(1 2 3 4 5 6 7)
             (multiple-value-list
              (do+ ((returning 1) (returning 2 3)
                    (returning (values 4 5) 6) (returning 7))
                (terminate))))))

(defun run-all-tests ()
  (run! 'doplus-suite))