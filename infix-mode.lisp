;;;; infix-mode.lisp


(in-package #:infix-mode)


(defparameter *test-deinfixize* '((:prefix-mode + + a a) ^ a + b ^ c ! ! ! + sin (x :infix-comma 2 + 3 * sin (x + 2 :infix-comma cos (:prefix-mode (c * a b))))))
(defparameter *test-sanitize* '(((((1)))) + (2) + (a + b (c) + ((d) (:prefix-mode e) (:prefix-mode)))))


(defparameter *infix-mode-stoppers* `(:prefix-mode quote ,(car '`nil)))

(defparameter *infix-comma-symbol* :infix-comma)

(defparameter *fake-infix-operator* :infix-fake-symbol)

(defparameter *infix-funcall-symbol* :infix-funcall)

(defparameter *infix-curry* :infix-curry)

(defparameter *default-infix-operator* '*)

(defparameter *infix-operator-order* `( (eql) (or) (and) (not) (= > < >= <=) (+ -) (* /) (^ :reverse) (!)  (:infix-funcall) (:infix-comma)))

(defparameter *binary-operator-set* `(:infix-funcall :infix-comma + * ^ - / ! = or and eql))

(defparameter *associative-operator-set* '(+ * and or))

(defparameter *reverse-functions* `((sin asin) (asin sin)))

(defparameter *pseudo-associative-operator-set* `(eql = > < >= <=))

(defparameter *right-operator-set* `(!))

(defparameter *left-operator-set* `(not))

(defparameter *infix-operator-funcall-hash* (let ((local-hash (make-hash-table)))
                                              (setf (gethash '+ local-hash) '+)
                                              (setf (gethash '* local-hash) '*)
                                              (setf (gethash '^ local-hash) 'expt)
                                              (setf (gethash '- local-hash) '-)
                                              (setf (gethash '/ local-hash) '/)                                              (setf (gethash '! local-hash) nil)
                                              (setf (gethash '= local-hash) '=)
                                              (setf (gethash '> local-hash) '<)
                                              (setf (gethash '< local-hash) '>)
                                              (setf (gethash '>= local-hash) '>=)
                                              (setf (gethash '<= local-hash) '<=)
                                              (setf (gethash 'not local-hash) 'not)
                                              (setf (gethash 'and local-hash) 'and)
                                              (setf (gethash 'or local-hash) 'or)
                                              (setf (gethash 'eql local-hash) 'eql)
                                              local-hash))

(defparameter *default-right-currying* (let ((local-hash (make-hash-table)))
                                        local-hash))

(defparameter *default-left-currying* (let ((local-hash (make-hash-table)))
                                          (setf (gethash '- local-hash) -)
                                          local-hash))






(define-condition orphane-operation (parse-error)
  ((path :initarg :path
         :initform nil
         :accessor path))
  (:report (lambda (condition stream)
                   (format stream "Operation with missing argument and no default carrying at ~a.~&" (path condition)))))

(define-condition missing-default-operation (error)
  ((path :initarg :path
         :initform nil
         :accessor path))
  (:report (lambda (condition stream)
                   (format stream "Expected operation, found value at ~a instead.~&" (path condition)))))






(defmacro xor (a b)
  `(if ,a
     (not ,b)
     ,b))

(defmacro is-operator (thing)
  `(or (member ,thing *binary-operator-set*)
       (member ,thing *right-operator-set*)
       (member ,thing *left-operator-set*)
       (eql ,thing *fake-infix-operator*)))

(defmacro is-not-operator (thing)
  `(not (is-operator ,thing)))

(defmacro is-function (thing)
  `(and (is-not-operator ,thing)
        (or (and (listp ,thing)
                 (eql (first ,thing) 'lambda))
            (eql (type-of ,thing) 'function)
            (fboundp ,thing))))



(defmacro needs-left-argument (thing)
  `(or (member ,thing *binary-operator-set*)
       (member ,thing *right-operator-set*)
       (eql ,thing *fake-infix-operator*)))

(defmacro needs-right-argument (thing)
  `(or (member ,thing *binary-operator-set*)
       (member ,thing *right-operator-set*)
       (eql ,thing *fake-infix-operator*)))





(defun check-local-integrity (first second)
  (cond

    ((xor (needs-right-argument first)           ;case when you don't need to modify anything at all
          (needs-left-argument second))
     nil)

    ((and (needs-right-argument first)           ;case when there are two operations that wants each other to be object
          (needs-left-argument second))          ;will try to curry one of them, otherwise return error
     (if (or (gethash second *default-left-currying*)
             (gethash first *default-right-currying*))
       *infix-curry*
       (make-condition 'orphane-operation :path (list first second))))

    ((and (is-not-operator first)                 ;case f (x), if f is a function, then it shall be
          (is-not-operator second)                ;transformed to f :funcall (x)
          (is-function first))
     *infix-funcall-symbol*)

    (t *default-infix-operator*)))                ;otherwise there is missing multiplication





(defun infix-associator (operator rest)
  (let (answer)
    (if (member operator *associative-operator-set*)
      (dolist
       (object rest)
       (print answer)
       (if (and (listp object)
                (eql (car object) operator))
         (setf answer (append (cdr object) answer))
         (push object answer)))
      (return-from infix-associator (list operator rest)))
   (list operator answer)))

(defun reverse-function (operator rest)
  (if (member (list operator (car rest)) *reverse-functions*)
    (cdr rest)
    (cons operator rest)))

;(defun reduce-number-of-multiplications (operator rest))
