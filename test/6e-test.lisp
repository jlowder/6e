(in-package :cl-user)

(defpackage :6e-test
  (:use :common-lisp
        :lisp-unit
        :6e))

(in-package :6e-test)

(defun 2.2.4.1 ()
  "Page 50 from Statistical Orbit Determination"
  '{c3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0})

(defun 2.2.4.2 ()
  "Page 51 from Statistical Orbit Determination"
  '{k3.9860044d14 7231745.57d0 0.0010013d0 [d98.9964] [d181.3428] [d113.9737] [d246.2483] 0d0})

(defun diff-of (t1i t2i)
  (let ((t1 (if (zerop t1i)
                1e-6
                t1i))
        (t2 (if (zerop t2i)
                1e-6
                t2i)))
    (- 1 (if (> (abs t1) (abs t2))
             (/ t2 t1)
             (/ t1 t2)))))

(defun diffs (l1 l2)
  (cond ((null l1) '())
        ((atom l1) (diff-of l1 l2))
        (t (cons (diff-of (car l1) (car l2)) (diffs (cdr l1) (cdr l2))))))

(define-test cart-to-kep
    ; look for small accumulated error between calculated and expected
    (assert-true (> 1d-3 (apply #'+ (diffs (2.2.4.1)
                                           (list 6828973.232519d0
                                                 0.0090173388450585d0
                                                 [d28.474011884869d0]
                                                 [d35.911822759495d0]
                                                 [d315.44415294721d0]
                                                 [d43.8860381032208d0]))))))

(define-test kep-to-cart
    ; look for small accumulated error between calculated and expected
    (assert-true (> 1d-3 (apply #'+ (diffs (2.2.4.2)
                                          '(-7232720.490d0
                                            -167227.700d0
                                            14595.566d0
                                            -5.243469d0
                                            1160.655450d0
                                            7329.834189d0))))))

(run-tests)
