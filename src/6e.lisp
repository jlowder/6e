(in-package :cl-user)

(defpackage :6e
  (:use :common-lisp
        :vector)
  (:export :6e 
           :xyz
           :display-6e
           :k->c))

(in-package :6e)

(defun radians->degrees (r)
  (/ (* 180 r) pi))

(defun degrees->radians (d)
  (/ (* d pi) 180))

(defun adf (stream arg colon at &rest args)
  ; aligned decimal "F"
  (declare (ignore colon at))
  (destructuring-bind (width digits &optional (pad #\Space)) args
    (let* ((string (format nil "~v,vf" width digits arg))
           (non-zero (position #\0 string :test #'char/= :from-end t))
           (dot (position #\. string :test #'char= :from-end t))
           (zeroes (- (length string) non-zero (if (= non-zero dot) 2 1)))
           (string (nsubstitute pad #\0 string :from-end t :count zeroes)))
      (write-string (string-right-trim " "string) stream))))

(defun printd (n v)
  (format t "~a: ~33T~28,18/6e:adf/°~%" n (radians->degrees v)))

(defun printr (n v)
  (format t "~a: ~33T~28,18/6e:adf/~%" n v))

(defun printu (n v u)
  (format t "~a: ~33T~28,18/6e:adf/~a~%" n v u))

(defun resolve (s c)
  (let* ((as (asin s))
         (ac (acos c))
         (r (cond ((and (not (minusp s)) (not (minusp c))) as)
                  ((and (not (minusp s)) (minusp c)) ac)
                  (t (- (* 2 pi) ac)))))
;    (if (realp r)
;        r
;        (if (realp as)
;            as
;            ac))))
    r))

(defun |#d-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream) (read-char stream)))
        ((char= #\] curr))
      (push curr chars))
    (degrees->radians (read-from-string (coerce (nreverse chars) 'string) nil nil))))

(unless (get-macro-character #\[)
  (make-dispatch-macro-character #\[))

(unless (get-macro-character #\{)
  (make-dispatch-macro-character #\{))

(set-dispatch-macro-character #\[ #\d #'|#d-reader|)

(defun norm-rev (val)
  (let ((2pi (* 2 pi)))
    (if (< val 0)
        (norm-rev (+ val 2pi))
        (if (> val 2pi)
            (norm-rev (- val 2pi))
            val))))

(defun eccentricity-vector (rv vv hv mu)
  (let* ((vch (cross vv hv))
         (p1 (scalevec (/ 1 mu) vch))
         (r (vlen3 rv))
         (p2 (scalevec (/ -1 r) rv)))
    (mapcar #'+ p1 p2)))

(defun xyz->true-anomaly (rv vv hv mu)
  (let* ((ev (eccentricity-vector rv vv hv mu))
         (der (dot ev rv))
         (e (vlen3 ev))
         (r (vlen3 rv))
         (v (acos (/ der
                     (* e r)))))
    (coerce (if (< (dot rv vv) 0)
                (* -1 v)
                v) 'double-float)))

(defun true->eccentric (theta e)
  (let* ((costheta (cos theta))
         (cose (/ (+ e costheta)
                  (+ 1 (* e costheta))))
         (sine (/ (* (sqrt (- 1 (* e e))) (sin theta))
                  (+ 1 (* e costheta))))
         (r (resolve sine cose)))
    r))

(defun roy-4.66 (a e f)
  "f is true anomaly"
  (/ (* a (- 1 (* e e)))
     (+ 1 (* e (cos f)))))

(defun roy-4.67 (a ecc E)
  "E is eccentric anomaly"
  (* a (- 1 (* ecc (cos E)))))

(defun roy-4.68 (ecc E)
  "this returns tan(f/2)"
  (* (sqrt (/ (+ 1 ecc)
              (- 1 ecc))) (tan (/ E 2))))

(defun roy-4.69 (ecc E)
  (- E (* ecc (sin E))))

(defun roy-4.70 (n time tau)
  "tau is time of perihelion passage"
  (* n (- time tau)))

(defun roy-4.71 (u a e)
  "this returns h * h"
  (* u a (- 1 (* e e))))

(defun roy-4.72 (u r a)
  "this returns V * V"
  (* u (- (/ 2 r) (/ 1 a))))

(defun roy-4.73 (n)
  (/ (* 2 pi) n))

(defun roy-4.74 (u a)
  (* (sqrt u) (expt a -3/2)))

(defun roy-4.75 (a e r)
  "this returns sin theta"
  (sqrt (/ (* a a (- 1 (* e e)))
           (* r (- (* 2 a) r)))))

(defun a->n (u a)
  (/ (* 86400 (roy-4.74 u a)) (* 2 pi)))

(defun inv-4.74 (u n)
  (expt (/ n (sqrt u)) -2/3))

(defun n->a (u n)
  (inv-4.74 u (/ (* n 2 pi) 86400)))

(defun 6e (u x y z dx dy dz)
  (declare (type long-float u x y z dx dy dz))
  (let* ((rv (list x y z)) ; position vector
         (vv (list dx dy dz)) ; velocity vector
         (r0 (vlen x y z)) ; distance between body and satellite
         (v0 (vlen dx dy dz)) ; magnitude of velocity
         (hx (- (* y dz) (* dy z))) ; angular momentum in each direction
         (hy (- (* z dx) (* dz x)))
         (hz (- (* x dy) (* dx y)))
         (hv (list hx hy hz))
         (h (vlen hx hy hz)) ; magnitude of angular momentum = sqrt(up)
         (p (/ (* h h) u)) ; semilatus rectum
         (i (acos (/ hz h))) ; angle of inclination
         (pmhx hx)
         (pmhy (* -1 hy))
         (hsini (* h (sin i))) ; intermediate for Ω
         (Ωsin (/ pmhx hsini)) ; sin(Ω)
         (Ωcos (/ pmhy hsini)) ; cos(Ω)
         (Ω (resolve Ωsin Ωcos))
         (ξ (- (/ (* v0 v0) 2) (/ u r0))) ; energy of vehicle. ellipse is neg, parabola is 0,
                                          ; hyperbola is positive
         (alpha (* -1.0 (/ u (* 2 ξ)))) ; semi-major axis
         (e (sqrt (- 1 (/ p alpha)))) ; eccentricity
         (f (xyz->true-anomaly rv vv hv u))
         (wfcos (+ (/ (* x (cos Ω)) r0) (/ (* y (sin Ω)) r0))) ; cos(ω + f)
         (wfsin (/ z (* r0 (sin i)))) ; sin(ω + f)
         (wf (resolve wfsin wfcos))
         (e0 (true->eccentric f e))
         (m0 (- e0 (* e (sin e0)))))
    ;(format t "6e: true anomaly = ~f~%" f)
    ;(list alpha e i Ω (norm-rev (- wf f)) (- (* 2 pi) m0))))
    ;(format t "hx=~a hy=~a hz=~a h=~a pmhx=~a pmhy=~a Ωsin=~a Ωcos=~a~%" hx hy hz h pmhx pmhy Ωsin Ωcos)
    (list alpha e i Ω (norm-rev (- wf f)) m0)))

(defun display-6e (elems)
  (destructuring-bind (alpha e i Ω w m0)
      elems
    (printu "Semi-Major Axis (α)" alpha "m")
    (printr "Eccentricity (e)" e)
    (printd "Angle of Inclination (ὶ)" i)
    (printd "Longitude of ascending node (Ω)" Ω)
    (printd "Argument of Perifocus (ω)" w)
    (printd "Mean Anomaly (M0)" m0)))

(defun |#6-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr #\( (read-char stream)))
        ((char= #\} curr))
      (push curr chars))
    (push #\) chars)
    (apply #'6e (read-from-string (coerce (nreverse chars) 'string) nil nil))))

(set-dispatch-macro-character #\{ #\c #'|#6-reader|)

(defun M0->Mt (m0 dt n)
  "propagate M0 to a new time"
  (loop for 2PI = (* 2 pi)
     for m = (+ m0 (* n dt 2PI)) then (- m 2PI)
     when (< m 2PI) return m))

(defun M->E (M e)
  "Kepler's equation: mean anomaly to eccentric anomaly. M is the output of M0->Mt"
  (let ((ε 10e-15))
    (loop for Ei = M then Ei+1
       for Ei+1 = (- Ei (/ (- Ei (* e (sin Ei)) M)
                           (- 1 (* e (cos Ei)))))
       when (< (abs (- Ei Ei+1)) ε)
       return Ei+1)))

(defun E->N (Et e m0)
  "Eccentric anomaly to true anomaly"
  (let* ((cosn (/ (- (cos Et) e)
                  (- 1 (* e (cos Et)))))
         (r (acos cosn)))
    (if (< m0 pi)
        (* -1 r)
        r)))

(defun mm->a (n u)
  "mean motion to semi-major axis:  a = [ u / (2pn)2 ] 1/3"
  (expt (/ u (* 2 pi n 2 pi n)) 1/3))

(defun a->mm (a u)
  "semi-major axis to mean motion:  n = sqrt(u/a3)/2p"
  (/ (sqrt (/ u (* a a a))) (* 2 pi)))

(defun perigee (a e)
  (* a (- 1 e)))

(defun R-at-t (p e n)
  "r(t) = [ P (1+e) ] / [ 1+ecosn(t) ]"
  (/ (* p (+ 1 e))
     (+ 1 (* (cos n) e))))

(defun rot1 (a alpha)
  (destructuring-bind (a1 a2 a3)
      a
    (list a1
          (+ (* a2 (cos alpha)) (* a3 (sin alpha))) ;    + a(2)*cos(alpha) + a(3)*sin(alpha);
          (- (* a3 (cos alpha)) (* a2 (sin alpha))))))  ;   - a(2)*sin(alpha) + a(3)*cos(alpha);

(defun rot3 (a gamma)
  (destructuring-bind (a1 a2 a3)
      a
    (list (+ (* a1 (cos gamma)) (* a2 (sin gamma))) ;a(1)*cos(gamma) + a(2)*sin(gamma) + 0 
          (+ (* -1 a1 (sin gamma)) (* a2 (cos gamma))) ;-a(1)*sin(gamma) + a(2)*cos(gamma)
          a3)))
                                      
(defun safely (v &rest r)
  (labels ((check (v1 v2)
             (cond ((or (null v1)
                        (null v2)) nil)
                    (t (< (abs (- v1 v2)) 1d-15)))))
    (cond ((null r) v)
          ((equal r '(nil)) v)
          ((atom r) (if (check r v)
                        (+ v 1d-6)
                        v))
          ((listp (car r)) (if (check (caar r) v)
                               (+ v 1d-6)
                               (safely v (cdr (car r)))))
          (t (if (check (car r) v)
                 (+ v 1d-6)
                 (safely v (cdr r)))))))
 

(defun k->c (u-i alpha-i e-i i-i Ω-i w-i m01-i dt-i)
  "Convert 6 element orbital elements to xyz format, at time dt from epoch"
  (let* ((u (coerce u-i 'double-float))
         (alpha (coerce alpha-i 'double-float))
         (e (coerce e-i 'double-float))
         (i (safely (coerce i-i 'double-float) 0d0))
         (Ω (norm-rev (safely (coerce Ω-i 'double-float) 0d0 pi (/ pi 2))))
         (w (norm-rev (safely (coerce w-i 'double-float) 0d0 pi (/ pi 2))))
         (m01 (norm-rev (safely (coerce m01-i 'double-float) 0d0 pi (/ pi 2))))
         (m0 (- (* 2 pi) m01))
         (dt (coerce dt-i 'double-float))
         (p (* alpha (- 1 (* e e)))) ; semi latus rectum
         (n (a->mm alpha u)) ; revolutions per day
         (Mt (M0->Mt m0 dt n)) ; Mean anomaly at t
         (Et (M->E Mt e)) ; Eccentric anomaly at t
         (Nt (E->N Et e m0)) ; true anomaly at t
         (RPQW1 (/ (* p (cos Nt))
                   (+ 1 (* e (cos Nt)))))
         (RPQW2 (/ (* p (sin Nt))
                   (+ 1 (* e (cos Nt)))))
         (RPQW3 0)
         (pos (rot3 (rot1 (rot3 (list RPQW1 RPQW2 RPQW3) (* -1 w)) (* -1 i)) (* -1 Ω)))
         (VPQW1 (* -1 (sqrt (/ u p)) (sin Nt)))
         (VPQW2 (* (sqrt (/ u p)) (+ e (cos Nt))))
         (VPQW3 0)
         (vel (rot3 (rot1 (rot3 (list VPQW1 VPQW2 VPQW3) (* -1 w)) (* -1 i)) (* -1 Ω))))
    (destructuring-bind ((r1 r2 r3) (v1 v2 v3))
        (list pos vel)
      ;(format t "True anomaly: ~e~%" Nt)
      (list r1 r2 r3 v1 v2 v3))))

(defun xyz (&key (u 3.9860044d14) (alpha 7231745.57d0) (e 0.0010013d0)
              (i 1.7278131088726605d0) (node 3.1650290038147086d0)
              (periapsis 1.9892163514596604d0) (m0 4.297843721947936d0)
              (revs 0) (dt 0))
  (when (not (zerop revs))
    (setq alpha (n->a u revs)))
  (k->c u alpha e i node periapsis m0 dt))

(defun |#k-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr #\( (read-char stream)))
        ((char= #\} curr))
      (push curr chars))
    (push #\) chars)
    (apply #'k->c (read-from-string (coerce (nreverse chars) 'string) nil nil))))

(set-dispatch-macro-character #\{ #\k #'|#k-reader|)

; test support functions follow

(defun 2.2.4.1 ()
  "Page 50 from Statistical Orbit Determination"
  (display-6e '{c3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0}))

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

;(defun run-test (u alpha e i Ω w m0)
;  (declare (type long-float u alpha e i Ω w m0))
;  ; first, convert to xyz
;  (destructuring-bind (x y z dx dy dz)
;      (xyz u alpha e i Ω w m0 0d0)
;  ; then back to 6e
;    (destructuring-bind (n-alpha n-e n-i n-Ω n-w n-m0)
;  ; then compare before and after to see how different they are
;        (6e u x y z dx dy dz)
;      (loop for x in (diffs (list alpha e i Ω w m0)
;                            (list n-alpha n-e n-i n-Ω n-w n-m0))
;         collect (< x .001)))))
;
;(defun run-show-test (u alpha e i Ω w m0)
;  (declare (type long-float u alpha e i Ω w m0))
;  (format t "i: alpha = ~f e = ~f i = ~f Ω = ~f w = ~f m0 = ~f~%" alpha e (radians->degrees i) (radians->degrees Ω) (radians->degrees w) (radians->degrees m0))
;  ; first, convert to xyz
;  (destructuring-bind (x y z dx dy dz)
;      (xyz u alpha e i Ω w m0 0d0)
;  ; then back to 6e
;    (format t "c: x=~f y=~f z=~f dx=~f dy=~f dz=~f~%" x y z dx dy dz)
;    (destructuring-bind (n-alpha n-e n-i n-Ω n-w n-m0)
;  ; then compare before and after to see how different they are
;        (6e u x y z dx dy dz)
;
;      (format t "k: n-alpha = ~f n-e = ~f n-i = ~f n-O = ~f n-W = ~f n-M0 = ~f~%" n-alpha n-e (radians->degrees n-i) (radians->degrees n-Ω) (radians->degrees n-w) (radians->degrees n-m0))
;      (loop for x in (diffs (list alpha e i Ω w m0)
;                            (list n-alpha n-e n-i n-Ω n-w n-m0))
;         collect (< x .001)))))
;
;(defun test ()
;  (destructuring-bind (u (alpha e i Ω w m0))
;      (list 3.9860044d14 (6e 3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0))
;    (declare (ignore m0))
;    (loop for j from 175 to 185
;       for y = (list 0 0 0 0 0) then x
;       for x = (xyz u alpha e i Ω w (degrees->radians j) 0d0)
;       for z = (mapcar #'- x y)
;       collect z)))
;                     
;(defun test2 (a b)
;  (destructuring-bind (u (alpha e i Ω w m0))
;      (list 3.9860044d14 (6e 3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0))
;    (declare (ignore m0))
;    (loop for j from a to b
;       collect (run-show-test u alpha e i Ω w (degrees->radians j)))))
;
;(defun test3 (a b)
;  (destructuring-bind (u (alpha e i Ω w m0))
;      (list 3.9860044d14 (6e 3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0))
;    (declare (ignore m0))
;    (with-open-file (s "data.csv" :direction :output :if-exists :supersede)
;      (loop for (n (x y z vx vy vz)) in 
;           (loop for j from a to b
;              collect (list j (xyz u alpha e i Ω w (degrees->radians j) 0d0)))
;         do (format s "~a,~a,~a,~a,~a,~a,~a~%" n x y z vx vy vz)))))
;      
;(defun test-i-1 (a b)
;  (destructuring-bind (u (alpha e i Ω w m0))
;      (list 3.9860044d14 (6e 3.9860044d14 5492000.34d0 3984001.4d0 2955.81d0 -3931.046491d0 5498.676921d0 3665.980697d0))
;    (declare (ignore i))
;    (loop for j from a to b
;       collect (run-show-test u alpha e (degrees->radians j) Ω w m0))))
;  
;(defun random-test (&key (a 7231745.57d0) (e .05d0) (u 3.9860044d14))
;  (let ((omega (random 360))
;        (w (random 360))
;        (m0 (random 360))
;        (i (random 180)))
;    (list a e i omega w m0 (run-show-test u a e (degrees->radians i) (degrees->radians omega)
;                   (degrees->radians w) (degrees->radians m0)))))
;  
;  
