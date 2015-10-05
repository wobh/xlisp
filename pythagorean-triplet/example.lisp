(defpackage #:triplet
  (:use #:cl)
  (:export #:primitive-triplets #:triplets-from #:pythagorean-p))

(in-package #:triplet)

(defun find-nn-mm (a b c)
  (assert (pythagorean-p (list a b c)))
  (min a b c))

(defun find-2*m*n (a b c)
  (assert (pythagorean-p (list a b c)))
  (second (sort #'< (list a b c))))

(defun find-mm+nn (a b c)
  (assert (pythagorean-p (list a b c)))
  (max a b c))

(defstruct (triplet (:type list)
                    (:conc-name nil)
                    (:constructor make-triplet-with-sides
                                  (a b c &aux
                                     (nn-mm (find-nn-mm a b c))
                                     (2*m*n (find-2*m*n a b c))
                                     (mm+nn (find-mm+nn a b c))))
                    (:constructor make-triplet-with-euclid
                                  (m n &optional (k 1) &aux
                                     (nn-mm (* k (- (* n n) (* m m))))
                                     (2*m*n (* k 2 m n))
                                     (mm+nn (* k (+ (* m m) (* n n)))))))
  "Pythagorean triple."
  (nn-mm 2 :type rational :read-only t)
  (2*m*n 3 :type rational :read-only t)
  (mm+nn 5 :type rational :read-only t))

(defun pythagorean-p (seq)
  "True if SEQUENCE elements are a Pythagorean triple."
  (error "Not implemented."))

(defun primitive-p (seq)
  "True if SEQUENCE elements are a primitive Pythagorean triple."
  (error "Not implemented."))

(defmethod perimeter ((triplet triplet))
  "Sum of the sides lengths of triplet."
  (reduce #'+ triplet))

(defun primitive-triplets (n)
  "A LIST of all primitive Pythagorean triplets having N."
  (error "Not implemented."))

(defun triplets-in-range (min max)
  "A LIST of all primitive Pythagorean triplets with MIN <= {a,b,c} <= MAX."
  (error "Not implemented."))

