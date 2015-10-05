(defpackage #:triplet
  (:use #:cl)
  (:export #:primitive-triplets #:triplets-from #:pythagorean-p))

(in-package #:triplet)

(defun primitive-triplets (n)
  "A LIST of all primitive Pythagorean triplets having N."
  (error "Not implemented."))

(defun triplets-in-range (min max)
  "A LIST of all primitive Pythagorean triplets with MIN <= {a,b,c} <= MAX."
  (error "Not implemented."))

(defun pythagorean-p (seq)
  "True if SEQUENCE elements are a primitive Pythagorean triple."
  (error "Not implemented."))
