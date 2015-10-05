(ql:quickload "lisp-unit")

(defpackage #:pythagorean-triplet-test
  (:use #:cl #:lisp-unit))

#-xlisp-test(load "triplet")

(in-package #:pythagorean-triplet-test)

(define-test triplets-with-4
  (assert-equal '((3 4 5)) (triplet::primitive-triplets 4)))

(define-test triplets-with-84
  (assert-equal '((13 84 85) (84 187 205) (84 437 445) (84 1763 1765))
                (triplet::primitive-triplets 84)))

(define-test triplets-with-288
  (assert-equal '((175 288 337) (288 20735 20737))
                (triplet::primitive-triplets 288)))

(define-test triplets-with-420
  (assert-equal '((29 420 421) (341 420 541) (420 851 949)
                  (420 1189 1261) (420 1739 1789) (420 4891 4909)
                  (420 11021 11029) (420 44099 44101))
                (triplet::primitive-triplets 420)))

(define-test primitive-triplets-5
  (assert-error 'type-error (triplet::primitive-triplets 5)))

(define-test triplets-from-1-to-10
  (assert-equal '((3 4 5) (6 8 10))
                (triplet::triplets-in-range 1 10)))

(define-test triplets-from-56-to-95
  (assert-equal '((57 76 95) (60 63 87))
                (triplet::triplets-in-range 56 95)))

(define-test pythagorean-p-29-20-21
  (assert-true (triplet::pythagorean-p '(29 20 21))))

(define-test pythagorean-p-25-25-1225
  (assert-false (triplet::pythagorean-p '(25 25 1225))))

(define-test pythagorean-p-924-43-925
  (assert-true (triplet::pythagorean-p '(924 43 925))))

#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :pythagorean-triplet-test))
