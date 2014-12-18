#!/usr/local/bin/clisp
;; -*- mode: lisp; -*-

;;;; make-skeleton.lisp

;;;; AUTHORS:
;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION:
;;;; Generates skeleton for exercism exercise.

(cl:in-package #:cl-user)

(cl:defpackage #:xlisp-bones
  (:use #:cl))

(cl:in-package #:xlisp-bones)

(defconstant +usage+ "make-skeleton EXERCISM"
  "Usage string.")

(defparameter *bones-file* nil
  "Path to skeleton file.")

(defparameter *bones-head* ()
  "Skeleton header: in CL-USER, DEFPACKAGE.")

(defparameter *bones-body* ()
  "Skeleton definitions. List of definitions.")

(defun make-in-pkg (name)
  "Given name (STRING), create IN-PACKAGE definition."
  (list 'cl:in-package (make-symbol name)))

(defun make-bones-pkg (name
                       &options keys
                       &key (uses '(#:cl))
                         nicknames export
                         import documentation)
  "Given name (STRING) and DEFPACKAGE keys create bones head."
  (let ((bones (list
                (make-in-pkg "CL-USER")
                (list 'cl:defpackage (make-symbol name)))))
    (loop
      for (key . val) in keys
      do
        (when val
          (funcall #'bones-add-symbol key val)))
    (values bones)))

(defun get-bones-pkg (bones &optional name)
  "Return first list with first item of 'DEFPACKAGE."
  (if name
      (find name bones
            :test #'equalp
            :key #'second)
      (find 'defpackage bones :key #'first)))

(defun bones-add-symbol (bones keyw str &rest strs)
  "Given bones, keyword and strings, extend skeleton package definition."
  (let ((pkg (get-bones-pkg bones)))
    (cond ((member keyw '(:nicknames :use :export :import-from))
           (nconc pkg
                  (list
                   (list* keyw
                          (mapcar #'make-symbol (list* str strs))))))
          ((eq keyw :documentation)
           (nconc pkg
                  (list (list keyw str))))
          (t
           (error "Invalid package definition symbol: ~S." defn)))
    (values bones)))

(defun bones-add-definition (bones defn)
  "Given a definition, add it to the bones file (use *BONES-BODY*)."
  (nconc bones defn))

(defun print-bone (bone &optional (stream *standard-output*))
  "Print definition list."
  (let ((*package* (find-package '#:keyword))
        (*read-eval* nil)
        (custom:*print-symbol-package-prefix-shortest* t)
        (*print-readably* nil)
        (*print-escape* t)
        (*print-gensym* t)
        (*print-circle* t)
        (*print-right-margin* 24)
        (*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (pprint bone stream)))

(defun finish-bones (bones-file bones-head bones-body)
  "Write skeleton definitions to file."
  (with-open-file (bones-file :direction output)
    (dolist (bone bones-head)
      (print-bone bone))
    (print-bone (make-in-pkg (second (second bones-head))))
    (dolist (bone bones-body)
      (terpri bones-file)
      (print-bone bone))))

(defun make-referenced-file (filepath)
  "Restart: create bones file."
  (ensure-directories-exist filepath)
  (open filepath :direction :probe :if-does-not-exist :create)
  (setf *bones-file* filepath))

(defun make-referenced-package (name)
  "Restart: define, create package"
  (nconc *bones-head* (make-bones-pkg name))
  (make-package (make-symbol name)))

(defun add-exported-symbol (symbol &optional (defn-type 'function))
  "Restart: define, intern, and export symbol."
  (bones-add-exports *bones-head* symbol)
  (bones-add-definition *bones-body*
                        (case defn-type
                          (function
                           (list 'defun symbol "" ()))
                          (parameter
                           (list 'defparameter symbol () ""))))
  (let ((pkg (find-package (second (get-bones-pkg *bones-head*)))))
    (intern symbol pkg)
    (export symbol pkg)))

(defun load-make-references (file &aux package)
  "Load file, use restarts to continue."
  (handler-bind
      ((file-error (lambda (err)
                     (let ((ref-file (file-error-pathname err)))
                       (unless (probe-file ref-file)
                         (make-referenced-file ref-file)))))
       (package-error (lambda (err)
                        (let ((ref-pack (package-error-package err)))
                          (unless (find-package ref-pack)
                            (make-referenced-package) ref-pack))))
       (cell-error (lambda (err)
                     (let ((ref-symb (cell-error-name err)))
                       (unless (find-symbol ref-symb package)
                         (invoke-restart ref-symb))))))
    (load file)))

(defun main (file)
  (load-make-references file)
  (when *bones-file*
    (finish-bones *bones-file*)))

(main (first ext:*args*))
