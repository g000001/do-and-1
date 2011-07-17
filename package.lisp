;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :do-and-1
  (:use)
  (:export :do&
           :&return
           :&pop
           :&push
           :&aref
           :&only-if))

(defpackage :do-and-1-internal
  (:use :do-and-1
        :cl
        :fiveam
        :fare-matcher))

