(defpackage sol-hash
  (:use :common-lisp)
  (:shadow :common-lisp get remove map count)
  (:export make
           get
           remove
           clear
           count
           map
           each
           test-name
           
           generate-test
           define-test
           undef-test))
(in-package :sol-hash)

(deftype positive-fixnum () '(unsigned-byte #.(integer-length most-positive-fixnum)))
(deftype hashcode-width () '(mod 33))
(deftype hashcode () '(unsigned-byte 32))
(deftype bucket () 'node)
(deftype buckets () '(simple-array bucket))
(deftype set-fn () '(function (t t map) t))
(deftype get-fn () '(function (t map t) (values t boolean)))
(deftype rem-fn () '(function (t map) boolean))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (defparameter *interface* '(optimize (speed 3) (safety 2) (debug 1)))
  (defparameter *normal* '(optimize (speed 1) (safety 3) (debug 2)))
  (defparameter *muffle-note* #-SBCL '(t t)
                              #+SBCL '(sb-ext:muffle-conditions sb-ext:compiler-note))
  (defconstant +HASHCODE_BITLEN+ 32)
  (defconstant +MAX_HASHCODE+ (1- (ash 1 32))))

(defparameter *test-repository* (make-hash-table :test #'eq))
