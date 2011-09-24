(defpackage sol-hash
  (:use :common-lisp)
  (:shadow :common-lisp get remove map count)
  (:export hashmap
           make
           get
           remove
           count
           map
           each

           hash))
(in-package :sol-hash)

(deftype positive-fixnum () '(unsigned-byte #.(integer-length most-positive-fixnum)))
(deftype hashcode () '(unsigned-byte 32))
(deftype hash-fn () '(function (t) hashcode))
(deftype test-fn () '(function (t t) boolean))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *fastest* '(optimize (speed 0) (safety 3) (debug 3)))
  (defconstant +HASHCODE_BITLEN+ 32)
  (defconstant +MAX_HASHCODE+ (1- (ash 1 32))))
