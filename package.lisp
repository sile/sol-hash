(defpackage sol-hash
  (:use :common-lisp)
  (:shadow :common-lisp get remove map count)
  (:export make
           get
           remove
           count
           map
           each))
(in-package :sol-hash)

(deftype positive-fixnum () '(unsigned-byte #.(integer-length most-positive-fixnum)))
(deftype hashcode-width () '(mod 33))
(deftype hashcode () '(unsigned-byte 32))
(deftype hash-fn () '(function (t) hashcode))
(deftype test-fn () '(function (t t) boolean))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
  (defconstant +HASHCODE_BITLEN+ 32)
  (defconstant +MAX_HASHCODE+ (1- (ash 1 32))))
