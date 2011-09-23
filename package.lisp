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

(defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0)))

(defconstant +FIXNUM_BITLEN+ (integer-length most-positive-fixnum))

(deftype positive-fixnum () '(unsigned-byte #.(integer-length most-positive-fixnum)))
(deftype hashcode () 'positive-fixnum)
(deftype hash-fn () '(function (t) hashcode))
(deftype test-fn () '(function (t t) boolean))

(defparameter *masks*
  (loop REPEAT 4 COLLECT (random most-positive-fixnum)))

(defparameter *SENTINEL* '#:sentinel)
