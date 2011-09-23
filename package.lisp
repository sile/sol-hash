(defpackage sol-hash
  (:use :common-lisp)
  (:shadow :common-lisp get remove map)
  (:export sol-hash
           make
           get
           remove
           map))
(in-package :sol-hash)


(defconstant +FIXNUM_BITLEN+ (integer-length most-positive-fixnum))

(deftype hashcode () '(unsigned-byte #.(integer-length most-positive-fixnum)))
