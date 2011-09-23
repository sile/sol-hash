(in-package :asdf)

(defsystem sol-hash
  :name "sol-hash"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A hash table implementation using Split-Ordered-Lists"

  :components ((:file "package")
               (:file "sol-hash")))
