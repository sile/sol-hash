(in-package :asdf)

(defsystem sol-hash
  :name "sol-hash"
  :version "0.1.0"
  :author "Takeru Ohta"
  :description "A hash table implementation using Split-Ordered-Lists"
  
  :serial t

  :components ((:file "package")
               (:file "sol-hash")))
