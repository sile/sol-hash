(in-package :sol-hash)

(declaim (inline bucket-id ordinary-hash sentinel-hash
                 bit-reverse parent-id
                 find-candidate find-node
                 set-impl
                 ))

(defstruct node
  (hash 0 :type hashcode)
  (key t :type t)
  (value t :type t))

(defstruct hashmap
  (head '() :type list)  ; list of ndoe
  (buckets #() :type (vector list))
  (bitlen 0 :type positive-fixnum)
  (count 0 :type positive-fixnum)
  
  (hash t :type hash-fn)
  (test t :type test-fn))

(defun bit-reverse (n)
  (declare (hashcode n)
           #.*fastest*)
  (setf n (logior (ash (logand #x55555555 n)  1)
                  (ash (logand #xAAAAAAAA n) -1))
        n (logior (ash (logand #x33333333 n)  2)
                  (ash (logand #xCCCCCCCC n) -2))
        n (logior (ash (logand #x0F0F0F0F n)  4)
                  (ash (logand #xF0F0F0F0 n) -4)))
  (logior (ash (logand n #x000000FF) 24)
          (ash (logand n #x0000FF00) 8)
          (ash (logand n #x00FF0000) -8)
          (ash n -24)))

(defun bucket-id (hash map)
  (declare (hashcode hash)
           #.*fastest*)
  (ldb (byte (the (mod #.+HASHCODE_BITLEN+) (hashmap-bitlen map)) 0) hash))

(defun ordinary-hash (x map)
  (declare (hashcode x)
           #.*fastest*)
  (let ((h (ldb (byte +HASHCODE_BITLEN+ 0) (funcall (hashmap-hash map) x))))
    (values (dpb 1 (byte 2 0) (bit-reverse h))
            (bucket-id h map))))

(defun sentinel-hash (x)
  (declare (hashcode x)
           #.*fastest*)
  (bit-reverse x))


(defconstant +MAX_HASHCODE+ (1- (ash 1 +HASHCODE_BITLEN+)))

(defun make (&key (size 4) (hash #'sxhash) (test #'eql))
  (let* ((bitlen (ceiling (log size 2)))
         (head (list (make-node :hash 0 :key *sentinel*)
                     (make-node :hash +MAX_HASHCODE+ :key *sentinel*)))
         (bucket (make-array (expt 2 bitlen) :initial-element '())))
    (make-hashmap :hash hash
                  :test test
                  :head head
                  :bitlen bitlen
                  :buckets (progn (setf (aref bucket 0) head)
                                  bucket))))

(defun find-candidate (hash head)
  (declare (hashcode hash)
           #.*fastest*)
  (labels ((recur (pred &aux (cur (second pred)))
             (if (> hash (node-hash cur))
                 (recur (cdr pred))
               pred)))
    (recur head)))

(defun parent-id (id)
  (declare #.*fastest*
           (positive-fixnum id))
  (dpb 0 (byte 1 (1- (integer-length id))) id))

(defun get-bucket-from-id (id map)
  (with-slots (buckets) (the hashmap map)
    (if #1=(aref buckets id)
        #1#
      (let* ((parent (get-bucket-from-id (parent-id id) map))
             (hashcode (sentinel-hash id))
             (pred (find-candidate hashcode parent)))
        (setf (cdr pred) (cons (make-node :hash hashcode :key *sentinel*
                                          :value (- id))
                               (cdr pred))
              #1# (cdr pred))))))

(defun find-node (key map)
  (declare #.*fastest*)
  (with-slots (test) (the hashmap map)
    (multiple-value-bind (hash id) (ordinary-hash key map)
      (declare (hashcode hash))
      (let* ((pred (find-candidate hash (get-bucket-from-id id map)))
             (x (second pred)))
        (values pred x (and (= hash (node-hash x))
                            (funcall test key (node-key x)))
                hash)))))

(defun get (key map)
  (declare #.*fastest*)
  (multiple-value-bind (pred node exists? hash) (find-node key map)
    (declare (ignore pred hash))
    (if exists?
        (values (node-value node) t)
      (values nil nil))))

(defun resize (map)
  (declare #.*fastest*)
  (with-slots (buckets bitlen) (the hashmap map)
    (incf bitlen)
    (setf buckets (adjust-array buckets (expt 2 bitlen)
                                :initial-element '()))))

(defun set-impl (new-value key map)
  (declare #.*fastest*)
  (multiple-value-bind (pred node exists? hash) (find-node key map)
    (if exists?
        (setf (node-value node) new-value)
      (with-slots (count buckets) (the hashmap map)
        (when (> (the positive-fixnum (incf count))
                 (the positive-fixnum (length buckets)))
          (resize map))
        (setf (cdr pred) (cons (make-node :hash hash :key key 
                                          :value new-value)
                               (cdr pred)))
        new-value))))
          
(defun (setf get) (new-value key map)
  (declare #.*fastest*)
  (set-impl new-value key map))

(defun count (map)
  (declare #.*fastest*)
  (hashmap-count map))

(defun map (fn map)
  (loop FOR node IN (hashmap-head map)
        UNLESS (eq *sentinel* (node-key node))
    COLLECT (funcall fn (node-key node) (node-value node))))

(defmacro each ((key value map &optional return-form) &body body)
  (let ((node (gensym)))
    `(loop FOR ,node IN (hashmap-head ,map)
           FOR ,key = (node-key ,node)
           FOR ,value = (node-value ,node)
           UNLESS (eq *sentinel* ,key)
       DO
       (locally ,@body)
       FINALLY
       (return ,return-form))))

(defun remove (key map)
  (multiple-value-bind (pred node exists? hash) (find-node key map)
    (declare (ignore node hash))
    (when exists?
      (decf (hashmap-count map))
      (setf (cdr pred) (cddr pred))
      t)))
