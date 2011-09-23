(in-package :sol-hash)

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

(declaim (ftype hash-fn hash))
(defun hash (x)
  (loop WITH h OF-TYPE hashcode = (sxhash x)
        FOR i FROM 0 BELOW +FIXNUM_BITLEN+ BY 8
    DO 
    (setf h (+ (ash (ldb (byte 8 0) h) #.(- +FIXNUM_BITLEN+ 8))
               (* (ash h -8) 13)))
    FINALLY
    (return h)))

(defun ordinary-hash (x map)
  (dpb 1 (byte 2 0) (funcall (hashmap-hash map) x)))

(defun sentinel-hash (x)
  (loop FOR i FROM 0 BELOW (integer-length x)
        FOR j FROM #.(1- +FIXNUM_BITLEN+) DOWNTO 0
        WHILE (< i j)
    DO (rotatef (ldb (byte 1 i) x) 
                (ldb (byte 1 j) x))
    FINALLY
    (return x)))

(defun make (&key (size 4) (hash #'hash) (test #'eql))
  (let* ((bitlen (ceiling (log size 2)))
         (head (list (make-node :hash 0 :key *sentinel*)
                     (make-node :hash most-positive-fixnum :key *sentinel*)))
         (bucket (make-array (expt bitlen 2) :initial-element '())))
    (make-hashmap :hash hash
                  :test test
                  :head head
                  :bitlen bitlen
                  :buckets (progn (setf (aref bucket 0) head)
                                  bucket))))

(defun find-candidate (hash head)
  (labels ((recur (pred &aux (cur (second pred)))
             (if (> hash (node-hash cur))
                 (recur (cdr pred))
               pred)))
    (recur head)))

(defun parent-id (id)
  (dpb 0 (byte 1 (1- (integer-length id))) id))

(defun get-bucket-from-id (id map)
  (with-slots (buckets) (the hashmap map)
    (if #1=(aref buckets id)
        #1#
      (let* ((parent (get-bucket-from-id (parent-id id) map))
             (hashcode (sentinel-hash id))
             (pred (find-candidate hashcode parent)))
        (setf (cdr pred) (cons (make-node :hash hashcode :key *sentinel*)
                               (cddr pred))
              #1# (cdr pred))))))

(defun bucket-id (hash bitlen)
  (ldb (byte bitlen (- +FIXNUM_BITLEN+ bitlen)) hash))

(defun get-bucket (hash map)
  (with-slots (bitlen) (the hashmap hash)
    (get-bucket-from-id (bucket-id hash bitlen) map)))

(defun find-node (key hash map)
  (with-slots (test) (the hashmap map)
    (let* ((pred (find-candidate hash (get-bucket hash map)))
           (x (second pred)))
      (values pred x (and (= hash (node-hash x))
                          (funcall test key (node-key x)))))))

(defun get (key map &aux (hash (ordinary-hash key map)))
  (multiple-value-bind (pred node exists?) (find-node key hash map)
    (declare (ignore pred))
    (if exists?
        (values (node-value node) t)
      (values nil nil))))

(defun resize (map)
  (with-slots (buckets bitlen) (the hashmap map)
    (incf bitlen)
    (setf buckets (adjust-array buckets (expt 2 bitlen) :initial-element '()))))

(defun set-impl (new-value key map &aux (hash (ordinary-hash key map)))
  (multiple-value-bind (pred node exists?) (find-node key hash map)
    (if exists?
        (setf (node-value node) new-value)
      (with-slots (count buckets) (the hashmap map)
        (when (> (incf count) (length buckets))
          (resize map))
        (setf (cdr pred) (cons (make-node :hash hash :key key :value new-value)
                               (cddr pred)))
        new-value))))
          
(defun (setf get) (new-value key map)
  (set-impl new-value key map))

(defun count (map)
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

(defun remove (key map &aux (hash (ordinary-hash key map)))
  (multiple-value-bind (pred node exists?) (find-node key hash map)
    (declare (ignore node))
    (when exists?
      (setf (cdr pred) (cddr pred))
      t)))
