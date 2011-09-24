(in-package :sol-hash)

(declaim (inline bucket-id ordinary-hash sentinel-hash
                 bit-reverse parent-id
                 find-candidate find-node
                 set-impl make-node))

(defstruct node
  (hash 0 :type hashcode)
  (next nil :type (or null base-node))
  (key t :type t)
  (value t :type t))

(defstruct hashmap
  (buckets #() :type (simple-array (or null node)))
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
  (declare #.*fastest*)
  (let ((h (ldb (byte +HASHCODE_BITLEN+ 0) (funcall (hashmap-hash map) x))))
    (values (dpb 1 (byte 1 0) (bit-reverse h))
            (bucket-id h map))))

(defun sentinel-hash (x)
  (declare (hashcode x)
           #.*fastest*)
  (bit-reverse x))

(defun make (&key (size 4) (hash #'sxhash) (test #'eql))
  (let* ((bitlen (ceiling (log size 2)))
         (bucket (make-array (expt 2 bitlen) :initial-element '())))
    (make-hashmap :hash hash
                  :test test
                  :bitlen bitlen
                  :buckets bucket)))

(defun find-candidate (hash head)
  (declare (hashcode hash)
           #.*fastest*)
  (labels ((recur (pred cur)
             (if (and cur (> hash (node-hash cur)))
                 (recur cur (node-next cur))
               (values pred cur))))
    (recur nil head)))

(defun parent-id (id bitlen)
  (declare #.*fastest*
           (positive-fixnum id)
           ((mod #.+HASHCODE_BITLEN+) bitlen))
  (if (zerop id)
      0
    (let* ((r-id (bit-reverse id))
           (r-parent-id (1- r-id))
           (parent-id (bit-reverse r-parent-id)))
      (ldb (byte bitlen 0) parent-id))))

(defun get-bucket-from-id (id map)
  (declare #.*fastest*
           (positive-fixnum id))
  (with-slots (buckets bitlen) (the hashmap map)
    (if #1=(aref buckets id)
        #1#
      (when (plusp id)
        (let* ((parent-id (parent-id id bitlen))
               (parent (get-bucket-from-id parent-id map))
               (hashcode (sentinel-hash id)))
          (multiple-value-bind (pred cur)
                               (find-candidate hashcode parent)
            (when cur
              (if (null pred)
                  (setf (aref buckets parent-id) nil)
                (setf (node-next pred) nil))
              (setf #1# cur))))))))

(defun find-node (key map)
  (declare #.*fastest*)
  (with-slots (test) (the hashmap map)
    (multiple-value-bind (hash id) (ordinary-hash key map)
      (declare (hashcode hash))
      (multiple-value-bind (pred cur)
                           (find-candidate hash (get-bucket-from-id id map))
        (labels ((recur (pred cur)
                   (if (or (null cur) (/= hash (node-hash cur)))
                       (values nil pred cur hash id)
                     (if (funcall test key (node-key cur))
                         (values t pred cur hash id)
                       (recur cur (node-next cur))))))
          (recur pred cur))))))

(defun get (key map &optional default)
  (declare #.*fastest*)
  (multiple-value-bind (exists? pred node) (find-node key map)
    (declare (ignore pred))
    (if exists?
        (values (node-value node) t)
      (values default nil))))

(defun resize (map)
  (declare #.*fastest*)
  (with-slots (buckets bitlen) (the hashmap map)
    (incf bitlen)
    (setf buckets (adjust-array buckets (expt 2 bitlen)
                                :initial-element '()))))

(defun resize2 (map)
  (declare #.*fastest*)
  (with-slots (buckets bitlen) (the hashmap map)
    (decf bitlen)
    (loop FOR i fixnum FROM (expt 2 bitlen) BELOW (expt 2 (1+ bitlen))
      DO
      (loop FOR node = (aref buckets i) THEN (node-next node)
            WHILE node
        DO
        (set-impl (node-value node) (node-key node) map nil)))
    (setf buckets (adjust-array buckets (expt 2 bitlen)
                                :initial-element '()))))

(defun set-impl (new-value key map &optional (resize t))
  (declare #.*fastest*)
  (multiple-value-bind (exists? pred node hash id) (find-node key map)
    (if exists?
        (setf (node-value node) new-value)
      (with-slots (count buckets) (the hashmap map)
        (when resize
         (when (> (the positive-fixnum (incf count))
                 (locally  #|xxx|#
                  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                  (the positive-fixnum 
                      (ceiling (* 0.75 
                                  (the positive-fixnum (length buckets)))))))
          (resize map)))
               
        (let ((new-node (make-node :hash hash :key key :value new-value
                                   :next node)))
          (if (null pred)
              (setf (aref buckets id) new-node)
            (setf (node-next pred) new-node))
          new-value)))))
          
(defun (setf get) (new-value key map)
  (declare #.*fastest*)
  (set-impl new-value key map))

(defun count (map)
  (declare #.*fastest*)
  (hashmap-count map))

#+C
(defmacro each ((key value map &optional return-form) &body body)
  (let ((node (gensym)))
    `(loop FOR ,node = (hashmap-head ,map) THEN (node-next ,node)
           WHILE ,node
           WHEN (typep ,node 'node)
       DO
       (let ((,key (node-key ,node))
             (,value (node-value ,node)))
         ,@body)
       FINALLY
       (return ,return-form))))

#+C
(defun map (fn map &aux acc)
  (each (k v map (nreverse acc))
    (push (funcall fn k v) acc)))

(defun remove (key map)
  (declare #.*fastest*)
  (multiple-value-bind (exists? pred node hash id) (find-node key map)
    (declare (ignore hash))
    (when exists?
      (if pred
          (setf (node-next pred) (node-next node))
        (setf (aref (hashmap-buckets map) id) (node-next node)))

      (when (< (decf (hashmap-count map))
               (ceiling (* 0.75 (length (hashmap-buckets map)))))
        (when (> (hashmap-count map) 4) ;xxx
          (resize2 map)))
      t)))
