(in-package :sol-hash)

#+C
(declaim (inline bucket-id ordinary-hash sentinel-hash
                 bit-reverse parent-id
                 find-candidate find-node
                 set-impl make-node))

;;;;;;;;;;
;;; struct
(defstruct node
  (next nil :type (or null node))
  (hash   0 :type hashcode)
  (key    t :type t)
  (value  t :type t))

(defstruct map
  (buckets #() :type (simple-array (or null node)))
  (bitlen    0 :type positive-fixnum)
  (count     0 :type positive-fixnum)
  (upper-border 0 :type positive-fixnum) 
  (lower-border 0 :type positive-fixnum) 
  (hash-fn   t :type hash-fn)
  (test-fn   t :type test-fn))

;; TODO: print-object

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun bit-reverse (n)
  (declare (hashcode n))
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

(defun lower-bits (width n)
  (declare (hashcode n)
           (hashcode-width width))
  (ldb (byte width 0) n))

(defun upper-bits (width n)
  (declare (hashcode n)
           (hashcode-width width))
  (ldb (byte width (- +HASHCODE_BITLEN+ width)) n))

(defun hashcode-and-bucket-id (key map)
  (with-slots (hash-fn bitlen) (the map map)
    (let* ((h1 (lower-bits +HASHCODE_BITLEN+ (funcall hash-fn key)))
           (h2 (bit-reverse h1)))
      (values h2                         ; key's hashcode
              (lower-bits bitlen h1)     ; bucket-id
              (upper-bits bitlen h2))))) ; reverse bucket-id

(defun bucket-hashcode (bucket-id)
  (bit-reverse bucket-id))

(defun predecessor-id (reverse-bucket-id bitlen)
  (declare (hashcode reverse-bucket-id)
           (hashcode-width bitlen))
  (if (zerop reverse-bucket-id)
      0
    (let* ((r-predecessor-id (1- reverse-bucket-id))
           (bit-offset (- +HASHCODE_BITLEN+ bitlen))
           (predecessor-id (bit-reverse (ash r-predecessor-id bit-offset))))
      (values predecessor-id
              r-predecessor-id))))

(defun parent-id (bucket-id)
  (let ((i (1- (integer-length bucket-id))))
    (dpb 0 (byte 1 i) bucket-id)))

(defun find-candidate (hash head)
  (declare (hashcode hash))
  (labels ((recur (pred cur)
             (if (and cur (> hash (node-hash cur)))
                 (recur cur (node-next cur))
               (values pred cur))))
    (recur nil head)))

(defun set-pred-next (pred-node buckets bucket-id &key next)
  (declare ((simple-array (or null node)) buckets)
           (positive-fixnum bucket-id)
           ((or null node) pred-node next))
  (if pred-node
      (setf (node-next pred-node) next)
    (setf (aref buckets bucket-id) next)))

(defun get-bucket (bucket-id reverse-id map)
  (with-slots (buckets bitlen) (the map map)
    (if (aref buckets bucket-id)
        (aref buckets bucket-id)
      (when (plusp bucket-id)
        (multiple-value-bind (pred-bucket-id pred-reverse-id)
                             (predecessor-id reverse-id bitlen)
          (let ((pred-bucket (get-bucket pred-bucket-id pred-reverse-id map))
                (bucket-hash (bucket-hashcode bucket-id)))
          (multiple-value-bind (pred node)
                               (find-candidate bucket-hash pred-bucket)
            (when node
              (set-pred-next pred buckets pred-bucket-id :next nil)
              (setf (aref buckets bucket-id) node)))))))))

(defun find-node (key map)
  (with-slots (test-fn) (the map map)
    (multiple-value-bind (hash id rev-id) (hashcode-and-bucket-id key map)
      (multiple-value-bind (pred cur)
                           (find-candidate hash (get-bucket id rev-id map))
        (labels ((recur (pred cur)
                   (if (or (null cur) (/= hash (node-hash cur)))
                       (values nil cur pred id hash)
                     (if (funcall test-fn key (node-key cur))
                         (values t cur pred id hash)
                       (recur cur (node-next cur))))))
          (recur pred cur))))))

(defmacro each-bucket ((head-node bucket-id map &key (start 0) end) &body body)
  (let ((buckets (gensym)))
    `(with-slots ((,buckets buckets)) (the map ,map)
       (loop FOR ,bucket-id fixnum FROM (1- ,(or end `(length ,buckets))) 
                                   DOWNTO ,start
             FOR ,head-node = (aref ,buckets ,bucket-id) 
             WHEN ,head-node
         DO
         (locally ,@body)))))

(defun upsize (map)
  (with-slots (buckets bitlen lower-border upper-border) (the map map)
    (incf bitlen)
    (let ((new-size (* 2 (length buckets))))
      (setf buckets (adjust-array buckets new-size :initial-element '())
            (values lower-border upper-border) (calc-borders new-size)))))

(defun rehash-bucket (head bucket-id map)
  (let ((tail (loop FOR node = head THEN next
                    FOR next = (node-next node)
                    WHILE next
                    FINALLY (return node)))
        (parent-id (parent-id bucket-id)))

    (with-slots (buckets) (the map map)  
      (multiple-value-bind (pred succ)
                           (find-candidate (node-hash head) 
                                           (aref buckets parent-id))
        (set-pred-next pred buckets parent-id :next head)
        (setf (node-next tail) succ)))))

(defun downsize (map)
  (with-slots (buckets bitlen lower-border upper-border) (the map map)
    (decf bitlen)

    (let ((new-size (floor (length buckets) 2)))
      (each-bucket (head bucket-id map :start new-size)
        (rehash-bucket head bucket-id map))

      (setf buckets (adjust-array buckets new-size)
            (values lower-border upper-border) (calc-borders new-size)))))

(defun calc-borders (size)
  (values (floor (* 0.4 size))
          (floor (* 0.9 size))))

(defun make (&key (size 4) (hash #'sxhash) (test #'eql))
  (let* ((bitlen (ceiling (log (max 2 size) 2)))
         (buckets (make-array (expt 2 bitlen) :initial-element '())))
    (multiple-value-bind (lower upper) (calc-borders (length buckets))
      (make-map :hash-fn hash
                :test-fn test
                :bitlen bitlen
                :upper-border upper
                :lower-border lower
                :buckets buckets))))

(defun get (key map &optional default)
  (multiple-value-bind (exists? node) (find-node key map)
    (if exists?
        (values (node-value node) t)
      (values default nil))))

(defun set-impl (new-value key map)
  (multiple-value-bind (exists? node pred bucket-id hash) (find-node key map)
    (if exists?
        (setf (node-value node) new-value)
      (with-slots (buckets count upper-border) (the map map)
         (when (> (incf count) upper-border)
           (upsize map))

         (set-pred-next pred buckets bucket-id 
                        :next (make-node :key key :value new-value
                                         :hash hash :next node))
         new-value))))
          
(defun (setf get) (new-value key map)
  (set-impl new-value key map))

(defun count (map)
  (map-count map))

(defun remove (key map)
  (multiple-value-bind (exists? node pred bucket-id) (find-node key map)
    (when exists?
      (with-slots (buckets count lower-border) (the map map)
        (set-pred-next pred buckets bucket-id :next (node-next node))
        (when (< (decf count) lower-border)
          (downsize map)))
      t)))

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


;; TODO: hash-fnとtest-fnをinline展開できる方法も用意する