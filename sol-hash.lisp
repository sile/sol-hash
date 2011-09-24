(in-package :sol-hash)

(declaim #.*fastest*
         (inline make get (setf get) remove map count

                 make-node node-next node-hash node-key node-value
                 make-map map-buckets map-bitlen map-count
                 map-upper-border map-lower-border map-hash-fn map-test-fn
                 
                 next sentinel sentinel? ordinal?
                 bit-reverse lower-bits mask-lower-bits
                 hashcode-and-bucket-id predecessor-id parent-id
                 find-candidate set-pred-next find-node
                 set-impl get-bucket rehash-bucket))

;;;;;;;;;;
;;; struct
(eval-when (:compile-toplevel :load-toplevel)
  (defstruct node
    (next nil :type (or null node))
    (hash   0 :type hashcode)
    (key    t :type t)
    (value  t :type t)))

(defstruct map
  (buckets    #() :type buckets)
  (bucket-size  0 :type positive-fixnum)
  (bitlen       0 :type positive-fixnum)
  (count        0 :type positive-fixnum)
  (upper-border 0 :type positive-fixnum) 
  (lower-border 0 :type positive-fixnum) 
  (hash-fn      t :type hash-fn)
  (test-fn      t :type test-fn))

(defmethod print-object ((o map) stream)
  (declare #.*normal*)
  (print-unreadable-object (o stream :type t :identity t)
    (with-slots (count) (the map o)
      (format stream "~s ~s" :count count))))

(eval-when (:compile-toplevel :load-toplevel)
  (unless (constantp '+SENTINEL+)
    (defconstant +SENTINEL+ (make-node :hash +MAX_HASHCODE+))))

(defun sentinel () +SENTINEL+) 
(defun sentinel? (node) (eq node (sentinel)))
(defun ordinal? (node) (not (sentinel? node)))
                

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun next (node)
  (declare (node node))
  (the node (node-next node)))

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

(defun mask-lower-bits (upper-bits-width n &aux (width upper-bits-width))
  (declare (hashcode n)
           (hashcode-width width))
  (dpb 0 (byte (- +HASHCODE_BITLEN+ width) 0) n))

(defun hashcode-and-bucket-id (key map)
  (with-slots (hash-fn bitlen) (the map map)
    (let* ((h1 (lower-bits +HASHCODE_BITLEN+ (funcall hash-fn key)))
           (h2 (bit-reverse h1)))
      (values h2                          ; key's hashcode
              (mask-lower-bits bitlen h2) ; bucket's hashcode
              (lower-bits bitlen h1)))))  ; bucket-id

(defun predecessor-id (bucket-hash bitlen)
  (declare (hashcode bucket-hash)
           (hashcode-width bitlen))
  (if (zerop bucket-hash)
      0
    (let* ((pred-hash (mask-lower-bits bitlen (1- bucket-hash)))
           (pred-id (bit-reverse pred-hash)))
      (values pred-id pred-hash))))

(defun parent-id (bucket-id)
  (declare (hashcode bucket-id))
  (let ((i (1- (integer-length bucket-id))))
    (dpb 0 (byte 1 i) bucket-id)))

(defun find-candidate (hash head)
  (declare (hashcode hash))
  (labels ((recur (pred cur)
             (if (> hash (node-hash cur))
                 (recur cur (next cur))
               (values pred cur))))
    (recur (sentinel) head)))

(defun set-pred-next (pred-node buckets bucket-id &key next)
  (declare (buckets buckets)
           (positive-fixnum bucket-id)
           (node pred-node next))
  (if (sentinel? pred-node)
      (setf (aref buckets bucket-id) next)
    (setf (node-next pred-node) next)))

(defun get-bucket-notinline (bucket-id bucket-hash map)
  (get-bucket bucket-id bucket-hash map))

(defun get-bucket (bucket-id bucket-hash map)
  (with-slots (buckets bitlen) (the map map)
    (if (or (ordinal? (aref buckets bucket-id))
            (zerop bucket-id))
        (aref buckets bucket-id)
      (multiple-value-bind (pred-bucket-id pred-bucket-hash)
                           (predecessor-id bucket-hash bitlen)
        (let ((pred-bucket (get-bucket-notinline
                            pred-bucket-id pred-bucket-hash map)))
          (multiple-value-bind (pred node)
                               (find-candidate bucket-hash pred-bucket)
            (set-pred-next pred buckets pred-bucket-id :next (sentinel))
            (setf (aref buckets bucket-id) node)))))))

(defun find-node (key map)
  (with-slots (test-fn) (the map map)
    (multiple-value-bind (hash bucket-hash bucket-id)
                         (hashcode-and-bucket-id key map)
      (declare (hashcode hash))
      (multiple-value-bind (pred cur)
                           (find-candidate hash 
                                           (get-bucket bucket-id bucket-hash map))
        (labels ((recur (pred cur)
                   (if (or (/= hash (node-hash cur))
                           (sentinel? cur))
                       (values nil cur pred bucket-id hash)
                     (if (funcall test-fn key (node-key cur))
                         (values t cur pred bucket-id hash)
                       (recur cur (next cur))))))
          (recur pred cur))))))

(defmacro each-bucket ((head-node bucket-id map &key (start 0) end return) 
                       &body body)
  (let ((buckets (gensym))
        (bucket-size (gensym)))
    `(with-slots ((,buckets buckets) (,bucket-size bucket-size)) (the map ,map)
       (loop FOR ,bucket-id fixnum FROM ,start BELOW ,(or end bucket-size)
             FOR ,head-node = (aref ,buckets ,bucket-id) 
             UNLESS (sentinel? ,head-node)
         DO
         (locally ,@body)
         FINALLY
         (return ,return)))))

(defun upsize (map)
  (with-slots (buckets bucket-size bitlen) (the map map)
    (incf bitlen)
    (let ((new-size (the positive-fixnum (* 2 bucket-size))))
      (update-size-and-borders map new-size)
      (setf buckets (adjust-array buckets new-size :element-type 'bucket
                                                   :initial-element (sentinel))))))

(defun rehash-bucket (head bucket-id map)
  (let ((tail (loop FOR node = head THEN next
                    FOR next = (next node)
                    UNTIL (sentinel? next)
                    FINALLY (return node)))
        (parent-id (parent-id bucket-id)))

    (with-slots (buckets) (the map map)  
      (multiple-value-bind (pred succ)
                           (find-candidate (node-hash head) 
                                           (aref buckets parent-id))
        (set-pred-next pred buckets parent-id :next head)
        (setf (node-next tail) succ)))))

(defun downsize (map)
  (with-slots (buckets bucket-size bitlen) (the map map)
    (decf bitlen)

    (let ((old-size bucket-size)
          (new-size (floor bucket-size 2)))
      (each-bucket (head bucket-id map :start new-size :end old-size)
        (rehash-bucket head bucket-id map))

      (update-size-and-borders map new-size))))

(defun calc-borders (size)
  (declare (positive-fixnum size)
           #.*muffle-note*)
  (values (floor (* 0.25 size))
          (ceiling (* 0.75 size))))

(defun update-size-and-borders (map new-size)
    (with-slots (bucket-size lower-border upper-border) (the map map)
      (setf (values lower-border upper-border) (calc-borders new-size)
            bucket-size new-size)))


;;;;;;;;;;;;;;;;;;;;;
;;; external function
(declaim 
 (ftype (function (&key (:hash hash-fn) (:test test-fn) (:size positive-fixnum)) map)
        make))
(defun make (&key (size 4) (hash #'sxhash) (test #'eql))
  (declare #.*interface*)
  (let* ((bitlen (ceiling (log (max 2 size) 2)))
         (bucket-size (expt 2 bitlen))
         (buckets (make-array bucket-size :element-type 'bucket 
                                          :initial-element (sentinel))))
    (multiple-value-bind (lower upper) (calc-borders bucket-size)
      (make-map :hash-fn hash
                :test-fn test
                :bitlen bitlen
                :upper-border upper
                :lower-border lower
                :buckets buckets
                :bucket-size bucket-size))))

(defun get (key map &optional default)
  (declare #.*interface*)
  (multiple-value-bind (exists? node) (find-node key map)
    (declare #.*fastest*)
    (if exists?
        (values (node-value node) t)
      (values default nil))))

(defun set-impl (new-value key map)
  (multiple-value-bind (exists? node pred bucket-id hash) (find-node key map)
    (if exists?
        (setf (node-value node) new-value)
      (with-slots (buckets count upper-border) (the map map)
         (when (> (the positive-fixnum (incf count)) upper-border)
           (upsize map))

         (set-pred-next pred buckets bucket-id 
                        :next (make-node :key key :value new-value
                                         :hash hash :next node))
         new-value))))
          
(defun (setf get) (new-value key map)
  (declare #.*interface*)
  (locally
   (declare #.*fastest*)
   (set-impl new-value key map)))

(defun count (map)
  (declare #.*interface*)
  (map-count map))

(defun remove (key map)
  (declare #.*interface*)
  (multiple-value-bind (exists? node pred bucket-id) (find-node key map)
    (declare #.*fastest*)
    (when exists?
      (with-slots (buckets count lower-border) (the map map)
        (set-pred-next pred buckets bucket-id :next (next node))
        (when (< (the positive-fixnum (decf count)) lower-border)
          (downsize map)))
      t)))

(defmacro each ((key value map &optional return-form) &body body)
  (let ((id (gensym))
        (node (gensym))
        (head (gensym)))
    `(each-bucket (,head ,id ,map :return ,return-form)
       (loop FOR ,node = ,head THEN (next ,node)
             UNTIL (sentinel? ,node)
         DO
         (let ((,key (node-key ,node))
               (,value (node-value ,node)))
           ,@body)))))

(defun map (fn map &aux acc)
  (declare #.*interface*
           (function fn))
  (each (k v map (nreverse acc))
    (push (funcall fn k v) acc)))
