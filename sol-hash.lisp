(in-package :sol-hash)

(declaim #.*fastest*
         (inline make get (setf get) remove map count clear

                 make-node node-next node-hash node-key node-value
                 make-map map-buckets map-bitlen map-count
                 map-resize-border map-hash-fn map-test-fn
                 
                 next sentinel sentinel? ordinal?
                 bit-reverse lower-bits
                 hashcode-and-bucket-id set-pred-next
                 find-candidate find-node
                 set-impl))

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
  (bitlen       0 :type hashcode-width)
  (count        0 :type positive-fixnum)
  (resize-border    0 :type positive-fixnum)
  (resize-threshold 0 :type number)
  (hash-fn      t :type hash-fn)
  (test-fn      t :type test-fn))

(defmethod print-object ((o map) stream)
  (declare #.*normal*)
  (print-unreadable-object (o stream :type t :identity t)
    (with-slots (count) (the map o)
      (format stream "~s ~s" :count count))))

(eval-when (:compile-toplevel :load-toplevel)
  (unless (constantp '+SENTINEL+)
    (defconstant +SENTINEL+ (make-node :hash +MAX_HASHCODE+))
    (setf (node-next +SENTINEL+) +SENTINEL+)))

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

(defun hashcode-and-bucket-id (key map)
  (with-slots (hash-fn bitlen) (the map map)
    (let* ((h1 (lower-bits +HASHCODE_BITLEN+ (funcall hash-fn key)))
           (h2 (bit-reverse h1)))
      (values h2                          ; key's hashcode
              (lower-bits bitlen h1)))))  ; bucket-id

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

(defun find-node (key map)
  (with-slots (test-fn buckets) (the map map)
    (multiple-value-bind (hash bucket-id)
                         (hashcode-and-bucket-id key map)
      (declare (hashcode hash))
      (multiple-value-bind (pred cur)
                           (find-candidate hash (aref buckets bucket-id))
        (labels ((recur (pred cur)
                   (if (or (/= hash (node-hash cur))
                           (sentinel? cur))
                       (values nil cur pred bucket-id hash)
                     (if (funcall test-fn key (node-key cur))
                         (values t cur pred bucket-id hash)
                       (recur cur (next cur))))))
          (recur pred cur))))))

(defmacro each-bucket ((head-node bucket-id map &key start end return) &body body)
  (let ((buckets (gensym))
        (bucket-size (gensym)))
    `(with-slots ((,buckets buckets) (,bucket-size bucket-size)) (the map ,map)
       (loop FOR ,bucket-id fixnum FROM ,(or start 0) BELOW ,(or end bucket-size)
             FOR ,head-node = (aref ,buckets ,bucket-id) 
             UNLESS (sentinel? ,head-node)
         DO
         (locally ,@body)
         FINALLY
         (return ,return)))))

(defun resize (map)
  (with-slots (buckets bucket-size bitlen) (the map map)
    (labels ((child (bucket) (dpb 1 (byte 1 bitlen) bucket))
             (bucket-hash (id) (bit-reverse id))

             (rehash-bucket (head parent &aux (child (child parent)))
               (multiple-value-bind (pred succ) 
                                    (find-candidate (bucket-hash child) head)
                 (set-pred-next pred buckets parent :next (sentinel))
                 (setf (aref buckets child) succ))))
      (declare (inline child rehash-bucket bucket-hash))

      (let ((old-size bucket-size)
            (new-size (the positive-fixnum (* 2 bucket-size))))
        (update-size-and-borders map new-size)
        (each-bucket (head bucket-id map :end old-size)
          (rehash-bucket head bucket-id))
        (incf bitlen)))))

(defun calc-border (size threshold)
  (declare #.*muffle-note*
           (positive-fixnum size)
           (number threshold))
  (ceiling (* size threshold)))

(defun update-size-and-borders (map new-size)
  (declare (positive-fixnum new-size))
  (with-slots (buckets bucket-size resize-border resize-threshold) (the map map)
    (when (< bucket-size new-size)
      (setf buckets (adjust-array buckets new-size :element-type 'bucket
                                                   :initial-element (sentinel))))
    (setf resize-border (calc-border new-size resize-threshold)
          bucket-size new-size)))


;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun make (&key (size 4) (hash #'sxhash) (test #'eql) (rehash-threshold 0.75))
  (declare #.*interface*
           (hash-fn hash)
           (test-fn test)
           (positive-fixnum size)
           (number rehash-threshold))
  (locally 
   (declare #.*fastest*)
   (let* ((bitlen (ceiling (log (max 2 size) 2)))
          (bucket-size (expt 2 bitlen))
          (buckets (make-array bucket-size :element-type 'bucket 
                                           :initial-element (sentinel))))
     (make-map :hash-fn hash
               :test-fn test
               :bitlen bitlen
               :resize-border (calc-border bucket-size rehash-threshold)
               :resize-threshold rehash-threshold
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
      (with-slots (buckets count resize-border) (the map map)
         (set-pred-next pred buckets bucket-id 
                        :next (make-node :key key :value new-value
                                         :hash hash :next node))
         (incf count)
         (when (> count resize-border)
           (resize map))
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
      (with-slots (buckets count) (the map map)
        (set-pred-next pred buckets bucket-id :next (next node))
        (decf count))
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

(defun clear (map)
  (declare #.*interface*)
  (locally
   (declare #.*fastest*)
   (with-slots (buckets bucket-size bitlen count
                resize-border resize-threshold) (the map map)
     (setf bucket-size 4
           bitlen 2
           count 0
           resize-border (calc-border 4 resize-threshold))
     (fill buckets (sentinel)))
   t))


          