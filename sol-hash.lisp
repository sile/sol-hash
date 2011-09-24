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
  (resize-upper-border 0 :type positive-fixnum) 
  (resize-lower-border 0 :type positive-fixnum) 
  (hash-fn   t :type hash-fn)
  (test-fn   t :type test-fn))


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
  (declare (positive-fixnum n)
           (hashcode-width width))
  (ldb (byte width 0) n))

(defun hashcode-and-bucket-id (key map)
  (with-slots (hash-fn bitlen) (the map map)
    (let ((h (lower-bits +HASHCODE_BITLEN+ (funcall hash-fn key))))
      (values (bit-reverse h)           ; hashcode
              (lower-bits bitlen h))))) ; bucket-id

(defun bucket-hashcode (bucket-id)
  (bit-reverse bucket-id))

(defun predecessor-id (bucket-id bitlen)
  (declare (positive-fixnum bucket-id))
  (if (zerop bucket-id)
      0
    (let ((predecessor-id (bit-reverse (1- (bit-reverse bucket-id)))))
      (lower-bits bitlen predecessor-id))))

(defun find-candidate (hash head)
  (declare (hashcode hash))
  (labels ((recur (pred cur)
             (if (and cur (> hash (node-hash cur)))
                 (recur cur (node-next cur))
               (values pred cur))))
    (recur nil head)))

(defun set-pred-next (pred-node buckets bucket-id &key next)
  (declare ((simple-array (or null node)) bucket-id)
           (positive-fixnum bucket-id))
  (if pred-node
      (setf (node-next pred-node) next)
    (setf (aref buckets bucket-id) next)))

(defun get-bucket (bucket-id map)
  (with-slots (buckets bitlen) (the map map)
    (if (aref buckets bucket-id)
        (aref buckets bucket-id)
      (when (plusp bucket-id)
        (let* ((pred-bucket-id (predecessor-id bucket-id bitlen))
               (pred-bucket (get-bucket pred-bucket-id map))
               (bucket-hash (bucket-hashcode bucket-id)))
          (multiple-value-bind (pred node)
                               (find-candidate bucket-hash pred-bucket)
            (when node
              (set-pred-next pred buckets pred-bucket-id :next nil)
              (setf (aref buckets bucket-id) node))))))))

(defun find-node (key map)
  (with-slots (test-fn) (the map map)
    (multiple-value-bind (hash id) (hashcode-and-bucket-id key map)
      (multiple-value-bind (pred cur)
                           (find-candidate hash (get-bucket id map))
        (labels ((recur (pred cur)
                   (if (or (null cur) (/= hash (node-hash cur)))
                       (values nil cur pred id hash)
                     (if (funcall test-fn key (node-key cur))
                         (values t cur pred id hash)
                       (recur cur (node-next cur))))))
          (recur pred cur))))))

(defun resize-upper (map)
  (with-slots (buckets bitlen resize-lower-border resize-upper-border) (the map map)
    (incf bitlen)
    (setf buckets (adjust-array buckets (* 2 (length buckets)) :initial-element '()))
    (multiple-value-bind (upper lower) (calc-borders (length buckets))
      (setf resize-lower-border lower
            resize-upper-border upper))))

;; TODO: 効率化
(defun rehash-node (nn map)
  (when (node-next nn)
    (rehash-node (node-next nn) map))

  (multiple-value-bind (_ node pred hash id) (find-node (node-key nn) map)
    (declare (ignore _ hash))
    (with-slots (buckets) (the hmap map)
      (let ((new-node nn))
        (setf (node-next new-node) node)
        (set-pred-next pred buckets id :next new-node))))

(defun resize-lower (map)
  (with-slots (buckets bitlen) (the map map)
    (decf bitlen)
    ;; TODO: map-impl
    (loop FOR i fixnum FROM (expt 2 bitlen) BELOW (expt 2 (1+ bitlen))
          FOR head = (aref buckets i) 
          WHEN head
      DO
      (rehash-node head map))
    (setf buckets (adjust-array buckets (expt 2 bitlen)
                                :initial-element '()))))

(defun calc-borders (size)
  (values (floor (* 0.9 size))
          (floor (* 0.25 size))))

(defun make (&key (size 4) (hash #'sxhash) (test #'eql))
  (let* ((bitlen (ceiling (log size 2)))
         (buckets (make-array (expt 2 bitlen) :initial-element '())))
    (multiple-value-bind (upper lower) (calc-borders (length buckets))
      (make-map :hash-fn hash
                :test-fn test
                :bitlen bitlen
                :resize-upper-border upper
                :resize-lower-border lower
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
      (with-slots (buckets count resize-upper-border) (the map map)
         (when (> (incf count) resize-upper-border)
           (resize-upper map))

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
      (with-slots (buckets count resize-lower-border) (the map map)
        (set-pred-next pred buckets bucket-id :next (node-next node))
        (when (< (decf count) resize-lower-border)
          (resize-lower map)))
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

