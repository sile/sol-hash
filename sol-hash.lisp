(in-package :sol-hash)

(declaim #.*fastest*
         (inline make count clear map get (setf get) remove

                 make-node node-next node-hash node-key node-value
                 make-map map-buckets map-bitlen map-count
                 map-resize-border map-hash-fn map-test-fn
                 
                 next sentinel sentinel? ordinal?
                 bit-reverse lower-bits
                 hashcode-and-bucket-id set-pred-next
                 find-candidate
                 ))

;;;;;;;;;;
;;; struct
(eval-when (:compile-toplevel :load-toplevel)
  (defstruct node
    (next nil :type (or null node))
    (hash   0 :type hashcode)
    (key    t :type t)
    (value  t :type t)))

(deftype set-fn () '(function (t t map) t))
(deftype get-fn () '(function (t map t) (values t boolean)))
(deftype rem-fn () '(function (t map) boolean))

(defstruct map
  (custom     nil :type t)
  (buckets    #() :type buckets)
  (bucket-size  0 :type positive-fixnum)
  (bitlen       0 :type hashcode-width)
  (count        0 :type positive-fixnum)
  (resize-border    0 :type positive-fixnum)
  (resize-threshold 0 :type number)
  (hash-fn      t :type hash-fn)
  (test-fn      t :type test-fn)

  (set-fn t :type set-fn)
  (get-fn t :type get-fn)
  (rem-fn t :type rem-fn))

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

(defun hashcode-and-bucket-id (raw-hash bitlen)
  (declare (positive-fixnum raw-hash)
           (hashcode-width bitlen))
  (let* ((h1 (lower-bits +HASHCODE_BITLEN+ raw-hash))
         (h2 (bit-reverse h1)))
    (values h2                         ; key's hashcode
            (lower-bits bitlen h1))))  ; bucket-id

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

(defmacro find-node (key map &key test hash )
  (let ((hashcode (gensym))
        (bucket-id (gensym))
        (pred (gensym))
        (cur (gensym))
        (recur (gensym))
        (test (or test `(lambda (x y) (funcall (map-test-fn ,map) x y))))
        (hash (or hash `(lambda (x) (funcall (map-hash-fn ,map) x)))))
  `(multiple-value-bind (,hashcode ,bucket-id)
                        (hashcode-and-bucket-id (,hash ,key) (map-bitlen ,map))
     (declare (hashcode ,hashcode))
     (multiple-value-bind (,pred ,cur)
                          (find-candidate ,hashcode (aref (map-buckets ,map) ,bucket-id))
       (labels ((,recur (,pred ,cur)
                  (if (or (/= ,hashcode (node-hash ,cur))
                          (sentinel? ,cur))
                      (values nil ,cur ,pred ,bucket-id ,hashcode)
                    (if (,test ,key (node-key ,cur))
                        (values t ,cur ,pred ,bucket-id ,hashcode)
                      (,recur ,cur (next ,cur))))))
         (,recur ,pred ,cur))))))

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
(defstruct cus
  (set-fn t :type set-fn)
  (get-fn t :type get-fn)
  (rem-fn t :type rem-fn))

(defparameter *customs* '())
(defmacro defcustom (name hash test)
  `(push (cons ,name
               (make-cus :set-fn (gen-set-fn ,hash ,test)
                         :get-fn (gen-get-fn ,hash ,test)
                         :rem-fn (gen-rem-fn ,hash ,test)))
         *customs*))

(defun get-custom (name)
  (declare (symbol name))
  (cdr (assoc name *customs*)))
         
(defun make (&key (size 4) (hash #'sxhash) (test #'eql) (rehash-threshold 0.75)
                  custom)
  (declare #.*interface*
           (hash-fn hash)
           (test-fn test)
           (positive-fixnum size)
           (number rehash-threshold)
           (symbol custom))
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
               :bucket-size bucket-size
               :custom (when custom
                         (get-custom custom))
               
               :set-fn (if (get-custom custom)
                           (cus-set-fn (get-custom custom))
                         (gen-set-fn))
               
               :get-fn (if (get-custom custom)
                           (cus-get-fn (get-custom custom))
                         (gen-get-fn))

               :rem-fn (if (get-custom custom)
                           (cus-rem-fn (get-custom custom))
                         (gen-rem-fn))
               ))))

(defmacro gen-get-fn (&optional hash test)
  (let ((exists? (gensym))
        (node (gensym))
        (key (gensym))
        (map (gensym))
        (default (gensym)))
    `(lambda (,key ,map ,default)
       (declare #.*fastest*)
       (multiple-value-bind (,exists? ,node) (find-node ,key ,map :test ,test :hash ,hash)
         (if ,exists?
             (values (node-value ,node) t)
           (values ,default nil))))))

(defmacro gen-set-fn (&optional hash test)
  (let ((exists? (gensym))
        (node (gensym))
        (pred (gensym))
        (bucket-id (gensym))
        (hashcode (gensym))
        (new-value (gensym))
        (key (gensym))
        (map (gensym)))
    `(lambda (,new-value ,key ,map)
       (declare #.*fastest*)
       (multiple-value-bind (,exists? ,node ,pred ,bucket-id ,hashcode) 
                            (find-node ,key ,map :test ,test :hash ,hash)
       
         (if ,exists?
             (setf (node-value ,node) ,new-value)
           (progn
             (set-pred-next ,pred (map-buckets ,map) ,bucket-id 
                            :next (make-node :key ,key :value ,new-value
                                             :next ,node :hash ,hashcode))
             (incf (map-count ,map))
             (when (> (map-count ,map) (map-resize-border ,map))
               (resize ,map))
             ,new-value))))))

(defmacro gen-rem-fn (&optional hash test)
  (let ((exists? (gensym))
        (node (gensym))
        (pred (gensym))
        (bucket-id (gensym))
        (key (gensym))
        (map (gensym)))
    `(lambda (,key ,map)
       (declare #.*fastest*)
       (multiple-value-bind (,exists? ,node ,pred ,bucket-id) 
                            (find-node ,key ,map :test ,test :hash ,hash)
         (when ,exists?
           (set-pred-next ,pred (map-buckets ,map) ,bucket-id :next (next ,node))
           (decf (map-count ,map))
           t)))))

(defun get (key map &optional default)
  (declare #.*interface*)
  (funcall (map-get-fn map) key map default))

(defun (setf get) (new-value key map)
  (declare #.*interface*)
  (funcall (map-set-fn map) new-value key map))

(defun count (map)
  (declare #.*interface*)
  (map-count map))

(defun remove (key map)
  (declare #.*interface*)
  (funcall (map-rem-fn map) key map))

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
