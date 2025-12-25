(in-package :basic)

(defconstant +rope-size+ 50
  "Size of individual rope segments in our rope array")

(defclass keymap ()
  ((hash
    :initarg :hash :initform (make-hash-table :test #'equalp) :accessor %hash
    :documentation "Case insensitive storage of non-numeric keys")
   (rope
    :initarg :rope :accessor %rope
    :documentation "Adjustable array of +ROPE-SIZE+ arrays.")))

(defmethod print-object ((h keymap) stream)
  (format stream "{~{~A => ~A~^, ~}}"
          (nconc (loop for r across (%rope h) for b from 0 by +rope-size+
                       when r nconc (loop for e across r for i from 0 when e collect (+ i (* b +rope-size+)) and collect e))
                 (loop for k being the hash-keys of (%hash h) using (hash-value v) collect k collect v))))

(defun keymap-p (km)
  (and (eq (class-name (class-of km)) 'keymap) km))

(defgeneric keymap (key km &optional coercer)
  (:method ((key integer) (km keymap) &optional coercer)
    (let ((bucket (floor key +rope-size+)))
      (lw:when-let (array (and (< bucket (length (%rope km))) (aref (%rope km) bucket)))
        (let ((value (aref array (- key (* bucket +rope-size+)))))
          (if coercer
              (funcall coercer value)
              value)))))
  
  (:method ((key string) (km keymap) &optional coercer)
    (let ((value (lw:if-let (num (fnumber? key)) (keymap num km) (gethash key (%hash km)))))
      (if coercer
          (funcall coercer value)
          value))))

(defun (setf keymap) (value key km)
  (if (and (integerp key) (>= key 0))
      (%set-keymap-index value key km)
      (let* ((string (string! key)) (number (fnumber? string)))
        (if (and number (>= number 0))
            (%set-keymap-index value number km)
            (setf (gethash string (%hash km)) value)))))

(defun %set-keymap-index (value index km)
  (let* ((bucket (floor index +rope-size+)) (index (- index (* bucket +rope-size+))))
    (lw:if-let (array (and (< bucket (length (%rope km))) (aref (%rope km) bucket)))
               ;; Rope is the right size, and has this segment filled in
               (setf (aref array index) value)
               (if (< bucket (length (%rope km)))
                   ;; Rope is the right size, but this segment is sparse
                   (setf (aref (setf (aref (%rope km) bucket) (make-array +rope-size+)) index) value)
                   ;; Rope is the wrong size
                   (setf (aref (setf (%rope km) (adjust-array (%rope km) (1+ bucket))
                                     (aref (%rope km) bucket) (make-array +rope-size+)) index) value)))))

(defgeneric make-keymap (value)
  (:method ((value null))
    (make-instance 'keymap :rope (make-array 0 :adjustable t)))
  (:method ((keymap keymap))
    keymap)
  (:method ((value integer))
    (make-instance 'keymap :rope (make-array (ceiling value +rope-size+) :adjustable t)))
  (:method ((cons cons))
    (let ((km (make-instance 'keymap :rope (make-array 1 :adjustable t))))
      (if (every #'consp cons)
          (loop for (key . value) in cons do (setf (keymap key km) value))
          (loop for idx from 1 for value in cons do (setf (keymap idx km) value)))
      km))
  (:method ((array array))
    (let ((km (make-instance 'keymap :rope (make-array 1 :adjustable t))))
      (loop for idx from 1 for value across array do (setf (keymap idx km) value))))
  (:method ((hash hash-table))
    (let ((km (make-instance 'keymap :rope (make-array 1 :adjustable t))))
      (loop for value being the hash-values of hash using (hash-key key) do (setf (keymap key km) value))
      km))
  (:method (object)
    (let ((km (make-instance 'keymap :rope (make-array 0 :adjustable t))))
      (loop for slot in (hcl:class-slots (class-of object))
            for symb = (hcl:slot-definition-name slot)
            do (setf (keymap (symbol-name symb) km) (and (slot-boundp object symb) (slot-value object symb))))
      km)))

(defun keymap-count (km)
  (reduce #'+ (%rope km) :key (%% (count-if #'identity %)) :initial-value (hash-table-count (%hash km))))

(defun keymap-map (function km &key index)
  "Run FUNCTION over the KM keys and values, returning the result in a list"
  (flet ((call (idx value)
           (if index
               (funcall function value idx)
               (funcall function value))))
    (let (result)
      (loop for segment-idx from 0
            for segment across (%rope km)
            when segment do
              (loop for idx from (* segment-idx +rope-size+)
                    for value across segment
                    when value do (setf result (cons (call idx value) result))))
      (loop for value being the hash-values of (%hash km) using (hash-key key)
            when value do (setf result (cons (call key value) result)))
      (nreverse result))))

(defun keymap-map* (function km)
  "Run FUNCTION over the KM values, returning the result in a list"
  (let (result)
    (loop for segment across (%rope km) do
      (loop for value across segment
            when value do (setf result (cons (funcall function value) result))))
    (loop for value being the hash-values of (%hash km)
          when value do (setf result (cons (funcall function value) result)))
    (nreverse result)))

(defun deep-copy-keymap (km copier-fn)
  (let ((copy (make-instance 'keymap :hash (funcall copier-fn (%hash km)) :rope (make-array (length (%rope km)) :adjustable t))))
    (loop for idx below (length (%rope km)) do (setf (aref (%rope copy) idx) (map 'vector copier-fn (aref (%rope km) idx))))
    copy))

(defun keymap-delete (km key)
  "Remove KEY from KM keymap"
  (if (and (integerp key) (>= key 0))
      (%set-keymap-index nil key km)
      (let* ((string (string! key)) (number (fnumber? string)))
        (if (and number (>= number 0))
            (%set-keymap-index nil number km)
            (remhash string (%hash km))))))
