(in-package :forth)
 
;;;; ============================================================================
;;;; Lisp based Forth
;;;; ============================================================================
;;;;
;;;; This is a very idiosyncratic forth dialect to suit a BASIC interpreter, since forth makes a much easier virtual machine target than a
;;;; traditional opcode-based VM.
;;;;
;;;; The dictionary is actually a stack of hash-tables to support local variables in subroutine calls.
;;;;
;;;; Implemented words:
;;;;
;;;; @           ( lvalue -- rvalue )                fetch the RVALUE from the LVALUE on top of the stack
;;;; .           ( string rvalue -- lvalue )         fetch the LVALUE dereferenced from the RVALUE and STRING on top of the stack
;;;; !           ( rvalue value -- )                 set the RVALUE's content to VALUE
;;;; !!          ( lvalue -- )                       delete the LVALUE from the variable stack
;;;; ARRAY       ( count [keys-and-values])          make a new LVALUE keymap
;;;; IF          ( n1 b1 -- )                        if B1 is nil, jump forward N1 instructions, otherwise R> then continue
;;;; THEN                                            pop the top of the return stack (X), jump forward Xinstructions
;;;; RETURN      ( any -- )                          immediately exit returning ANY to the caller
;;;; VARIABLE    (--immediate-mode--)                create a new RVALUE named STRING
;;;; VARIABLE?   (--immediate-mode--)                find a previous RVALUE named STRING in earlier dicts or create a new RVALUE
;;;; VARIABLE[]  (--immediate-mode--)                create a new RVALUE pre-declared to be a keymap
;;;; >r                                              push the top of the parameter stack onto the return stack
;;;; r>                                              pop the top of the return stack
;;;;
;;;; The default RVALUE type is a "keymap" - combination array and hash-table, so we can support ordered mapping
;;;; plus storing data by string keys in the one object. Cheap version of PHP's array() primitive.
;;;;
;;;; Variable references are implemented as (DEFSTRUCT VAR) to allow late binding.
;;;;   In a traditional forth, we'd hardcode variable references as functions returning a place. But we need to support late
;;;;   binding for our RXML dialect, since we won't know at compilation what variables will actually be supplied to the template or not.
;;;;   And late binding has another benefit - supporting multi-threaded invocations, since each thread will carry its own dict hash-table.
;;;;   Hence no need for a GIL like Python or Ruby.
;;;;
;;;; Variable values are implemented as (DEFSTRUCT PLACE) entries in a dict hash-table.
 
(define-condition forth-compilation-error (basic::basic-parse-error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream) (write-string (message condition) stream))))
 
(define-condition return-value ()
  ((arg :initarg :arg :accessor arg)))
 
(defparameter *strict-mode-p* nil
  "If set, we error on undefined vars. If not set, we create vars on first use")
 
(defparameter *stack* nil
  "Runtime values")
 
(defparameter *rstack* nil
  "Return stack to power THEN thunks")
 
(defparameter *pc* nil
  "Runtime programme counter")
 
(defparameter *opcodes* nil
  "Programme listing")
 
(defvar *dict* (list (make-hash-table :test #'equalp))
  "List of dictionaries for forth words/vars")
 
(defmethod print-object ((place place) out)
  (format out "[~A~@[.~A~]]" (place-value place) (place-idx place)))
 
(defmethod print-object ((func func) out)
  (format out "#'~A" (func-name func)))
 
(defmethod print-object ((var var) out)
  (format out "{~A}" (var-name var)))
 
(defmethod print-object ((jump jump) out)
  (format out "&JMP:~A" (jump-to jump)))
 
(defmethod print-object ((label label) out)
  (format out "&LABEL:~A" (label-name label)))
 
(defmacro with-dict (dict &body body)
  `(let ((*dict* (cons ,dict *dict*)))
     ,@body))
 
(defmacro with-variables (env &body body)
  `(let ((*dict* (cons (make-hash-table :test #'equalp) *dict*)))
     (loop for (key . value) in ,env do (setf (gethash key (car *dict*)) (make-place :value value)))
     ,@body))
 
;;;;============================================================================
;;;; COMPILATION
;;;;============================================================================
 
(defun lookup (name &key debug)
  (let ((func (loop for dict in *dict* thereis (gethash name dict))))
    (or (if debug func (func-thunk func))
        (error "~S not found in scope" name))))
 
(defun has-var-p (var)
  (loop for dict in *dict* for count below (if (var-local var) 2 999) thereis (gethash (var-name var) dict)))
 
(defun lookup-var (var)
  (or (loop for dict in *dict* for count below (if (var-local var) 2 999) thereis (gethash (var-name var) dict))
      (setf (gethash (var-name var) (car *dict*)) (make-place :name (var-name var)))))
 
(defun compile-forth (*opcodes* &key debug)
  (loop with jumps = (make-hash-table :test #'equalp)
        with instructions = (make-array 1 :adjustable t :fill-pointer 0)
        initially (when (stringp *opcodes*) (setf *opcodes* (basic:parse *opcodes*)))
        for op = (pop *opcodes*)
        while (or op *opcodes*)
        do (cond ((const-p op) (vector-push-extend (const-value op) instructions))
                 ((var-p op) (vector-push-extend op instructions))
                 ((jump-p op) (vector-push-extend op instructions))
                 ((label-p op) (setf (gethash (label-name op) jumps) (fill-pointer instructions)))
                 ((eq op :sub) (compile-forth-sub (pop *opcodes*) (loop repeat *opcodes* collect (pop *opcodes*)) (loop repeat *opcodes* collect (pop *opcodes*))))
                 (t (vector-push-extend (lookup op :debug debug) instructions)))
        finally (return (compile-forth-jump-labels instructions jumps))))
 
(defun make-thunk (args forth)
  (lambda ()
    (let ((*dict* (cons (make-hash-table :test #'equalp) *dict*)))
      (loop for arg in args do (setf (gethash arg (car *dict*)) (pop *stack*)))
      (push (run forth) *stack*))))
 
(defun compile-forth-sub (name args instructions)
  (setf (gethash name (car *dict*))
        (make-func :name name :thunk (make-thunk args (compile-forth instructions)))))
 
(defun compile-forth-jump-labels (instructions labels)
  (flet ((jump-address (to)
           (or (gethash to labels) (error "Missing label ~A" to))))
    (map 'vector (lambda (op) (if (jump-p op) (jump-address (jump-to op)) op)) instructions)))
 
;;;;============================================================================
;;;; RUNTIME
;;;;============================================================================
 
(defun run (instructions &key debug)
  (when (stringp instructions)
    (setf instructions (compile-forth instructions :debug debug)))
  (handler-bind ((return-value (lambda (v) (return-from run (desugar (arg v)))))
                 (forth-compilation-error (lambda (c) (return-from run (message c)))))
    (with-variables ()
      (loop with *pc* = 0
            with pclen = (length instructions)
            for op = (when (< *pc* pclen) (aref instructions *pc*))
            while (< *pc* pclen)
            when debug
              do (format t "OP: ~A; Stack: ~A~%" op *stack*)
            do (incf *pc*)
               (cond ((functionp op) (funcall op))
                     ((func-p op) (funcall (func-thunk op)))
                     ((var-p op) (push (lookup-var op) *stack*))
                     (t (push op *stack*)))
            finally (return (desugar (pop *stack*)))))))
 
(defun desugar (value)
  (unless (eq value :false)
    value))
 
(defgeneric deref (value idx)
  (:method (value (idx null)) value)
  (:method (value (idx cons)) (loop for i in (reverse idx) do (setf value (deref value i)) finally (return value)))
  (:method ((value array) (idx integer)) (and (<= 1 idx (length value)) (aref value (1- idx))))
  (:method ((value keymap) (idx cons)) (loop for i in (reverse idx) do (setf value (deref value i)) finally (return value)))
  (:method ((value keymap) (idx null)) value)
  (:method ((value keymap) idx) (keymap idx value))
  (:method ((value cons) (idx integer)) (nth-value (1- idx) value))
  (:method ((value cons) (idx string)) (cdr (assoc idx value :test #'equalp)))
  (:method ((value hash-table) idx) (gethash idx value))
  (:method (value idx) :false))
 
(defun find-slot-by-name (object name)
  (let ((slots (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))))
    (find name slots :test #'string-equal :key #'symbol-name)))
 
(defmethod deref (value (idx string))
  (when-let (slot (find-slot-by-name value idx))
    (slot-value value slot)))
 
(defun set-place (place value)
  (flet ((try-set (container idx)
           (let ((htp (hash-table-p container)) (hap (keymap-p container)))
             (cond ((and htp (or (hash-table-p (gethash idx container)) (keymap-p (gethash idx container)))) (gethash idx container))
                   ((and hap (or (hash-table-p (keymap idx container)) (keymap-p (keymap idx container)))) (keymap idx container))
                   (htp (setf (gethash idx container) (make-keymap nil)))
                   (hap (setf (keymap idx container) (make-keymap nil)))
                   (t (error "Underlying container should be hashy ~A:~A" container idx)))))
 
         (hydrate (place idx)
           (let ((container (place-value place)))
             (if (or (hash-table-p container) (keymap-p container) (find-slot-by-name container idx))
                 (place-value place)
                 (setf (place-value place) (make-keymap nil))))))
 
    (if (null (place-idx place))
        (setf (place-value place) value)
        (loop with indices = (nreverse (place-idx place))
              with container = (hydrate place (car indices))
              for (idx . next) on indices
              do (cond (next (setf container (try-set container idx)))
                       ((keymap-p container) (setf (keymap idx container) value))
                       ((hash-table-p container) (setf (gethash idx container) value))
                       (t (when-let (slot (find-slot-by-name container idx))
                            (setf (slot-value container slot) value))))))))
 
(defmacro define-primitive (name &body body)
  `(setf (gethash ,name (car *dict*)) (make-func :name ,name :thunk (lambda () ,@body))))
 
(define-primitive "@"
  (let ((place (pop *stack*)))
    (if (place-p place)
        (push (deref (place-value place) (place-idx place)) *stack*)
        (error (make-condition 'forth-compilation-error :message (format nil "@: Expecting a PLACE, received ~S of type ~A" place (class-of place)))))))
 
(define-primitive "."
  (let* ((idx (pop *stack*)) (place (pop *stack*)))
    (if (place-p place)
        (push (make-place :value (place-value place) :idx (cons idx (place-idx place))) *stack*)
        (error (make-condition 'forth-compilation-error :message (format nil ".: Expecting a PLACE, received ~S" place))))))
 
(define-primitive "!"
  (let ((place (pop *stack*)) (value (pop *stack*)))
    (if (place-p place)
        (set-place place value)
        (error (make-condition 'forth-compilation-error :message (format nil "!: Expecting a PLACE, received ~S of type ~A" place (class-of place)))))))
 
(define-primitive "!!"
  (error (make-condition 'forth-compilation-error :message "Unimplemented DELETE")))
 
(define-primitive "IF"
  (if (pop *stack*)
      (pop *rstack*)
      (setf *pc* (pop *rstack*))))
 
(define-primitive "THEN"
  (setf *pc* (pop *rstack*)))
 
(define-primitive "RETURN"
  (signal (make-condition 'return-value :arg (pop *stack*))))
 
(define-primitive ">R"
  (push (pop *stack*) *rstack*))
 
(define-primitive "R>"
  (pop *rstack*))

