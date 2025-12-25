(in-package :forth)
 
(defmacro define-forth-function (name args &body body)
  (flet ((make-arg (a)
           (if (atom a)
               `(,a (pop *stack*))
               (case (cadr a)
                 (:number `(,(car a) (fnumber! (pop *stack*))))
                 (:integer `(,(car a) (truncate (fnumber! (pop *stack*)))))
                 (:string `(,(car a) (fstring! (pop *stack*))))
                 (:boolean `(,(car a) (fboolean! (pop *stack*))))
                 (t (error "Unknown basic type ~S for ~A" (cadr a) (car a)))))))
 
    `(setf (gethash ,name (car *dict*))
           (make-func :name ,name :thunk (lambda () (push (let ,(mapcar #'make-arg args) ,@body) *stack*))))))
 
;;;============================================================================
;;; BASIC MATHS
;;;============================================================================
 
(define-forth-function "+" ((a :number) (b :number)) (+ a b))
(define-forth-function "-" ((a :number) (b :number)) (- a b))
(define-forth-function "*" ((a :number) (b :number)) (* a b))
(define-forth-function "/" ((a :number) (b :number)) (/ a b))
(define-forth-function "AND" (a b) (when (and (fboolean! a) (fboolean! b)) b))
(define-forth-function "OR" (a b) (cond ((fboolean! a) a) ((fboolean! b) b) (t nil)))
(define-forth-function "NOT" ((a :boolean)) (not a))
(define-forth-function "^" ((a :number) (b :number)) (expt a b))
(define-forth-function "%" ((a :number) (b :number)) (mod a b))
(define-forth-function "==" (a b) (equality? a b))
(define-forth-function "<>" (a b) (not (equality? a b)))
(define-forth-function "<" (a b) (less-than? a b))
(define-forth-function ">" (a b) (not (or (less-than? a b) (equality? a b))))
(define-forth-function "<=" (a b) (or (less-than? a b) (equality? a b)))
(define-forth-function ">=" (a b) (not (less-than? a b)))
 
;;;============================================================================
;;; OTHER MATHS
;;;============================================================================
 
(define-forth-function "ABS" ((a :number))
  (abs a))
 
(define-forth-function "ATN" ((a :number))
  (atan a))
 
(define-forth-function "BIT" ((int :integer) (pos :integer))
  (plusp (logbitp int pos)))
 
(define-forth-function "BITAND" ((a :integer) (b :integer))
  (logand a b))
 
(define-forth-function "BITOR" ((a :integer) (b :integer))
  (logior a b))
 
(define-forth-function "BITXOR" ((a :integer) (b :integer))
  (logxor a b))
 
(define-forth-function "COS" ((a :number))
  (cos a))
 
(define-forth-function "COUNT" (array)
  (cond ((keymap-p array) (keymap-count array))
        ((hash-table-p array) (hash-table-count array))
        ((or (arrayp array) (consp array)) (length array))
        (t 0)))
 
(define-forth-function "HEX$" ((a :number))
  (format nil "~@:(~X~)" a))
 
(define-forth-function "HEX2DEC" ((a :string))
  (or (parse-integer a :junk-allowed t :radix 16) 0))
 
(define-forth-function "ISSET" ((a :string))
  (fboolean! (has-var-p a)))
 
(define-forth-function "MAX" ((a :number) (b :number))
  (max a b))
 
(define-forth-function "MIN" ((a :number) (b :number))
  (min a b))
 
(define-forth-function "OCT$" ((a :number))
  (format nil "~O" a))
 
(define-forth-function "OCT2DEC" ((a :string))
  (or (parse-integer a :junk-allowed-t :radix 8) 0))
 
(define-forth-function "RANDOM" ((a :number))
  (random a))
 
(define-forth-function "RND" ()
  (random 1.0))
 
(define-forth-function "ROUND" ((number :number) (nearest :number))
  (* (truncate number (max nearest 1))))
 
(define-forth-function "SGN" ((number :number))
  (signum number))
 
(define-forth-function "SIN" ((a :number))
  (sin a))
 
(define-forth-function "SQR" ((a :number))
  (sqrt (max a 1)))
 
(define-forth-function "TAN" ((a :number))
  (tan a))
 
;;;============================================================================
;;; STRINGS
;;;============================================================================
 
(define-forth-function "&" ((a :string) (b :string))
  (concatenate 'string a b))
 
(define-forth-function "&&" ((a :string) (b :string))
  (concatenate 'string a " " b))
 
(define-forth-function "~" ((regex :string) (string :string))
  (when (cl-ppcre:scan regex string)
    t))
 
(define-forth-function "ABN$" ((string :string))
  (format-abn string))
 
(define-forth-function "ASC" ((char :string))
  (if (string/= char "")
      (char-code (char char 0))
      0))
 
(define-forth-function "CHR$" ((char :integer))
  (char-code char))
 
(define-forth-function "HTML$" ((text :string))
  (markdown:make-html text))
 
(define-forth-function "INSTR" ((start :integer) (hay :string) (needle :string))
  (1+ (or (search needle hay :test #'char-equal :start2 (max 0 (min (1- start) (length hay)))) -1)))
 
(define-forth-function "LCASE$" ((string :string))
  (string-downcase string))
 
(define-forth-function "LEFT$" ((string :string) (count :integer))
  (subseq string 0 (max 0 (min count (length string)))))
 
(define-forth-function "LPAD$" ((string :string) (length :integer) (padding :string))
  (concatenate 'string (make-string (max (- length (length string)) 0) :initial-element (char (if (plusp (length padding)) padding " ") 0)) string))
 
(define-forth-function "LTRIM$" ((string :string))
  (string-left-trim '(#\Space #\Tab #\Return #\Linefeed) string))
 
(define-forth-function "MARKDOWN$" ((text :string))
  (markdown:parse text))
 
(define-forth-function "MID$" ((string :string) (start :integer) (length :integer))
  (cond ((and (< 0 start (1+ (length string))) (<= 0 (+ start length) (length string)))
         (subseq string (1- start) (+ -1 start length)))
        ((< 0 start (1+ (length string))) (subseq string (1- start)))
        (t "")))
 
(define-forth-function "MONEY$" ((number :number))
  (multiple-value-bind (i f) (truncate (abs number) 100)
    (format nil "~:[~;- ~]$~:D.~2,'0D" (minusp number) i (max 0 (min 99 f)))))
 
(define-forth-function "PHONE$" ((string :string))
  (format-phone-number string))
 
(define-forth-function "PLURAL$" ((word :string) (quantity :number))
  (if (/= quantity 1)
      (pluralise-word word)
      word))
 
(define-forth-function "REPLACE$" ((string :string) (old :string) (new :string))
  (format nil "~{~A~}" (intersperse new (split-sequence-by-sequence old string :test #'char-equal))))
 
(define-forth-function "RIGHT$" ((string :string) (count :integer))
  (cond ((< 0 count (length string)) (subseq string (- (length string) count)))
        ((>= count (length string)) string)
        ((zerop (length string)) "")
        (t (subseq string (1- (length string))))))
 
(define-forth-function "RPAD$" ((string :string) (width :integer) (fill :string))
  (concatenate 'string string (make-string (- width (length string)) 0) :initial-element (char (if (plusp (length fill)) fill " ") 0)))
 
(define-forth-function "RTRIM$" ((string :string))
  (string-right-trim '(#\Space #\Tab #\Return #\Linefeed) string))
 
(define-forth-function "SPACE$" ((count :integer))
  (make-string count :initial-element #\Space))
 
(define-forth-function "TCASE$" ((text :string))
  (let* ((lower '("a" "an" "and" "but" "for" "to" "is" "are" "by" "from" "with" "the" "it" "he" "she" "its" "her" "his" "our" "of" "or"))
         (words (mapcar (lambda (w) (if (find w lower :test #'string-equal) (string-downcase w) (format nil "~:(~A~)" w))) (split-string #\Space text)))
         (text (join words #\Space)))
    (unless (string= text "")
      (setf (char text 0) (char-upcase (char text 0))))
    text))
 
(define-forth-function "TRIM$" ((string :string))
  (string-trim '(#\Space #\Tab #\Return #\Linefeed) string))
 
(define-forth-function "UCASE$" ((string :string))
  (string-upcase string))
 
(define-forth-function "URLIFY$" ((string :string))
  (urlify string))
 
;;;============================================================================
;;; DATE
;;;============================================================================
 
(define-forth-function "DATE$" (date)
  (make-date date))
 
(define-forth-function "DAY$" ((day :integer))
  (aref #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday") (max 0 (min (1- day) 6))))
 
(define-forth-function "FORMAT$" (date (format :string))
  (format-basic-date-string (make-date date) format))
 
(define-forth-function "MONTH$" ((month :integer))
  (aref #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") (max 0 (min (1- month) 11))))
 
(define-forth-function "NOW$" ()
  (now))
 
(define-forth-function "TIME$" (date)
  (make-date date))
 
(define-forth-function "UUID4$" ()
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x~)"
          (random #.(expt 2 32))
          (random #.(expt 2 16))
          (dpb #b0100 (byte 4 12) (random #.(expt 2 16)))
          (dpb #b10 (byte 2 14) (random #.(expt 2 16)))
          (random #.(expt 2 48))))
 
;;;============================================================================
;;; ARRAY
;;;============================================================================
 
(define-forth-function "^ARRAY" ((count :integer))
  (loop with array = (make-keymap count)
        for auto = 0
        for idx = (pop *stack*)
        for val = (pop *stack*)
        repeat count
        do (setf (keymap (or idx (incf auto)) array) val)
        finally (return array)))
 
(define-forth-function "BASE64$" (object)
  (crypto:encode-base-64 (if (arrayp object) object (string! object))))
 
(define-forth-function "^CAR" (list)
  (car list))
 
(define-forth-function "^CDR" (list)
  (cdr list))
 
(define-forth-function "CONJ" (a b)
  (cons a b))
 
(define-forth-function "FIND" ((needle :string) haystack (key :string))
  (or (find needle haystack :test #'string-equal :key (%% (string! (deref % key)))) :false))
 
(define-forth-function "JSON_ENCODE$" (object)
  (json:encode object))
 
(define-forth-function "JSON_DECODE$" ((string :string))
  (json:decode string))
 
(define-forth-function "MKLIST" (object)
  (cond ((arrayp object) (coerce object 'list))
        ((hash-table-p object) (loop for v being the hash-values of object collect v))
        ((keymap-p object) (keymap-map #'identity object))
        ((consp object) object)
        (t (coerce (string! object) 'list))))
 
(define-forth-function "READ_CSV" ((input :string))
  (let* ((csv (slurp-csv input))
         (headings (pop csv))
         (array (make-keymap (list (make-keymap headings)))))
 
    (loop for line = (pop csv)
          for idx from 2
          while line
          do (let ((row (make-keymap 0)))
               (loop for x from 1
                     for h in headings
                     for l in line
                     do (setf (keymap x row) l (keymap h row) l)
                     finally (setf (keymap idx array) row))))
    array))
 
(define-forth-function "REVERSE" (list)
  (cond ((keymap-p list) (nreverse (keymap-map #'identity list)))
        ((or (consp list) (arrayp list)) (reverse list))
        (t list)))
 
(define-forth-function "SORT" ((comparator :string) (key :string) values)
  (map 'vector #'cdr (sort (map 'list (lambda (v) (cons (if key (deref v key) v) v))
                                (cond ((keymap-p values) (keymap-map #'identity values))
                                      ((stringp values) nil)
                                      ((arrayp values) values)
                                      ((consp values) values)
                                      (t nil)))
                           (if (string/= comparator ">") #'greater-than? #'less-than?)
                           :key #'car)))

