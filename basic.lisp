(in-package :basic)
 
(define-condition basic-parse-error (error)
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream) (format stream "Basic error: ~S" (message condition)))))
 
(progn
  (defparameter *counter* 0)
 
  (defun mkvar (name)
    (if (char= (char name 0) #\@)
        (make-var :name (subseq name 1) :local t)
        (make-var :name name)))
 
  (defun mkvar* (name)
    (make-var :name (format nil "~A_~D" name (incf *counter*))))
 
  (defun mklabel* (name)
    (make-label :name (format nil "~A_~D" name (incf *counter*))))
 
  (defun mkjump (label)
    (make-jump :to (label-name label))))
 
 
(parsergen:defparser basic-parser ((<toplevel> <statement*>))
  (<primary>
   ((TRUE) (make-const :value t))
   ((FALSE) (make-const :value :false))
   ((VAR #\( :CONSTANT #\)) (list (mkvar $3) "@"))
   ((:CONSTANT) (make-const :value $1))
   ((:IDENTIFIER #\( <funargs> #\)) (nreverse (cons $1 $3)))
   ((:IDENTIFIER <derefs>) (list (mkvar $1) $2 "@"))
   ((#\( <expression> #\)) $2)
   ((#\{ <fat-arrows> #\}) (list $2 (make-const :value (length $2)) "^ARRAY"))
   ((#\[ <funargs> #\]) (list (loop for e in $2 for i from 1 collect e collect (make-const :value i)) (make-const :value (length $2)) "^ARRAY")))
 
  (<fat-arrows>
   ((:IDENTIFIER #\= #\> <expression> #\, <fat-arrows>) (list* (list $4 (make-const :value $1)) $6))
   ((:IDENTIFIER #\= #\> <expression>) (list (list $4 (make-const :value $1))))
   (() nil))
 
  (<funargs>
   ((<expression> #\, <funargs>) (cons $1 $3))
   ((<expression>) (cons $1 nil))
   (() nil))
 
  (<derefs>
   ((#\[ <expression> #\] <derefs>) (list* $2 "." $4))
   ((#\. :IDENTIFIER <derefs>) (list* (make-const :value $2) "." $3))
   ((#\. END <derefs>) (list* (make-const :value "END") "." $3))
   (() nil))
 
  (<unary>
   ((<primary>) $1)
   ((#\- <unary>) (list $2 "-"))
   ((#\+ <unary>) $2)
   ((NOT <unary>) (list $2 "NOT")))
 
  (<exponent>
   ((<unary>) $1)
   ((<exponent> #\^ <unary>) (list $3 $1 "^")))
 
  (<multiplicative>
   ((<exponent>) $1)
   ((<multiplicative> #\* <exponent>) (list $3 $1 "*"))
   ((<multiplicative> #\/ <exponent>) (list $3 $1 "/"))
   ((<multiplicative> #\% <exponent>) (list $3 $1 "%")))
 
  (<additive>
   ((<multiplicative>) $1)
   ((<additive> #\+ <multiplicative>) (list $3 $1 "+"))
   ((<additive> #\- <multiplicative>) (list $3 $1 "-")))
 
  (<relational>
   ((<additive>) $1)
   ((<relational> == <additive>) (list $3 $1 "=="))
   ((<relational> #\~ <primary>) (list $3 $1 "~"))
   ((<relational> #\> <additive>) (list $3 $1 ">"))
   ((<relational> #\> #\= <additive>) (list $4 $1 ">="))
   ((<relational> #\< <additive>) (list $3 $1 "<"))
   ((<relational> #\< #\= <additive>) (list $4 $1 "<="))
   ((<relational> #\< #\> <additive>) (list $4 $1 "<>")))
 
  (<concatenation>
   ((<relational>) $1)
   ((<concatenation> #\& <relational>) (list $3 $1 "&"))
   ((<concatenation> #\& #\& <relational>) (list $4 $1 "&&")))
 
  (<and>
   ((<concatenation>) $1)
   ((<and> AND <concatenation>) (list $3 $1 "AND")))
 
  (<nor>
   ((<and>) $1)
   ((<nor> NOR <and>) (list $3 $1 "NOR")))
 
  (<expression>
   ((<nor>) $1)
   ((<expression> OR <nor>) (list $3 $1 "OR"))
   ((SORT #\( <expression> #\) BY :CONSTANT ASC) (list $3 (make-const :value $6) (make-const :value ">") "SORT"))
   ((SORT #\( <expression> #\) BY :CONSTANT DESC) (list $3 (make-const :value $6) (make-const :value "<") "SORT"))
   ((FOR :IDENTIFIER IN <expression> WHERE <expression> END FOR)
    (let* ((coll (mkvar* "COLLECTION")) (var (mkvar $2)) (result (mkvar* "RESULT")) (start (mklabel* "START")) (go-start (mkjump start)) (end (mklabel* "END")) (go-end (mkjump end)))
      (list $4 "MKLIST" coll "!"
            start coll "@" "^CAR" var "!" coll "@" "^CDR" coll "!" go-end ">R" var "@" "IF" go-start ">R" $6 "IF"
            result "@" var "@" "CONJ" result "!" go-start ">R" "THEN" end result "@" "REVERSE")))
   ((:error) (error 'basic-parse-error :message "Expected an expression or test")))
 
  (<proto>
   ((:IDENTIFIER #\, <proto>) (cons $1 $3))
   ((:IDENTIFIER) (cons $1 nil))
   (() nil))
 
  (<statement>
   ((SUB :IDENTIFIER #\( <proto> #\) <statement*> END SUB) (let ((instrs (flatten $6))) (list* :SUB $2 (length $4) $4 (length instrs) instrs)))
   ((:IDENTIFIER <derefs> #\= <expression>) (list $4 (mkvar $1) $2 "!"))
   ((LET :IDENTIFIER #\= <expression>) (list $4 (mkvar $1) "!"))
   ((LET :IDENTIFIER) (list (mkvar $1) nil "!"))
 
   ((FOR :IDENTIFIER #\= <expression> TO <expression> STEP <expression> #\: <statement*> NEXT)
    (let* ((var (mkvar $2)) (start (mklabel* "START")) (go-start (mkjump start)) (end (mklabel* "END")) (go-end (mkjump end)))
      (list $4 var "!"
            start $6 var "@" "<" go-end ">R" "IF"
            $10 var "@" $8 "+" var "!" go-start ">R" "THEN" end)))
 
   ((FOR :IDENTIFIER #\= <expression> TO <expression> #\: <statement*> NEXT)
    (let* ((var (mkvar $2)) (start (mklabel* "START")) (go-start (mkjump start)) (end (mklabel* "END")) (go-end (mkjump end)))
      (list $4 var "!"
            start $6 var "@" "<>" go-end ">R" "IF"
            $8 var "@" 1 "+" var "!" go-start ">R" "THEN" end)))
 
   ((WHILE <expression> #\: <statement*> WEND)
    (let* ((start (mklabel* "WHILE")) (go-start (mkjump start)) (end (mklabel* "WEND")) (go-end (mkjump end)))
      (list start go-end ">R" $2 "IF" $4 go-start ">R" "THEN" end)))
 
   ((IF <expression> THEN <statement*> END IF)
    (let* ((end (mklabel* "ENDIF")) (go-end (mkjump end)))
      (list go-end ">R" $2 "IF" $4 end)))
 
   ((IF <expression> THEN <statement*> ELSE <statement*> END IF)
    (let* ((else (mklabel* "ELSE")) (go-else (mkjump else)) (end (mklabel* "ENDIF")) (go-end (mkjump end)))
      (list go-else ">R" $2 "IF" $4 go-end ">R" "THEN" else $6 end)))
 
   ((LABEL :IDENTIFIER) (make-label :name $2))
   ((GOTO :IDENTIFIER) (make-jump :to $2))
   ((RETURN <expression>) (list $2 "RETURN"))
   ((<expression>) $1))
 
  (<statement*>
   (())
   ((#\: <statement*>) $2)
   ((<statement>) (mklist $1))
   ((<statement> #\: <statement*>) (cons $1 $3))
   ((:error) (error 'basic-parse-error :message "Expected a statement or series of statements"))))
 
 
(progn
  (defparameter +basic-keywords+
    '(ELSE IF THEN
      LABEL GOTO
      WHILE WEND
      FOR TO STEP NEXT IN WHERE
      SORT BY ASC DESC
      SUB
      LET VAR
      RETURN
      END
      AND OR NOT NOR TRUE FALSE))
  (defparameter +basic-specials+ (let ((hash (make-hash-table :test #'equalp))) (loop for key in +basic-keywords+ do (setf (gethash (symbol-name key) hash) key)) hash)))
 
(defstruct (lexed-basic (:constructor mlb (type value line))) type value line)
 
(defvar *basic-tokens* nil
  "List of tokens in progress")
 
(defvar *basic-line* nil
  "Line currently being parsed")
 
(defun parse (string)
  (let ((*basic-line* 0))
    (handler-case (let ((*basic-tokens* (lex string))) (flatten (basic-parser #'next-token)))
      (basic-parse-error (c)
        (error (format nil "BASIC: ~A near line ~D near ~A" (message c) (1+ *basic-line*) *basic-tokens*))))))
 
(defun next-token ()
  (lw:when-let (token (pop *basic-tokens*))
    (setf *basic-line* (lexed-basic-line token))
    (values (lexed-basic-type token) (lexed-basic-value token))))
 
(defun lex (string)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (pathnamep string) (setf string (slurp-string string)))
  (flet ((nl-p (char) (or (char= char #\Return) (char= char #\Linefeed))))
    (loop with end of-type fixnum = (length string)
          with line of-type fixnum = 0
          for i of-type fixnum below end
          for char of-type character = (char string i)
          if (find char '(#\Space #\Tab) :test #'char=) do (progn)
            else if (nl-p char) collect (mlb #\: #\: line) and
                   do (setf line (1+ line) i (1- (or (position-if-not #'nl-p string :start i) end)))
          else if (or (char= char #\:) (char= char #\;))
                 collect (mlb #\: #\: line)
          else if (and (char= char #\=) (< i (1- end)) (char= (char string (1+ i)) #\=))
                 collect (mlb '== '== line) and do (incf i)
          else if (find char "{}()[]=<>" :test #'char=)
                 collect (mlb char char line)
          else if (find char "~.^*%/+-&=,><" :test #'char=)
                 collect (mlb char char line)
          else if (or (char= char #\') (char= char #\"))
                 collect (multiple-value-bind (string end) (lex/substring string (1+ i) end char line)
                           (prog1 string (setf i end)))
          else if (digit-char-p char)
                 collect (multiple-value-bind (number end) (lex/number string i end line)
                           (prog1 number (setf i end)))
          else if (and (char= char #\$) (< i (1- end)) (digit-char-p (char string (1+ i))))
                 collect (multiple-value-bind (number end) (lex/number string (1+ i) end line)
                           (setf i end (lexed-basic-value number) (* 100 (lexed-basic-value number)))
                           number)
          else if (string-equal "REM" string :start2 i :end2 (min end (+ i 3)))
                 do (setf i (or (position #\Linefeed string :start i :test #'char=) end) line (1+ line))
          else
            collect (multiple-value-bind (bareword end) (lex/bareword string i end line)
                      (prog1 bareword (setf i end))))))
 
(defun lex/substring (string idx end terminator line)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string) (type fixnum idx end line) (type character terminator))
  (let* ((eos (position terminator string :start idx :end end :test #'char=))
         (str (nsubseq string idx eos)))
    (values (mlb :CONSTANT str line) (or eos end))))
 
(defun lex/number (string i end line)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string) (type fixnum i end line))
  (let ((integer 0) (point nil) (frac 0) (divisor 1))
    (loop for char = (when (< i end) (char string i))
          for digit = (when char (digit-char-p char))
          while (and char (or digit (char= char #\_) (and (not point) (char= char #\.))))
          if (char= char #\.) do (setf point t)
            else if (and digit point) do (setf frac (+ (* frac 10) digit) divisor (* 10 divisor))
                   else if digit do (setf integer (+ (* integer 10) digit))
                          do (incf i))
    (values (mlb :CONSTANT (+ integer (/ frac divisor)) line) (1- i))))
 
(defun lex/bareword (string i end line)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string) (type fixnum i end line))
  (let* ((terminators #.(format nil " ~C~C~C: ;({[]}).^*%/+-&,'\"<>=" #\Tab #\Return #\Linefeed))
         (eow (position-if (lambda (c) (find c terminators :test #'char=)) string :start i))
         (word (nsubseq string i eow)))
    (if-let (special (gethash word +basic-specials+))
            (values (mlb special special line) (if eow (1- eow) end))
            (values (mlb :IDENTIFIER word line) (if eow (1- eow) end)))))

