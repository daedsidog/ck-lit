(defpackage #:ck-lli-cpp-transpiler
  (:use #:cl #:ck-clle)
  (:export #:transpile))

(in-package #:ck-lli-cpp-transpiler)

;; Operators are Lisp functions, macros, or special operators.
;; Routines are operators which also have a function lambda expression.  This means that we can
;; define transpiled routines via the use of the definitions of operators.
(defclass expression () ())

(defclass routine (expression)
  ((name      :initarg :name      :accessor routine-name)
   (cpp-name  :initarg :cpp-name  :accessor routine-cpp-name)
   (signature :initarg :signature :accessor routine-signature)
   (body      :initarg :body      :accessor routine-body :initform nil)))

(defmethod print-object ((obj routine) stream)
  (format stream "#<ROUTINE ~A>"
          (routine-name obj)))

(defvar *op-table* (make-hash-table))
(defvar *requirement-table* (make-hash-table))

(defparameter +namespace-prefix+ "ck_lli_cpp")
(defparameter +indentation+ "  ")
(defparameter +cpp-nil+ "0")

(defvar *routine-table* (make-hash-table))
(defvar *requirement-inclusion-table* (make-hash-table))
(defvar *op-requirement-registration-table* (make-hash-table))
(defvar *entry-point-function-symbol* nil)

(defun transpile (entry-point-function-symbol &optional (destination *standard-output*))
  "Transpile a Lisp function to a target language and save to a file or write to a stream.

The ENTRY-POINT-FUNCTION-SYMBOL parameter should be a symbol associated with a Lisp function that
takes either two arguments, corresponding to C argc and argv arguments, or no arguments at all.

The DESTINATION parameter specifies either a stream (defaults to *STANDARD-OUTPUT*) or a file path
where the transpiled code will be saved."
  ;; Check if ENTRY-POINT-FUNCTION-SYMBOL has an associated function.
  (let ((entry-point-function (symbol-function entry-point-function-symbol))
        lambda-list-len)
    (when (nullp entry-point-function)
      (error "~A does not have an associated function." entry-point-function-symbol))
    ;; Check the number of parameters for the ENTRY-POINT-FUNCTION.
    (setf lambda-list-len (length (cadr (function-lambda-expression entry-point-function))))
    (unless (or (= lambda-list-len 0)
                (= lambda-list-len 2))
      (error "~A is not a function with either 0 or 2 arguments." entry-point-function-symbol))
    ;; Check if DESTINATION is a valid file path or stream.
    (unless (or (streamp destination)
                (and (stringp destination)
                     (probe-file (truename (merge-pathnames destination)))))
      (error "~A is not a valid file path or a stream." destination))
    (let ((*entry-point-function-symbol* entry-point-function-symbol)
          ;; This might be a bad idea down the road.  We should think of a way to reuse transpiled
          ;; routines instead of re-transpiling everything every time.
          (*routine-table* (make-hash-table))
          (*requirement-inclusion-table* (make-hash-table))
          (*op-requirement-registration-table* (make-hash-table)))
      ;; Transpile the program from its entry point.
      (transpile-routine entry-point-function-symbol)
      ;; Write final transpiled program.  Start with writing the include requirements, such as
      ;; include files and other preamble that should go before the namespace.
      (let ((have-include-requirements nil))
        (maphash (lambda (req flag)
                   (declare (ignore flag))
                   (when (typep req 'include-requirement)
                     (format destination "~&~A~%" (requirement-cpp req))
                     (setf have-include-requirements t)))
                 *requirement-inclusion-table*)
        (when have-include-requirements
          (format destination "~%")))
      (with-output-to-string (namespace)
        ;; Write preamble code that should be part of the namespace, such as various structs and
        ;; other constant data.
        (maphash (lambda (req flag)
                   (declare (ignore flag))
                   (when (typep req 'namespace-requirement)
                     (princ (ck-clle/string:indent (format nil "~%~%~A" (requirement-cpp req))
                                                   +indentation+)
                            namespace)))
                 *requirement-inclusion-table*)
        ;; Write the transpiled routines under their own namespace definition.
        (maphash (lambda (k routine)
                   (unless (eqp k *entry-point-function-symbol*)
                     (format namespace "~A"
                             (ck-clle/string:indent
                              (format nil "~%~%~A {~%~A~%}"
                                      (routine-signature routine)
                                      (ck-clle/string:indent (routine-body routine)
                                                             +indentation+))
                              +indentation+))))
                 *routine-table*)
        (let ((ns-defs (get-output-stream-string namespace)))
          (unless (string-empty-p ns-defs)
            (format destination "~&namespace ~A {" +namespace-prefix+)
            (write-string ns-defs destination)
            (format destination "~%}~%~%"))))
      (format destination "~&int main(~A) {~%"
              (if (zerop lambda-list-len)
                  ""
                  "int argc, char* argv[]"))
      (format destination "~A"
              (ck-clle/string:indent (routine-body (routine *entry-point-function-symbol*))
                                     +indentation+))
      (format destination "~%}"))))

(defun routine (sym)
  (gethash sym *routine-table*))

(defun routinesquep (routine-sym)
  (and routine-sym
       (let ((routine-func (symbol-function routine-sym)))
         (and routine-func
              (functionp routine-func)
              (function-lambda-expression routine-func)))))

(defun cpp-alphanumericate (input-string)
  "Prune INPUT-STRING to keep only the alphanumeric portion and replace hyphens with underscores."
  (let ((result (map 'list (lambda (char)
                             (if (alphanumericp char)
                                 char
                                 #\_))
                     input-string)))
    (coerce result 'string)))

(defun cpp-identificate (input-string &optional (package-name ""))
  "Transform INPUT-STRING into a valid C++ identifier."
  (let* ((cleaned-string (cpp-alphanumericate input-string))
         (gensym-number (gensym "SUB"))
         (result (format nil "~A_~A_~A"
                         (cpp-alphanumericate package-name)
                         gensym-number
                         cleaned-string)))
    result))

(defun verbose-symbol-name (sym)
  (format nil "~A from ~A"
          sym
          (package-name (symbol-package sym))))

(defparameter *routine* nil)
(defparameter *routine-args* ())
(defparameter *tagbody-labels* ())

(defmacro transpile-forms (forms)
  `(mapcar #'transpile-form ,forms))

(defun cpp-argnamicate (arg-sym)
  (format nil "ARG_~A" (cpp-alphanumericate (symbol-name arg-sym))))

(defun cpp-lambdicate (expression &optional bindings)
  (let ((cpp-closure (format nil "[~{&~A~^, ~}]"
                             (mapcar #'cpp-argnamicate *routine-args*)))
        (cpp-arglist (format nil "(~{auto ~A~^, ~})"
                             (mapcar #'cpp-argnamicate (mapcar #'car bindings))))
        (args (mapcar #'cadr bindings)))
    (format nil "~A~A {~%~A~%}(~{~A~^, ~})"
            cpp-closure
            cpp-arglist
            (ck-clle/string:indent expression +indentation+)
            args)))

(defclass op ()
  ((symbol           :initarg :symbol :accessor op-symbol :type symbol
                     :initform (error "Operation must be associated with a symbol."))
   (expansion-lambda :initarg :lambda :accessor op-lambda :type function
                     :initform (error "Operation must be associated with an expansion lambda."))
   (requirements     :initform nil    :accessor op-requirements)))

(defmethod initialize-instance :after ((instance op) &key lambda &allow-other-keys)
  (unless (and (functionp lambda)
               (multiple-value-bind (1st 2nd lambda-sig) (function-lambda-expression lambda)
                 (declare (ignore 1st 2nd))
                 (= (length (cadr lambda-sig)) 1)))
    (error "~A is not a lambda expression with exactly one argument" lambda)))

(defun op (op-sym)
  (gethash op-sym *op-table*))

(defmethod print-object ((obj op) stream)
  (format stream "#<OP ~S>"
          (op-symbol obj)))

(defclass requirement ()
  ((name :initarg :name :reader requirement-name)
   (code :initarg :code :reader requirement-cpp)))

(defclass include-requirement (requirement) ())

(defclass namespace-requirement (requirement) ())

(defmethod print-object ((obj requirement) stream)
  (format stream "#<REQUIREMENT ~S>"
          (requirement-name obj)))

(defmacro define-requirement (requirement-type name code)
  `(let ((req (make-instance ',(if (eqp requirement-type 'include)
                                   'include-requirement
                                   'namespace-requirement)
                             :name ',name
                             :code ,code)))
     (when-let ((existing-req (gethash ',name *requirement-table*)))
       (format t "~&; Overwriting ~A" existing-req))
     (setf (gethash ',name *requirement-table*) req)
     req))

(defmacro define-dependencies (&body schema)
  `(loop for (op-syms req-syms) in ',schema do
    (loop for op-sym in op-syms do
      (loop for req-sym in req-syms do
        (let ((req (gethash req-sym *requirement-table*))
              (op (op op-sym)))
          (unless req
            (error "~A does not exist in the requirement table." req-sym))
          (pushnew req (op-requirements op))
          (format nil "~&; ~A added as a requirement of ~A" req op))))))

(defun register-op (op)
  (when-let ((existing-op (gethash (op-symbol op) *op-table*)))
    (format t "~&; ~A overwritten with ~A" existing-op op))
  (setf (gethash (op-symbol op) *op-table*) op))

(defun op-func (op-sym)
  (op-lambda (gethash op-sym *op-table*)))

(defmacro define-op (sym args-binding &body body)
  `(register-op (make-instance 'op
                               :symbol ,sym
                               :lambda (lambda ,args-binding
                                         ,@body))))

(defun transpile-form (form)
  "Return a string corresponding to C++ code equivalent to the Lisp FORM."
  (cond
    ((symbolp form)
     ;; The first case we want to examine is if the current form is a specific symbol.  The NIL
     ;; symbol transpiles as C++ nullptr.
     (if (eqp form nil)
         +cpp-nil+
         (if (eqp form t)
             "1"
             (if (memberp form *routine-args*)
                 (cpp-argnamicate form)
                 (cpp-alphanumericate form)))))
    ((stringp form)
     ;; Check if it is a string.  Strings are transpiled as-is.
     (format nil "~S" form))
    ((numberp form)
     ;; Check if the form is a number.  Usually, numbers should also transpile as-is.  At the
     ;; moment, we do not handle the entire CL numerical tower.
     (format nil "~A" form))
    ((listp form)
     ;; Check if the form is a list.  This is where it gets interesting, because lists will also
     ;; contain functions and macros.
     ;;
     ;; We handle 3 things here:
     ;;   1. Operator calls: calls that are not defined by usercode, and we have to provide their
     ;;      implementation.
     ;;   2. Routine calls: functions defined by usercode, for which we have to just invoke.
     ;;   3. Macro calls: calls which we have to macroexpand and process.
     (let ((car (car form)))
       (when (and (boundp '*entry-point-function-symbol*)
                  (eqp car *entry-point-function-symbol*))
         (error "Recursive call to main function ~A." car))
       (if-let ((op (op car)))
         (format nil "~A" (expand-op (op-symbol op) (cdr form)))
         (if (macro-function car)
             (transpile-form (macroexpand form))
             (let ((routine (routine car)))
               (when (nullp routine)
                 (setf routine (transpile-routine car)))
               (labels ((routine-expansion (name args)
                          (format nil "~A::~A(~{~A~^, ~})"
                                  +namespace-prefix+
                                  name
                                  (mapcar #'transpile-form args))))
                 (if (routine-cpp-name routine)
                     (routine-expansion (routine-cpp-name routine) (cdr form))
                     (progn
                       (if (routinesquep car)
                           (progn
                             (transpile-routine car)
                             (routine-expansion (routine-cpp-name car) (cdr form)))
                           (error "~A is not a symbol, routine, or supported operator."
                                  car))))))))))))

(defun transpile-routine (routine-sym)
  (unless (routinesquep routine-sym)
    (error "~A is not a valid routine symbol." routine-sym))
  (let* ((routine (or (routine routine-sym) (make-instance 'routine)))
         (*routine* routine))
    (when (routine-body routine)
      (format t "~&; Overwriting routine ~A" (verbose-symbol-name routine-sym))
      (setf (routine-body routine) nil))
    (setf (gethash routine-sym *routine-table*) routine)
    (setf (routine-name routine) routine-sym)
    (let* ((lambda-expr (function-lambda-expression (symbol-function routine-sym)))
           (arglist (cadr lambda-expr))
           (*routine-args* arglist)
           (body (cddr lambda-expr))
           (transpiled-body (mapcar #'transpile-form body))
           (signature nil))
      (setf (routine-cpp-name routine)
            (cpp-identificate (symbol-name routine-sym)
                              (package-name (symbol-package routine-sym))))
      (let ((return-type (if transpiled-body
                             "auto"
                             "void")))
        (setf signature (concatenate 'string
                                     (unless (zerop (length arglist))
                                       (format nil "template <~{typename T~A~^, ~}>~%"
                                               (iota (length arglist))))
                                     (format nil "~A ~A(~{~A~^, ~})"
                                             return-type
                                             (routine-cpp-name routine)
                                             (mapcar (lambda (idx arg)
                                                       (format nil "T~A ~A"
                                                               idx (cpp-argnamicate arg)))
                                                     (iota (length arglist))
                                                     arglist)))))
      (let ((transpiled-body (nreverse transpiled-body)))
        (setf (routine-body routine)
              (format nil "~{~A;~%~}return ~A;"
                      (butlast transpiled-body)
                      (car (last transpiled-body)))))
      (setf (routine-signature routine) signature)
      (format t "~&; Transpiled routine ~A~A"
              (verbose-symbol-name routine-sym)
              (if (and (boundp '*entry-point-function-symbol*)
                       (eqp routine-sym *entry-point-function-symbol*))
                  " (entry point)" ""))))
  (routine routine-sym))

(defun expand-op (op-sym args)
  (let ((op (op op-sym)))
    (when (boundp '*entry-point-function-symbol*)
      ;; Check if the operator has already been registered, which means that its requirements have
      ;; already been satisfied.
      (unless (gethash op *op-requirement-registration-table*)
        (when-let ((requirements (op-requirements op)))
          (loop for req in requirements
                do (unless (gethash req *requirement-inclusion-table*)
                     (setf (gethash req *requirement-inclusion-table*) t)
                     (format t "~&; ~A requested by ~A" req op))))
        (setf (gethash op *op-requirement-registration-table*) t)))
    (funcall (op-lambda op) args)))

;;; OPERATORS

(define-op 'cl:+ (args)
  (if (nullp args)
      "0"
      (format nil "(~{~A~^ + ~})" (transpile-forms args))))

(define-op 'cl:- (args)
  (if (nullp args)
      "0"
      (if (= (length args) 1)
          (format nil "(-~A)" (car args))
          (format nil "(~{~A~^ - ~})" (transpile-forms args)))))

(define-op 'cl:* (args)
  (if (nullp args)
      "1"
      (format nil "(~{~A~^ * ~})" (transpile-forms args))))

(define-op 'cl:/ (args)
  (if (nullp args)
      "1"
      (if (= (length args) 1)
          (format nil "(1/~A)" (car args))
          (format nil "(~{~A~^ / ~})" (transpile-forms args)))))

(define-op 'cl:or (args)
  (if (nullp args)
      "false"
      (format nil "(~{~A~^ || ~})" (transpile-forms args))))

(define-op 'cl:and (args)
  (if (nullp args)
      "true"
      (format nil "(~{~A~^ && ~})" (transpile-forms args))))

(define-op 'cl:not (args)
  (format nil "(!(~A))" (car (transpile-forms args))))

(define-op 'cl:princ (args)
  (let ((stream (if (null (cdr args))
                    "std::cout"
                    (car (cdr args))))
        (expr-name (cpp-argnamicate (gensym)))
        (expr (car (transpile-forms args))))
    (cpp-lambdicate (format nil "auto ~A = ~A;~%~A << ~A;~%return ~A;"
                            expr-name
                            expr
                            stream
                            expr-name
                            expr-name))))

(define-op 'cl:terpri (args)
  (let ((stream (if (null args)
                    "std::cout"
                    (car args))))
    (cpp-lambdicate (format nil "~A << std::endl;~%return nullptr;" stream))))

(defmacro cpp-comparison-expr (args cpp-operator &optional should-logical-or)
  `(let ((expanded-args (transpile-forms ,args)))
    (cond ((= (length expanded-args) 1) "true")
          ((= (length expanded-args) 2) (format nil ,(format nil "~~A ~A ~~A" cpp-operator)
                                                (first expanded-args)
                                                (second expanded-args)))
          (t 
           (let ((argnames (loop for arg in expanded-args collect (cpp-argnamicate (gensym)))))
             (cpp-lambdicate (concatenate 'string
                                          (format nil "~{~{auto ~A = ~A;~%~}~}"
                                                  (loop for argname in argnames
                                                        for arg in expanded-args
                                                        collect (list argname arg)))
                                          (format nil
                                                  ,(format
                                                    nil
                                                    "return (~~{(~~{~~A ~A ~~A~~})~~^ ~A ~~});"
                                                    cpp-operator
                                                    (if should-logical-or "||" "&&"))
                                                  (loop for (n1 n2) on argnames
                                                        when n2 collect (list n1 n2))))))))))

(define-op 'cl:= (args)
  (cpp-comparison-expr args "=="))

(define-op 'cl:< (args)
  (cpp-comparison-expr args "<"))

(define-op 'cl:> (args)
  (cpp-comparison-expr args ">"))

(define-op 'cl:>= (args)
  (cpp-comparison-expr args ">="))
  
(define-op 'cl:<= (args)
  (cpp-comparison-expr args "<="))

(define-op 'cl:/= (args)
  (cpp-comparison-expr args "!=" t))

(define-op 'cl:zerop (args)
  (transpile-form `(= 0 ,(car args))))

(define-op 'cl:setq (args)
  (if (eqp (length args) 2)
      (format nil "~A = ~A"
              (cpp-argnamicate (car args))
              (transpile-form (cadr args)))
      (let ((assignments (loop for (first second) on args by #'cddr
                               collect (cons first second))))
        (cpp-lambdicate (concatenate 'string
                                     (format nil "~{~A;~%~}"
                                             (mapcar (lambda (arg-pair)
                                                       (format nil "~A = ~A"
                                                               (cpp-argnamicate (car arg-pair))
                                                               (transpile-form (cdr arg-pair))))
                                                     (butlast assignments)))
                                     (let* ((last-elt (car (last assignments)))
                                            (argname (car last-elt))
                                            (argval (cdr last-elt)))
                                     (format nil "return ~A = ~A;"
                                             (cpp-argnamicate argname)
                                             (transpile-form argval))))))))

(define-op 'cl:read-byte (args)
  (destructuring-bind (stream &optional eof-error-p eof-value) args
    ;; TODO: Handle EOF-ERROR-P and EOF-VALUE
    (declare (ignore eof-error-p eof-value))
    (let ((instream (if (eqp stream '*standard-input*)
                        "std::cin"
                        stream))
          (argname (cpp-argnamicate (gensym))))
      (cpp-lambdicate (format nil "char ~A = ~A.get();
return ~A == std::char_traits<char>::eof()? 0 : ~A;"
                              argname
                              instream
                              argname
                              argname)))))

(define-op 'cl:cons (args)
  (destructuring-bind (se1 se2) (transpile-forms args)
    (format nil "~A::cons(~A, ~A)"
            +namespace-prefix+
            se1
            se2)))

(define-op 'cl:byte (args)
  (destructuring-bind (size position) args
    (transpile-form `(cons ,size ,position))))

(define-op 'cl:car (args)
  (format nil "~A.car" (car (transpile-forms args))))

(define-op 'cl:cdr (args)
  (format nil "~A.cdr" (car (transpile-forms args))))

(define-op 'cl:ldb (args)
  (destructuring-bind (bytespec integer) (transpile-forms args)
    (cpp-lambdicate
     (concatenate 'string
                  (format nil "auto cons = ~A;~%"
                          bytespec)
                  (format nil "return (~A >> cons.cdr) & ((1 << cons.car) -1);~%"
                          integer)))))

(define-op 'cl:go (args)
  (let ((label (car args)))
    (loop for (lisp-label . cpp-label-name) in *tagbody-labels*
          when (eqp label lisp-label)
            do (return (cpp-lambdicate (format nil "throw ~A_ENUM;" cpp-label-name))))))

;; FIXME: Needs to handle the case of returning out of named blocks.
(define-op 'cl:return-from (args)
  (destructuring-bind (block-name &optional value) args
    (if (nullp block-name)
        (if value
            (format nil "return ~A" (transpile-form value))
            "return"))))

(define-op 'cl:return (args)
  (if (car args)
      (format nil "return ~A" (transpile-form (car args)))
      "return"))

(define-op 'cl:progn (args)
  (cpp-lambdicate (format nil "~{~A;~%~}return ~A;"
                          (transpile-forms (butlast args))
                          (car (transpile-forms (last args))))))

;; KLUDGE: This is not how CL:BLOCK works, but it's what we use for now in order to have primitive
;; functionality.
(define-op 'cl:block (args)
  (destructuring-bind (name &rest rest-args) args
    (declare (ignore name))
          (transpile-form `(cl:progn ,@rest-args))))
  
(define-op 'cl:if (args)
  (destructuring-bind (cond-expr then-expr &optional else-expr) args
    (cond
      ((and then-expr else-expr)
       (cpp-lambdicate (format nil"return ~A? ~A : ~A;"
                               (transpile-form cond-expr)
                               (transpile-form then-expr)
                               (transpile-form else-expr))))
      (then-expr
       (cpp-lambdicate (format nil "return ~A? ~A : ~A;"
                               (transpile-form cond-expr)
                               (transpile-form then-expr)
                               +cpp-nil+))))))

(define-op 'cl:when (args)
  (destructuring-bind (cond-expr &rest forms) args
    (transpile-form `(cl:if ,cond-expr (cl:progn ,@forms)))))

(define-op 'cl:unless (args)
  (destructuring-bind (cond-expr &rest forms) args
    (transpile-form `(cl:if (cl:not ,cond-expr) (cl:progn ,@forms)))))

(define-op 'cl:let (args)
  (destructuring-bind (bindings &rest body) args
    (cpp-lambdicate (format nil "~{~A;~%~}return ~A;"
                            (let ((*routine-args* (append *routine-args* (mapcar #'car bindings))))
                              (transpile-forms (butlast body)))
                            (let ((*routine-args* (append *routine-args* (mapcar #'car bindings))))
                              (transpile-form (car (last body)))))
                    (mapcar (lambda (binding)
                              (list (car binding) (transpile-form (cadr binding))))
                            bindings))))

(define-op 'cl:let* (args)
  (destructuring-bind (bindings &rest body) args
    (if (= (length bindings) 1)
        (transpile-form `(cl:let ,bindings ,@body))
        (transpile-form `(cl:let (,(car bindings))
                           (cl:let* ,(cdr bindings)
                             ,@body))))))

(define-op 'cl:tagbody (args)
  (let* ((*tagbody-labels* *tagbody-labels*)
         (enum-name (cpp-alphanumericate (format nil "~A" (gensym "ENUM"))))
         (result (format nil "enum ~A {" enum-name)))
    (loop for form in args
          when (not (listp form))
            do (let ((label-name
                       (format nil "LABEL_~A"
                               (cpp-alphanumericate (symbol-name (gensym (symbol-name form)))))))
                 (push (cons form label-name) *tagbody-labels*)
                 (setf result (format nil "~A~&~A~A_ENUM,"
                                      result
                                      +indentation+
                                      label-name))))
    (setf result (format nil "~A~&};" result))
    (let ((open nil))
      (labels ((catch-block ()
                 (let ((result (format nil "catch (~A label_enum) {" enum-name)))
                   (when *tagbody-labels*
                     (setf result (format nil "~A~%~Aif (label_enum == ~A) goto ~A;~%"
                                          result
                                          +indentation+
                                          (format nil "~A_ENUM" (cdr (car *tagbody-labels*)))
                                          (cdr (car *tagbody-labels*))))
                     (setf result (format nil "~A~{~{~Aelse if (label_enum == ~A) goto ~A;~%~}~}"
                                          result
                                          (mapcar (lambda (label)
                                                    (list +indentation+
                                                          (format nil "~A_ENUM" (cdr label))
                                                          (cdr label)))
                                                  (cdr *tagbody-labels*)))))
                   (setf result (format nil "~A}" result)))))
        (loop for form in args
              if (not (listp form))
                do (setf result (format nil "~A~&~A~A:~%try {"
                                        result
                                        (if open
                                            (format nil "}~%~A~%"
                                                    (catch-block))
                                            (progn
                                              (setf open t)
                                              ""))
                                        (cdr (assoc form *tagbody-labels*))))
              else do
                (unless open
                  (setf result (format nil "~A~&try {" result))
                  (setf open t))
                (setf result (format nil "~A~&~A;"
                                     result
                                     (ck-clle/string:indent (transpile-form form)
                                                            +indentation+))))
        (when open
          (setf result (format nil "~A}~%~A"
                               result
                               (catch-block))))))
    (setf result (format nil "~A~%return ~A;" result +cpp-nil+))
    (cpp-lambdicate result)))

(define-op 'cl:loop (args)
  (transpile-form (macroexpand `(khazern-extrinsic:loop ,@args))))

(define-op 'cl:declare (args)
  (declare (ignore args))
  "/* CL:DECLARE ignored */")

;;; OPERATOR DEPENDENCIES

(define-requirement include iostream
  "#include <iostream>")

(define-requirement namespace cons-cell-struct
  "template <typename T, typename V>
struct cons {
  T car;
  V cdr;
  cons(T car) : car(car), cdr(nullptr) {}
  cons(T car, V cdr) : car(car), cdr(cdr) {}
};

template<typename T, typename V>
std::ostream& operator<<(std::ostream& os, const cons<T, V>& c) {
  os << \"(\" << c.car;
  auto current = &c.cdr;
  while (true) {
      if (current == nullptr) {
        break;
      }
      else if (auto next_cons = dynamic_cast<const cons<typename V::car_type,
                                                        typename V::cdr_type>*>(current)) {
        os << \" \" << next_cons->car;
        current = &next_cons->cdr;
      }
      else {
        os << \" . \" << *current;
        break;
      }
  }
  os << \")\";
  return os;
}

std::ostream& operator<<(std::ostream& os, std::nullptr_t) {
  return os << \"NIL\";
}")

(define-requirement include exception
  "#include <exception>")

(define-dependencies
  ((cl:princ cl:terpri cl:read-byte) (iostream))
  ((cl:cons) (cons-cell-struct iostream))
  ((cl:tagbody) (exception)))
