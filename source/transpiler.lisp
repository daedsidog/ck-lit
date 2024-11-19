(defpackage #:ck-lli-cpp-transpiler
  (:use #:cl #:ck-clle)
  (:export #:transpile))

(in-package #:ck-lli-cpp-transpiler)

;; Operators are Lisp functions, macros, or special operators.
;; Routines are operators which also have a function lambda expression.  This means that we can
;; define transpiled routines via the use of the definitions of operators.
(defclass expression () ())

(defclass routine (expression)
  ((cpp-name  :initarg :cpp-name  :accessor routine-cpp-name)
   (signature :initarg :signature :accessor routine-signature)
   (body      :initarg :body      :accessor routine-body :initform nil)))

(defmethod print-object ((obj routine) stream)
  (format stream "#<ROUTINE ~A>"
          (routine-cpp-name obj)))

(defun expressionp (obj)
  (or (typep obj 'expression)
      (numberp obj)
      (symbolp obj)))

(defvar *routine-table* (make-hash-table))
(defvar *op-table* (make-hash-table))
(defvar *entry-point-function-sym* nil)
(defvar *ignored-ops* (make-hash-table))

(defparameter +namespace-prefix+ "ck_lli_cpp")
(defparameter +indentation+ "  ")

(defun transpile (entry-point-function-symbol &optional (destination *standard-output*))
  "Transpile a Lisp function to a target language and save to a file or write to a stream.

The ENTRY-POINT-FUNCTION-SYMBOL parameter should be a symbol associated with a Lisp function that
takes either two arguments, corresponding to C argc and argv arguments, or no arguments at all.

The DESTINATION parameter specifies either a
stream (defaults to *STANDARD-OUTPUT*) or a file path where the transpiled code will be saved."
  ;; Check if ENTRY-POINT-FUNCTION-SYMBOL has an associated function.
  (let ((entry-point-function (symbol-function entry-point-function-symbol))
        lambda-list-len)
    ;; This might be a bad idea down the road.  We should think of a way to reuse transpiled
    ;; routines instead of re-transpiling everything every time.
    (setf *routine-table* (make-hash-table))
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
    (let ((*entry-point-function-sym* entry-point-function-symbol))
      (transpile-routine entry-point-function-symbol)
      ;; Write final transpiled program.
      (with-output-to-string (namespace)
        (maphash (lambda (k routine)
                   (unless (eqp k *entry-point-function-sym*)
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
            (write-string  ns-defs destination)
            (format destination "~%}~%~%"))))
      (format destination "~&int main(~A) {~%"
              (if (zerop lambda-list-len)
                  ""
                  "int argc, char* argv[]"))
      (format destination "~A"
              (ck-clle/string:indent (routine-body (routine *entry-point-function-sym*))
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
         (gensym-number (gensym))
         (result (format nil "~A_SUB_~A_~A"
                         (cpp-alphanumericate package-name)
                         gensym-number
                         cleaned-string)))
    result))

(defun verbose-symbol-name (sym)
  (format nil "~A from ~A"
          sym
          (package-name (symbol-package sym))))

(defparameter *should-return-result* nil)
(defparameter *is-toplevel-expression* t)
(defparameter *routine-args* ())

(defun cpp-argnamicate (arg-sym)
  (format nil "ARG_~A" (cpp-alphanumericate (symbol-name arg-sym))))

(defun transpile-form (form)
  "Return a string corresponding to C++ code equivalent to the Lisp FORM."
  (labels ((format-expr (expr)
             (format nil "~A~A~A"
                     (if (and *should-return-result*
                              *is-toplevel-expression*)
                         "return "
                         "")
                     expr
                     (if *is-toplevel-expression*
                         ";"
                         ""))))
    (cond
      ((symbolp form)
       (if (memberp form *routine-args*)
           (format-expr (cpp-argnamicate form))
           (format-expr (cpp-alphanumericate form))))
      ((numberp form)
       (format-expr form))
      ((listp form)
       (let ((car (car form)))
         (when (eqp car *entry-point-function-sym*)
           (error "Recursive call to main function ~A." car))
         (if-let ((op (op car)))
           (if (expressionp op)
               (format-expr (let ((*is-toplevel-expression* nil))
                              (funcall (op-lambda op) (cdr form))))
               (format nil "~A" (funcall (op-lambda op) (cdr form))))
           (let ((routine (routine car)))
             (if (nullp routine)
                 (let ((*should-return-result* t)
                       (*is-toplevel-expression* t))
                   (setf routine (transpile-routine car))))
             (labels ((routine-expansion (name args)
                        (format-expr
                         (format nil "~A::~A(~{~A~^, ~})"
                                 +namespace-prefix+
                                 name
                                 (let ((*is-toplevel-expression* nil))
                                   (mapcar #'transpile-form args))))))
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
  (let ((routine (or (routine routine-sym) (make-instance 'routine))))
    (when (routine-body routine)
      (format t "~&; Overwriting routine ~A" (verbose-symbol-name routine-sym))
      (setf (routine-body routine) nil))
    (setf (gethash routine-sym *routine-table*) routine)
    (let* ((lambda-expr (function-lambda-expression (symbol-function routine-sym)))
           (arglist (cadr lambda-expr))
           (body (cddr lambda-expr))
           (transpiled-body ())
           (signature nil))
      (setf (routine-cpp-name routine)
            (cpp-identificate (symbol-name routine-sym)
                              (package-name (symbol-package routine-sym))))
      (let ((last-form (last body))
            (*routine-args* arglist))
        (labels ((transpile-aux (body)
                   (when body
                     (if (eqp last-form body)
                         (let ((*should-return-result* t))
                           (push (transpile-form (car body)) transpiled-body))
                         (push (transpile-form (car body)) transpiled-body))
                     (transpile-aux (cdr body)))))
          (transpile-aux body)))
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
      (setf (routine-body routine)
            (format nil "~{~A~^~%~}" (nreverse transpiled-body)))
      (setf (routine-signature routine) signature)
      (format t "~&; Transpiled routine ~A~A"
              (verbose-symbol-name routine-sym)
              (if (eqp routine-sym *entry-point-function-sym*) " (entry point)" ""))))
  (routine routine-sym))

;;; OPERATORS

(defmacro expand-args (args)
  `(mapcar #'transpile-form ,args))

(defclass op ()
  ((symbol           :initarg :symbol :accessor op-symbol :type symbol
                     :initform (error "Operation must be associated with a symbol."))
   (expansion-lambda :initarg :lambda :accessor op-lambda :type function
                     :initform (error "Operation must be associated with an expansion lambda."))))

(defmethod initialize-instance :after ((instance op) &key lambda &allow-other-keys)
  (unless (and (functionp lambda)
               (multiple-value-bind (1st 2nd lambda-sig) (function-lambda-expression lambda)
                 (declare (ignore 1st 2nd))
                 (= (length (cadr lambda-sig)) 1)))
    (error "~A is not a lambda expression with exactly one argument" lambda)))

(defclass expression-op (expression op) ())

(defun op (op-sym)
  (gethash op-sym *op-table*))

(defun expression-op-p (obj)
  (typep obj 'expression-op))

(defmethod print-object ((obj expression-op) stream)
  (format stream "#<EXPRESSION-OP ~S>"
          (op-symbol obj)))

(defclass control-op (op) ())

(defun control-op-p (op)
  (typep op 'control-op))

(defmethod print-object ((obj control-op) stream)
  (format stream "#<CONTROL-OP ~S>"
          (op-symbol obj)))

(defun register-op (op)
  (when-let ((existing-op (gethash (op-symbol op) *op-table*)))
    (format t "~&; ~A overwritten with ~A" existing-op op))
  (setf (gethash (op-symbol op) *op-table*) op))

(defun define-expr-op (sym lambda)
  (register-op (make-instance 'expression-op :symbol sym :lambda lambda)))

(defun define-control-op (sym lambda)
  (register-op (make-instance 'control-op :symbol sym :lambda lambda)))

(defun op-func (op-sym)
  (op-lambda (gethash op-sym *op-table*)))

;;; EXPRESSION OPERATORS

(define-expr-op 'cl:+
    (lambda (args)
      (if (nullp args)
          "0"
          (format nil "(~{~A~^ + ~})" (expand-args args)))))

(define-expr-op 'cl:-
    (lambda (args)
      (if (nullp args)
          "0"
          (if (= (length args) 1)
              (format nil "(-~A)" (car args))
              (format nil "(~{~A~^ - ~})" (expand-args args))))))

(define-expr-op 'cl:*
    (lambda (args)
      (if (nullp args)
          "1"
          (format nil "(~{~A~^ * ~})" (expand-args args)))))

(define-expr-op 'cl:/
    (lambda (args)
      (if (nullp args)
          "1"
          (if (= (length args) 1)
              (format nil "(1/~A)" (car args))
              (format nil "(~{~A~^ / ~})" (expand-args args))))))

(define-expr-op 'cl:or
    (lambda (args)
      (if (nullp args)
          "false"
          (format nil "(~{~A~^ || ~})" (expand-args args)))))

(define-expr-op 'cl:and
    (lambda (args)
      (if (nullp args)
          "true"
          (format nil "(~{~A~^ && ~})" (expand-args args)))))

(define-expr-op 'cl:not
    (lambda (args)
      (format nil "(!~A)" (car (expand-args args)))))

(define-expr-op 'cl:=
    (lambda (args)
      (cond ((= (length args) 1) "true")
            (t (format nil "(~{~A~^ == ~})" (expand-args args))))))

;;; CONTROL OPERATORS

(define-control-op 'cl:block
    (lambda (args)
      (destructuring-bind (name &rest args) args
        (declare (ignore name))
        (concatenate 'string
                     (format nil "{~{~%~A~}"
                             (mapcar (lambda (arg)
                                       (ck-clle/string:indent (format nil "~A" arg) +indentation+))
                                     (expand-args args)))
                     (format nil "~%}")))))

(define-control-op 'cl:if
    (lambda (args)
      (destructuring-bind (cond-expr then-expr &optional else-expr) args
        (cond
          ((and then-expr else-expr)
           (format nil "if (~A) ~A~%else ~A"
                   (let ((*is-toplevel-expression* nil))
                     (transpile-form cond-expr))
                   (funcall (op-func 'cl:block) (list nil then-expr))
                   (funcall (op-func 'cl:block) (list nil else-expr))))
          (then-expr
           (format nil "if (~A) ~A"
                   (let ((*is-toplevel-expression* nil))
                     (transpile-form cond-expr))
                   (funcall (op-func 'cl:block) (list nil then-expr))))))))

(define-control-op 'cl:progn
    (lambda (args)
      (destructuring-bind (&rest forms) args
        (funcall (op-func 'cl:block) `(nil ,@forms)))))

(define-control-op 'cl:when
  (lambda (args)
    (destructuring-bind (cond-expr &rest forms) args
      (format nil "if (~A) ~A"
              (let ((*is-toplevel-expression* nil))
                (transpile-form cond-expr))
              (funcall (op-func 'cl:block) `(nil ,@forms))))))

(define-control-op 'cl:unless
    (lambda (args)
      (destructuring-bind (cond-expr &rest forms) args
        (funcall (op-func 'cl:when) `((not ,cond-expr) ,@forms)))))

(define-control-op 'cl:let
  (lambda (args)
    (destructuring-bind (bindings &rest body) args
      (let ((cpp-bindings
              (loop for binding in bindings
                    collect (format nil "auto ~A = [~{~A~^, ~}]() ~A();"
                                    (cpp-argnamicate (car binding))
                                    (mapcar #'cpp-argnamicate *routine-args*)
                                    (let ((*should-return-result* t))
                                      (funcall (op-func 'cl:block)
                                               `(nil ,(cadr binding))))))))
        (format nil "~{~A~^~%~}~%~A"
                cpp-bindings
                (let ((*routine-args*
                        (append *routine-args* (mapcar #'car bindings))))
                  (funcall (op-func 'cl:block) `(nil ,@body))))))))

;;; IGNORED OPERATORS

(defmacro ignore-op-symbols (&rest op-syms)
  `(progn
     ,@(mapcar (lambda (op-sym)
                 `(setf (gethash ',op-sym *ignored-ops*) t))
               op-syms)))

(defun ignored-op-symbol-p (op-sym)
  (gethash op-sym *ignored-ops*))

(ignore-op-symbols cl:declare)
