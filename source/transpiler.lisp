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
          (routine-cpp-name obj)))

(defun expressionp (obj)
  (or (typep obj 'expression)
      (numberp obj)
      (symbolp obj)))

(defvar *op-table* (make-hash-table))
(defvar *requirement-table* (make-hash-table))
(defvar *ignored-ops-table* (make-hash-table))

(defparameter +namespace-prefix+ "ck_lli_cpp")
(defparameter +indentation+ "  ")

(defvar *routine-table*)
(defvar *requirement-inclusion-table*)
(defvar *op-requirement-registration-table*)
(defvar *entry-point-function-symbol*)

(defun transpile (entry-point-function-symbol &optional (destination *standard-output*))
  "Transpile a Lisp function to a target language and save to a file or write to a stream.

The ENTRY-POINT-FUNCTION-SYMBOL parameter should be a symbol associated with a Lisp function that
takes either two arguments, corresponding to C argc and argv arguments, or no arguments at all.

The DESTINATION parameter specifies either a
stream (defaults to *STANDARD-OUTPUT*) or a file path where the transpiled code will be saved."
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

(defparameter *should-return-result* nil)
(defparameter *is-toplevel-expression* t)
(defparameter *routine* nil)
(defparameter *routine-args* ())
(defparameter *tagbody-labels* ())

(defun cpp-argnamicate (arg-sym)
  (format nil "ARG_~A" (cpp-alphanumericate (symbol-name arg-sym))))

(defun transpile-form (form)
  "Return a string corresponding to C++ code equivalent to the Lisp FORM."
  (format t "~&; Transpiling form ~A with SRR = ~A" form *should-return-result*)
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
       (if (eqp form nil)
           "nullptr"
           (if (memberp form *routine-args*)
               (format-expr (cpp-argnamicate form))
               (format-expr (cpp-alphanumericate form)))))
      ((stringp form) (format nil "~S" form))
      ((numberp form) (format-expr form))
      ((listp form)
       (let ((car (car form)))
         (when (ignored-op-symbol-p car)
           (return-from transpile-form (format nil "/* Ignored operator: ~A */"
                                               (verbose-symbol-name car))))
         (when (and (boundp '*entry-point-function-symbol*)
                    (eqp car *entry-point-function-symbol*))
           (error "Recursive call to main function ~A." car))
         (if-let ((op (op car)))
           (if (expressionp op)
               (format-expr (let ((*is-toplevel-expression* nil))
                              (expand-op (op-symbol op) (cdr form))))
               (format nil "~A" (expand-op (op-symbol op) (cdr form))))
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
  (let* ((routine (or (routine routine-sym) (make-instance 'routine)))
         (*routine* routine))
    (when (routine-body routine)
      (format t "~&; Overwriting routine ~A" (verbose-symbol-name routine-sym))
      (setf (routine-body routine) nil))
    (setf (gethash routine-sym *routine-table*) routine)
    (setf (routine-name routine) routine-sym)
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
    ;; Lisp doesn't have a distinction between control operators and expression operators like most
    ;; languages do, and so in Lisp we can arbitrarily compose forms as we please.
    ;;
    ;; For example, in Lisp we can do
    ;;
    ;;   (+ 1 (IF (PREDP 1) 2 3)) ; Lisp code
    ;;
    ;; which is a valid expression, but we cannot do the same in C++ unless we use the tertiary
    ;; assignment syntax, e.g.:
    ;;
    ;;   (1 + pred()? 2 : 3) // C++ code
    ;;
    ;; Even with the tertiary syntax, we are still limited to expressions inside it.  We cannot, for
    ;; example, run loops or other constructs.
    ;;
    ;; The solution is to use C++11 lambdas to compose these type of programs.
    (if (and (control-op-p op)
             (not *is-toplevel-expression*))
        (format nil "~%[~{~A~^, ~}]() ~A()"
                (mapcar #'cpp-argnamicate *routine-args*)
                (let ((*should-return-result* t)
                      (*is-toplevel-expression* t))
                  (expand-op 'cl:progn `((,op-sym ,@args)))))
        (funcall (op-lambda op) args))))

;;; OPERATORS

(defmacro expand-args (args)
  `(loop for arg in ,args
         for arg-idx from 0
         with arg-count = (length ,args)
         with expanded-args = ()
         do (let ((*should-return-result*
                    (if (nullp *should-return-result*)
                        nil
                        (= arg-idx (1- arg-count)))))
              (push (transpile-form arg) expanded-args))
         finally (return (nreverse expanded-args))))

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

(defclass expression-op (expression op) ())

(defun op (op-sym)
  (gethash op-sym *op-table*))

(defun expression-op-p (obj)
  (typep obj 'expression-op))

(defmethod print-object ((obj expression-op) stream)
  (format stream "#<EXPRESSION-OP ~S>"
          (op-symbol obj)))

(defclass control-op (op) ())

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

(defun control-op-p (op)
  (typep op 'control-op))

(defmethod print-object ((obj control-op) stream)
  (format stream "#<CONTROL-OP ~S>"
          (op-symbol obj)))

(defun register-op (op)
  (when-let ((existing-op (gethash (op-symbol op) *op-table*)))
    (format t "~&; ~A overwritten with ~A" existing-op op))
  (setf (gethash (op-symbol op) *op-table*) op))

(defun op-func (op-sym)
  (op-lambda (gethash op-sym *op-table*)))

(defmacro define-expr-op (sym args-binding &body body)
  `(register-op (make-instance 'expression-op
                               :symbol ,sym
                               :lambda (lambda ,args-binding
                                         ,@body))))

(defmacro define-control-op (sym args-binding &body body)
  `(register-op (make-instance 'control-op
                               :symbol ,sym
                               :lambda (lambda ,args-binding
                                         ,@body))))

;;; EXPRESSION OPERATORS

(define-expr-op 'cl:+ (args)
  (if (nullp args)
      "0"
      (format nil "(~{~A~^ + ~})" (expand-args args))))

(define-expr-op 'cl:- (args)
  (if (nullp args)
      "0"
      (if (= (length args) 1)
          (format nil "(-~A)" (car args))
          (format nil "(~{~A~^ - ~})" (expand-args args)))))

(define-expr-op 'cl:* (args)
  (if (nullp args)
      "1"
      (format nil "(~{~A~^ * ~})" (expand-args args))))

(define-expr-op 'cl:/ (args)
  (if (nullp args)
      "1"
      (if (= (length args) 1)
          (format nil "(1/~A)" (car args))
          (format nil "(~{~A~^ / ~})" (expand-args args)))))

(define-expr-op 'cl:or (args)
  (if (nullp args)
      "false"
      (format nil "(~{~A~^ || ~})" (expand-args args))))

(define-expr-op 'cl:and (args)
  (if (nullp args)
      "true"
      (format nil "(~{~A~^ && ~})" (expand-args args))))

(define-expr-op 'cl:not (args)
  (format nil "(!~A)" (car (expand-args args))))

(defun cpp-lambdicate (expression)
  (format nil "[~{~A~^, ~}]() {~%~A~%}()"
          (mapcar #'cpp-argnamicate *routine-args*)
          (ck-clle/string:indent expression +indentation+)))

(define-expr-op 'cl:princ (args)
  (let ((stream (if (null (cdr args))
                    "std::cout"
                    (car (cdr args))))
        (expr-name (cpp-argnamicate (gensym)))
        (expr (car (expand-args args))))
    (cpp-lambdicate (format nil "auto ~A = ~A;~%~A << ~A;~%return ~A;"
                            expr-name
                            expr
                            stream
                            expr-name
                            expr-name))))

(define-expr-op 'cl:terpri (args)
  (let ((stream (if (null args)
                    "std::cout"
                    (car args))))
    (cpp-lambdicate (format nil "~A << std::endl;~%return nullptr;" stream))))

(defmacro cpp-comparison-expr (args cpp-operator &optional should-logical-or)
  `(let ((expanded-args (expand-args ,args)))
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

(define-expr-op 'cl:= (args)
  (cpp-comparison-expr args "=="))

(define-expr-op 'cl:< (args)
  (cpp-comparison-expr args "<"))

(define-expr-op 'cl:> (args)
  (cpp-comparison-expr args ">"))

(define-expr-op 'cl:>= (args)
  (cpp-comparison-expr args ">="))
  
(define-expr-op 'cl:<= (args)
  (cpp-comparison-expr args "<="))

(define-expr-op 'cl:/= (args)
  (cpp-comparison-expr args "!=" t))

(define-expr-op 'cl:zerop (args)
  (transpile-form `(= 0 ,(car args))))

(define-expr-op 'cl:setq (args)
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

(define-expr-op 'cl:read-byte (args)
  (destructuring-bind (stream &optinoal eof-error-p eof-value) args
    ;; TODO: Handle EOF-ERROR-P and EOF-VALUE
    (declare (ignore eof-error-p eof-value))
    (let ((instream (if (eqp stream *standard-input*)
                        "std::cin"
                        instream))
          (argname (cpp-argnamicate (gensym))))
      (cpp-lambdicate (format nil "char ~A;~%~A >> ~A;~%return ~A;"
                              argname
                              instream
                              argname
                              argname)))))

(define-expr-op 'cl:cons (args)
  (destructuring-bind (se1 se2) (expand-args args)
    (format nil "~A::cons(~A, ~A)"
            +namespace-prefix+
            se1
            se2)))

(define-expr-op 'cl:byte (args)
  (destructuring-bind (size position) args
    (transpile-form `(cons ,size ,position))))

(define-expr-op 'cl:car (args)
  (format nil "~A.car" (car (expand-args args))))

(define-expr-op 'cl:cdr (args)
  (format nil "~A.cdr" (car (expand-args args))))

(define-expr-op 'cl:ldb (args)
  (destructuring-bind (bytespec integer) (expand-args args)
    (cpp-lambdicate
     (concatenate 'string
                  (format nil "auto cons = ~A;~%"
                          bytespec)
                  (format nil "return (~A >> cons.cdr) % cons.car;~%"
                          integer)))))

(define-expr-op 'cl:go (args)
  (let ((label (car args)))
    (loop for (lisp-label . cpp-label-name) in *tagbody-labels*
          when (eqp label lisp-label)
            do (return (format nil "goto ~A" cpp-label-name)))))

;;; EXPRESSION OPERATOR DEPENDENCIES

(define-requirement include ostream
  "#include <ostream>")

(define-requirement include istream
  "#include <istream>")

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

(define-dependencies
  ((cl:princ cl:terpri) (ostream))
  ((cl:cons) (cons-cell-struct ostream))
  ((cl:read-byte) (istream)))

;;; CONTROL OPERATORS

(define-control-op 'cl:progn (args)
  (concatenate 'string
               (format nil "{~{~%~A~}"
                       (mapcar (lambda (arg)
                                 (ck-clle/string:indent (format nil "~A" arg) +indentation+))
                               (expand-args args)))
               (format nil "~%}")))

(define-control-op 'cl:block (args)
  (destructuring-bind (name &rest rest-args) args
    ;; TODO: This is clearly not how CL:BLOCK works, but it's what we use for now in order to have
    ;; primitive functionality.
    (if (and *routine*
             (eqp (routine-name *routine*) name))
        (format nil "~{~A~^~%~}"
                  (expand-args rest-args))
        (expand-op 'cl:progn rest-args))))

(define-control-op 'cl:if (args)
  (destructuring-bind (cond-expr then-expr &optional else-expr) args
    (cond
      ((and then-expr else-expr)
       (format nil "if (~A) ~A~%else ~A"
               (let ((*is-toplevel-expression* nil))
                 (transpile-form cond-expr))
               (expand-op 'cl:progn (list then-expr))
               (expand-op 'cl:progn (list else-expr))))
      (then-expr
       (format nil "if (~A) ~A"
               (let ((*is-toplevel-expression* nil))
                 (transpile-form cond-expr))
               (expand-op 'cl:progn (list then-expr)))))))

(define-control-op 'cl:when (args)
  (destructuring-bind (cond-expr &rest forms) args
    (format nil "if (~A) ~A"
            (let ((*is-toplevel-expression* nil))
              (transpile-form cond-expr))
            (expand-op 'cl:progn forms))))

(define-control-op 'cl:unless (args)
  (destructuring-bind (cond-expr &rest forms) args
    (expand-op 'cl:when `((not ,cond-expr) ,@forms))))

(defvar *capture-previous-bindings* nil)

(define-control-op 'cl:let (args)
  (destructuring-bind (bindings &rest body) args
    (let ((*routine-args* *routine-args*))
      (let ((cpp-bindings
              (loop for binding in bindings
                    collect (format nil "auto ~A = [~{~A~^, ~}]() ~A();"
                                    (cpp-argnamicate (car binding))
                                    (mapcar #'cpp-argnamicate *routine-args*)
                                    (let ((*should-return-result* t))
                                      (expand-op 'cl:progn `(,(cadr binding)))))
                    when *capture-previous-bindings*
                      do (setf *routine-args* (push (car binding) *routine-args*)))))
        (concatenate 'string
                     "{"
                     (ck-clle/string:indent
                      (concatenate 'string
                                   (format nil "~{~%~A~}"
                                           cpp-bindings)
                                   (let ((*routine-args*
                                           (append *routine-args* (mapcar #'car bindings))))
                                     (format nil "~{~%~A~}"
                                             (expand-args body))))
                      +indentation+)
                     (format nil "~%}"))))))

(define-control-op 'cl:let* (args)
  (let ((*capture-previous-bindings* t))
    (expand-op 'cl:let args)))

(define-control-op 'cl:tagbody (args)
  (let ((*tagbody-labels* *tagbody-labels*)
        (*should-return-result* nil)
        (result ""))
    (loop for form in args
          if (not (listp form))
            do (let ((label-name
                       (format nil "LABEL_~A"
                               (cpp-alphanumericate (symbol-name (gensym (symbol-name form)))))))
                 (push (cons form label-name) *tagbody-labels*)
                 (setf result (format nil "~A~&~A:"
                                      result
                                      label-name)))
          else do
            (setf result (format nil "~A~&~A"
                                 result
                                 (transpile-form form))))
    result))

;;; IGNORED OPERATORS

(defmacro ignore-op-symbols (&rest op-syms)
  `(progn
     ,@(mapcar (lambda (op-sym)
                 `(setf (gethash ',op-sym *ignored-ops-table*) t))
               op-syms)))

(defun ignored-op-symbol-p (op-sym)
  (gethash op-sym *ignored-ops-table*))

(ignore-op-symbols cl:declare)
