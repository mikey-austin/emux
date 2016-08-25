(require 'json)

(cl-defstruct emux--closure
  vars
  func)

(defun emux--partial-apply (f args)
  (if (emux--closure-p f)
      (make-emux--closure :func (emux--closure-func f)
                          :vars (append (emux--closure-vars f) args))
    (make-emux--closure :func f
                        :vars args)))

(defun emux--partial-funcall (f &rest args)
  (emux--partial-apply f args))

(defun emux--apply (f args)
  (if (emux--closure-p f)
      (apply (emux--closure-func f) (append (emux--closure-vars f) args))
    (apply f args)))

(defun emux--funcall (f &rest args)
  (emux--apply f args))

;; result type encoding a value and a error case.
(cl-defstruct emux--result
  variant
  value)

(defun emux--result-ok (value)
  "Return VALUE wrapped in the :ok variant of the result type."
  (make-emux--result :variant :ok :value value))

(defun emux--result-error (err)
  "Return ERR wrapped in the :error variant of the result type."
  (make-emux--result :variant :error :value err))

(defmacro emux--result-match (binding &rest body)
  (let ((val (cl-gensym))
        (var (car binding))
        (value (cadr binding)))
    `(let* ((,val ,value)
            (,var (emux--result-value ,val)))
       (case (emux--result-variant ,val)
         ,@body))))
(put 'emux--result-match 'lisp-indent-function 1)

(defmacro emux--result-do (binding &rest body)
  "With VAR bound to the value in VAL if it's the :ok variant, evaluate the forms in BODY.  Return VAL if it is the :error variant."
  (let ((val (cl-gensym))
        (var (car binding))
        (value (cadr binding)))
    `(let ((,val ,value))
       (emux--result-match (,var ,val)
         (:error ,val)
         (:ok ,@body)))))
(put 'emux--result-do 'lisp-indent-function 1)

(defun emux--result-bind (val f)
  "With the value inside VAL as argument, call F if VAL is the :ok variant.  Return VAL if it's the :error variant."
  (emux--result-do (x val)
    (emux--funcall f x)))

(defun emux--result-apply (val &rest args)
  "Apply the function inside VAL to ARGS if VAL is the :ok variant.  Return VAL if not."
  (emux--result-do (f val)
    (emux--apply f args)))

(defun emux--result-is-error (val)
  (eq (emux--result-variant val) :error))

(defun emux--result-seq (list)
  (block func-body
    (let ((unwrapped ()))
     (dolist (res list)
       (emux--result-match (val res)
         (:ok (push val unwrapped))
         (:error (return-from func-body res))))
     (emux--result-ok (reverse unwrapped)))))

(defun emux--puthash (key value table)
  "Insert the KEY VALUE pair into TABLE with the VALUE wrapped into an emux--result."
  (puthash key (emux--result-ok value) table))

(defun emux--gethash (key table not-found)
  "Retrieve the value for KEY in TABLE.  Return (emux--result-error NOT-FOUND) if there's no value for KEY."
  (let ((val (gethash key table)))
    (if val
        val
      (emux--result-error not-found))))

;; Field specs
(defvar emux--spec-table (make-hash-table :test 'eq))

(cl-defstruct emux--spec
  arity
  func)

(defun emux--spec-concrete-p (s)
  (= (emux--spec-arity s) 0))

(defun emux--spec-wrap-predicate (spec-s pred x)
  (let ((success (emux--funcall pred x)))
    (if success
        (emux--result-ok x)
      (emux--result-error (format "%S is not a valid %S" x spec-s)))))

(defmacro emux--defspec (name type-args data-var &rest body)
  `(emux--puthash ',name
                  (make-emux--spec :arity ,(length type-args)
                                   :func (lambda (,@type-args ,data-var)
                                           ,@body))
                  emux--spec-table))
(put 'emux--defspec 'lisp-indent-function 3)

(defun emux--fetch-spec (name)
  (emux--gethash name
                 emux--spec-table
                 (format "%S is not a known spec" name)))

(defun emux--normalize-spec-sexp (spec-s)
  (cond ((symbolp spec-s) (list (spec-s)))
        ((null (cdr spec-s)) (emux--normalize-spec-sexp (car spec-s)))
        (t spec-s)))

(defun emux--getspec-func (spec-s)
  (let* ((normalized-spec-s (emux--normalize-spec-sexp spec-s))
         (name (car normalized-spec-s))
         (arg-list (cdr normalized-spec-s)))
    (emux--result-do (spec (emux--fetch-spec name))
      (let ((arity (emux--spec-arity spec))
            (func (emux--spec-func spec)))
        (cond ((/= (length arg-list) arity)
               (emux--result-error (format "Spec %S has arity %i and can't be used with %S as arguments"
                                           name arity arg-list)))
              ((= arity 0) (emux--result-ok func))
              (t (emux--result-do (args (emux--result-seq (mapcar 'emux--getfield-type-unwrapped arg-list)))
                   (emux--result-ok (emux--partial-apply func args)))))))))

(defun emux--getspec-pred (spec-s)
  (emux--result-do (f (emux--getspec-func spec-s))
    (emux--result-ok (emux--partial-funcall 'emux--spec-wrap-predicate spec-s f))))

(emux--defspec string () data
  (stringp data))

(emux--defspec vector (a) data
  (and (vectorp data)
       (every a data)))

(emux--defspec option (a) data
  (or (not data)
      (funcall a data)))

(defvar emux--process nil)

(defun emux-start-client (path)
  (setf emux--process (make-network-process :name "emux"
                                            :bufer (get-buffer-create "emux")
                                            :remote path
                                            :filter #'emux--process-filter)))

(defun emux-finish-client ()
  (delete-process emux--process))

(defun emux-running? ()
  (process-live-p emux--process))

(defun emux--encode (&rest args)
  (json-encode args))

(defun emux-command (machine command id tags)
  (process-send-string emux--process (emux--encode :type "execute"
                                                   :id id
                                                   :machine machine
                                                   :tags tags)))

(unless (emux-running?)
  (emux-start-client "/home/raphael/work/emux/sock_test"))

(if (emux-running?)
    (emux-command "app" "ls" "id" '()))
