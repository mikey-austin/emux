;; -*- lexical-binding: t -*-

(require 'json)
(require 'subr-x)

(defvar emux--process-name "emux")
(defvar emux-path "emux")
(defconst emux-log-buffer-name "*emux-logs*")

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
  (cond ((symbolp spec-s) (list spec-s))
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
              (t (emux--result-do (args (emux--result-seq (mapcar 'emux--getspec-func arg-list)))
                   (emux--result-ok (emux--partial-apply func args)))))))))

(defun emux--getspec-pred (spec-s)
  (emux--result-do (f (emux--getspec-func spec-s))
    (emux--result-ok (emux--partial-funcall 'emux--spec-wrap-predicate spec-s f))))

(defvar emux--response-type-table (make-hash-table :test 'equal))

(defun emux--validate-apply (args-and-specs func obj)
  (emux--result-do (values (emux--result-seq (mapcar (lambda (x)
                                                       (emux--funcall (cdr x) (cdr (assoc (car x) obj))))
                                                     args-and-specs)))
    (emux--result-ok (emux--apply func values))))

(defun emux--extract-arg-name-and-spec (x)
  (let ((name (symbol-name (car x)))
        (spec-s (cadr x)))
    (emux--result-do (spec (emux--getspec-pred spec-s))
      (emux--result-ok (cons name spec)))))

(defun emux--make-args-and-specs (arg-list)
  (emux--result-seq (mapcar 'emux--extract-arg-name-and-spec arg-list)))

(defmacro emux--defresponse-type (name args &rest body)
  (let* ((args-and-specs (cl-gensym))
         (arg-names (mapcar 'car args))
         (func `(lambda ,arg-names ,@body)))
    `(emux--result-do (,args-and-specs (emux--make-args-and-specs ',args))
       (puthash (symbol-name ',name)
                (emux--partial-funcall 'emux--validate-apply ,args-and-specs ,func)
                emux--response-type-table))))
(put 'emux--defresponse-type 'lisp-indent-function 2)

(defun emux--json-decode (json-str)
  (let* ((json-array-type 'vector)
         (json-key-type 'string)
         (json-object-type 'alist))
    (condition-case err
        (let ((obj (json-read-from-string json-str)))
          (emux--result-ok obj))
      (error (emux--result-error err)))))

(defun emux--process-filter (process output)
  (let ((lines (split-string output "\n")))
    (cl-loop for l in lines
             unless (string= "" (string-trim l))
             do (emux--result-match (alist-val (emux--json-decode l))
                  (:ok (let ((handler (gethash (cdr (assoc "type" alist-val)) emux--response-type-table)))
                         (if handler
                             (emux--result-match (val (emux--funcall handler alist-val))
                               (:ok t)
                               (:error (warn val)))
                           (warn "unknown type"))))
                  (:error (warn alist-val))))))

(defun emux--add-log (line)
  (with-current-buffer (get-buffer-create emux-log-buffer-name)
    (insert line)
    (newline)))

(defun emux--send-message (args)
  (process-send-string emux--process-name
                       (concat (json-encode (remove-if-not 'cdr args)) "\n")))

(defun emux--prefix-symbol (prefix-str symb)
  (intern (concat prefix-str
                  (symbol-name symb))))

(defun emux--message-alist (type-var symbol-list)
  (cons 'list (cons `(cons "type" ,type-var)
                    (reduce (lambda (acc s)
                              (cons `(cons ,(symbol-name s),s)
                                    acc))
                            symbol-list :initial-value ()))))

(defmacro emux--defmessage-type (name args &rest body)
  (let ((arg-names (mapcar 'car args))
        (spec-sexps (mapcar 'cadr args))
        (specs (cl-gensym))
        (specs-val (cl-gensym))
        (val (cl-gensym))
        (type-as-string (symbol-name name)))
    `(let ((,specs (emux--result-seq (mapcar (lambda (as)
                                               (emux--getspec-pred as))
                                             ',spec-sexps))))
       (emux--result-match (,specs-val ,specs)
         (:ok (cl-defun ,(emux--prefix-symbol "emux-" name) ,(cons '&key arg-names)
                (emux--result-match (,val (emux--result-seq (mapcar* (lambda (p x)
                                                                       (emux--funcall p x))
                                                                     ,specs-val
                                                                     (list ,@arg-names))))
                  (:ok (emux--send-message ,(emux--message-alist type-as-string
                                                                 arg-names))
                       ,@body)
                  (:error (error ,val)))))
         (:error (error ,specs-val))))))
(put 'emux--defmessage-type 'lisp-indent-function 2)

(emux--defspec string () data
  (stringp data))

(emux--defspec integer () data
  (integerp data))

(emux--defspec vector (a) data
  (and (vectorp data)
       (every a data)))

(emux--defspec option (a) data
  (or (not data)
      (emux--funcall a data)))

(emux--defresponse-type output ((id string) (content string))
  (emux--write-to-emux-buffer id (base64-decode-string content)))

(emux--defresponse-type finished ((id string) (exit_code integer)))

(emux--defmessage-type execute ((id string)
                                (command string)
                                (machine (option string))
                                (tags (option (vector string)))))

(defun emux--write-to-emux-buffer (section content)
  (let ((proc (get-process emux--process-name)))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert (concat "\n=== " section " ===\n"))
            (insert content)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))))))

(emux--defresponse-type error_output ((id (option string)) (content string))
  (let ((content (base64-decode-string content)))
    (if (null id)
        (emux--add-log content)
      (emux--write-to-emux-buffer (concat id " (stderr)") content))))

(emux--defmessage-type state ())

(emux--defmessage-type mute ((id (option (vector string)))
                             (tags (option (vector string)))))

(defun emux-start-client (&optional path)
  (when (emux-running?)
    (emux-finish-client))
  (if path
      (make-network-process :name emux--process-name
                            :buffer "*emux*"
                            :filter #'emux--process-filter
                            :remote path)
    (when (start-process emux--process-name "*emux*" emux-path)
      (set-process-filter (get-process emux--process-name)
                          #'emux--process-filter))))

(defun emux-finish-client ()
  (delete-process emux--process-name))

(defun emux-running? ()
  (process-live-p emux--process-name))
