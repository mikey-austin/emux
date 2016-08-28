;;; emux.el --- Frontend for the emux tool. -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Maxim Velesyuk, Michael Austin and Raphael Sousa Santos

;; Author: Maxim Velesyuk <email>, Michael Austin <mikey@mikeyaustin.com> and Raphael Sousa Santos <contact@raphaelss.com>
;; URL:
;; Package-Requires:
;; Version: 0.1
;; Keywords:

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Brief description of usage

;;; Code:

(require 'json)
(require 'subr-x)

(defvar emux-path "emux")
(defconst emux-log-buffer-name "*emux-logs*")

(defvar emux--process-name "emux")
(defvar emux--spec-table (make-hash-table :test 'eq))
(defvar emux--response-type-table (make-hash-table :test 'equal))

(defun emux--partial-apply (f args)
  (lambda (&rest rest)
    (apply f (append args rest))))

(defun emux--partial-funcall (f &rest args)
  (emux--partial-apply f args))

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

(defun emux--result-seq-map (f xs)
  (block func-body
   (let ((result ()))
     (dolist (x xs (emux--result-ok (nreverse result)))
       (let ((f-x (funcall f x)))
         (emux--result-match (val f-x)
           (:ok (push val result))
           (:error (return-from func-body f-x))))))))

(defun emux--result-seq-fzip (fs xs)
  (block func-body
    (let ((result ()))
      (while (and fs xs)
        (let ((f-x (funcall (pop fs)
                            (pop xs))))
          (emux--result-match (val f-x)
            (:ok (push val result))
            (:error (return-from func-body f-x)))))
      (emux--result-ok (nreverse result)))))

(defun emux--puthash (key value table)
  "Insert the KEY VALUE pair into TABLE with the VALUE wrapped into an emux--result."
  (puthash key (emux--result-ok value) table))

(defun emux--gethash (key table not-found)
  "Retrieve the value for KEY in TABLE.  Return (emux--result-error NOT-FOUND) if there's no value for KEY."
  (let ((val (gethash key table)))
    (if val
        val
      (emux--result-error not-found))))

(cl-defstruct emux--spec
  arity
  func)

(defun emux--spec-wrap-predicate (spec-s pred x)
  (let ((success (funcall pred x)))
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
              (t (emux--result-do (args (emux--result-seq-map 'emux--getspec-func arg-list))
                   (emux--result-ok (emux--partial-apply func args)))))))))

(defun emux--getspec-pred (spec-s)
  (emux--result-do (f (emux--getspec-func spec-s))
    (emux--result-ok (emux--partial-funcall 'emux--spec-wrap-predicate spec-s f))))

(defun emux--validate-apply (args-and-specs func obj)
  (emux--result-do (values (emux--result-seq-map (lambda (x)
                                                   (funcall (cdr x)
                                                            (cdr (assoc (car x) obj))))
                                                 args-and-specs))
    (emux--result-ok (apply func values))))

(defun emux--extract-arg-name-and-spec (x)
  (let ((name (symbol-name (car x)))
        (spec-s (cadr x)))
    (emux--result-do (spec (emux--getspec-pred spec-s))
      (emux--result-ok (cons name spec)))))

(defun emux--make-args-and-specs (arg-list)
  (emux--result-seq-map 'emux--extract-arg-name-and-spec arg-list))

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

(defun last-char-is-}? (str)
  (eq ?\} (aref str (- (length str) 1))))

(let ((rest nil))
 (defun emux--process-filter (process output)
   (let ((lines (split-string output "\n")))
     (dolist (l lines)
       (let ((trimmed (string-trim l)))
         (unless (string= "" trimmed)
           (when rest
             (setf trimmed (concat rest trimmed)
                   rest nil))
           (if (last-char-is-}? trimmed)
               (emux--result-match (alist-val (emux--json-decode trimmed))
                 (:ok (let ((handler (gethash (cdr (assoc "type" alist-val)) emux--response-type-table)))
                        (if handler
                            (emux--result-match (val (funcall handler alist-val))
                              (:ok t)
                              (:error (warn val)))
                          (warn "unknown type"))))
                 (:error (warn alist-val)))
             (setf rest trimmed))))))))

(defun emux--send-message (args)
  (process-send-string emux--process-name
                       (concat (json-encode (remove-if-not 'cdr args))
                               "\n")))

(defun emux--prefix-symbol (prefix-str symb)
  (intern (concat prefix-str
                  (symbol-name symb))))

(defun emux--message-alist (type symbol-list)
  `(list '("type" . ,type)
         ,@(reduce (lambda (acc s)
                     (cons `(cons ,(symbol-name s) ,s)
                           acc))
                   symbol-list :initial-value ())))

(defmacro emux--defmessage-type (name args &rest body)
  (let ((arg-names (mapcar 'car args))
        (spec-sexps (mapcar 'cadr args))
        (specs (cl-gensym))
        (specs-val (cl-gensym))
        (val (cl-gensym))
        (type-as-string (symbol-name name)))
    `(let ((,specs (emux--result-seq-map (lambda (as)
                                           (emux--getspec-pred as))
                                         ',spec-sexps)))
       (emux--result-match (,specs-val ,specs)
         (:ok (cl-defun ,(emux--prefix-symbol "emux-" name) ,(cons '&key arg-names)
                (emux--result-match (,val (emux--result-seq-fzip ,specs-val
                                                                 (list ,@arg-names)))
                  (:ok (emux--send-message ,(emux--message-alist type-as-string
                                                                 arg-names))
                       ,@body)
                  (:error (error ,val)))))
         (:error (error ,specs-val))))))
(put 'emux--defmessage-type 'lisp-indent-function 2)

(defun emux--write-to-scrolling-buffer (buffer &rest strings)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((initial-point-max (point-max)))
        (save-excursion
          (goto-char (point-max))
          (mapc #'insert strings))
        (dolist (window (get-buffer-window-list buffer nil 0))
          (when (= (window-point window)
                   initial-point-max)
            (set-window-point window (point-max))))))))

(defun emux--write-to-emux-buffer (section content)
  (let ((buffer (process-buffer (get-process emux--process-name))))
    (emux--write-to-scrolling-buffer buffer
                                     "\n=== " section " ===\n"
                                     content)))

(defun emux--add-log (line)
  (emux--write-to-scrolling-buffer (get-buffer-create emux-log-buffer-name)
                                   line "\n"))

(emux--defspec string () data
  (stringp data))

(emux--defspec integer () data
  (integerp data))

(emux--defspec vector (a) data
  (and (vectorp data)
       (every a data)))

(emux--defspec option (a) data
  (or (not data)
      (funcall a data)))

(emux--defspec process () data
  (and (integerp (assoc-cdr "created" data))
       (stringp (assoc-cdr "command" data))
       (stringp (assoc-cdr "machine" data))
       (stringp (assoc-cdr "id" data))
       (let ((tags (assoc-cdr "tags" data)))
         (and (vectorp tags)
              (every #'stringp tags)))))

(emux--defresponse-type output ((id string) (content string))
  (emux--write-to-emux-buffer id (base64-decode-string content)))

(emux--defresponse-type finished ((id string) (exit_code integer))
  (emux--write-to-emux-buffer (format "%s (exit code: %i)" id exit_code) ""))

(emux--defresponse-type error_output ((id (option string)) (content string))
  (let ((content (base64-decode-string content)))
    (if (null id)
        (emux--add-log content)
      (emux--write-to-emux-buffer (concat id " (stderr)") content))))

(emux--defresponse-type state ((tags (vector string))
                               (processes (vector process)))
  (emux--write-to-emux-buffer "state" (format "%S\n" processes)))

(emux--defmessage-type execute ((id string)
                                (command string)
                                (machine (option string))
                                (tags (option (vector string)))))

(emux--defmessage-type state ())

(emux--defmessage-type mute ((id (option (vector string)))
                             (tags (option (vector string)))))

(emux--defmessage-type unmute ((id (option (vector string)))
                               (tags (option (vector string)))))

(emux--defmessage-type stop ((id (option (vector string)))
                             (tags (option (vector string)))))

(defun emux-start-client (&optional path)
  (interactive (list (read-string "Socket: " (getenv "EMUX_SOCKET") nil nil t)))
  (when (emux-running?)
    (emux-finish-client))
  (if (and path
           (not (string= "" (string-trim path))))
      (make-network-process :name emux--process-name
                            :buffer "*emux*"
                            :filter #'emux--process-filter
                            :remote path)
    (when (start-process emux--process-name "*emux*" emux-path)
      (set-process-filter (get-process emux--process-name)
                          #'emux--process-filter))))

(defun emux-finish-client ()
  (interactive)
  (delete-process emux--process-name))

(defun emux-running? ()
  (and (process-live-p emux--process-name)
       t))

(provide 'emux)
;;; emux.el ends here
