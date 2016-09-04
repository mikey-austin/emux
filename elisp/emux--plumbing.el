;;; emux--plumbing.el --- Internal functions needed by emux.el. -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Maxim Velesyuk, Michael Austin and Raphael Sousa Santos

;; Author: Maxim Velesyuk <max.velesyuk@gmail.com>, Michael Austin <mikey@mikeyaustin.com> and Raphael Sousa Santos <contact@raphaelss.com>
;; URL:
;; Package-Requires:
;; Version: 0.1
;; Keywords:

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Definitions from this file shouldn't be used outside emux.el

;;; Code:

(require 'json)
(require 'subr-x)

(defvar emux--response-type-table (make-hash-table :test 'equal))

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


(defun emux--spec-wrap-predicate (spec-s pred)
  (lambda (x)
    (let ((success (funcall pred x)))
      (if success
          (emux--result-ok x)
        (emux--result-error (format "%S is not a valid %S" x spec-s))))))

(defun emux--prefix-symbol (prefix-str symb)
  (intern (concat prefix-str
                  (symbol-name symb))))

(defun emux--make-spec-func-name (name)
  (emux--prefix-symbol "emux--spec-" name))

(defmacro emux--defspec (name type-args data-var &rest body)
  `(defun ,(emux--make-spec-func-name name) (,@type-args)
     (lambda (,data-var)
       ,@body)))
(put 'emux--defspec 'lisp-indent-function 3)

(defun emux--getspec-sexp (spec-s)
  (if (symbolp spec-s)
      (list (emux--make-spec-func-name spec-s))
    (cons (emux--make-spec-func-name (car spec-s))
          (mapcar 'emux--getspec-sexp (cdr spec-s)))))

(defun emux--getspec-pred (spec-s)
  (if (symbolp spec-s)
      (funcall (emux--make-spec-func-name spec-s))
    (apply (emux--make-spec-func-name (car spec-s))
           (mapcar 'emux--getspec-pred (cdr spec-s)))))

(defun emux--getspec-func (spec-s)
  (emux--spec-wrap-predicate spec-s (emux--getspec-pred spec-s)))

(defun emux--extract-arg-name-and-spec (x)
  (let ((key (emux--prefix-symbol ":" (car x)))
        (spec-s (cadr x)))
    (cons key (emux--getspec-func spec-s))))

(defun emux--validate-apply (args-and-specs func)
  (lambda (obj)
    (emux--result-do (values (emux--result-seq-map (lambda (x)
                                                     (funcall (cdr x)
                                                              (plist-get obj (car x))))
                                                   args-and-specs))
      (emux--result-ok (apply func values)))))

(defmacro emux--defresponse-type (name args &rest body)
  (let* ((args-and-specs (cl-gensym))
         (arg-names (mapcar 'car args))
         (func `(lambda ,arg-names ,@body)))
    `(let ((,args-and-specs (mapcar 'emux--extract-arg-name-and-spec
                                    ',args)))
       (puthash (symbol-name ',name)
                (emux--validate-apply ,args-and-specs ,func)
                emux--response-type-table))))
(put 'emux--defresponse-type 'lisp-indent-function 2)

(defun emux--json-decode (json-str)
  (let* ((json-array-type 'vector)
         (json-key-type 'keyword)
         (json-false nil)
         (json-object-type 'plist))
    (condition-case err
        (let ((obj (json-read-from-string json-str)))
          (emux--result-ok obj))
      (error (emux--result-error err)))))

(defun emux--last-char-is-}-p (str)
  (eq ?\} (aref str (- (length str) 1))))

(let (rest)
 (defun emux--process-filter (process output)
   (let ((lines (split-string output "\n" t " ")))
     (dolist (l lines)
       (when rest
         (setq l (concat rest l)
               rest nil))
       (if (emux--last-char-is-}-p l)
           (emux--result-match (plist-val (emux--json-decode l))
             (:ok (let* ((response-type (plist-get plist-val :type))
                         (handler (gethash response-type
                                           emux--response-type-table)))
                    (if handler
                        (emux--result-match (val (funcall handler plist-val))
                          (:ok t)
                          (:error (warn val)))
                      (warn (format "Received unknown type: %a"
                                    response-type)))))
             (:error (warn plist-val)))
         (setq rest l))))))

(defun emux--plist-strip-nils (plist)
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (when value
          (push value result)
          (push key result))))
    result))

(defun emux--send-message (args)
  (process-send-string emux--process-name
                       (concat (json-encode (emux--plist-strip-nils args))
                               "\n")))

(defun emux--message-plist (type symbol-list)
  `(list :type ,type
         ,@(reduce (lambda (acc s)
                     (cons `,(emux--prefix-symbol ":" s)
                           (cons s acc)))
                   symbol-list :initial-value ())))

(defmacro emux--defmessage-type (name args &rest body)
  (let ((arg-names (mapcar 'car args))
        (spec-sexps (mapcar 'cadr args))
        (specs (cl-gensym))
        (specs-val (cl-gensym))
        (val (cl-gensym))
        (type-as-string (symbol-name name))
        (interactive-form (when (eq (caar body) 'interactive)
                            (list (pop body)))))
    `(let ((,specs (mapcar (lambda (as)
                             (emux--getspec-func as))
                           ',spec-sexps)))
       (cl-defun ,(emux--prefix-symbol "emux-" name) ,(cons '&key arg-names)
         ,@interactive-form
         (emux--result-match (,val (emux--result-seq-fzip ,specs
                                                          (list ,@arg-names)))
           (:ok (emux--send-message ,(emux--message-plist type-as-string
                                                          arg-names))
                ,@body)
           (:error (error ,val)))))))
(put 'emux--defmessage-type 'lisp-indent-function 2)

(defmacro emux--obj-with-keys (obj &rest key-type-pairs)
  (let (spec-list)
    (while key-type-pairs
      (let ((key (pop key-type-pairs))
            (spec-sexp (emux--getspec-sexp (pop key-type-pairs))))
        (push `(funcall ,spec-sexp (plist-get ,obj ,key)) spec-list)))
   `(and (listp ,obj)
         ,@spec-list)))
(put 'emux--obj-with-keys 'lisp-indent-function 1)

(provide 'emux--plumbing)
;;; emux--plumbing.el ends here
