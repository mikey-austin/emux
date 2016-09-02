;;; emux.el --- Frontend for the emux tool. -*- lexical-binding: t; -*-

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

;; Brief description of usage

;;; Code:

(require 'json)
(require 'subr-x)
(require 'tabulated-list)

(defvar emux-path "emux")
(defconst emux--default-socket "/tmp/emux.sock")

(defvar emux--process-name "emux")
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

(defvar emux-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" nil) ; nothing to revert
    (define-key map "s" 'emux-state)
    map))

(define-derived-mode emux-buffer-mode special-mode "Emux"
  "Major mode used in the \"*emux*\" and \"*emux-log*\" buffers.")

(defvar emux-state-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "r" 'emux-state)
    map))

(define-derived-mode emux-state-mode tabulated-list-mode "Emux state"
  "Major mode for \"*emux-state*\" buffer")

(let ((name "*emux-state*"))
  (defun emux-state-buffer ()
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (emux-state-mode)
          (current-buffer)))))

(let ((name "*emux*"))
  (defun emux-buffer ()
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (emux-buffer-mode)
          (current-buffer)))))

(let ((name "*emux-log*"))
  (defun emux-log-buffer ()
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (emux-buffer-mode)
          (current-buffer)))))

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
  (let ((name (symbol-name (car x)))
        (spec-s (cadr x)))
    (cons name (emux--getspec-func spec-s))))

(defun emux--validate-apply (args-and-specs func)
  (lambda (obj)
    (emux--result-do (values (emux--result-seq-map (lambda (x)
                                                     (funcall (cdr x)
                                                              (cdr (assoc (car x) obj))))
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
         (json-key-type 'string)
         (json-false nil)
         (json-object-type 'alist))
    (condition-case err
        (let ((obj (json-read-from-string json-str)))
          (emux--result-ok obj))
      (error (emux--result-error err)))))

(defun emux--last-char-is-}? (str)
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
           (if (emux--last-char-is-}? trimmed)
               (emux--result-match (alist-val (emux--json-decode trimmed))
                 (:ok (let ((handler (gethash (cdr (assoc "type" alist-val))
                                              emux--response-type-table)))
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
           (:ok (emux--send-message ,(emux--message-alist type-as-string
                                                          arg-names))
                ,@body)
           (:error (error ,val)))))))
(put 'emux--defmessage-type 'lisp-indent-function 2)

(defun emux--write-to-scrolling-buffer (buffer &rest strings)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((initial-point-max (point-max)))
        (save-excursion
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (mapc #'insert strings)
            (ansi-color-apply-on-region initial-point-max (point-max))))
        (dolist (window (get-buffer-window-list buffer nil 0))
          (when (= (window-point window)
                   initial-point-max)
            (set-window-point window (point-max))))))))

(let ((last-section nil))
  (defun emux--write-to-emux-buffer (section content &optional force-section)
    (if (and (not force-section)
             (string= section last-section))
        (emux--write-to-scrolling-buffer (emux-buffer) content)
      (emux--write-to-scrolling-buffer (emux-buffer)
                                       "\n=== " section " ===\n"
                                       content)
      (setf last-section section))))

(let (last-line last-line-count)
  (defun emux--add-log (line)
    (if (and last-line (string= line last-line))
        (with-current-buffer (emux-log-buffer)
          (let ((move-back-to (and (/= (point) (point-max))
                                   (point))))
            (save-excursion
              (goto-char (- (point-max) 1))
              (let ((inhibit-read-only t))
                (delete-region (line-beginning-position) (point-max))))
            (incf last-line-count)
            (emux--write-to-scrolling-buffer (current-buffer)
                                             line " [" (int-to-string last-line-count) " times]\n")
            (when move-back-to
              (goto-char move-back-to))))
      (setq last-line line
            last-line-count 1)
      (emux--write-to-scrolling-buffer (emux-log-buffer)
                                       line "\n"))))

(defmacro emux--obj-with-keys (obj &rest key-type-pairs)
  `(and (listp ,obj)
        ,@(mapcar (lambda (pair)
                    (let ((key (symbol-name (car pair)))
                          (type (cadr pair)))
                      `(funcall ,(emux--getspec-sexp type)
                                (cdr (assoc ,key ,obj)))))
                  key-type-pairs)))
(put 'emux--obj-with-keys 'lisp-indent-function 1)

(let ((format-entry
       (lambda (p)
         (list p
               (vector
                " "
                (if (cdr (assoc "muted" p))
                    "*"
                  " ")
                (cdr (assoc "id" p))
                (cdr (assoc "machine" p))
                (format-time-string "%Y-%m-%d-%H-%M-%S"
                                    (seconds-to-time (cdr (assoc "created" p))))
                (cdr (assoc "command" p))
                (string-join (cdr (assoc "tags" p)) ","))))))
  (defun emux--refresh-state-buffer (processes)
    (with-current-buffer (emux-state-buffer)
      (setq tabulated-list-format (vector
                                   '(" " 1 t :pad-right 0)
                                   '("M" 1 t)
                                   '("Id" 16 t)
                                   '("Machine" 12 t)
                                   '("Created" 20 t)
                                   '("Command" 24 t)
                                   '("Tags" 30 nil))
            tabulated-list-use-header-line t
            tabulated-list-entries (map 'list format-entry processes))
      (tabulated-list-init-header)
      (tabulated-list-print))))

(emux--defspec boolean () data
  (booleanp data))

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
  (emux--obj-with-keys data
    (created integer)
    (muted boolean)
    (command string)
    (machine (option string))
    (id string)
    (muted boolean)
    (tags (vector string))))

(emux--defspec pipeline-command () data
  (emux--obj-with-keys data
    (id string)
    (command string)
    (machine (option string))
    (tags (option (vector string)))))

(emux--defresponse-type output ((id string) (content string))
  (let ((decoded (base64-decode-string content)))
    (emux--broadcast 'output id decoded)
    (emux--write-to-emux-buffer id decoded)))

(defun emux--broadcast (kind id data)
  ;; just a hack for now
  (when (functionp 'emux--repl-handle-output)
    (emux--repl-handle-output kind id data)))

(emux--defresponse-type finished ((id string) (exit_code integer))
  (emux--broadcast 'finished id exit_code)
  (emux--write-to-emux-buffer (format "%s (exit code: %i)" id exit_code) "" t))

(emux--defresponse-type error_output ((id (option string)) (content string))
  (let ((content (base64-decode-string content)))
    (emux--broadcast 'error id content)
    (if (null id)
        (emux--add-log content)
      (emux--write-to-emux-buffer (concat id " (stderr)") content))))

(emux--defresponse-type state ((tags (vector string))
                               (processes (vector process)))
  (emux--refresh-state-buffer processes))

(emux--defmessage-type execute ((id string)
                                (command string)
                                (machine (option string))
                                (tags (option (vector string)))))

(emux--defmessage-type pipeline ((pipeline (vector pipeline-command))))

(emux--defmessage-type state ()
  (interactive)
  (switch-to-buffer (emux-state-buffer)))

(emux--defmessage-type mute ((id (option (vector string)))
                             (tags (option (vector string)))))

(emux--defmessage-type unmute ((id (option (vector string)))
                               (tags (option (vector string)))))

(emux--defmessage-type stop ((id (option (vector string)))
                             (tags (option (vector string)))))

(defun emux-start-client (&optional path)
  (interactive (list (read-string "Socket: " (or (getenv "EMUX_SOCKET") emux--default-socket) nil nil t)))
  (when (emux-running?)
    (emux-finish-client))
  (if (and path
           (not (string= "" (string-trim path))))
      (make-network-process :name emux--process-name
                            :buffer (emux-buffer)
                            :filter #'emux--process-filter
                            :remote path)
    (when (start-process emux--process-name (emux-buffer) emux-path)
      (set-process-filter (get-process emux--process-name)
                          #'emux--process-filter))))

(defun emux-finish-client ()
  (interactive)
  (delete-process emux--process-name))

(defun emux-running? ()
  (interactive)
  (and (process-live-p emux--process-name)
       t))

(defun emux-erase-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
   (with-current-buffer (emux-buffer)
     (erase-buffer))))

(defun emux-erase-log-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
   (with-current-buffer (emux-log-buffer)
     (erase-buffer))))

(provide 'emux)
;;; emux.el ends here
