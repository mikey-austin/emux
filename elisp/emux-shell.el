(require 'subr-x)

(defvar emux--shell-input)
(defconst emux--shell-buffer-name "*emux-repl*")
(defconst emux--shell-header "*** Welcome to EMUX ***  Type ? for help.\n")
(defvar emux--shell-machine "localhost")
(defconst emux--shell-prompt-format "EMUX@%s > ")
(defvar emux--shell-prompt  (format emux--shell-prompt-format emux--shell-machine))

(defvar emux--shell-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'emux--shell-return)
    (define-key map "\C-j" 'emux--shell-return)
    map)
  "Keymap for emux-repl mode.")

(defvaralias 'inferior-emux-mode-map 'emux--shell-map)

(defun emux--shell-input-sender (_proc input)
  (let ((no-props-input input))
    (set-text-properties 0 (length no-props-input) nil no-props-input)
    (setq emux--shell-input no-props-input)))

(defun emux--shell-process ()
  (get-buffer-process (current-buffer)))

(defun emux--shell-pm ()
    (process-mark (get-buffer-process (current-buffer))))

(defun emux--shell-set-pm (pos)
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun emux--shell-send-input ()
  (interactive)
  (let (emux--shell-input)
    (comint-send-input)
    (emux--shell-eval-input emux--shell-input)))

(defun emux--shell-handle-output (kind id data)
  (when (string= id (number-to-string emux--shell-request-id))
    (case kind
      ('output (emux--shell-print data :show-prompt nil))
      ('error (emux--shell-print data :show-prompt nil))
      ('finished (emux--shell-print "" :error-code data)))))

(defvar emux--shell-request-id 0)
(defun emux--shell-next-id ()
  (format "%s" (incf emux--shell-request-id)))

(defun emux--shell-show-help (input)
  (cond
   ((string= input "?") ":m machine ; sets current machine\n")
   (t "; detailed help is not here yet\n")))

(defun emux--shell-command (input)
  (cond
   ((string-match-p ":m .+" input)
    (emux-change-machine (second (split-string input)))
    (concat "; machine set to " emux--shell-machine "\n"))
   (t "; special commands not implemented yet\n")))

(defun emux-change-machine (machine)
  (interactive)
  (setq emux--shell-machine machine)
  (setq emux--shell-prompt (format emux--shell-prompt-format emux--shell-machine)))

(cl-defun emux--shell-print (text &key (show-prompt t) (error-code 0))
  (let ((prompt (if (zerop error-code) emux--shell-prompt
                  (format "(%s)%s" error-code emux--shell-prompt))))
    (comint-output-filter (emux--shell-process)
                          (if show-prompt (concat text prompt)
                            text))))

(defun emux--shell-eval-input (input)
  (cond
   ((string-prefix-p "?" input) (emux--shell-print (emux--shell-show-help input)))
   ((string-prefix-p ":" input) (emux--shell-print (emux--shell-command input)))
   (t (emux-execute :command input :id (emux--shell-next-id) :machine emux--shell-machine))))

(defun emux--shell-return ()
  (interactive)
  (emux--shell-send-input))

(define-derived-mode inferior-emux-mode comint-mode "EMUX"
  "Major mode for interactive EMUX sessions"
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote emux--shell-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'emux--shell-input-sender)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (setq mode-line-process '(":%s on"))

  (setq-local comment-start "#")
  (setq-local comment-use-syntax t)

  (set (make-local-variable 'emux--shell-working-buffer) (current-buffer))
  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "emux-repl" (current-buffer) "hexl")
      (file-error (start-process "emux-repl" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (emux--shell-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
    (insert emux--shell-header)
    (emux--shell-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (emux--shell-process) emux--shell-prompt)
    (set-marker comint-last-input-start (emux--shell-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun emux-shell ()
  (interactive)
  (let (old-point)
    (with-current-buffer (get-buffer-create emux--shell-buffer-name)
      (unless (emux--shell-process)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (inferior-emux-mode)))
    (switch-to-buffer emux--shell-buffer-name)
    (when old-point (push-mark old-point))))
