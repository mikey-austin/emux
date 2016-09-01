(require 'subr-x)

(defvar emux--repl-input)
(defconst emux--repl-buffer-name "*emux-repl*")
(defconst emux--repl-header "*** Welcome to EMUX ***  Type ? for help.\n")
(defvar emux--repl-machine "localhost")
(defconst emux--repl-prompt-format "EMUX@%s > ")
(defvar emux--repl-prompt  (format emux--repl-prompt-format emux--repl-machine))

(defvar emux--repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'emux--repl-return)
    (define-key map "\C-j" 'emux--repl-return)
    map)
  "Keymap for emux-repl mode.")

(defvaralias 'inferior-emux-mode-map 'emux--repl-map)

(defun emux--repl-input-sender (_proc input)
  (setq emux--repl-input input))

(defun emux--repl-process ()
  (get-buffer-process (current-buffer)))

(defun emux--repl-pm ()
    (process-mark (get-buffer-process (current-buffer))))

(defun emux--repl-set-pm (pos)
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun emux--repl-send-input ()
  (interactive)
  (let (emux--repl-input)
    (comint-send-input)
    (emux--repl-eval-input emux--repl-input)))

(defun emux--repl-handle-output (id data)
  (emux--repl-print data))

(defvar emux--repl-request-id 0)
(defun emux--repl-next-id ()
  (format "%s" (incf emux--repl-request-id)))

(defun emux--repl-show-help (input)
  (cond
   ((string= input "?") ":m machine ; sets current machine")
   (t "; detailed help is not here yet")))

(defun emux--repl-command (input)
  (cond
   ((string-match-p ":m .+" input)
    (emux-change-machine (second (split-string input)))
    (concat "; machine set to " emux--repl-machine))
   (t "; special commands not implemented yet")))

(defun emux-change-machine (machine)
  (interactive)
  (setq emux--repl-machine machine)
  (setq emux--repl-prompt (format emux--repl-prompt-format emux--repl-machine)))

(defun emux--repl-print (text)
  (comint-output-filter (emux--repl-process) (concat text "\n" emux--repl-prompt)))

(defun emux--repl-eval-input (input)
  (cond
   ((string-prefix-p "?" input) (emux--repl-print (emux--repl-show-help input)))
   ((string-prefix-p ":" input) (emux--repl-print (emux--repl-command input)))
   (t (emux-execute :command input :id (emux--repl-next-id) :machine emux--repl-machine))))

(defun emux--repl-return ()
  (interactive)
  (emux--repl-send-input))

(define-derived-mode inferior-emux-mode comint-mode "EMUX"
  "Major mode for interactive EMUX sessions"
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote emux--repl-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'emux--repl-input-sender)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (setq mode-line-process '(":%s on"))

  (setq-local comment-start "#")
  (setq-local comment-use-syntax t)

  (set (make-local-variable 'emux--repl-working-buffer) (current-buffer))
  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "emux-repl" (current-buffer) "hexl")
      (file-error (start-process "emux-repl" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (emux--repl-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
    (insert emux--repl-header)
    (emux--repl-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (emux--repl-process) emux--repl-prompt)
    (set-marker comint-last-input-start (emux--repl-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun emux-repl ()
  (interactive)
  (let (old-point)
    (unless (comint-check-proc emux--repl-buffer-name)
      (with-current-buffer (get-buffer-create emux--repl-buffer-name)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (inferior-emux-mode)))
    (switch-to-buffer emux--repl-buffer-name)
    (when old-point (push-mark old-point))))
