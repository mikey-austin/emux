(defvar emux--repl-input)
(defconst emux--repl-buffer-name "*emux-repl*")
(defconst emux--repl-header "*** Welcome to EMUX ***  Type ? for help.\n")
(defconst emux--repl-prompt "EMUX> ")

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
  (comint-output-filter (emux--repl-process) (concat data "\n" emux--repl-prompt)))

(defvar emux--repl-request-id 0)
(defun emux--repl-next-id ()
  (format "%s" (incf emux--repl-request-id)))

(defun emux--repl-show-help (input)
  (concat "help not implemented yet" "\n" emux--repl-prompt))

(defun emux--repl-command (input)
  (concat "special commands not implemented yet" "\n" emux--repl-prompt))

(defun emux--repl-eval-input (input)
  (comint-output-filter
   (emux--repl-process)
   (cond
    ((string-prefix-p "?" input) emux--repl-show-help)
    ((string-prefix-p ":" input) emux--repl-command)
    (t (emux-execute :command input :id (emux--repl-next-id) :machine "off")))))

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
