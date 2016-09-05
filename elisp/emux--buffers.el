;;; emux--buffers.el --- Internal functions needed by emux.el. -*- lexical-binding: t; -*-

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

(require 'tabulated-list)
(require 'subr-x)

(defvar emux--running-processes (make-hash-table :test 'equal))
(defvar emux--inhibit-state-updates nil)
(defvar emux-buffer-section-face font-lock-comment-face)
(defvar emux-buffer-content-face nil)

(defun emux--set-header-line (line)
  (with-current-buffer (emux-buffer)
    (setq header-line-format line))
  (with-current-buffer (emux-log-buffer)
    (setq header-line-format line)))

(defun emux--update-header-line (working-machines)
  (emux--set-header-line
   (if working-machines
       (format "Working machine%s: %s"
               (if (> (length working-machines) 1)
                   "s"
                 "")
               (string-join working-machines " "))
     "Working machine: localhost")))

(defvar emux-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" nil) ; nothing to revert
    (define-key map "l" 'emux-switch-to-log-buffer)
    (define-key map "b" 'emux-switch-to-buffer)
    (define-key map "s" 'emux-switch-to-state-buffer)
    map))

(define-derived-mode emux-buffer-mode special-mode "Emux"
  "Major mode used in the \"*emux*\" and \"*emux-log*\" buffers.")

(defvar emux-state-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" 'emux-state-buffer-stop)
    (define-key map "m" 'emux-state-buffer-mute)
    (define-key map "u" 'emux-state-buffer-unmute)
    (define-key map "x" 'emux-state-buffer-execute)
    (define-key map "c" 'emux-state-buffer-clear-op)
    (define-key map "l" 'emux-switch-to-log-buffer)
    (define-key map "b" 'emux-switch-to-buffer)
    (define-key map "s" 'emux-switch-to-state-buffer)
    map))

(define-derived-mode emux-state-mode tabulated-list-mode "Emux state"
  "Major mode used in the \"*emux-state*\" buffer."
  (add-hook 'tabulated-list-revert-hook 'emux-state))

(defmacro emux--defbuffer-funcs (suffix mode &rest before-switch)
  (let* ((suffix (if suffix (concat "-" suffix) ""))
         (buffer-creation-func-name (intern (format "emux%s-buffer" suffix))))
    `(let ((name ,(format "*emux%s*" suffix)))
       (defun ,buffer-creation-func-name ()
         (or (get-buffer name)
             (with-current-buffer (get-buffer-create name)
               (,mode)
               (current-buffer))))
       (defun ,(intern (format "emux-switch-to%s-buffer" suffix)) ()
         (interactive)
         ,@before-switch
         (switch-to-buffer (,buffer-creation-func-name))))))

(emux--defbuffer-funcs nil emux-buffer-mode)
(emux--defbuffer-funcs "log" emux-buffer-mode)
(emux--defbuffer-funcs "state" emux-state-mode (emux-state))

(let (scheduled-message)
  (defun emux--schedule-state-message ()
    (unless emux--inhibit-state-updates
      (when scheduled-message
        (cancel-timer scheduled-message))
      (setq scheduled-message (run-at-time 0.2 nil 'emux-state)))))

(defun emux--write-to-scrolling-buffer (buffer properties &rest strings)
  (with-current-buffer buffer
    (let ((initial-point (point))
          (initial-point-max (point-max)))
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (mapc #'insert strings)
          (when properties
            (set-text-properties initial-point-max (point-max) properties))
          (ansi-color-apply-on-region initial-point-max (point-max))))
      (when (= initial-point initial-point-max)
        (goto-char (point-max)))
      (dolist (window (get-buffer-window-list buffer nil t))
        (when (= (window-point window)
                 initial-point-max)
          (set-window-point window (point-max)))))))

(defun emux-erase-log-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
   (with-current-buffer (emux-log-buffer)
     (erase-buffer))))

(let (last-section content-properties section-properties)
  (defun emux-erase-buffer ()
    (interactive)
    (setq last-section nil)
    (let ((inhibit-read-only t))
      (with-current-buffer (emux-buffer)
        (erase-buffer))))

  (defun emux--write-to-emux-buffer (section content &optional force-section)
    (when (or force-section
              (not (string= section last-section)))
      (let ((shared-properties (list :section section)))
        (setq last-section section
              section-properties (cons 'face (cons emux-buffer-section-face
                                                   shared-properties))
              content-properties (cons 'face (cons emux-buffer-content-face
                                                   shared-properties))))
      (emux--write-to-scrolling-buffer (emux-buffer)
                                       section-properties
                                       "\n=== " section " ===\n"))
    (emux--write-to-scrolling-buffer (emux-buffer)
                                     content-properties
                                     content)))

(let (previous repeated-count)
  (defun emux--add-log (content)
    (if (and previous (string= content previous))
        (with-current-buffer (emux-log-buffer)
          (let* ((delete-from (- (point-max)
                                 (length previous)
                                 (if (= repeated-count 1)
                                     1
                                   (+ (floor (log10 repeated-count))
                                      11))))
                 (delete-to (point-max)))
            (incf repeated-count)
            (emux--write-to-scrolling-buffer (current-buffer) nil
                                             content " [" (int-to-string repeated-count) " times]\n")
            (let ((inhibit-read-only t))
              (delete-region delete-from delete-to))))
      (setq previous content
            repeated-count 1)
      (emux--write-to-scrolling-buffer (emux-log-buffer) nil
                                       content "\n"))))

;;; State buffer handling functions

(defun emux--clear-running-processes ()
  (clrhash emux--running-processes)
  (emux--write-state-schedule-message))

(let ((register-f (lambda (p)
                    (puthash (plist-get p :id)
                             p
                             emux--running-processes))))
  (defun emux--reset-running-processes (process-vector)
    (let ((emux--inhibit-state-updates t))
      (emux--clear-running-processes))
    (mapc register-f process-vector)
    (emux--write-state-buffer)))

(defun emux--register-running-process (id &optional tags machine command)
  (unless (gethash id emux--running-processes)
    (puthash id
             (list :id id :tags tags
                   :machine machine :command command
                   :created (time-to-seconds (current-time)))
             emux--running-processes)
    (emux--write-state-schedule-message)))

(defun emux--deregister-id (id)
  (remhash id emux--running-processes)
  (emux--write-state-buffer)
  (emux--schedule-state-message))

(defun emux--string-vector-member (elt vector)
  (cl-block func
    (dotimes (i (length vector))
      (when (string= (aref vector i) elt)
        (cl-return-from func t)))))

(defmacro emux--do-running-processes (id proc ids tags &rest body)
  `(progn
     (maphash (lambda (,id ,proc)
                (when (or (emux--string-vector-member ,id ,ids)
                          (some (lambda (t)
                                  (emux--string-vector-member t ,tags))
                                (plist-get ,proc :tags)))
                  ,@body))
              emux--running-processes)
     (emux--write-state-schedule-message)))
(put 'emux--do-running-processes 'lisp-indent-function 4)

(defun emux--running-processes-set (ids tags prop value)
  (emux--do-running-processes id proc ids tags
    (puthash id (plist-put proc prop value) emux--running-processes)))

(defun emux--running-processes-stop (ids tags)
  (emux--do-running-processes id proc ids tags
    (remhash id emux--running-processes)))

(defun emux--format-running-process (id proc)
  (let ((created (plist-get proc :created)))
    (list id
          (vector
           ""
           (if (plist-get proc :muted)
               "*"
             "")
           id
           (or (plist-get proc :machine) "localhost")
           (if created
               (format-time-string "%Y-%m-%d-%H-%M-%S"
                                   (seconds-to-time created))
             "")
           (or (plist-get proc :command) "")
           (string-join (plist-get proc :tags) " ")))))

(defun emux--formated-running-processes ()
  (let (result)
    (maphash (lambda (id p)
               (push (emux--format-running-process id p) result))
            emux--running-processes)
    result))

(defun emux--write-state-buffer ()
  (unless emux--inhibit-state-updates
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
            tabulated-list-entries (emux--formated-running-processes))
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(defun emux--write-state-schedule-message ()
  (emux--write-state-buffer)
  (emux--schedule-state-message))

(defun emux-state-buffer-stop ()
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "D" t)
    (forward-line 1)))

(defun emux-state-buffer-mute ()
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "M" t)
    (forward-line 1)))

(defun emux-state-buffer-unmute ()
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "U" t)
    (forward-line 1)))

(defun emux-state-buffer-clear-op ()
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "" t)
    (forward-line 1)))

(defmacro emux--do-state-buffer (var &rest body)
  `(with-current-buffer (emux-state-buffer)
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         (let ((,var (tabulated-list-get-entry)))
           (when ,var
             ,@body))))))
(put 'emux--do-state-buffer 'lisp-indent-function 1)

(defun emux-state-buffer-execute ()
  (interactive)
  (let (to-stop to-mute to-unmute)
    (emux--do-state-buffer entry
      (let ((op (aref entry 0))
            (id (aref entry 2)))
        (cond ((string= op "D")
               (push id to-stop))
              ((string= op "M")
               (push id to-mute))
              ((string= op "U")
               (push id to-unmute))))
      (tabulated-list-set-col 0 " ")
      (forward-line 1))
    (let ((emux--inhibit-state-updates t))
      (when to-stop
        (emux-stop :id (apply 'vector to-stop))
        (setq redraw t))
      (when to-mute
        (emux-mute :id (apply 'vector to-mute))
        (setq redraw t))
      (when to-unmute
        (emux-unmute :id (apply 'vector to-unmute))
        (setq redraw t)))
    (when (or to-stop to-mute to-unmute)
      (emux--write-state-schedule-message))))

(provide 'emux--buffers)
;;; emux--buffers.el ends here
