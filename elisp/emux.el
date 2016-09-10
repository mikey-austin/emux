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

(require 'emux--buffers)
(require 'emux--plumbing)
(require 'subr-x)

(defvar emux-path "emux")
(defvar emux-default-socket "/tmp/emux.sock")

(defvar emux-working-machines nil)

(defvar emux--process-name "emux")

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
    :created integer
    :muted boolean
    :command string
    :machine (option string)
    :id string
    :tags (vector string)))

(emux--defspec pipeline-command () data
  (emux--obj-with-keys data
    :id string
    :command string
    :machine (option string)
    :tags (option (vector string))))

(emux--defresponse-type output ((id string) (tags (vector string)) (content string))
  (let ((decoded (base64-decode-string content)))
    (emux--broadcast 'output id decoded)
    (emux--write-output id tags decoded))
  (emux--register-running-process id))

(defun emux--broadcast (kind id data)
  ;; just a hack for now
  (when (functionp 'emux--repl-handle-output)
    (emux--repl-handle-output kind id data)))

(emux--defresponse-type finished ((id string) (exit_code integer))
  (emux--broadcast 'finished id exit_code)
  (emux--write-finished id [] exit_code)
  (emux--deregister-id id))

(emux--defresponse-type error_output ((id (option string)) (content string))
  (let ((content (base64-decode-string content)))
    (emux--broadcast 'error id content)
    (if (null id)
        (emux--add-log content)
      (emux--write-error-output id [] content)
      (emux--register-running-process id))))

(emux--defresponse-type state ((tags (vector string))
                               (processes (vector process)))
  (emux--reset-running-processes processes))

(defun emux--string-or-nil (string)
  (if (string-blank-p string)
      nil
    string))

(defun emux--read-option-vector-string (prompt &optional initial-value history)
  (vconcat (split-string (read-string prompt initial-value history))))

(emux--defmessage-type execute ((id string)
                                (command string)
                                (machine (option string))
                                (tags (option (vector string))))
  (interactive
   (let* ((input-id (read-string "Id: " nil 'emux-id-history))
          (input-command (read-string "Command: " nil 'emux-command-history))
          (input-machine (read-string "Machine: " nil 'emux-machine-history))
          (input-tags (emux--read-option-vector-string "Tags: " input-machine 'emux-tags-history)))
     (list :id input-id :command input-command :machine input-machine :tags input-tags)))
  (emux--register-running-process id tags machine command))

(emux--defmessage-type pipeline ((pipeline (vector pipeline-command))))

(emux--defmessage-type state ())

(emux--defmessage-type mute ((id (option (vector string)))
                             (tags (option (vector string))))
  (interactive
   (list
    :id (emux--read-option-vector-string "Ids: " nil 'emux-ids-history)
    :tags (emux--read-option-vector-string "Tags: " nil 'emux-tags-history)))
  (emux--running-processes-set id tags :muted t))

(emux--defmessage-type unmute ((id (option (vector string)))
                               (tags (option (vector string))))
  (interactive
   (list
    :id (emux--read-option-vector-string "Ids: " nil 'emux-ids-history)
    :tags (emux--read-option-vector-string "Tags: " nil 'emux-tags-history)))
  (emux--running-processes-set id tags :muted nil))

(emux--defmessage-type stop ((id (option (vector string)))
                             (tags (option (vector string))))
  (interactive
   (list
    :id (emux--read-option-vector-string "Ids: " nil 'emux-ids-history)
    :tags (emux--read-option-vector-string "Tags: " nil 'emux-tags-history)))
  (emux--running-processes-stop id tags))

(defun emux--id-from-command-machine (command machine)
  (replace-regexp-in-string "[^[:alpha:]]" "_"
                            (concat command "_" machine)))

(defun emux-run-command-on (command machines)
  (interactive
   (list
    (read-string "Command: " nil 'emux-command-history)
    (emux--read-option-vector-string "Machines: " nil 'emux-machines-history)))
  (if (or (not machines)
          (= 0 (length machines)))
    (emux-execute :id (emux--id-from-command-machine command nil)
                  :command command))
  (mapc (lambda (m)
          (emux-execute :id (emux--id-from-command-machine command m)
                        :tags (vector m)
                        :machine m
                        :command command))
        machines))

(defun emux-run-command (command)
  (interactive (list (read-string "Command: " nil 'emux-command-history)))
  (emux-run-command-on command emux-working-machines))

(defun emux-set-working-machines (machines)
  (interactive
   (list
    (emux--read-option-vector-string "Machines: " nil 'emux-machines-history)))
  (setq emux-working-machines machines)
  (emux--update-header-line emux-working-machines))

(defun emux--default-socket ()
  (or (getenv "EMUX_SOCKET")
      emux-default-socket))

(defun emux-start-client (&optional path)
  (interactive (list (read-string "Socket: " (emux--default-socket) nil nil t)))
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
                          #'emux--process-filter)))
  (emux--clear-running-processes)
  (emux--update-header-line emux-working-machines))

(defun emux-finish-client ()
  (interactive)
  (delete-process emux--process-name))

(defun emux ()
  (interactive)
  (emux-start-client (emux--default-socket))
  (emux-switch-to-buffer))

(defun emux-running? ()
  (interactive)
  (and (process-live-p emux--process-name)
       t))

(provide 'emux)
;;; emux.el ends here
