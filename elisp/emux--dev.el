;; -*- lexical-binding: t; -*-

;; This file should not be included in the final package

(require 'subr-x)

(let* ((files-to-load '("emux--plumbing.el" "emux--buffers.el" "emux.el"))
       (file-path (or load-file-name (buffer-file-name)))
       (split-path (split-string file-path "/"))
       (base-path-reversed (cdr (reverse split-path))))
  (add-to-list 'load-path (string-join (reverse base-path-reversed) "/"))
  (defun emux--dev-reload-everything ()
    (interactive)
    (dolist (file files-to-load)
      (load-file (string-join (reverse (cons file base-path-reversed)) "/")))))
