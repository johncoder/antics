;;; antics.el --- Process management. -*- lexical-binding: t -*-

;; Copyright (c) John Nelson

;; Author: John Nelson <jnelson@johncoder.com>
;; Homepage: https://github.com/johncoder/antics
;; Keywords: lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; File: antics.el

;;; Commentary:

;; Read a file containing a specification of processes that need to be
;; run and tracked.  Each process needs to run async, and log to a
;; buffer.  A major mode tabulates the processes with key bindings for
;; various actions, like view, start, stop, restart, more (perhaps a
;; transient?)

;;; Code:
(require 'cl-lib)
(require 'eieio)

(defun antics--read-config (antics-file-path)
  "Read s-expression from ANTICS-FILE-PATH."
  (with-current-buffer (find-file-noselect antics-file-path)
                 (goto-char (point-min))
                 (read (current-buffer))))

(defclass antics--configuration ()
  ((name :initarg :name
         :initform "Unknown Antics")
   (items :initarg :items
          :initform ())))


(let ((config (cadr (antics--read-config "example.antics"))))
  (while config
    (message "processing %s" (car config))
    (let ((item (car config)))
      (cond
       ((listp item)
        (message "found items! %s" (length item)))
       ((symbolp item)
        (message "found name! %s" item))
       (t (message "something else %s" item))))
    (pop config)))

(define-derived-mode antics-mode tabulated-list-mode "antics"
  "Antics mode"
  
  )

(defun antics ()
  "Start antics, viewing a list of processes."
  (interactive)
  (switch-to-buffer "*antics*")
  (antics-mode))

(let ((example (antics--read-config "example.antics")))
  (message "%s" example))

(provide 'antics)
;;; antics.el ends here
