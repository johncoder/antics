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

(defvar antics-filename "example.antics"
  "The file to be loaded by antics.")

(defun antics--read-config (antics-file-path)
  "Read s-expression from ANTICS-FILE-PATH."
  (cadr (with-current-buffer (find-file-noselect antics-file-path)
          (goto-char (point-min))
          (read (current-buffer)))))

(defclass antics--configuration ()
  ((name :initarg :name
         :initform "Unknown Antics")
   (items :initarg :items
          :initform ())))

(defclass antics--item ()
  ((name :initarg :name
         :initform "Unknown Item")
   (cwd :initarg :cwd
        :initform "./")
   (cmd :initarg :cmd
        :initform '(error "Property CMD is required for antics items"))
   (proc :initform nil)))

(cl-defmethod procname ((obj antics--item))
  "Process name for ANTICS--ITEM instance OBJ."
  (with-slots (name) obj
    (format "antics: %s" name)))

(cl-defmethod start ((obj antics--item))
  "Start a process for ANTICS--ITEM instance OBJ."
  (with-slots (proc name cwd cmd) obj
    (unless proc
      (setq proc (make-process :name (procname obj)
                               :buffer (procname obj)
                               :command (list "bash")
                               :connection-type 'pipe))
      (process-send-string proc cmd)
      (message "starting process for %s" name))))

(defun antics--parse-item (lst)
  "Create an antics item from LST."
  (let (name cwd cmd current)
    (setq name (pop lst))
    (while lst
      (setq current (pop lst))
      (when (symbolp current)
        (cond ((eq current :cwd)
               (setq cwd (pop lst)))
              ((eq current :cmd)
               (setq cmd (pop lst)))
              (t (message "unknown symbol in antics item")))))
    (antics--item :name name :cwd cwd :cmd cmd)))

(defun antics--parse-config (config)
  "Parse s-expressions in CONFIG."
  (let (name items current)
    (while config
      (setq current (pop config))
      (when (symbolp current)
        (cond ((eq current :name)
               (setq name (pop config)))
              ((eq current :items)
               (while (and config (listp (car config)))
                 (push (antics--parse-item (pop config)) items)))
              (t (message "unknown symbol %s" current)))))
    (antics--configuration :name name :items items)))

(defun antics--mode-cols ()
  "Columns for antics-mode."
  (vector (list "Name" (/ 100 3))
          (list "CWD" (/ 100 3))
          (list "Command" (/ 100 3))))

(defun antics--mode-rows (config)
  "Rows for anticss-mode in CONFIG slot ITEMS."
  (mapcar
   (lambda (item)
     (list item
           (vector
            (slot-value item 'name)
            (slot-value item 'cwd)
            (slot-value item 'cmd))))
   (slot-value config 'items)))

(defun antics-select-item ()
  "View an ITEM."
  (interactive)
  (start (tabulated-list-get-id))
  (switch-to-buffer (procname (tabulated-list-get-id))))

(defun antics-refresh ()
  "Refresh antics content."
  (interactive)
  (let* ((config-file-contents (antics--read-config antics-filename))
         (config (antics--parse-config config-file-contents))
         (columns (antics--mode-cols))
         (rows (reverse (antics--mode-rows config))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print t t)))

(defvar antics-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'antics-select-item)
    (define-key keymap (kbd "g") 'antics-refresh)
    keymap)
  "Key map for antics-mode.")

(define-derived-mode antics-mode tabulated-list-mode "antics"
  "Antics mode"
  (antics-refresh))

(defun antics ()
  "Start antics, viewing a list of processes."
  (interactive)
  (switch-to-buffer "*antics*")
  (antics-mode))

;; (let ((config (antics--read-config antics-filename)))
;;   (antics--parse-config config))

;; (let ((example (antics--parse-config (antics--read-config antics-filename))))
;;   ;; (pp (antics--mode-rows example))
;;   (with-slots (items) example
;;     (pp items)))

(provide 'antics)
;;; antics.el ends here
