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

(defvar antics--current-config nil
  "The current configuration loaded for antics.")

(defconst antics-default-filename "example.antics"
  "The default filename to be loaded by antics.")

(defvar antics-filename antics-default-filename
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
   ;; TODO: ensure that this does not leak processes!
   (proc :initform nil)
   (proc-status :initform "<none>")))

(cl-defmethod procname ((obj antics--item))
  "Process name for ANTICS--ITEM instance OBJ."
  (with-slots (name) obj
    (format "antics: %s" name)))

(cl-defmethod update-proc-status ((obj antics--item) status)
  "Update STATUS process for ANTICS--ITEM instance OBJ."
  (with-slots (proc-status) obj
    (setq proc-status status)))

(defun ordinary-insertion-filter (proc string)
  "Orindary filter for inserting STRING to PROC buffer."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(cl-defmethod start ((obj antics--item))
  "Start a process for ANTICS--ITEM instance OBJ."
  (with-slots (proc name cwd cmd) obj
    (unless proc
      (setq proc (make-process :name (procname obj)
                               :buffer (procname obj)
                               :command (list "bash")
                               :connection-type 'pipe
                               :filter 'ordinary-insertion-filter
                               :sentinel 'update-proc-status))
      (process-send-string proc cmd)
      (process-send-eof proc)
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
  (vector (list "Name" (/ 100 4))
          (list "CWD" (/ 100 4))
          (list "Command" (/ 100 4))
          (list "Process" (/ 100 4))))

(defun antics--mode-rows (config)
  "Rows for anticss-mode in CONFIG slot ITEMS."
  (mapcar
   (lambda (item)
     (list item
           (vector
            (slot-value item 'name)
            (slot-value item 'cwd)
            (slot-value item 'cmd)
            (let ((proc (slot-value item 'proc)))
              (cond (())
                    (t "None")))
            (if (slot-value item 'proc)
                "Y"
              "N"))))
   (slot-value config 'items)))

(defun antics-select-item ()
  "View an ITEM."
  (interactive)
  (let ((item (tabulated-list-get-id)))
    (unless (slot-value item 'proc)
      (start (tabulated-list-get-id)))
    (switch-to-buffer (procname item))))

(defun antics--load (filepath &optional force)
  "Load an antics configuration file at FILEPATH, FORCE."
  (when (or (not antics--current-config)
              force)
    (setq antics--current-config
          (antics--parse-config (antics--read-config filepath)))))

(defun antics-load-config ()
  "Load configuration; use universal args to force load."
  (interactive)
  (let ((force current-prefix-arg))
    (antics--load antics-filename force)))

(defun antics-refresh ()
  "Refresh antics content."
  (interactive)
  (antics--load antics-filename)
  (let ((columns (antics--mode-cols))
        (rows (reverse (antics--mode-rows antics--current-config))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print t t)))

(defun antics-kill ()
  "Kill antics process."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (proc (slot-value item 'proc)))
    (when proc
      (kill-process proc)))
  (antics-refresh))

(defvar antics-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'antics-select-item)
    (define-key keymap (kbd "g") 'antics-refresh)
    (define-key keymap (kbd "R") 'antics-load-config)
    (define-key keymap (kbd "k") 'antics-kill)
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

(provide 'antics)
;;; antics.el ends here
