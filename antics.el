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

(defconst antics-default-filename "example.antics"
  "The default filename to be loaded by antics.")

(defvar antics--current-config nil
  "The current configuration loaded for antics.")

(defvar antics-filename antics-default-filename
  "The file to be loaded by antics.")

(defvar-local antics-item nil
  "The antics item in a view buffer.")

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
   (proc-status :initform "started")))

(cl-defmethod procname ((obj antics--item))
  "Process name for ANTICS--ITEM instance OBJ."
  (with-slots (name) obj
    (format "antics: %s" name)))

(cl-defmethod update-proc-status ((obj antics--item) status)
  "Update STATUS process for ANTICS--ITEM instance OBJ."
  (with-slots (proc-status) obj
    (setq proc-status (string-trim status))))

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

(defun antics--current-time ()
  "Get the current time."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun antics--item-sentinel (item)
  "Create a sentinel for ITEM."
  (lambda (process status)
    (update-proc-status item status)
    (when (cond ((string-prefix-p "finished\n" status) t)
                ((string-prefix-p "finished\n" status) t)
                ((string-prefix-p "deleted\n" status) t)
                ((string-prefix-p "exited abnormally with code " status) t)
                ((string-prefix-p "failed with code fail-code\n" status) t)
                ((string-prefix-p "signal-description " status) t)
                ((string-prefix-p "open from host-name\n" status) t)
                ((string-prefix-p "open\n" status) t)
                ((string-prefix-p "run\n" status) t)
                ((string-prefix-p "connection broken by remote peer\n" status) t)
                (t nil))
      (with-current-buffer (process-buffer process)
        (when (buffer-name (process-buffer process))
          (insert (format "\nProcess %s at %s\n" (string-trim status) (antics--current-time))))))))

(cl-defmethod start ((obj antics--item))
  "Start a process for ANTICS--ITEM instance OBJ."
  (with-slots (proc name cwd cmd proc-status) obj
    (unless proc
      (with-current-buffer (get-buffer-create (procname obj))
        (erase-buffer)
        (insert (format "-*- mode: antics-view-mode; cwd: %s\nProcess started at: %s\n\n%s\n"
                        cwd
                        (antics--current-time)
                        cmd)))
      (setq proc-status "started")
      (setq proc (make-process :name (procname obj)
                               :buffer (procname obj)
                               :command (list shell-file-name)
                               :connection-type 'pipe
                               :filter 'ordinary-insertion-filter
                               :sentinel (antics--item-sentinel obj)))
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
  (vector (list "Name" (/ 85 3))
          (list "pid" 5)
          (list "Process" 10)
          (list "Command" (/ 85 3))
          (list "CWD" (/ 85 3))))

(defun antics--mode-rows (config)
  "Rows for anticss-mode in CONFIG slot ITEMS."
  (mapcar
   (lambda (item)
     (with-slots (name proc cmd cwd proc-status) item
       (let ((pid (if proc (format "%s" (process-id proc)) "-"))
             (status (if (and proc proc-status) proc-status "-")))
         ;; this is the actual row data
         (list item (vector name pid status cmd cwd)))))
   (slot-value config 'items)))

(defun antics--view (item)
  "Switch to view for antics ITEM."
  (when (slot-value item 'proc)
    (switch-to-buffer (procname item))
    (antics-view-mode)
    (setq-local antics-item item)))

(defun antics-select-item ()
  "View an ITEM."
  (interactive)
  (let ((item (tabulated-list-get-id)))
    (unless (slot-value item 'proc)
      (start (tabulated-list-get-id)))
    (antics--view item)))

(defun antics-view ()
  "View an item."
  (interactive)
  (antics--view (tabulated-list-get-id)))

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

(defun antics-delete ()
  "Delete antics process."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (proc (slot-value item 'proc))
         (buffer-name (procname item)))
    (progn
      (when proc
        (ignore-errors
          (delete-process proc))
        (setf (slot-value item 'proc) nil)
        (setf (slot-value item 'proc-status) "")
        (ignore-errors
          (kill-buffer buffer-name)))))
  (antics-refresh))

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
    (if proc
        (progn
          (delete-process proc))
      (message "antics: no process for %s" (slot-value item 'name))))
  (antics-refresh))

(defun antics-start ()
  "Start antics process."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (proc (slot-value item 'proc))
         (buffer-name (procname item)))
    (when proc
      (ignore-errors
        (delete-process proc))
      (setf (slot-value item 'proc) nil)
      (setf (slot-value item 'proc-status) ""))
    (start item)))

(defvar antics-mode-map nil
  "Key map for antics-mode.")

(let ((keymap (make-sparse-keymap)))
  (define-key keymap (kbd "RET") 'antics-select-item)
  (define-key keymap (kbd "g") 'antics-refresh)
  (define-key keymap (kbd "R") 'antics-load-config)
  (define-key keymap (kbd "k") 'antics-kill)
  (define-key keymap (kbd "s") 'antics-start)
  (define-key keymap (kbd "v") 'antics-view)
  (define-key keymap (kbd "d") 'antics-delete)
  (define-key keymap (kbd "P") #'(lambda () (interactive)
                                   (list-processes)
                                   (switch-to-buffer "*Process List*")))
  (setq antics-mode-map keymap))

(define-derived-mode antics-mode tabulated-list-mode "antics"
  "Antics mode"
  (antics-refresh))

(defun antics ()
  "Start antics, viewing a list of processes."
  (interactive)
  (switch-to-buffer-other-window "*antics*")
  (antics-mode))

(defun antics--view-kill ()
  (interactive)
  (when antics-item
    (with-slots (proc) antics-item
      (when proc
        (with-current-buffer (process-buffer proc)
          (insert (format "\nProcess killed at %s" (antics--current-time))))
        (ignore-errors
          (kill-process proc))
        (setf (slot-value antics-item 'proc) nil)))))

(defun antics--view-rerun ()
  (interactive)
  (when antics-item
    (with-slots (proc) antics-item
      (when (and proc (y-or-n-p "Stop current process?"))
        (with-current-buffer (process-buffer proc)
          (insert (format "\nProcess killed at %s" (antics--current-time))))
        (ignore-errors
          (kill-process proc))
        (setf (slot-value antics-item 'proc) nil)))
    (start antics-item)))

(defvar antics-view-mode-map nil
  "Key map for antics-view-mode.")

(let ((keymap (make-sparse-keymap)))
  (set-keymap-parent keymap special-mode-map)
  (define-key keymap (kbd "q") 'quit-window)
  (define-key keymap (kbd "g") 'antics--view-rerun)
  (define-key keymap (kbd "k") 'antics--view-kill)
  (setq antics-view-mode-map keymap))

(define-minor-mode antics-view-mode
  "A minor mode for antics buffers."
  :keymap antics-view-mode-map
  :lighter "antics"
  (setq-local window-point-insertion-type t))

(provide 'antics)
;;; antics.el ends here
