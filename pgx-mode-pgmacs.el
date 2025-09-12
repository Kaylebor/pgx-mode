;;; pgx-mode-pgmacs.el --- PGmacs integration for pgx-mode -*- lexical-binding: t; -*-

;; Author: Ender Veiga Bueno
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (pgx-mode "0.1.0") (pgmacs "0.1.0"))
;; Keywords: database, postgresql, sql

;;; Commentary:

;; Optional integration between pgx-mode and pgmacs for better table display.
;; When loaded, this provides an alternative display method using pgmacs'
;; advanced table widget instead of the simple text-based display.

;;; Code:

(require 'pgmacs nil t)
(require 'pgmacstbl nil t)

(defcustom pgx-use-pgmacs-display nil
  "When non-nil, use pgmacs table widget for displaying results."
  :type 'boolean
  :group 'pgx)

(defun pgx-pgmacs-available-p ()
  "Check if pgmacs is available and loaded."
  (and (featurep 'pgmacs)
       (featurep 'pgmacstbl)))

(defun pgx-pgmacs-format-results (results)
  "Format RESULTS using pgmacs table widget.
RESULTS should be a pgresult struct from pg-exec."
  (unless (pgx-pgmacs-available-p)
    (error "pgmacs is not available"))
  (if (pgresult-p results)
      (let* ((fields (pgresult-attributes results))
             (tuples (pgresult-tuples results))
             (status (pgresult-status results))
             (buffer-name (concat pgx-result-buffer-name " [pgmacs]"))
             (buf (get-buffer-create buffer-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; Display status and row count
            (insert (propertize "Query Results\n" 'face 'bold))
            (insert (format "Status: %s\n" status))
            (insert (format "Rows: %d\n\n" (length tuples)))
            
            ;; Create pgmacs table if we have data
            (when (and fields tuples)
              (let ((columns (cl-loop 
                             for (name type _) in fields
                             collect (make-pgmacstbl-column
                                     :name name
                                     :min-width (max 10 (length name))
                                     :max-width 50
                                     :align 'left))))
                ;; make-pgmacstbl auto-inserts by default when :insert is t (default)
                ;; So we don't need to call pgmacstbl-insert separately
                (make-pgmacstbl
                 :columns columns
                 :objects tuples
                 :use-header-line nil  ; Don't use header line
                 :face 'pgmacstbl
                 :row-colors '((t . (:background "gray95"))
                              (nil . (:background "white"))))))
            
            ;; Make buffer read-only with quit binding
            (setq buffer-read-only t)
            (local-set-key (kbd "q") 'quit-window)
            (goto-char (point-min))))
        buf)
    (error "Invalid results format")))

(defun pgx-display-results-with-pgmacs (results buffer-name)
  "Display RESULTS using pgmacs if available, otherwise fall back.
BUFFER-NAME is ignored when using pgmacs display."
  (if (and pgx-use-pgmacs-display (pgx-pgmacs-available-p))
      (let ((buf (pgx-pgmacs-format-results results)))
        (pop-to-buffer buf))
    ;; Fall back to original display method
    (pgx-display-results results buffer-name)))

;;;###autoload
(defun pgx-enable-pgmacs-display ()
  "Enable pgmacs table widget for query results display."
  (interactive)
  (if (pgx-pgmacs-available-p)
      (progn
        (setq pgx-use-pgmacs-display t)
        (advice-add 'pgx-display-results :override #'pgx-display-results-with-pgmacs)
        (message "pgmacs display enabled for pgx-mode"))
    (error "pgmacs is not available. Please install pgmacs first")))

;;;###autoload
(defun pgx-disable-pgmacs-display ()
  "Disable pgmacs table widget, use simple text display."
  (interactive)
  (setq pgx-use-pgmacs-display nil)
  (advice-remove 'pgx-display-results #'pgx-display-results-with-pgmacs)
  (message "pgmacs display disabled for pgx-mode"))

;;;###autoload  
(defun pgx-toggle-pgmacs-display ()
  "Toggle between pgmacs table widget and simple text display."
  (interactive)
  (if pgx-use-pgmacs-display
      (pgx-disable-pgmacs-display)
    (pgx-enable-pgmacs-display)))

(provide 'pgx-mode-pgmacs)
;;; pgx-mode-pgmacs.el ends here