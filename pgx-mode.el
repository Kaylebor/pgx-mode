;;; pgx-mode.el --- PostgreSQL execution mode with auth-source integration -*- lexical-binding: t; -*-

;; Author: Ender Veiga Bueno
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (pg "0.37"))
;; Keywords: database, postgresql, sql
;; URL: https://github.com/Kaylebor/pgx-mode

;;; Commentary:

;; pgx-mode provides a secure PostgreSQL execution environment for Emacs.
;; It wraps pg-el with auth-source integration to execute SQL queries
;; against any PostgreSQL database without storing passwords in memory.
;;
;; Features:
;; - Execute SQL queries with ephemeral connections
;; - Auth-source integration for secure password retrieval
;; - Support for multiple database connections
;; - Buffer-local connection tracking
;; - Result display in dedicated buffers

;;; Code:

(require 'pg)
(require 'sql)
(require 'auth-source)

(defgroup pgx nil
  "PostgreSQL execution with auth-source integration."
  :group 'sql
  :prefix "pgx-")

(defcustom pgx-result-buffer-name "*PGX Results*"
  "Name of the buffer to display query results."
  :type 'string
  :group 'pgx)

(defcustom pgx-connections nil
  "Alist of PostgreSQL connections.
Each entry is (connection-name . plist) with keys: server, port, database, user."
  :type '(alist :key-type symbol :value-type plist)
  :group 'pgx)

(defcustom pgx-default-connection 'localhost
  "Default connection to use when pgx-mode starts."
  :type 'symbol
  :group 'pgx)

(defcustom pgx-verify-server-cert t
  "Whether to verify server certificates for SSL connections.
When non-nil (the default), pgx-mode will verify certificates
according to the sslmode setting. Set to nil to disable all
certificate verification (not recommended for production)."
  :type 'boolean
  :group 'pgx)

(defvar-local pgx-current-connection nil
  "Current connection for this buffer.")

(defvar pgx-connection-cache (make-hash-table :test 'equal)
  "Cache for auth-source lookups to avoid repeated prompts.")

;;; Auth-source integration

(defun pgx-auth-source-get-password (host user port)
  "Get password from auth-source for HOST, USER, and PORT.
Returns a lambda that fetches the password when called."
  (let ((cache-key (format "%s:%s:%s" host user port)))
    ;; Check cache first
    (or (gethash cache-key pgx-connection-cache)
        ;; If not cached, create a lambda that fetches the password
        (let ((auth-lambda
               (lambda ()
                 (let* ((auth (car (auth-source-search
                                   :host host
                                   :user user
                                   :port port
                                   :require '(:secret)
                                   :max 1)))
                        (secret (plist-get auth :secret)))
                   (when secret
                     (if (functionp secret)
                         (funcall secret)
                       secret))))))
          ;; Store the lambda in cache
          (puthash cache-key auth-lambda pgx-connection-cache)
          auth-lambda))))

;;; Connection management

(defun pgx-get-connection-params (connection-name)
  "Get connection parameters for CONNECTION-NAME from pgx-connections."
  (let* ((conn-spec (assoc connection-name pgx-connections))
         (params (cdr conn-spec)))
    (when params
      (let ((server (cadr (assoc 'server params)))
            (port (or (cadr (assoc 'port params)) 5432))
            (user (cadr (assoc 'user params)))
            (database (cadr (assoc 'database params)))
            (sslmode (or (cadr (assoc 'sslmode params)) "prefer")))
        (list :server server :port port :user user :database database :sslmode sslmode)))))

(defun pgx-connect (connection-name)
  "Create a PostgreSQL connection using CONNECTION-NAME.
Returns a connection object or signals an error."
  (let* ((params (pgx-get-connection-params connection-name))
         (server (plist-get params :server))
         (port (plist-get params :port))
         (user (plist-get params :user))
         (database (plist-get params :database))
         (sslmode (plist-get params :sslmode)))
    (unless params
      (error "Connection %s not found" connection-name))
    ;; For localhost connections, use empty password
    ;; For remote connections, use auth-source lambda
    (let* ((password (if (member server '("localhost" "127.0.0.1"))
                        ""
                       (pgx-auth-source-get-password server user port)))
           ;; Determine TLS options based on sslmode
           ;; Note: pg-el expects t for basic TLS or a plist for gnutls-negotiate options
           (tls-options 
            (cond
             ((string= sslmode "disable") nil)
             ((string= sslmode "allow") nil)  ; Try without TLS first
             ((string= sslmode "prefer") t)   ; Try with TLS, fall back if needed
             ((string= sslmode "require")
              ;; Enable TLS without certificate verification
              ;; (matches JDBC behavior with sslmode=require)
              '(:verify-error nil 
                :verify-hostname-error nil
                :priority-string "NORMAL:%COMPAT"
                :min-prime-bits nil))
             ((string= sslmode "verify-ca")
              ;; Verify certificate chain but not hostname
              (if pgx-verify-server-cert
                  '(:verify-hostname-error nil
                    :priority-string "NORMAL:%COMPAT")
                '(:verify-error nil :verify-hostname-error nil
                  :priority-string "NORMAL:%COMPAT"
                  :min-prime-bits nil)))
             ((string= sslmode "verify-full")
              ;; Full verification
              (if pgx-verify-server-cert
                  '(:priority-string "NORMAL:%COMPAT")
                '(:verify-error nil :verify-hostname-error nil
                  :priority-string "NORMAL:%COMPAT"
                  :min-prime-bits nil)))
             (t nil)))   ; Default to no TLS if sslmode not specified
           ;; For sslmode=require, temporarily override global gnutls-verify-error
           (orig-gnutls-verify-error gnutls-verify-error))
      (unwind-protect
          (progn
            ;; Temporarily set global variable for sslmode=require
            (when (string= sslmode "require")
              (setq gnutls-verify-error nil))
            ;; Use pg-connect with lambda password support
            (if tls-options
                (pg-connect database user password server port tls-options)
              (pg-connect database user password server port)))
        ;; Restore original value
        (setq gnutls-verify-error orig-gnutls-verify-error)))))

;;; Query execution

(defun pgx-execute-query (connection-name query)
  "Execute QUERY using CONNECTION-NAME.
Opens a connection, executes the query, closes the connection."
  (let ((con nil)
        (results nil))
    (unwind-protect
        (progn
          (setq con (pgx-connect connection-name))
          (setq results (pg-exec con query))
          results)
      (when con
        (pg-disconnect con)))))

(defun pgx-format-results (results)
  "Format RESULTS from pg-exec for display."
  (if (pgresult-p results)
      (let* ((fields (pgresult-attributes results))
             (tuples (pgresult-tuples results))
             (status (pgresult-status results)))
        (with-temp-buffer
          (insert (format "-- Status: %s\n" status))
          (insert (format "-- Rows: %d\n\n" (length tuples)))
          ;; Header
          (when fields
            (insert (mapconcat #'car fields " | "))
            (insert "\n")
            (insert (make-string (current-column) ?-))
            (insert "\n"))
          ;; Data
          (dolist (tuple tuples)
            (insert (mapconcat (lambda (val)
                                (if val (format "%s" val) "NULL"))
                              tuple " | "))
            (insert "\n"))
          (buffer-string)))
    (format "Error: %s" results)))

(defun pgx-display-results (results buffer-name)
  "Display RESULTS in buffer BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pgx-format-results results))
        (goto-char (point-min))
        (sql-mode)
        (setq buffer-read-only t)))
    (pop-to-buffer buf)))

;;; Interactive commands

(defun pgx-switch-connection ()
  "Switch the current connection for this buffer."
  (interactive)
  (let* ((names (mapcar #'car pgx-connections))
         (current (or pgx-current-connection pgx-default-connection))
         (choice (intern (completing-read
                         (format "Connection (current: %s): " current)
                         (mapcar #'symbol-name names)
                         nil t))))
    (setq pgx-current-connection choice)
    (message "Switched to connection: %s" choice)))

(defun pgx-list-databases ()
  "List available databases on the current connection."
  (interactive)
  (let* ((connection (or pgx-current-connection pgx-default-connection))
         (query "SELECT datname FROM pg_database WHERE datistemplate = false ORDER BY datname")
         (results (pgx-execute-query connection query)))
    (if (pgresult-p results)
        (mapcar #'car (pgresult-tuples results))
      (error "Failed to list databases"))))

(defun pgx-switch-database ()
  "Switch to a different database on the current connection."
  (interactive)
  (let* ((connection-name (or pgx-current-connection pgx-default-connection))
         (databases (pgx-list-databases))
         (current-db (plist-get (pgx-get-connection-params connection-name) :database))
         (new-db (completing-read
                  (format "Database (current: %s): " current-db)
                  databases nil t)))
    ;; Update the connection's database
    (let* ((conn-spec (assoc connection-name pgx-connections))
           (params (cdr conn-spec)))
      (setcdr (assoc 'database params) new-db)
      (message "Switched to database: %s" new-db))))

(defun pgx-execute-statement-at-point ()
  "Execute the SQL statement at point."
  (interactive)
  (let* ((connection (or pgx-current-connection pgx-default-connection))
         (bounds (pgx-statement-bounds))
         (query (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (message "Executing on %s..." connection)
    (condition-case err
        (let ((results (pgx-execute-query connection query)))
          (pgx-display-results results pgx-result-buffer-name)
          (message "Query executed successfully on %s" connection))
      (error
       (message "Error: %s" (error-message-string err))))))

(defun pgx-execute-region (start end)
  "Execute the SQL in the selected region."
  (interactive "r")
  (let* ((connection (or pgx-current-connection pgx-default-connection))
         (query (buffer-substring-no-properties start end)))
    (message "Executing on %s..." connection)
    (condition-case err
        (let ((results (pgx-execute-query connection query)))
          (pgx-display-results results pgx-result-buffer-name)
          (message "Query executed successfully on %s" connection))
      (error
       (message "Error: %s" (error-message-string err))))))

(defun pgx-execute-buffer ()
  "Execute the entire buffer as SQL."
  (interactive)
  (pgx-execute-region (point-min) (point-max)))

(defun pgx-statement-bounds ()
  "Find the bounds of the SQL statement at point.
Statements are delimited by semicolons."
  (save-excursion
    (let* ((end (or (save-excursion
                     (when (re-search-forward ";" nil t)
                       (point)))
                   (point-max)))
           (start (or (save-excursion
                       (when (re-search-backward ";" nil t)
                         (forward-char)
                         (skip-chars-forward " \t\n")
                         (point)))
                     (point-min))))
      (cons start end))))

;;; Minor mode

(defvar pgx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pgx-execute-statement-at-point)
    (define-key map (kbd "C-c C-r") #'pgx-execute-region)
    (define-key map (kbd "C-c C-b") #'pgx-execute-buffer)
    (define-key map (kbd "C-c C-s") #'pgx-switch-connection)
    (define-key map (kbd "C-c C-d") #'pgx-switch-database)
    map)
  "Keymap for pgx-mode.")

;;;###autoload
(define-minor-mode pgx-mode
  "Minor mode for PostgreSQL execution with auth-source integration."
  :lighter " PGX"
  :keymap pgx-mode-map
  (when pgx-mode
    ;; Set default connection
    (unless pgx-current-connection
      (setq pgx-current-connection pgx-default-connection))))

;;;###autoload
(defun pgx-setup ()
  "Set up pgx-mode with sql-mode integration."
  (add-hook 'sql-mode-hook #'pgx-mode))

;; Load optional integrations if available
(if (featurep 'pgmacs)
    ;; pgmacs already loaded, load integration now
    (require 'pgx-mode-pgmacs nil t)
  ;; pgmacs not yet loaded, load integration when it loads
  (with-eval-after-load 'pgmacs
    (require 'pgx-mode-pgmacs nil t)))

(provide 'pgx-mode)
;;; pgx-mode.el ends here