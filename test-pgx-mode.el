;;; test-pgx-mode.el --- Tests for pgx-mode -*- lexical-binding: t; -*-

;; Author: Ender Veiga Bueno
;; Keywords: database, postgresql, test

;;; Commentary:

;; Test suite for pgx-mode.
;; Run with: emacs -batch -l test-pgx-mode.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'pgx-mode)

;;; Test fixtures

(defvar test-pgx-connections
  '((test-localhost
     (server "localhost")
     (port 5432)
     (database "test_db")
     (user "test_user")
     (sslmode "disable"))
    (test-remote
     (server "test.example.com")
     (port 5432)
     (database "test_db")
     (user "test_user")
     (sslmode "require"))))

;;; Tests for connection parameter handling

(ert-deftest test-pgx-get-connection-params ()
  "Test getting connection parameters."
  (let ((pgx-connections test-pgx-connections))
    ;; Test valid connection
    (let ((params (pgx-get-connection-params 'test-localhost)))
      (should (equal (plist-get params :server) "localhost"))
      (should (equal (plist-get params :port) 5432))
      (should (equal (plist-get params :database) "test_db"))
      (should (equal (plist-get params :user) "test_user"))
      (should (equal (plist-get params :sslmode) "disable")))
    ;; Test non-existent connection
    (should-not (pgx-get-connection-params 'non-existent))))

(ert-deftest test-pgx-sslmode-default ()
  "Test that sslmode defaults to prefer when not specified."
  (let ((pgx-connections '((test-no-ssl
                            (server "localhost")
                            (port 5432)
                            (database "test_db")
                            (user "test_user")))))
    (let ((params (pgx-get-connection-params 'test-no-ssl)))
      (should (equal (plist-get params :sslmode) "prefer")))))

;;; Tests for auth-source integration

(ert-deftest test-pgx-auth-source-cache ()
  "Test that auth-source lambdas are cached."
  (let ((pgx-connection-cache (make-hash-table :test 'equal)))
    ;; First call should create and cache a lambda
    (let ((lambda1 (pgx-auth-source-get-password "test.com" "user" 5432)))
      (should (functionp lambda1))
      ;; Second call should return the same cached lambda
      (let ((lambda2 (pgx-auth-source-get-password "test.com" "user" 5432)))
        (should (eq lambda1 lambda2))))))

;;; Tests for statement boundary detection

(ert-deftest test-pgx-statement-bounds ()
  "Test SQL statement boundary detection."
  (with-temp-buffer
    (insert "SELECT * FROM users;\nSELECT * FROM posts;\n")
    (goto-char 10) ; Middle of first statement
    (let ((bounds (pgx-statement-bounds)))
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 21)))
    (goto-char 30) ; Middle of second statement
    (let ((bounds (pgx-statement-bounds)))
      (should (= (car bounds) 22))
      (should (= (cdr bounds) 43)))))

(ert-deftest test-pgx-statement-bounds-no-semicolon ()
  "Test statement bounds when no semicolon is present."
  (with-temp-buffer
    (insert "SELECT * FROM users")
    (goto-char 10)
    (let ((bounds (pgx-statement-bounds)))
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 20)))))

;;; Tests for result formatting

(ert-deftest test-pgx-format-results-with-data ()
  "Test formatting of query results with data."
  (let* ((result (make-pgresult))
         (formatted nil))
    (setf (pgresult-status result) "SELECT 2")
    (setf (pgresult-attributes result) '(("id" 23 4) ("name" 25 -1)))
    (setf (pgresult-tuples result) '((1 "Alice") (2 "Bob")))
    (setq formatted (pgx-format-results result))
    (should (string-match "Status: SELECT 2" formatted))
    (should (string-match "Rows: 2" formatted))
    (should (string-match "id | name" formatted))
    (should (string-match "1 | Alice" formatted))
    (should (string-match "2 | Bob" formatted))))

(ert-deftest test-pgx-format-results-empty ()
  "Test formatting of empty query results."
  (let* ((result (make-pgresult))
         (formatted nil))
    (setf (pgresult-status result) "SELECT 0")
    (setf (pgresult-attributes result) '(("id" 23 4)))
    (setf (pgresult-tuples result) '())
    (setq formatted (pgx-format-results result))
    (should (string-match "Status: SELECT 0" formatted))
    (should (string-match "Rows: 0" formatted))))

(ert-deftest test-pgx-format-results-null-values ()
  "Test formatting of NULL values in results."
  (let* ((result (make-pgresult))
         (formatted nil))
    (setf (pgresult-status result) "SELECT 1")
    (setf (pgresult-attributes result) '(("value" 25 -1)))
    (setf (pgresult-tuples result) '((nil)))
    (setq formatted (pgx-format-results result))
    (should (string-match "NULL" formatted))))

;;; Tests for mode setup

(ert-deftest test-pgx-mode-keymap ()
  "Test that pgx-mode sets up keybindings correctly."
  (should (keymapp pgx-mode-map))
  (should (eq (lookup-key pgx-mode-map (kbd "C-c C-c")) 
              'pgx-execute-statement-at-point))
  (should (eq (lookup-key pgx-mode-map (kbd "C-c C-r")) 
              'pgx-execute-region))
  (should (eq (lookup-key pgx-mode-map (kbd "C-c C-b")) 
              'pgx-execute-buffer))
  (should (eq (lookup-key pgx-mode-map (kbd "C-c C-s")) 
              'pgx-switch-connection))
  (should (eq (lookup-key pgx-mode-map (kbd "C-c C-d")) 
              'pgx-switch-database)))

(ert-deftest test-pgx-mode-initialization ()
  "Test that pgx-mode initializes correctly."
  (with-temp-buffer
    (let ((pgx-default-connection 'test-localhost)
          (pgx-connections test-pgx-connections))
      (pgx-mode 1)
      (should pgx-mode)
      (should (eq pgx-current-connection 'test-localhost))
      (pgx-mode -1)
      (should-not pgx-mode))))

;;; Run tests if executed directly

(when noninteractive
  (ert-run-tests-batch))

(provide 'test-pgx-mode)
;;; test-pgx-mode.el ends here