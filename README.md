# pgx-mode

PostgreSQL execution mode for Emacs with secure auth-source integration.

## Features

- Execute SQL queries with ephemeral connections (no persistent connections)
- Auth-source integration for secure password retrieval via lambda functions
- Support for multiple database connections with SSL/TLS configuration
- Buffer-local connection tracking
- Clean result display in dedicated buffers
- No passwords stored in memory
- Compatible with AlloyDB and other cloud PostgreSQL providers
- Configurable SSL/TLS modes matching PostgreSQL's `sslmode` parameter
- Optional integration with pgmacs for advanced table display

## Installation

### With Elpaca

```elisp
;; Install pg-el with lambda password support
(use-package pg
  :ensure (:host github 
           :repo "Kaylebor/pg-el" 
           :branch "feature/lambda-password-support"))

;; Install pgx-mode
(use-package pgx-mode
  :ensure (:host github 
           :repo "Kaylebor/pgx-mode")
  :after (sql pg)
  :hook (sql-mode . pgx-mode))
```

### With built-in package.el

```bash
# Clone both repositories:
git clone -b feature/lambda-password-support https://github.com/Kaylebor/pg-el.git ~/.emacs.d/site-lisp/pg-el
git clone https://github.com/Kaylebor/pgx-mode.git ~/.emacs.d/site-lisp/pgx-mode
```

```elisp
;; Add to your init.el:
(add-to-list 'load-path "~/.emacs.d/site-lisp/pg-el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/pgx-mode")
(require 'pg)
(require 'pgx-mode)
(add-hook 'sql-mode-hook #'pgx-mode)
```

## Configuration

### Define Connections

Define your connections in your Emacs configuration (e.g., `sql-connections.el`):

```elisp
(setq pgx-connections
      '((localhost
         (server "localhost")
         (port 5432)
         (database "mydb")
         (user "myuser")
         (sslmode "disable"))  ; No SSL for localhost
        
        (production
         (server "prod.example.com")
         (port 5432)
         (database "proddb")
         (user "produser")
         (sslmode "require"))   ; Encrypted but no cert verification (like JDBC)
        
        (staging
         (server "staging.example.com")
         (port 5432)
         (database "stagingdb")
         (user "staginguser")
         (sslmode "verify-full"))))  ; Full certificate verification

;; Set default connection
(setq pgx-default-connection 'localhost)
```

### SSL/TLS Modes

The `sslmode` parameter controls connection security:

- `"disable"` - No encryption
- `"allow"` - Try non-SSL first, then SSL
- `"prefer"` - Try SSL first, fall back to non-SSL
- `"require"` - Force SSL, no certificate verification (matches JDBC behavior)
- `"verify-ca"` - Force SSL, verify certificate chain
- `"verify-full"` - Force SSL, verify certificate and hostname

### Password Management

For remote connections, store passwords in auth-source (e.g., `~/.authinfo.gpg`):

```
machine prod.example.com port 5432 login produser password secret123
```

For localhost connections, pgx-mode uses an empty password by default.

### Customization Options

```elisp
;; Buffer name for query results
(setq pgx-result-buffer-name "*PGX Results*")

;; Default connection to use
(setq pgx-default-connection 'localhost)

;; Certificate verification behavior (not recommended to change)
(setq pgx-verify-server-cert t)  ; Default: t
```

## Usage

### Key Bindings

- `C-c C-c` - Execute SQL statement at point
- `C-c C-r` - Execute selected region
- `C-c C-b` - Execute entire buffer
- `C-c C-s` - Switch connection
- `C-c C-d` - Switch database (on current connection)

In the results buffer:
- `q` - Close the results window

### Commands

- `pgx-execute-statement-at-point` - Execute the complete SQL statement at point
- `pgx-execute-region` - Execute SQL in selected region
- `pgx-execute-buffer` - Execute entire buffer
- `pgx-switch-connection` - Switch active connection for current buffer
- `pgx-switch-database` - Switch to a different database on current connection

## How It Works

1. **Ephemeral Connections**: Each query execution creates a new connection, executes, and disconnects
2. **Secure Password Handling**: Passwords are fetched from auth-source via lambda functions, never stored as strings
3. **Connection Isolation**: Each buffer tracks its own active connection
4. **Clean Results**: Query results displayed in a dedicated read-only buffer with SQL syntax highlighting
5. **SSL/TLS Compatibility**: Works with cloud providers (AlloyDB, RDS, etc.) by handling GnuTLS/OpenSSL differences

## Compatibility Notes

### AlloyDB and Cloud Providers

pgx-mode handles the known incompatibility between GnuTLS (used by Emacs) and OpenSSL (used by most PostgreSQL services). When using `sslmode="require"`, certificate verification is disabled to match JDBC driver behavior, allowing connections to services with self-signed or problematic certificates.

### GnuTLS vs OpenSSL

Emacs uses GnuTLS for TLS connections while most PostgreSQL services expect OpenSSL. This can cause certificate validation issues. pgx-mode works around this by:
- Using GnuTLS compatibility mode (`NORMAL:%COMPAT`)
- Properly handling the global `gnutls-verify-error` setting
- Matching JDBC's certificate verification behavior for `sslmode="require"`

## Dependencies

- Emacs 27.1+
- [pg-el](https://github.com/emarsden/pg-el) with lambda password support
- auth-source (built-in)
- GnuTLS support in Emacs

## Optional: pgmacs Integration

pgx-mode can optionally use [pgmacs](https://github.com/emarsden/pgmacs)' advanced table widget for displaying query results with better formatting, colors, and interactivity.

### Setup with Elpaca

```elisp
;; Install pgmacs
(use-package pgmacs
  :ensure (:host github 
           :repo "emarsden/pgmacs"))

;; Configure pgx-mode with automatic pgmacs integration
(use-package pgx-mode
  :ensure (:host github 
           :repo "Kaylebor/pgx-mode")
  :after (sql pg)
  :hook (sql-mode . pgx-mode)
  :config
  ;; Automatically enable pgmacs display if available
  (when (require 'pgx-mode-pgmacs nil t)
    (pgx-enable-pgmacs-display)))
```

### Manual Control

Once loaded, you can toggle between display modes:

```
M-x pgx-toggle-pgmacs-display
```

When enabled, query results will be displayed using pgmacs' table widget with:
- Alternating row colors
- Column alignment
- Better handling of wide data
- Interactive features from pgmacs

## Troubleshooting

### Certificate Verification Errors

If you get certificate verification errors with cloud providers:
1. Ensure you're using `sslmode="require"` (not `"verify-ca"` or `"verify-full"`)
2. This matches DBeaver/JDBC behavior which doesn't verify certificates with `sslmode=require`

### Connection Timeouts

Adjust the connection timeout if needed:
```elisp
(setq pg-connect-timeout 10)  ; Default is 5 seconds
```

### Debug Connection Issues

Enable query logging:
```elisp
(setq pg-log-queries t)
```

## License

GPL-3.0

## Author

Ender Veiga Bueno (@Kaylebor)

## Acknowledgments

- [pg-el](https://github.com/emarsden/pg-el) by Eric Marsden for the PostgreSQL wire protocol implementation
- Auth-source for secure credential management