;;; packages.el --- SQLi functions File for Spacemacs

(defun zc-sql/sql-connections ()
  "Return predefined SQL connections for helm selection."
  (mapcar (lambda (connection) (car connection)) zc-sql-connection-alist))

(defun zc-sql/sql-connect-preset (connection)
  ;; Load database passwords
  (require 'zc-secrets "/Users/fredc/dotfiles/secret/secrets.el.gpg")

  (let* ((connect-set (assoc-string connection zc-sql-connection-alist))
         (sql-password (assoc-string `sql-password connect-set))
         (password (if sql-password (cadr sql-password)
                      (cadr (assoc-string connection zc-sql-password-alist)))))
    ;; Replace SQL connection password
    (setq connect-set (assq-delete-all 'sql-password connect-set))
    (nconc connect-set `((sql-password ,password)))
    ;; Replace SQL connection presets
    (setq sql-connection-alist (cons connect-set '()))
    ;; Connect to database with SQLi
    (sql-connect connection)

    ;; Clear the loaded secrets
    (unload-feature 'zc-secrets)))

(defun zc-sql/sql-connect ()
  "Connect to a predefined SQL database and start inferior SQLi process."
  (interactive)
  (ivy-read "Predefined SQL connections:"
            (zc-sql/sql-connections)
            :require-match t
            :action #'zc-sql/sql-connect-preset
            :caller 'zc-sql/sql-connect))
