;;; packages.el --- SQLi functions File for Spacemacs

(defun zc-sql/sql-connections ()
  "Return predefined SQL connections for helm selection."
  `((name . "Predefined SQL Connections")
    (candidates . ,(mapcar (lambda (connection) (car connection))
                           zc-sql-connection-alist))
    (action . (lambda (candidate) (helm-marked-candidates)))))

(defun zc-sql/sql-connect-preset (connection connect-set)
  ;; Load database passwords
  (require 'zc-secrets "/Users/fredc/dotfiles/secret/secrets.el.gpg")

  (let* ((sql-password (assoc-string `sql-password connect-set))
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
  (let ((connection (car (helm
                          :sources (list (zc-sql/sql-connections))))))
    (let* ((connect-set (assoc-string connection zc-sql-connection-alist)))
      (zc-sql/sql-connect-preset connection connect-set))))
