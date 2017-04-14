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
         (password `(if sql-password (cadr sql-password)
                      (cadr (assoc-string connection zc-sql-password)))))
    (print zc-sql-password)
    ;; Replace SQL connection password
    (setq connect-set (assq-delete-all 'sql-password connect-set))
    (nconc connect-set `((sql-password ,password)))
    ;; Replace SQL connection presets
    (setq sql-connection-alist (cons connect-set '()))
    (print sql-connection-alist)
    ;; Connect to database with SQLi
    (sql-connect connection)
    ;; Clear the loaded secrets
    (makunbound 'zc-sql-password)))

(defun zc-sql/sql-connect ()
  "Connect to a predefined SQL database and start inferior SQLi process."
  (interactive)
  (let ((connection (car (helm
                          :sources (list (zc-sql/sql-connections))))))
    (let* ((connection-info (assoc-string connection zc-sql-connection-alist))
           (connect-set (remove (assoc `ssh-params connection-info) connection-info))
           (ssh-params (assoc-string `ssh-params connection-info)))
      ;; Advice SQLi establish connection via SSH tunnel
      (if ssh-params
          (let ((ssh-hostname (cadr (assoc-string `hostname ssh-params))))
            (defun zc-sql/sql-comint-with-remote (orig-func &rest args)
              "Connect to database via SSH tunnel."
              (let ((default-directory (format "/ssh:%s" ssh-hostname)))
                (apply orig-func args)))
            (advice-add 'sql-comint :around #'zc-sql/sql-comint-with-remote)
            (zc-sql/sql-connect-preset connection connect-set)
            (advice-remove 'sql-comint #'zc-sql/sql-comint-with-remote))
        (zc-sql/sql-connect-preset connection connect-set))
      )))
