;;; packages.el --- SQLi functions File for Spacemacs

(defun zc-sql/sql-connections ()
  "Return predefined SQL connections for helm selection."
  `((name . "Predefined SQL Connections")
    (candidates . ,(mapcar (lambda (connection) (car connection))
                           sql-connection-alist))
    (action . (lambda (candidate) (helm-marked-candidates)))))

(defun zc-sql/sql-connect ()
  "Connect to a predefined SQL database and start inferior SQLi process."
  (interactive)
  (let ((connection (car (helm
                          :sources (list (zc-sql/sql-connections))))))
    ;; Advice SQLi establish connection via SSH tunnel
    (let (connect-set ssh-params)
      (setq connect-set (assoc-string connection zc-sql-connection-alist))
      (setq ssh-params (assoc-string `ssh-params connect-set))
      (if ssh-params
          (let ((ssh-hostname (cadr (assoc-string `hostname ssh-params))))
            (defun zc-sql/sql-comint-with-remote (orig-func &rest args)
              "Connect to database via SSH tunnel."
              (let ((default-directory (format "/ssh:%s" ssh-hostname)))
                (apply orig-func args)))
            (advice-add 'sql-comint-mysql :around #'zc-sql/sql-comint-with-remote)
            (sql-connect connection)
            (advice-remove 'sql-comint-mysql #'zc-sql/sql-comint-with-remote))
        (sql-connect connection)))))
