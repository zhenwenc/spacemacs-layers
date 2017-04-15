;;; packages.el --- SQLi functions File for Spacemacs

(defun zc-sql/connections ()
  "Return predefined SQL connections for helm selection."
  (mapcar (lambda (connection) (car connection)) zc-sql-connection-alist))

(defun zc-sql/get-password (connection)
  "Return the password of predefined SQL connection."
  (interactive "sPredefined SQL connections: ")
  (let ((alist (zc-core/get-secrets `zc-sql-password-alist)))
    (cadr (assoc-string connection alist))))

(defun zc-sql/connect-preset (connection)
  (interactive "sPredefined SQL connections: ")
  (let* ((connect-set (assoc-string connection zc-sql-connection-alist))
         (sql-password (assoc-string `sql-password connect-set))
         (password (if sql-password (cadr sql-password)
                     (zc-sql/get-password connection))))
    ;; Replace SQL connection password
    (setq connect-set (assq-delete-all 'sql-password connect-set))
    (nconc connect-set `((sql-password ,password)))
    ;; Replace SQL connection presets
    (setq sql-connection-alist (cons connect-set '()))
    ;; Connect to database with SQLi
    (condition-case nil
        (sql-connect connection connection)
      ((debug error) (sql-connect connection connection)))))

(defun zc-sql/start-connection-p ()
  (interactive "P")
  (when (y-or-n-p "Do you want to start new SQL connection? ")
    (zc-sql/start-connection)))

(defun zc-sql/start-connection ()
  "Connect to a predefined SQL database and start inferior SQLi process."
  (interactive)
  (ivy-read "Predefined SQL connections:"
            (zc-sql/connections)
            :require-match t
            :action #'zc-sql/connect-preset
            :caller 'zc-sql/connect))



(defun zc-sql/visible-sqli-buffer-p (buffer)
  "True when buffer is a visible alive SQLi buffer."
  (and buffer (get-buffer-window buffer) (sql-buffer-live-p buffer)))

(defun zc-sql/find-visible-sqli-buffer ()
  "Return the current visible SQLi buffer. In order to guarantee deterministic
result, the function returns nil if there are multiple visible SQLi buffers."
  (let ((bufs (remove-if-not #'zc-sql/visible-sqli-buffer-p (buffer-list))))
    (when (= (length bufs) 1) (car bufs))))

(defun zc-sql/find-sqli-buffer ()
  "Retrun the name of the an alive SQLi buffer."
  (interactive)
  (when (or (not (null (zc-sql/sqli-buffers)))
            (zc-sql/start-connection-p))
    (let ((buffer (zc-sql/find-visible-sqli-buffer)))
      (if buffer (buffer-name buffer)
        (zc-sql/read-sqli-buffer "Attach to SQLi buffer:")))))

(defun zc-sql/set-sqli-buffer-and-focus (buffer)
  "Set the SQLi buffer SQL strings are sent to."
  (interactive)
  (if (and buffer (derived-mode-p 'sql-mode))
      (progn
        (message "Attach to interactive SQLi buffer: %s" buffer)
        (pop-to-buffer (current-buffer))
        (setq sql-buffer buffer)
        (run-hooks 'sql-set-sqli-hook))
    (error "No SQLi buffer to attach to")))

(defun zc-sql/attach-to-sqli-buffer ()
  "Attach to the selected alive SQLi buffer.

If there has no alive SQLi buffer, prompt create new SQL connection;
If there has only one SQLi buffer, attach to the SQLi buffer;
If there has multiple SQLi buffer, prompt select which buffer to attach to."
  (interactive)
  (when (derived-mode-p 'sql-mode)
    (let (buffer (sql-buffers (zc-sql/sqli-buffers)))
      (cond ((null (zc-sql/sqli-buffers))
             (zc-sql/attach-to-new-sqli-buffer))
            ((= 1 (length (zc-sql/sqli-buffers)))
             (setq buffer (zc-sql/find-visible-sqli-buffer)))
            (t (setq buffer (zc-sql/read-sqli-buffer))))
      (when buffer (zc-sql/set-sqli-buffer-and-focus buffer)))))

(defun zc-sql/attach-to-sqli-buffer-auto ()
  "Automatic attach to the currently visible alive SQLi buffer if there is
only one such buffer. Othersize fallback to \\[zc-sql/attach-to-sql-buffer]"
  (interactive)
  (when (derived-mode-p 'sql-mode)
    (let ((buffer (zc-sql/find-visible-sqli-buffer)))
      (if buffer (zc-sql/set-sqli-buffer-and-focus buffer)
        (zc-sql/attach-to-sqli-buffer)))))

(defun zc-sql/attach-to-new-sqli-buffer ()
  "Prompt create new SQL connection, and attach to the created SQLi buffer."
  (interactive)
  (when (derived-mode-p 'sql-mode)
    (zc-sql/start-connection-p)
    (zc-sql/set-sqli-buffer-and-focus (zc-sql/find-visible-sqli-buffer))))

(defun zc-sql/show-attached-sqli-buffer ()
  "Display the attached SQLi buffer."
  (interactive)
  (message "Currently attached to SQLi buffer: %s" sql-buffer))



(defun zc-sql/sqli-buffers ()
  "Return the currently alive SQLi buffers."
  (remove-if-not #'sql-buffer-live-p (buffer-list)))

(defun zc-sql/read-sqli-buffer (&optional prompt)
  "Return the selected alive SQLi buffer."
  (interactive)
  (ivy-read (if prompt prompt "SQLi buffer:")
            (mapcar #'buffer-name
                    (remove-if-not #'sql-buffer-live-p (buffer-list)))
            :require-match t
            :caller 'zc-sql/read-sqli-buffer))

(defun zc-sql/switch-to-sqli-buffer ()
  "Switch to an alive SQLi buffer."
  (interactive)
  (switch-to-buffer
   (zc-sql/read-sqli-buffer "Switch to SQLi buffer:")))
