;;; funcs.el --- Ensime Layer functions File for My Spacemacs

(autoload 'ensime "ensime")
(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")

(defun zc-ensime/setup-ensime ()
  "Setup ENSIME."
  ;; jump handler
  (add-to-list 'spacemacs-jump-handlers 'ensime-edit-definition)
  ;; ensure the file exists before starting `ensime-mode'
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode 1)
    )
   ((buffer-file-name)
    (add-hook 'after-save-hook 'ensime-mode nil t))))

(defun zc-ensime/ensime-refactor-accept ()
  (interactive)
  (with-no-warnings (funcall continue-refactor))
  (ensime-popup-buffer-quit-function))

(defun zc-ensime/ensime-refactor-cancel ()
  (interactive)
  (with-no-warnings (funcall cancel-refactor))
  (ensime-popup-buffer-quit-function))

(defun zc-ensime/ensime-gen-and-restart()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (sbt-command ";ensimeConfig;ensimeConfigProject")
    (ensime-shutdown)
    (ensime)))

(defun zc-ensime/ensime-inf-eval-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (ensime-inf-eval-buffer)
  (ensime-inf-switch)
  (evil-insert-state))

(defun zc-ensime/ensime-inf-eval-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (ensime-inf-switch)
  (ensime-inf-eval-region start end)
  (evil-insert-state))

;; HACK: Manually reset some company variables that were set by Ensime
;; TODO: Revisit company backends for ensime
(defun zc-ensime/set-company-variables (&rest _)
  (unless (ensime-connected-p)
    (setq-local company-idle-delay 0.5)
    (setq-local company-backends '(ensime-company))
    (setq-local company-minimum-prefix-length (default-value 'company-minimum-prefix-length))))
