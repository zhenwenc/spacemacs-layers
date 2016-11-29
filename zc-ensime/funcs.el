;;; funcs.el --- Ensime Layer functions File for My Spacemacs

(autoload 'ensime "ensime")

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
