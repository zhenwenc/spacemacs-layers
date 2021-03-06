;;; packages.el --- zc-java layer packages file for Spacemacs.

;;; Code:

(defconst zc-java-packages
  '(aggressive-indent
    ensime
    (java-mode :location built-in)
    (google-c-style :location local)))

(defun zc-java/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'java-mode))))

(defun zc-java/post-init-flycheck ()
  (use-package flycheck
    :init
    (spacemacs/add-flycheck-hook 'java-mode)
    :config
    (progn
      ;; Disable java checker if ensime mode is active
      (defun zc-java/disable-flycheck-java ()
        (when (boundp 'flycheck-disabled-checkers)
          (push 'java flycheck-disabled-checkers)))
      (add-hook 'ensime-mode-hook #'zc-java/disable-flycheck-java))))

(defun zc-java/post-init-eldoc ()
  (add-hook 'java-mode-local-vars-hook #'zc-java/setup-ensime-eldoc))

(defun zc-java/init-google-c-style ()
  (use-package google-c-style
    :defer t
    :commands (google-c-style google-set-c-style)
    :init
    (add-hook 'c-mode-common-hook #'zc-java/setup-indentation-style)))

(defun zc-java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook #'zc-ensime/setup-ensime)

      (put 'java-backend 'safe-local-variable 'symbolp)

      ;; Define command prefixes
      (evil-define-key 'normal java-mode-map
        (kbd "TAB") 'indent-for-tab-command
        (kbd "C-i") 'indent-for-tab-command)

      ;; Define command prefixes
      (setq zc-java/key-binding-prefixes '(("me" . "errors")
                                           ("md" . "eclimd")
                                           ("mf" . "find")
                                           ("mg" . "goto")
                                           ("mr" . "refactor")
                                           ("mh" . "documentation")
                                           ("mm" . "maven")
                                           ("ma" . "ant")
                                           ("mp" . "project")
                                           ("mt" . "test")))
      (mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                         'java-mode (car x) (cdr x)))
            zc-java/key-binding-prefixes))
    ))

(defun zc-java/post-init-ensime ()
  (use-package ensime
    :defer t
    :config
    (progn
      ;; NOTE: see ensime-emacs issue #408
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "ee" 'zc-java/ensime-print-errors-at-point))
    ))


;;; packages.el ends here
