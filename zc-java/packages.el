;;; packages.el --- zc-java layer packages file for Spacemacs.

;;; Code:

(defconst zc-java-packages
  '(aggressive-indent
    ensime
    (java-mode :location built-in)))

(defun zc-java/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'java-mode))))

(defun zc-java/init-java-mode ()
  (use-package java-mode
    :defer t
    :mode ("\\.java\\'" . java-mode)
    ))

(defun zc-java/post-init-ensime ()
  (use-package ensime
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook #'zc-ensime/setup-ensime)

      (dolist (prefix '(("me" . "java/errors")
                        ("md" . "java/eclimd")
                        ("mf" . "java/find")
                        ("mg" . "java/goto")
                        ("mr" . "java/refactor")
                        ("mh" . "java/documentation")
                        ("mm" . "java/maven")
                        ("ma" . "java/ant")
                        ("mp" . "java/project")
                        ("mt" . "java/test")))
        (spacemacs/declare-prefix-for-mode 'java-mode (car prefix) (cdr prefix))))
    ))

;;; packages.el ends here
