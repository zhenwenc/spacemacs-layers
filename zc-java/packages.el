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
    :mode ("\\.java\\'" . java-mode)
    :config
    (progn
      ;; WAT
      )))

(defun zc-java/post-init-ensime ()
  (use-package ensime
    :after java-mode

    :init
    (progn
      (add-hook 'java-mode-hook #'ensime-mode)

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

    :config
    (progn
      (setq ensime-startup-snapshot-notification nil)
      (setq ensime-startup-notification nil)
      (setq ensime-auto-generate-config t)
      (setq ensime-implicit-gutter-icons nil)
      (setq ensime-startup-dirname (f-join spacemacs-cache-directory "ensime"))
      (setq ensime-sem-high-faces
            `((var . scala-font-lock:var-face)
              ;; (val . (:inherit font-lock-constant-face :slant italic))
              (varField . scala-font-lock:var-face)
              ;; (valField . (:inherit font-lock-constant-face :slant italic))
              (functionCall . font-lock-function-name-face)
              (operator . font-lock-keyword-face)
              (param . (:slant italic))
              (class . font-lock-type-face)
              (trait .  (:inherit font-lock-type-face :slant italic))
              (object . font-lock-constant-face)
              (package . font-lock-preprocessor-face)
              (implicitConversion . (:underline ,(with-no-warnings cb-vars-solarized-hl-cyan)))
              (implicitParams . (:underline ,(with-no-warnings cb-vars-solarized-hl-cyan)))
              (deprecated . (:strike-through "dark gray"))))
      )))

;;; packages.el ends here
