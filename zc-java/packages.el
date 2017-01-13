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

    :leader-bind
    (:mode java-mode
           ("/" . ensime-search)
           ("'" . ensime-inf-switch)

           ("bc" . ensime-sbt-do-compile)
           ("bC" . ensime-sbt-do-clean)
           ("bi" . ensime-sbt-switch)
           ("bp" . ensime-sbt-do-package)
           ("br" . ensime-sbt-do-run)

           ("ct" . ensime-typecheck-current-buffer)
           ("cT" . ensime-typecheck-all)

           ("ee" . ensime-print-errors-at-point)
           ("el" . ensime-show-all-errors-and-warnings)
           ("es" . ensime-stacktrace-switch)

           ("gg" . ensime-edit-definition)
           ("gp" . ensime-pop-find-definition-stack)
           ("gi" . ensime-goto-impl)
           ("gt" . ensime-goto-test)

           ("hh" . ensime-show-doc-for-symbol-at-point)
           ("hT" . ensime-type-at-point-full-name)
           ("ht" . ensime-type-at-point)
           ("hu" . ensime-show-uses-of-symbol-at-point)

           ("ii" . ensime-import-type-at-point)
           ("iI" . ensime-inspect-type-at-point-other-frame)
           ("ip" . ensime-inspect-project-package)

           ("nf" . ensime-reload)
           ("nF" . ensime-reload-open-files)
           ("ns" . ensime)
           ("nS" . zc-ensime/ensime-gen-and-restart)

           ("ra" . ensime-refactor-add-type-annotation)
           ("rd" . ensime-refactor-diff-inline-local)
           ("rD" . ensime-undo-peek)
           ("rf" . ensime-format-source)
           ("ri" . ensime-refactor-diff-organize-imports)
           ("rm" . ensime-refactor-diff-extract-method)
           ("rr" . ensime-refactor-diff-rename)
           ("rt" . ensime-import-type-at-point)
           ("rv" . ensime-refactor-diff-extract-local)

           ("ta" . ensime-sbt-do-test-dwim)
           ("tr" . ensime-sbt-do-test-quick-dwim)
           ("tt" . ensime-sbt-do-test-only-dwim)

           ("sa" . ensime-inf-load-file)
           ("sb" . ensime-inf-eval-buffer)
           ("sB" . zc-ensime/ensime-inf-eval-buffer-switch)
           ("si" . ensime-inf-switch)
           ("sr" . ensime-inf-eval-region)
           ("sR" . zc-ensime/ensime-inf-eval-region-switch)

           ("z" . ensime-expand-selection-command))

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
