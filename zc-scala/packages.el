;;; packages.el --- Scala Layer packages File for Spacemacs

(eval-when-compile
  (require 'use-package nil t))

(defconst zc-scala-packages
  '(aggressive-indent
    ensime
    flycheck
    sbt-mode
    scala-mode))

(defun zc-scala/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :config
    (progn
      (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
      (add-to-list 'aggressive-indent-excluded-modes 'sbt-file-mode))))

(defun zc-scala/post-init-flycheck ()
  (use-package flycheck
    :init
    (spacemacs/add-flycheck-hook 'scala-mode)
    :config
    (progn
      (setq flycheck-scalastylerc "~/.scalastyle.xml")

      (defun zc-scala/disable-flycheck-scala ()
        (when (boundp 'flycheck-disabled-checkers)
          (push 'scala flycheck-disabled-checkers)))
      (add-hook 'ensime-mode-hook #'zc-scala/disable-flycheck-scala))))

(defun zc-scala/init-scala-mode ()
  (use-package scala-mode
    :mode ("\\.scala\\'" . scala-mode)
    :init
    ;; For Play Framework
    (add-to-list 'auto-mode-alist '("/conf/routes\\'" . conf-mode))

    :leader-bind
    (:mode scala-mode
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
      (setq scala-indent:align-forms t)
      (setq scala-indent:align-parameters t)
      (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)

      ;; Automatically replace arrows with unicode ones when enabled
      (when zc-scala-use-unicode-arrows
        (define-key scala-mode-map (kbd ">") 'scala/unicode-gt)
        (define-key scala-mode-map (kbd "-") 'scala/unicode-hyphen))

      ;; Fuck the `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters nil
            scala-indent:default-run-on-strategy scala-indent:operator-strategy)

      (custom-set-faces
       `(scala-font-lock:var-face
         ((t (:foreground, (with-no-warnings cb-vars-solarized-hl-orange) :underline nil))))))

    ))

(defun zc-scala/post-init-ensime ()
  (use-package ensime
    :after scala-mode

    :init
    (progn
      (add-hook 'scala-mode-hook #'ensime-mode)

      (dolist (prefix '(("mb" . "scala/build")
                        ("mc" . "scala/check")
                        ("md" . "scala/debug")
                        ("me" . "scala/errors")
                        ("mg" . "scala/goto")
                        ("mh" . "scala/docs")
                        ("mi" . "scala/inspect")
                        ("mn" . "scala/ensime")
                        ("mr" . "scala/refactor")
                        ("mt" . "scala/test")
                        ("ms" . "scala/repl")
                        ("my" . "scala/yank")))
        (spacemacs/declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix)))
      )

    :config
    (progn
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
      ))
  )

(defun zc-scala/post-init-indent-dwim ()
  (use-package indent-dwim
    :after ensime
    :config
    (add-to-list 'indent-dwim-commands-alist '(scala-mode . ensime-format-source))))
