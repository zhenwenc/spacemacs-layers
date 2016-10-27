;;; packages.el --- Scala Layer packages File for Spacemacs

(eval-when-compile
  (require 'use-package nil t))

(defconst zc-scala-packages
  '(aggressive-indent
    ensime
    flycheck
    sbt-mode
    scala-mode
    (ensime-diminished-modeline :location local)))

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
         ((t (:foreground, (with-no-warnings cb-vars-solarized-hl-orange) :underline nil))))))))

(defun zc-scala/init-ensime ()
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

    :evil-bind
    (:map
     ensime-mode-map
     :state insert
     ("M-." . ensime-edit-definition)
     ("M-," . ensime-pop-find-definition-stack)
     :state normal
     ("M-." . ensime-edit-definition)
     ("M-," . ensime-pop-find-definition-stack)
     ("RET" . ensime-inspect-type-at-point)

     :map ensime-popup-buffer-map
     :state normal
     ("q" . ensime-popup-buffer-quit-function)

     :map ensime-inspector-mode-map
     :state normal
     ("M-." . ensime-inspector-browse-source)
     ("K" . ensime-inspector-browse-doc)
     ("q" . ensime-popup-buffer-quit-function)
     ("," . ensime-inspector-backward-page)
     ("." . ensime-inspector-forward-page)
     ("^" . ensime-inspector-backward-page)

     :map ensime-refactor-info-map
     :state normal
     ("q" . zc-scala/ensime-refactor-cancel)
     ("c" . zc-scala/ensime-refactor-accept)
     ("RET" . zc-scala/ensime-refactor-accept)

     :map ensime-compile-result-map
     :state normal
     ("g" . ensime-show-all-errors-and-warnings)
     ("TAB" . forward-button)
     ("<backtab>" . backward-button)
     ("M-n" . forward-button)
     ("M-p" . backward-button)
     ("n" . forward-button)
     ("N" . backward-button))

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

           ("nF" . ensime-reload-open-files)
           ("ns" . ensime)
           ("nS" . zc-scala/ensime-gen-and-restart)

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
           ("sB" . zc-scala/ensime-inf-eval-buffer-switch)
           ("si" . ensime-inf-switch)
           ("sr" . ensime-inf-eval-region)
           ("sR" . zc-scala/ensime-inf-eval-region-switch)

           ("z" . ensime-expand-selection-command))

    :config
    (progn
      (setq ensime-startup-snapshot-notification nil)
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

      (setq ensime-goto-test-config-defaults
            (list :test-class-names-fn #'ensime-goto-test--test-class-names
                  :test-class-suffixes '("Test" "Tests"
                                         "IntTest" "IntTests" "IntegrationTest" "IntegrationTests"
                                         "Spec" "Specs" "Specification" "Specifications"
                                         "Prop" "Props" "Property" "Properties"
                                         "Check" "Checks")
                  :impl-class-name-fn #'ensime-goto-test--impl-class-name
                  :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
                  :is-test-dir-fn #'ensime-goto-test--is-test-dir))

      (spacemacs/register-repl 'ensime #'ensime-inf-switch "ensime")

      ;; Never ever let ensime check the whole project
      (define-key ensime-mode-map (kbd "C-c C-c a") 'ensime-show-all-errors-and-warnings)

      ;; HACK: Fix errors with ensime eldoc function.
      (with-eval-after-load 'ensime-inspector
        (defun ensime-type-at-point (&optional arg)
          "Echo the type at point to the minibuffer.
      A prefix argument will add the type to the kill ring."
          (interactive "P")
          (let* ((type (ensime-rpc-get-type-at-point))
                 (fullname (ensime-type-full-name-with-args type)))
            (when arg
              (kill-new fullname))
            (message fullname))))
      ))

  (use-package ensime-expand-region
    :after ensime)

  (use-package ensime-company
    :after ensime
    :config
    (progn
      ;; HACK: Reset company idle delay.
      ;; (advice-add 'ensime-company-enable :after #'scala/set-company-variables)

      ;; HACK: Prevent ensime from clobbering company settings.
      (with-eval-after-load 'ensime-company
        (defun ensime-company-enable ()
          (set (make-local-variable 'company-backends) '(ensime-company))
          (company-mode)
          (yas-minor-mode-on)
          (set (make-local-variable 'company-idle-delay) 0))))))

(defun zc-scala/init-ensime-diminished-modeline ()
  (use-package ensime-diminished-modeline
    :after ensime))

(defun zc-scala/post-init-indent-dwim ()
  (use-package indent-dwim
    :after ensime
    :config
    (add-to-list 'indent-dwim-commands-alist '(scala-mode . ensime-format-source))))
