;;; packages.el --- Scala Layer packages File for Spacemacs

(eval-when-compile
  (require 'use-package nil t))

(defconst zc-scala-packages
  '(aggressive-indent
    ensime
    flycheck
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
    (spacemacs/enable-flycheck 'scala-mode)
    :config
    (progn
      (setq flycheck-scalastylerc "~/.scalastyle.xml")

      (defun zc-scala/disable-flycheck-scala ()
        (when (boundp 'flycheck-disabled-checkers)
          (push 'scala flycheck-disabled-checkers)))
      (add-hook 'ensime-mode-hook #'zc-scala/disable-flycheck-scala))))

(defun zc-scala/init-scala-mode ()
  (use-package scala-mode
    :defer t
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
         ((t (:foreground, (with-no-warnings cb-vars-solarized-hl-orange) :underline nil))))))

    ))

(defun zc-scala/pre-init-ensime ()
  (spacemacs|use-package-add-hook ensime
        :pre-config (add-to-list 'zc-ensime--ensime-modes 'scala-mode)))

(defun zc-scala/post-init-ensime ()
  (use-package ensime
    :defer t
    :init
    (progn
      (add-hook 'scala-mode-hook #'zc-ensime/setup-ensime)

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
        (spacemacs/declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix))))
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

      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
        "yT"     'scala/yank-type-at-point-full-name
        "yt"     'scala/yank-type-at-point)
      ))
  )

(defun zc-scala/post-init-indent-dwim ()
  (use-package indent-dwim
    :after ensime
    :config
    (add-to-list 'indent-dwim-commands-alist '(scala-mode . ensime-format-source))))
