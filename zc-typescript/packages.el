;;; packages.el --- zc-typescript layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst zc-typescript-packages
  '(tide
    company
    flycheck
    js2-mode
    web-mode
    typescript-mode))

(defun zc-typescript/post-init-company ()
  (when (configuration-layer/package-usedp 'tide)
    (spacemacs|add-company-backends
      :backends company-tide
      :modes typescript-mode)))

(defun zc-typescript/post-init-eldoc ()
  (add-hook 'typescript-mode-hook 'eldoc-mode))

(defun zc-typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode))

(defun zc-typescript/init-tide ()
  (use-package tide
    :defer t
    :commands (zc-typescript/jump-to-type-def)
    :init
    (progn
      (add-hook 'js2-mode-hook 'tide-setup)
      (add-hook 'typescript-mode-hook 'tide-setup)
      (add-to-list 'spacemacs-jump-handlers-typescript-mode 'tide-jump-to-definition)

      (with-eval-after-load 'tide

        (defun tide-doc-buffer (string)
          (switch-to-buffer-other-window "*tide-documentation*")
          (setq buffer-read-only t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (when string
              (save-excursion
                (insert string))))
          (evil-local-set-key 'normal (kbd "q") #'quit-window))

        ) ;; -- End override tide functions

      (dolist (prefix `(("mg" . "goto")
                        ("mh" . "help")
                        ("mn" . "name")
                        ("mr" . "rename")
                        ("mS" . "server")
                        ("ms" . "send")))
        (spacemacs/declare-prefix-for-mode 'typescript-mode (car prefix) (cdr prefix))))

    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "gu" 'tide-references
        "hh" 'tide-documentation-at-point
        "rr" 'tide-rename-symbol
        "ns" 'tide-restart-server)

      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gu" 'tide-references
        "hh" 'tide-documentation-at-point
        "rr" 'tide-rename-symbol
        "ns" 'tide-restart-server)

      (evilified-state-evilify tide-references-mode tide-references-mode-map
        (kbd "p")   'tide-find-previous-reference
        (kbd "n")   'tide-find-next-reference
        (kbd "g")   'tide-goto-reference
        (kbd "q")   'quit-window

        (kbd "C-k") 'tide-find-previous-reference
        (kbd "C-j") 'tide-find-next-reference
        (kbd "C-l") 'tide-goto-reference)

      (evil-define-key 'insert tide-mode-map
        (kbd "M-.") 'tide-jump-to-definition
        (kbd "M-,") 'tide-jump-back)

      (evil-define-key 'normal tide-mode-map
        (kbd "M-.") 'zc-typescript/jump-to-type-def
        (kbd "M-,") 'tide-jump-back)
      )))



(defun zc-typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config
    (progn
      (setq typescript-indent-level 2)
      (when typescript-fmt-on-save
        (add-hook 'typescript-mode-hook 'zc-typescript/fmt-before-save-hook))
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "rf"  'zc-typescript/format))))



(defun zc-typescript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode))
    :init
    ;; Required to make imenu functions work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    :config
    (progn
      ;; (setq js2-basic-offset 2)

      (dolist (prefix '(("mh" . "documentation")
                        ("mg" . "goto")
                        ("mr" . "refactor")
                        ("mz" . "floding")))
        (spacemacs/declare-prefix-for-mode 'js2-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "ee" 'zc-typescript/print-error-at-point
        "tw" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))

    ;; Override the ugly face colors
    (set-face-foreground 'js2-function-call nil)
    (set-face-foreground 'js2-object-property nil)
    (set-face-underline 'js2-warning '(:color "#d0bf8f" :style wave))))



(defun zc-typescript/post-init-web-mode ()
  ;; HACK: Delete web-mode auto-mode config set by Spacemacs so that I can use
  ;; specialised derived modes instead.
  (setq auto-mode-alist
        (-remove (-lambda ((_ . mode))
                   (equal 'web-mode mode))
                 auto-mode-alist))

  (use-package web-mode
    :defer t
    :config
    (progn
      (remove-hook 'web-mode-hook #'spacemacs/toggle-smartparens-off)

      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-auto-pairing nil)

      ;; Use 2 spaces for indentation
      (defun zc-typescript/set-local-vars ()
        (setq-local web-mode-enable-auto-quoting nil)
        (setq web-mode-markup-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-code-indent-offset 2))

      (add-hook 'web-mode-hook #'zc-typescript/set-local-vars))))

;;; packages.el ends here
