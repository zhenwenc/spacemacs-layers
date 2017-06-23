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
        "hd" 'tide-documentation-at-point
        "rr" 'tide-rename-symbol
        "ns" 'tide-restart-server)

      (spacemacs/set-leader-keys-for-major-mode 'zc-web-js-mode
        "gu" 'tide-references
        "hd" 'tide-documentation-at-point
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


;; Typescript

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


;; Javascript

(defun zc-typescript/post-init-web-mode ()
  (use-package web-mode
    :defines (web-mode-markup-indent-offset
              web-mode-css-indent-offset)
    :defer t
    :preface (autoload 'sp-local-pair "smartparens")
    :config
    (progn
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-enable-auto-quoting nil)

      ;; Disable web-mode-reload binding
      (define-key web-mode-map (kbd "C-c C-r") nil)

      ;; Use line comments when commenting in JS.
      (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

      ;; Change default indentation behaviour.
      (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
      (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
      (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

      ;; Treat es6 files as JS files.
      (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
      (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'")))))

(defun zc-typescript/init-zc-web-modes ()
  (use-package zc-web-modes
    :defer t
    :mode (("\\.es6\\'"  . zc-web-js-mode)
           ("\\.jsx?\\'" . zc-web-js-mode))
    :defines (flycheck-html-tidy-executable)
    :config
    (with-eval-after-load 'flycheck
      (let ((tidy-bin "/usr/local/bin/tidy"))
        (when (file-exists-p tidy-bin)
          (setq flycheck-html-tidy-executable tidy-bin)))

      (flycheck-add-mode 'javascript-eslint 'zc-web-js-mode))))

(defun zc-typescript/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :preface
    (defun zc-typescript/in-flow-strict-object-type-p ()
      (when (derived-mode-p 'zc-web-js-mode)
        (-let [(depth start) (syntax-ppss)]
          (and (plusp depth)
               (eq (char-after start) ?{)
               (eq (char-after (1+ start)) ?|)))))
    :config
    (progn
      (add-to-list 'aggressive-indent-dont-indent-if '(zc-typescript/in-flow-strict-object-type-p))
      (add-hook 'aggressive-indent-stop-here-hook #'zc-typescript/in-flow-strict-object-type-p))))

;; (defun zc-typescript/init-js2-mode ()
;;   (use-package js2-mode
;;     :defer t
;;     :mode (("\\.js\\'" . js2-mode)
;;            ("\\.jsx\\'" . js2-jsx-mode))
;;     :init
;;     ;; Required to make imenu functions work correctly
;;     (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
;;     :config
;;     (progn
;;       (dolist (prefix '(("mh" . "documentation")
;;                         ("mg" . "goto")
;;                         ("mr" . "refactor")
;;                         ("mz" . "floding")))
;;         (spacemacs/declare-prefix-for-mode 'js2-mode (car prefix) (cdr prefix)))

;;       (spacemacs/set-leader-keys-for-major-mode 'js2-mode
;;         "ee" 'zc-typescript/print-error-at-point
;;         "tw" 'js2-mode-toggle-warnings-and-errors
;;         "zc" 'js2-mode-hide-element
;;         "zo" 'js2-mode-show-element
;;         "zr" 'js2-mode-show-all
;;         "ze" 'js2-mode-toggle-element
;;         "zF" 'js2-mode-toggle-hide-functions
;;         "zC" 'js2-mode-toggle-hide-comments))

;;     ;; Override the default styles
;;     (setq js2-basic-offset 2)
;;     (setq js2-strict-trailing-comma-warning nil)
;;     (setq js2-mode-show-parse-errors t)
;;     (setq js2-mode-show-strict-warnings t)

;;     ;; Override the ugly face colors
;;     (set-face-foreground 'js2-function-call nil)
;;     (set-face-foreground 'js2-object-property nil)
;;     (set-face-underline 'js2-warning '(:color "#d0bf8f" :style wave))))



;; (defun zc-typescript/post-init-web-mode ()
;;   ;; HACK: Delete web-mode auto-mode config set by Spacemacs so that I can use
;;   ;; specialised derived modes instead.
;;   (setq auto-mode-alist
;;         (-remove (-lambda ((_ . mode))
;;                    (equal 'web-mode mode))
;;                  auto-mode-alist))

;;   (use-package web-mode
;;     :defer t
;;     :config
;;     (progn
;;       (remove-hook 'web-mode-hook #'spacemacs/toggle-smartparens-off)

;;       (setq web-mode-enable-current-element-highlight t)
;;       (setq web-mode-enable-auto-pairing nil)

;;       ;; Use 2 spaces for indentation
;;       (defun zc-typescript/set-local-vars ()
;;         (setq-local web-mode-enable-auto-quoting nil)
;;         (setq web-mode-markup-indent-offset 2)
;;         (setq web-mode-css-indent-offset 2)
;;         (setq web-mode-code-indent-offset 2))

;;       (add-hook 'web-mode-hook #'zc-typescript/set-local-vars))))

;;; packages.el ends here
