;;; packages.el --- zc-web layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst zc-web-packages
  '(aggressive-indent
    company
    eldoc
    flycheck
    tide
    typescript-mode
    web-mode
    (zc-web-modes :location local)))


;; Tide

(defun zc-web/init-tide ()
  (use-package tide
    :defer t
    :commands (zc-web/jump-to-type-def)
    :init
    (progn
      (add-hook 'zc-web-js-mode-hook 'tide-setup)
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

      (spacemacs/set-leader-keys-for-major-mode 'zc-web-js-mode
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
        (kbd "M-.") 'zc-web/jump-to-type-def
        (kbd "M-,") 'tide-jump-back)
      )))


;; Typescript

(defun zc-web/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config
    (progn
      (setq typescript-indent-level 2)
      (when typescript-fmt-on-save
        (add-hook 'typescript-mode-hook 'zc-web/fmt-before-save-hook))
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "rf"  'zc-web/format))))


;; Javascript

(defun zc-web/init-zc-web-modes ()
  (use-package zc-web-modes
    :defer t
    :mode (("\\.es6\\'"  . zc-web-js-mode)
           ("\\.jsx?\\'" . zc-web-js-mode))))

(defun zc-web/post-init-web-mode ()
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



(defun zc-web/post-init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :preface
    (defun zc-web/in-flow-strict-object-type-p ()
      (when (derived-mode-p 'zc-web-js-mode)
        (-let [(depth start) (syntax-ppss)]
          (and (plusp depth)
               (eq (char-after start) ?{)
               (eq (char-after (1+ start)) ?|)))))
    :config
    (progn
      (add-to-list 'aggressive-indent-dont-indent-if '(zc-web/in-flow-strict-object-type-p))
      (add-hook 'aggressive-indent-stop-here-hook #'zc-web/in-flow-strict-object-type-p))))

(defun zc-web/post-init-company ()
  (when (configuration-layer/package-usedp 'tide)
    (spacemacs|add-company-backends
      :backends company-tide
      :modes typescript-mode zc-web-js-mode)))

(defun zc-web/post-init-eldoc ()
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (add-hook 'zc-web-js-mode-hook 'eldoc-mode))

(defun zc-web/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :commands (flycheck-select-checker)
    :functions (flycheck-add-next-checker flycheck-add-mode)
    :defines (flycheck-html-tidy-executable)
    :preface
    (progn
      (autoload 'projectile-project-p "projectile")
      (autoload 'f-join "f")

      (defun zc-web/add-node-modules-bin-to-path ()
        "Use binaries from node_modules, where available."
        (when-let (root (projectile-project-p))
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

      (with-eval-after-load 'flycheck
        (let ((tidy-bin "/usr/local/bin/tidy"))
          (when (file-exists-p tidy-bin)
            (setq flycheck-html-tidy-executable tidy-bin)))

        (flycheck-add-mode 'javascript-eslint 'zc-web-js-mode)
        ))
    :init
    (progn
      (spacemacs/enable-flycheck 'typescript-mode)
      (spacemacs/enable-flycheck 'zc-web-js-mode))
    :config
    (progn
      (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
      (add-hook 'zc-web-js-mode-hook #'zc-web/add-node-modules-bin-to-path))))

;;; packages.el ends here
