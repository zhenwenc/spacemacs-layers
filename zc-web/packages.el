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

;; Flycheck:
;; * inspect flycheck config, e.g. check if config is valid:
;; - SPC e v [flycheck-verify-setup]
;; * verify eslint config:
;; - ./node_modules/.bin/eslint --print-config .

(defconst zc-web-packages
  '(aggressive-indent
    emmet-mode
    eldoc
    flycheck
    tide
    web-mode
    (prettier-js :location local)
    (zc-web-modes :location local)))


;; Tide

(defun zc-web/init-tide ()
  (use-package tide
    :defer t
    :diminish "Ⓣ"
    :init
    (progn
      (add-hook 'zc-web-js-mode-hook 'tide-setup)
      (add-hook 'zc-web-ts-mode-hook 'tide-setup)
      (add-to-list 'spacemacs-jump-handlers-typescript-mode
                   '(tide-jump-to-definition :async t))

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

        (defun tide-completion-doc-buffer (name) "Bye!") ;; FIXME

        ) ;; -- End override tide functions

      (dolist (prefix `(("mg" . "goto")
                        ("mh" . "help")
                        ("mn" . "server")
                        ("mr" . "refactor")
                        ("ms" . "send")))
        (spacemacs/declare-prefix-for-mode 'zc-web-ts-mode (car prefix) (cdr prefix))))

    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'zc-web-ts-mode
        "gg" 'tide-jump-to-definition
        "gi" 'tide-jump-to-implementation
        "gj" 'counsel-imenu
        "hu" 'tide-references
        "hh" 'tide-documentation-at-point
        "rr" 'tide-rename-symbol
        "ns" 'tide-restart-server)

      (spacemacs/set-leader-keys-for-major-mode 'zc-web-js-mode
        "gg" 'tide-jump-to-definition
        "gi" 'tide-jump-to-implementation
        "hu" 'tide-references
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
        (kbd "M-.") 'tide-jump-to-definition
        (kbd "M-,") 'tide-jump-back)
      )))


;; Typescript / Javascript

(defun zc-web/init-zc-web-modes ()
  (use-package zc-web-modes
    :defer t
    :mode (("\\.json\\'" . zc-web-json-mode)
           ("\\.eslintrc\\'" . zc-web-json-mode)
           ("\\.babelrc\\'" . zc-web-json-mode)
           ("\\.es6\\'"  . zc-web-js-mode)
           ("\\.jsx?\\'" . zc-web-js-mode)
           ("\\.tsx?\\'"  . zc-web-ts-mode)
           ("\\.css\\'"  . zc-web-css-mode)
           ("\\.scss\\'"  . zc-web-css-mode)
           ("\\.html\\'" . zc-web-html-mode))
    :defines (flycheck-html-tidy-executable)
    :config
    (remove-hook 'web-mode-hook #'spacemacs/toggle-smartparens-off)))

(defun zc-web/init-web-mode ()
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
      (setq typescript-indent-level 2)

      (add-hook 'zc-web-js-mode-hook
                #'(lambda ()
                    ;; Force indentation for JSX
                    (setq web-mode-attr-indent-offset 2)))

      (add-hook 'zc-web-css-mode-hook
                #'(lambda ()
                    (set (make-local-variable 'company-backends) '(company-css))
                    (company-mode)))

      (add-hook 'zc-web-ts-mode-hook
                #'(lambda ()
                    (set (make-local-variable 'company-backends) '(company-tide))
                    (company-mode)))

      (add-hook 'zc-web-js-mode-hook
                #'(lambda ()
                    (set (make-local-variable 'company-backends) '(company-tide))
                    (company-mode)))

      ;; Disable web-mode-reload binding
      (define-key web-mode-map (kbd "C-c C-r") nil)

      ;; Use line comments when commenting in JS
      (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

      ;; Change default indentation behaviour
      (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
      (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
      (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

      ;; Treat es6 files as JS files
      (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
      (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
      (add-to-list 'web-mode-content-types '("tsx" . "\\.tsx\\'"))

      ;; Enhance smartparens
      (sp-with-modes '(zc-web-ts-mode)
        (sp-local-pair "<" ">"))
      (sp-with-modes '(zc-web-ts-mode zc-web-js-mode)
        (sp-local-pair "/**" "*/" :post-handlers '(newline-and-indent)))

      )))


;; HTML

(defun zc-web/init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :defines (emmet-expand-jsx-className?)
    :commands (emmet-mode emmet-expand-line)
    :init
    (progn
      (add-hook 'web-mode-hook #'zc-web/emmet-mode-maybe)
      (add-hook 'zc-web-ts-mode-hook #'zc-web/emmet-mode-maybe))
    :preface
    (defun zc-web/emmet-enable-expand-jsx-classname ()
      (setq-local emmet-expand-jsx-className? t))
    :config
    (progn
      (evil-define-key 'insert emmet-mode-keymap (kbd "M-e") 'zc-web/emmet-expand)

      ;; TODO Fix key conflicts with emmet
      (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'yas-expand)
      (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'yas-expand)

      (spacemacs|hide-lighter emmet-mode)
      (add-hook 'zc-web-js-mode-hook #'zc-web/emmet-enable-expand-jsx-classname)
      (add-hook 'zc-web-ts-mode-hook #'zc-web/emmet-enable-expand-jsx-classname))))



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

(defun zc-web/post-init-eldoc ()
  (add-hook 'zc-web-ts-mode-hook 'eldoc-mode)
  (add-hook 'zc-web-js-mode-hook 'eldoc-mode))

(defun zc-web/init-prettier-js ()
  (use-package prettier-js
    :after zc-web-modes
    :commands (prettier-js prettier-js-mode)
    :diminish "Ⓟ"
    :init
    ;; Format typescript file on save
    (when zc-web-fmt-on-save
      (add-hook 'zc-web-js-mode-hook 'prettier-js-mode)
      (add-hook 'zc-web-ts-mode-hook 'prettier-js-mode))
    :config
    (progn
      (setq prettier-js-args '("--single-quote"
                               "--trailing-comma" "es5"))
      (spacemacs/set-leader-keys-for-major-mode 'zc-web-js-mode
        "rf" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'zc-web-ts-mode
        "rf" 'prettier-js))))

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

      (with-eval-after-load 'flycheck
        (let ((tidy-bin "/usr/local/bin/tidy"))
          (when (file-exists-p tidy-bin)
            (setq flycheck-html-tidy-executable tidy-bin)))

        (with-eval-after-load 'tide
          (flycheck-add-mode 'typescript-tide 'zc-web-ts-mode))

        (flycheck-add-mode 'javascript-eslint 'zc-web-js-mode)
        (flycheck-add-mode 'css-csslint 'zc-web-css-mode)
        (flycheck-add-mode 'json-jsonlint 'zc-web-json-mode)
        (flycheck-add-mode 'html-tidy 'zc-web-html-mode)))
    :init
    (progn
      (spacemacs/add-flycheck-hook 'zc-web-ts-mode)
      (spacemacs/add-flycheck-hook 'zc-web-js-mode))
    :config
    (progn
      (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
      (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
      (add-to-list 'flycheck-disabled-checkers 'css-csslint)

      (add-hook 'zc-web-js-mode-hook #'zc-web/add-node-modules-bin-to-path)
      (add-hook 'zc-web-ts-mode-hook #'zc-web/add-node-modules-bin-to-path)
      (add-hook 'zc-web-css-mode-hook #'zc-web/add-node-modules-bin-to-path))))

;;; packages.el ends here
