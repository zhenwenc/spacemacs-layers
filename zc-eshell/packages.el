;;; packages.el --- Eshell packages

(eval-when-compile
  (require 'use-package nil t)
  (require 'f nil t))

(defconst zc-eshell-packages
  '('(eshell-prompt-extras :excluded t)))

;; Disable company for eshell.
(with-eval-after-load 'company
  (setq company-global-modes '(not eshell-mode)))

(defun zc-eshell/post-init-eshell ()
  ;; (setq eshell-directory-name (f-join user-dropbox-directory "emacs/eshell/"))
  (global-set-key (kbd "<f1>") 'zc-eshell-bring)

  (setq eshell-buffer-shorthand t)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ignoredups t)
  (setq eshell-error-if-no-glob t)
  (setq eshell-glob-case-insensitive nil)
  (setq eshell-scroll-to-bottom-on-input t)
  (setq eshell-prompt-function 'zc-eshell--prompt)

  ;; See ./funcs.el for definition.
  (setq eshell-prompt-regexp (rx bol (or
                                      ;; default
                                      (and (* space) "λ" (or ">" "#") " ")
                                      ;; boot2docker
                                      (and "@" (+ nonl) ":" (+ nonl) "$" space))))

  (evil-set-initial-state 'eshell-mode 'insert)

  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "ib" 'eshell-insert-buffer-name)
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "ii" 'eshell-insert-process)
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "iv" 'eshell-insert-envvar)

  (defun zc-eshell/setup ()
    (vi-tilde-fringe-mode -1)
    (local-set-key (kbd "C-c RET") 'eshell-toggle-direct-send))

  (add-hook 'eshell-mode-hook 'zc-eshell/setup)
  (add-hook 'eshell-mode-hook 'smartparens-strict-mode)

  (defun pcomplete/sudo ()
    (let ((prec (pcomplete-arg 'last -1)))
      (cond ((string= "sudo" prec)
             (while (pcomplete-here*
                     (funcall pcomplete-command-completion-function)
                     (pcomplete-arg 'last) t)))))))
