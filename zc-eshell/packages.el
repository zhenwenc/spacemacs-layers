;;; packages.el --- Eshell packages

(defconst zc-eshell-packages
  '(term
    (eshell :location built-in)
    (eshell-prompt-extras :excluded t)))

(defconst zc-eshell/prompt-regexp
  (rx bol (or
           ;; default
           (and (* space) "λ" (or ">" "#") " ")
           ;; docker
           (and "@" (+ nonl) ":" (+ nonl) "$" space))))

;; Disable company for eshell.
(with-eval-after-load 'company
  (setq company-global-modes '(not eshell-mode)))

(defun zc-eshell/post-init-eshell ()
  (use-package eshell
    :defer t
    :init
    (progn
      (setq eshell-buffer-shorthand t)
      (setq eshell-cmpl-ignore-case t)
      (setq eshell-history-size 10000)
      (setq eshell-hist-ignoredups t)
      (setq eshell-error-if-no-glob t)
      (setq eshell-glob-case-insensitive nil)
      (setq eshell-scroll-to-bottom-on-input t)
      (evil-set-initial-state 'eshell-mode 'insert)

      (spacemacs/set-leader-keys "\"" 'zc-eshell/bring)

      (defun pcomplete/sudo ()
        (let ((prec (pcomplete-arg 'last -1)))
          (cond ((string= "sudo" prec)
                 (while (pcomplete-here*
                         (funcall pcomplete-command-completion-function)
                         (pcomplete-arg 'last) t))))))

      (defun zc-eshell/prompt-function ()
        (let ((root-user? (= (user-uid) 0))
              (color "#268bd2"))
          (concat
           (propertize "λ" 'face `(:foreground ,color))
           (propertize (if root-user? "#" ">") 'face `(:foreground ,color))
           " ")))

      (setq eshell-prompt-function 'zc-eshell/prompt-function)
      (setq eshell-prompt-regexp zc-eshell/prompt-regexp)

      (add-hook 'eshell-mode-hook 'zc-eshell/setup)
      (add-hook 'eshell-mode-hook 'smartparens-strict-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
        "ib" 'eshell-insert-buffer-name
        "ii" 'eshell-insert-process
        "iv" 'eshell-insert-envvar)

      ;; These don't work well in normal state
      ;; due to evil/emacs cursor incompatibility
      (evil-define-key 'insert eshell-mode-map
        (kbd "C-p") 'eshell-previous-matching-input-from-input
        (kbd "C-n") 'eshell-next-matching-input-from-input)
      )))
