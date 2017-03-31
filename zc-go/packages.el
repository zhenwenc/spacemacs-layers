;;; packages.el --- Go Layer packages File for My Spacemacs

(defconst zc-go-packages
  '(go-mode))

(defun fk-hacky-set-go-keybindings ()
  (define-key go-mode-map (kbd "<f5>") nil)
  (define-key go-mode-map (kbd "<f6>") nil))

(add-hook 'go-mode-hook #'fk-hacky-set-go-keybindings)

(defun zc-go/post-init-go-mode ()
  (use-package go-mode
    :defer t
    :config
    (progn
      (use-package go-oracle
    :after go-mode
    :load-path "~/gocode/src/golang.org/x/tools/cmd/oracle/"
    :config
    (progn
      (define-key go-mode-map (kbd "<f5>") nil)
      (define-key go-mode-map (kbd "<f6>") nil)))
      ;; Don't show line number: performance sucks!
      (add-hook 'go-mode-hook (lambda () (linum-mode nil))))))
