;;; packages.el --- zc-yasnippet layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst zc-yasnippet-packages
  '(yasnippet))

(defvar zc-yasnippet/main-snippets-dir
  (f-join user-layers-directory "zc-yasnippet/snippets"))

(defun zc-yasnippet/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (add-hook 'prog-mode-hook (lambda () (require 'yasnippet)))
    :config
    (progn
      (setq yas-snippet-dirs zc-yasnippet/main-snippets-dir)

      ;; This advice could be added to other functions that usually insert
      ;; balanced parens, like `try-expand-list'.
      (advice-add 'yas-hippie-try-expand
                  :after-while #'zc-yasnippet/disable-sp-hippie-advice)

      (bind-key (kbd "TAB") #'yas-expand prog-mode-map)
      (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)

      (yas/reload-all)
      (yas-global-mode +1)
      )))

;;; packages.el ends here
