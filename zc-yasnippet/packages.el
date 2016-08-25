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
  (f-join root-layers-directory "zc-yasnippet/snippets"))

(defun zc-yasnippet/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (add-hook 'prog-mode-hook (lambda () (require 'yasnippet)))
    :config
    (progn
      (setq yas-snippet-dirs zc-yasnippet/main-snippets-dir)

      (bind-key (kbd "TAB") #'yas-expand prog-mode-map)
      (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)
      (bind-key "<backspace>" 'yas/backspace yas-keymap)
      (evil-define-key 'insert yas-keymap (kbd "SPC") #'yas/space)

      (yas/reload-all)
      (yas-global-mode +1)
      )))

;;; packages.el ends here
