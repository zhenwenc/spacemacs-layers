;;; packages.el --- JavaScript Layer packages File for My Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst zc-js-packages
  '(js2-mode
    js))

(defun zc-js/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.json\\'" . js2-mode))
    :init
    (progn
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
      (defalias 'json-mode 'js2-mode))
    :config
    (progn
      (setq js2-basic-offset 2)
      ;; Use flycheck for checking.
      (setq js2-mode-show-parse-errors nil)
      (setq js2-mode-show-strict-warnings nil))))

(defun zc-js/post-init-js ()
  (use-package js
    :defer t
    :config
    (setq js-indent-level 2)))
