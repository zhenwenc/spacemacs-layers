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
    web-mode
    typescript-mode))

(defun zc-typescript/post-init-tide ()
  (use-package tide
    :defer t
    :init (progn
            (evilified-state-evilify tide-references-mode tide-references-mode-map
              (kbd "p") 'tide-find-previous-reference
              (kbd "n") 'tide-find-next-reference
              (kbd "g") 'tide-goto-reference
              (kbd "q") 'quit-window))
    :evil-bind
    (:map
     tide-mode-map
     :state normal
     ("M-." . tide-jump-to-definition)
     ("M-," . tide-jump-back)
     :state insert
     ("M-." . tide-jump-to-definition)
     ("M-," . tide-jump-back))))

(defun zc-typescript/post-init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config (progn
              (setq typescript-indent-level 2))))

;; Copy from: https://github.com/chrisbarrett/spacemacs-layers/blob/master/cb-js/packages.el#L157
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
