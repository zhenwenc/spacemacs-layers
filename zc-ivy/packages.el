;;; packages.el --- zc-ivy layer packages file for Spacemacs.
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

(defconst zc-ivy-packages
  '(ivy
    counsel
    swiper))

(defun zc-ivy/post-init-ivy ()
  (use-package ivy
    :config
    (progn
      ;; Unset the fucking key binding
      (global-set-key (kbd "<f6>") #'eyebrowse-switch-to-window-config-2)

      ;; To match ISearch behaviour
      (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

      ;; Prevent minibuffer close
      (set-variable 'ivy-on-del-error-function '(lambda()))

      ;; Fuzzy matching result sorting
      ;; http://oremacs.com/2016/01/06/ivy-flx/
      (setq ivy-re-builders-alist '((t . ivy--regex-plus))))))

(defun zc-ivy/post-init-counsel ()
  (use-package counsel
    :config
    (progn
      ;; Display separator in kill-ring buffer
      (setq counsel-yank-pop-separator (concat "\n" (make-string 30 ?-) "\n"))
      )))

(defun zc-ivy/post-init-swiper ()
  (use-package swiper
    :config
    (progn
      (add-to-list 'swiper-font-lock-exclude 'scala-mode))))

;;; packages.el ends here
