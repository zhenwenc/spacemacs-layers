;;; packages.el --- zc-editing layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst zc-editing-packages
  '(evil-mc
    company))

(defun zc-editing/post-init-evil-mc ()
  (use-package evil-mc
    :defer t
    :config
    (progn
      (evil-define-key 'normal map (kbd "C-p") nil))))

(defun zc-editing/post-init-company ()
  (use-package company
    :commands company-mode
    :init
    (setq
     company-dabbrev-ignore-case nil
     company-dabbrev-code-ignore-case nil
     company-dabbrev-downcase nil
     company-idle-delay 0
     company-minimum-prefix-length 4)
    :config
    ;; (define-key company-active-map (kbd "<tab>") nil)
    (dolist (map (list company-active-map company-search-map company-filter-map))
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous)
      (define-key map (kbd "C-h") 'company-show-doc-buffer)
      (define-key map (kbd "C-w") nil)
      (define-key map (kbd "TAB") nil)
      (define-key map [tab] nil))))

;;; packages.el ends here
