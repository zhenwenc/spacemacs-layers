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
  '((evil-mc :excluded t)
    company
    iedit))

(defun zc-editing/post-init-evil-mc ()
  (use-package evil-mc
    :defer t
    :init
    ;; Enable evil multiple-cursors.
    ;; https://github.com/gabesoft/evil-mc
    (global-evil-mc-mode)
    :config
    (progn
      ;; HACK: clear all cursors when escape/save
      (advice add 'zc-core/evil-escape :after #'evil-mc-undo-all-cursors)

      (evil-define-key 'normal map (kbd "C-p") nil))))

(defun zc-editing/post-init-company ()
  (use-package company
    :commands company-mode
    :init
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-tooltip-align-annotations t
          company-dabbrev-ignore-case nil
          company-dabbrev-code-ignore-case nil
          company-dabbrev-downcase nil)))

(defun zc-editing/init-iedit ()
  (use-package iedit
    :commands iedit-mode
    :config
    (set-face-background 'iedit-occurrence "PaleVioletRed4")))

;;; packages.el ends here
