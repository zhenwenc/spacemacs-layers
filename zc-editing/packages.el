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
  '(vil-mc))

(defun zc-editing/post-init-evil-mc ()
  (use-package evil-mc
    :defer t
    :config
    (progn
      (evil-define-key 'normal map (kbd "C-p") nil))))

;;; packages.el ends here
