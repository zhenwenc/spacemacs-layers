;;; packages.el --- zc-html layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst zc-html-packages
  '(scss-mode))

(defun zc-html/post-init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode))
  :config (progn
            ;; Use 2 spaces for indentation
            (setq css-indent-offset 2)))

;;; packages.el ends here
