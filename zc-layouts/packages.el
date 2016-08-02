;;; packages.el --- zc-layouts layer packages file for Spacemacs.
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

(defconst zc-layouts-packages
  '(eyebrowse))

(defun zc-layouts/post-init-eyebrowse ()
  (bind-key (kbd "<f5>") #'eyebrowse-switch-to-window-config-1)
  (bind-key (kbd "<f6>") #'eyebrowse-switch-to-window-config-2)
  (bind-key (kbd "<f7>") #'eyebrowse-switch-to-window-config-3)
  (bind-key (kbd "<f8>") #'eyebrowse-switch-to-window-config-4))

;;; packages.el ends here
