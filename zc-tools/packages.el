;;; packages.el --- zc-tools layer packages file for Spacemacs.
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

(defconst zc-tools-packages
  '((kubernetes :location local)))

(defun zc-tools/init-kubernetes ()
  (use-package kubernetes
    :commands (kubernetes-display-pods)
    :config
    (use-package kubernetes-evil :after evil)))

;;; packages.el ends here
