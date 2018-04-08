;;; packages.el --- zc-graphql layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Frederick <fredc@Fredericks-MacBook-Pro-2.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst zc-graphql-packages
  '(graphql-mode))

(defun zc-graphql/init-graphql-mode ()
  (use-package graphql-mode))

;;; packages.el ends here
