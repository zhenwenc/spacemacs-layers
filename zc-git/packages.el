;;; packages.el --- zc-git layer packages file for Spacemacs.
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

(defconst zc-git-packages
  '((git-commit-jira-prefix :location local)))

(defun zc-git/init-git-commit-jira-prefix ()
  (use-package git-commit-jira-prefix
    :after git-commit
    :commands git-commit-jira-prefix-init
    :config (git-commit-jira-prefix-init)))

;;; packages.el ends here
