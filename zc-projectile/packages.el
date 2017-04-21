;;; packages.el --- zc-projectile layer packages file for Spacemacs.
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

(defconst zc-projectile-packages
  '(projectile))

(defun zc-projectile/post-init-projectile ()
  (use-package projectile
    :init
    (counsel-projectile-on)

    :config
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)
      (setq projectile-cache-file (concat zc-cache-directory "/projectile"))
      (setq projectile-enable-caching t)

      (dolist (f '("TAGS" ".DS_Store"))
        (add-to-list 'projectile-globally-ignored-files f))

      (dolist (s '("gz" "zip" "tar" "elc"))
        (add-to-list 'projectile-globally-ignored-file-suffixes s))

      (dolist (d '(".ensime_cache"
                     ".git"
                     "build"
                     "dist"
                     "node_modules"
                     "target"
                     "vendor"))
        (add-to-list 'projectile-globally-ignored-directories d))
      )))

;;; packages.el ends here
