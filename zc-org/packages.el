;;; packages.el --- zc-org layer packages file for Spacemacs.
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

(defconst zc-org-packages
  '(org
    plantuml-mode))

(defvar zc-org/plantuml-jar-path
  (expand-file-name "~/code/tools/scripts/plantuml.jar"))

(defun zc-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (progn
      (setq org-plantuml-jar-path zc-org/plantuml-jar-path)

      ;; (spacemacs|use-package-add-hook org
      ;;   :post-config
      ;;   (add-to-list 'org-babel-load-languages '(plantuml . t)))
      )
    :config
    (add-to-list 'org-babel-load-languages '(plantuml . t))
    ))

(defun zc-org/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :mode ("\\.pum\\'" . plantuml-mode)
    :init
    (setq plantuml-jar-path zc-org/plantuml-jar-path)
    :config (spacemacs/set-leader-keys-for-major-mode 'plantuml-mode
              "cc" 'plantuml-preview
              "co" 'plantuml-set-output-type)))

;;; packages.el ends here
