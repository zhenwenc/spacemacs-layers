;;; packages.el --- TypeScript Layer packages File for My Spacemacs

;; (defun zc-ts/postinit-typescript-mode ()
;;   (setq typescript-indent-level 2))

(defun zc-ts/post-init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :commands (typescript/format-buffer)
    :config
    (progn
      (setq typescript-indent-level 2))))

