(defvar spacemacs-autosaves-directory
  (concat user-emacs-directory "autosaves/"))

(defconst zc-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst zc-secret-sources
  '((zc-db-secrets . "~/dotfiles/secret/db-secrets.el.gpg")))

;; ----------------------------------------------------------------------------

;; Resolve HELM find file slow issue
;;
;; NOTE: If projectile cache gets out of sync, running 'projectile-find-file'
;;       again with a prefix argument (SPC u) or (C-u) will refresh the cache.
;; NOTE: Disabled since switch from Helm to Ivy.
;; (setq projectile-enable-caching t)
