;; Unify auto-save files
(defvar spacemacs-autosaves-directory
  (concat user-emacs-directory "autosaves/"))

;; Turn on delete selection mode
(delete-selection-mode 1)


;; ----------------------------------------------------------------------------

;; Resolve HELM find file slow issue
;;
;; NOTE: If projectile cache gets out of sync, running 'projectile-find-file'
;;       again with a prefix argument (SPC u) or (C-u) will refresh the cache.
;; NOTE: Disabled since switch from Helm to Ivy.
;; (setq projectile-enable-caching t)
