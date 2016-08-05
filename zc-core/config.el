;; Unify auto-save files
(defvar spacemacs-autosaves-directory
  (concat user-emacs-directory "autosaves/"))

;; Turn on delete selection mode
(delete-selection-mode 1)

;; Company

(with-eval-after-load 'company
  (dolist (map (list company-active-map company-search-map company-filter-map))
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)
    (define-key map (kbd "C-h") 'company-show-doc-buffer)
    (define-key map (kbd "C-w") nil)))


;; ----------------------------------------------------------------------------

;; Resolve HELM find file slow issue
;;
;; NOTE: If projectile cache gets out of sync, running 'projectile-find-file'
;;       again with a prefix argument (SPC u) or (C-u) will refresh the cache.
(setq projectile-enable-caching t)
