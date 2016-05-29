;; Unify auto-save files
(defvar spacemacs-autosaves-directory
  (concat user-emacs-directory "autosaves/"))

;; Turn on delete selection mode
(delete-selection-mode 1)

;; ----------------------------------------------------------------------------
;; Key bindings

;; GG
(with-eval-after-load 'auto-highlight-symbol

  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil))

;; Fullscreen on Mac
(global-set-key (kbd "C-x 9")           'toggle-frame-fullscreen)

;; OSX
(global-set-key (kbd "M-Z")             'undo-tree-redo)
(global-set-key (kbd "M-z")             'undo-tree-undo)
(global-set-key (kbd "M-v")             'yank)
(global-set-key (kbd "M-s")             'save-buffer)
(global-set-key (kbd "M-c")             'kill-ring-save)

;; Custom key bindings
(global-set-key (kbd "A-<left>")        'backward-word)

(global-set-key (kbd "A-<backspace>")   'backward-kill-word)
(global-set-key (kbd "A-<right>")       'forward-word)
(global-set-key (kbd "C-<left>")        'backward-sentence)

(global-set-key (kbd "M-<left>")        'spacemacs/smart-move-beginning-of-line)
(global-set-key (kbd "M-<right>")       'move-end-of-line)

(global-set-key (kbd "C-<right>")       'forward-sentence)
(global-set-key (kbd "C-<return>")      'core/start-newline-next)
(global-set-key (kbd "M-<backspace>")   'core/backward-kill-line)
(global-set-key (kbd "C-S-k")           'kill-whole-line)
(global-set-key (kbd "M-k")             'kill-whole-line)

(global-set-key (kbd "M-<up>")          'evil-scroll-up)
(global-set-key (kbd "M-<down>")        'evil-scroll-down)
(global-set-key (kbd "C-S-v")           'evil-scroll-up)
(global-set-key (kbd "C-v")             'evil-scroll-down)

(global-set-key (kbd "M-p")             'helm-projectile-find-file)
(global-set-key (kbd "M-P")             'helm-projectile-switch-project)
(global-set-key (kbd "M-y")             'helm-show-kill-ring)

(global-set-key (kbd "C-s")             'helm-occur)
(global-set-key (kbd "C-S-s")           'helm-projectile-ag)

(global-set-key (kbd "M-C-<up>")        'core/move-line-up)
(global-set-key (kbd "M-C-<down>")      'core/move-line-down)

;; (global-set-key (kbd "A-M-<left>")      'windmove-left)
;; (global-set-key (kbd "A-M-<right>")     'windmove-right)
;; (global-set-key (kbd "A-M-<up>")        'windmove-up)
;; (global-set-key (kbd "A-M-<down>")      'windmove-down)

;; Multiple cursors

(global-set-key (kbd "C-S-c C-S-c")     'mc/edit-lines)
(global-set-key (kbd "C->")             'mc/mark-next-like-this)
(global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->")         'mc/mark-all-like-this)

;; I-Search

(define-key isearch-mode-map (kbd "M-v")           'isearch-yank-pop)
(define-key isearch-mode-map (kbd "M-<backspace>") 'isearch-delete-char)

;; Browse file system

(global-set-key (kbd "M-0")             'neotree-toggle)

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
