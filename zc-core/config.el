;; Unify auto-save files
(defvar spacemacs-autosaves-directory
  (concat user-emacs-directory "autosaves/"))

;; ----------------------------------------------------------------------------
;; Key bindings

;; Fullscreen on Mac

(global-set-key (kbd "C-x 9")           'toggle-frame-fullscreen)

(global-set-key (kbd "M-z")             'undo-tree-undo)
(global-set-key (kbd "M-Z")             'undo-tree-redo)
(global-set-key (kbd "M-s")             'save-buffer)
(global-set-key (kbd "M-c")             'kill-ring-save)
(global-set-key (kbd "M-v")             'yank)

;; Custom key bindings

(global-set-key (kbd "A-<left>")        'backward-word)
(global-set-key (kbd "A-<right>")       'forward-word)
(global-set-key (kbd "A-<backspace>")   'core/backward-kill-word)

(global-set-key (kbd "M-<left>")        'move-beginning-of-line)
(global-set-key (kbd "M-<right>")       'move-end-of-line)
(global-set-key (kbd "C-<left>")        'backward-sentence)
(global-set-key (kbd "C-<right>")       'forward-sentence)

(global-set-key (kbd "C-<return>")      'core/start-newline-next)
(global-set-key (kbd "M-<backspace>")   'core/backward-kill-line)
(global-set-key (kbd "C-S-k")           'kill-whole-line)

(global-set-key (kbd "M-<down>")        'scroll-up-command)
(global-set-key (kbd "M-<up>")          'scroll-down-command)
(global-set-key (kbd "C-v")             'scroll-up-command)
(global-set-key (kbd "C-S-v")           'scroll-down-command)

(global-set-key (kbd "M-p")             'helm-projectile-find-file)
(global-set-key (kbd "M-P")             'helm-projectile-switch-project)
(global-set-key (kbd "M-y")             'helm-show-kill-ring)

(global-set-key (kbd "C-s")             'helm-occur)
(global-set-key (kbd "C-S-s")           'helm-projectile-ag)

(evil-global-set-key 'normal (kbd "M-C-<left>")     'core/move-line-up)
(evil-global-set-key 'normal (kbd "M-C-<right>")    'core/move-line-down)

;; I-Search

(define-key isearch-mode-map (kbd "M-v")           'isearch-yank-pop)
(define-key isearch-mode-map (kbd "M-<backspace>") 'isearch-delete-char)
