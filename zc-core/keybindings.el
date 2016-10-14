;;; keybindings.el --- General keybindings that should be set unconditionally.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;; Vim key bindings
;; https://github.com/syl20bnr/evil-escape

;; NOTE: Disabled as it creates weird effect while using j/k
;; (setq-default evil-escape-key-sequence "jk")
;; (setq-default evil-escape-delay 0.85)
;; (setq-default evil-escape-unordered-key-sequence t)

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-c C-g") 'evil-escape)

(evil-global-set-key 'normal (kbd "M-s")   'core/evil-escape-and-save)
(evil-global-set-key 'insert (kbd "M-s")   'core/evil-escape-and-save)
(evil-global-set-key 'visual (kbd "M-s")   'core/evil-escape-and-save)

(evil-global-set-key 'normal (kbd "M-a")   'core/evil-escape)
(evil-global-set-key 'insert (kbd "M-a")   'core/evil-escape)
(evil-global-set-key 'visual (kbd "M-a")   'core/evil-escape)

(evil-global-set-key 'normal (kbd "C-e")   'evil-end-of-line)
(evil-global-set-key 'insert (kbd "C-e")   'mwim-end-of-code-or-line)
(evil-global-set-key 'visual (kbd "C-e")   'evil-end-of-line)

(evil-global-set-key 'insert (kbd "C-d")   'delete-char)
(evil-global-set-key 'insert (kbd "C-S-d") 'backward-kill-word)
(evil-global-set-key 'insert (kbd "C-w")   'spacemacs/backward-kill-word-or-region)

;; Emacs key bindings
(global-set-key (kbd "A-<left>")        'backward-word)

(global-set-key (kbd "A-<backspace>")   'backward-kill-word)
(global-set-key (kbd "A-<right>")       'forward-word)
(global-set-key (kbd "C-<left>")        'backward-sentence)

(global-set-key (kbd "M-<left>")        'mwim-beginning-of-code-or-line)
(global-set-key (kbd "M-<right>")       'mwim-end-of-code-or-line)

(global-set-key (kbd "C-<right>")       'forward-sentence)
(global-set-key (kbd "C-<return>")      'core/start-newline-next)
(global-set-key (kbd "M-<backspace>")   'core/backward-kill-line)
(global-set-key (kbd "C-S-d")           'kill-whole-line)
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

;;; Multiple cursors

(global-set-key (kbd "C-S-c C-S-c")     'mc/edit-lines)
(global-set-key (kbd "C->")             'mc/mark-next-like-this)
(global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->")         'mc/mark-all-like-this)

;; I-Search

(define-key isearch-mode-map (kbd "M-v")           'isearch-yank-pop)
(define-key isearch-mode-map (kbd "M-<backspace>") 'isearch-delete-char)

;;; Browse file system

(global-set-key              (kbd "M-0")   'neotree-toggle)
(evil-global-set-key 'normal (kbd "M-0")   'neotree-toggle)

;;; keybindings.el ends here
