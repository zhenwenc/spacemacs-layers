;;; keybindings.el --- General keybindings that should be set unconditionally.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; GG
(with-eval-after-load 'auto-highlight-symbol
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil))

;; macOS
(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "bf" 'reveal-in-osx-finder)

  ;; this is only applicable to GUI mode
  (when (display-graphic-p)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'alt)

    (global-set-key (kbd "M-v") 'yank)
    (global-set-key (kbd "M-c") 'evil-yank)
    (global-set-key (kbd "M-z") 'undo-tree-undo)
    (global-set-key (kbd "M-Z") 'undo-tree-redo)
    (global-set-key (kbd "M-s") 'zc-core/evil-escape-and-save)
    (global-set-key (kbd "M-Z") 'undo-tree-redo)
    (global-set-key (kbd "C-M-f") 'spacemacs/toggle-frame-fullscreen)
    ;; Emacs sometimes registers C-s-f as this weird keycode
    (global-set-key (kbd "<C-M-268632070>") 'spacemacs/toggle-frame-fullscreen)))

;; Vim key bindings
;; https://github.com/syl20bnr/evil-escape

;; NOTE: Disabled as it creates weird effect while using j/k
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-unordered-key-sequence nil)
(setq-default evil-escape-excluded-states '(normal visual))
(setq-default evil-escape-excluded-major-modes '(help-mode neotree-mode))

(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "C-c C-g") 'evil-escape)
(global-set-key (kbd "TAB")     'indent-for-tab-command)

;; Force dabbrev - buffer-only completion
(evil-global-set-key 'insert (kbd "C-<tab>") 'dabbrev-expand)
(evil-global-set-key 'insert (kbd "TAB")     'indent-for-tab-command)
(evil-global-set-key 'normal (kbd "C-o")     'goto-last-change)
(evil-global-set-key 'normal (kbd "C-S-o")   'goto-last-change-reverse)
(evil-global-set-key 'normal (kbd "C-z")     'spacemacs/toggle-maximize-buffer)

;; Unset keybinding of define macro
(evil-global-set-key 'normal (kbd "q") nil)
(evil-global-set-key 'insert (kbd "C-k") nil)

(evil-global-set-key 'normal (kbd "M-s")   'zc-core/evil-escape-and-save)
(evil-global-set-key 'insert (kbd "M-s")   'zc-core/evil-escape-and-save)
(evil-global-set-key 'visual (kbd "M-s")   'zc-core/evil-escape-and-save)

(evil-global-set-key 'normal (kbd "M-a")   'zc-core/evil-escape)
(evil-global-set-key 'insert (kbd "M-a")   'zc-core/evil-escape)
(evil-global-set-key 'visual (kbd "M-a")   'zc-core/evil-escape)

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
(global-set-key (kbd "C-<return>")      'zc-core/start-newline-next)
(global-set-key (kbd "M-<backspace>")   'zc-core/backward-kill-line)
(global-set-key (kbd "C-S-d")           'kill-whole-line)
(global-set-key (kbd "M-k")             'kill-whole-line)

(global-set-key (kbd "M-<up>")          'evil-scroll-up)
(global-set-key (kbd "M-<down>")        'evil-scroll-down)
(global-set-key (kbd "C-S-v")           'evil-scroll-up)
(global-set-key (kbd "C-v")             'evil-scroll-down)

;; TODO: Set to alternative in Ivy
(global-set-key (kbd "M-y")             'counsel-yank-pop)

(global-set-key (kbd "C-s")             'counsel-grep-or-swiper)
(global-set-key (kbd "C-S-s")           'spacemacs/search-project-auto)
(spacemacs/set-leader-keys "ss"         'counsel-grep-or-swiper)

(global-set-key (kbd "M-C-<up>")        'zc-core/move-line-up)
(global-set-key (kbd "M-C-<down>")      'zc-core/move-line-down)

;; (global-set-key (kbd "A-M-<left>")      'windmove-left)
;; (global-set-key (kbd "A-M-<right>")     'windmove-right)
;; (global-set-key (kbd "A-M-<up>")        'windmove-up)
;; (global-set-key (kbd "A-M-<down>")      'windmove-down)

(global-set-key (kbd "M-1")            'winum-select-window-1)
(global-set-key (kbd "M-2")            'winum-select-window-2)
(global-set-key (kbd "M-3")            'winum-select-window-3)
(global-set-key (kbd "M-4")            'winum-select-window-4)
(global-set-key (kbd "M-5")            'winum-select-window-5)

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
