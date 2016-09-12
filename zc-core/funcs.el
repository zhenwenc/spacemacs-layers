;;; funcs.el --- Scala Layer functions File for My Spacemacs

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun core/start-newline-next ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun core/start-newline-prev ()
  (interactive)
  (if (eq (forward-line -1) 0)
      (start-newline-next)
    (progn
      (message "begin new line at the start")
      (beginning-of-line)
      (newline)
      (forward-line -1))))

;; Copy the current line to below
(defun core/copy-line-buttom ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank))

;; Backward kill line, behave as 'Command + delete'
(defun core/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Move the current line up
(defun core/move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

;; Move the current line down
(defun core/move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

;; Evil escape everything and save buffer
(defun core/evil-escape-and-save ()
  "Escape everything and save buffer."
  (save-buffer)
  (interactive)
  (evil-mc-undo-all-cursors)
  (evil-escape))
