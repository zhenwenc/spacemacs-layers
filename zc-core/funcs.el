;;; funcs.el --- Scala Layer functions File for My Spacemacs

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun zc-core/start-newline-next ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun zc-core/start-newline-prev ()
  (interactive)
  (if (eq (forward-line -1) 0)
      (start-newline-next)
    (progn
      (message "begin new line at the start")
      (beginning-of-line)
      (newline)
      (forward-line -1))))

;; Copy the current line to below
(defun zc-core/copy-line-buttom ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank))

;; Backward kill line, behave as 'Command + delete'
(defun zc-core/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Move the current line up
(defun zc-core/move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

;; Move the current line down
(defun zc-core/move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

;; Evil escape everything and save buffer
(defun zc-core/evil-escape-and-save ()
  "Escape everything and save buffer."
  (interactive)
  (zc-core/evil-escape)
  (save-buffer))

;; Evil escape everything
(defun zc-core/evil-escape ()
  "Escape everything."
  (interactive)
  (if (bound-and-true-p iedit-mode)
      (iedit-quit))
  ;; (evil-mc-undo-all-cursors)
  (evil-escape))

;; Ivy swiper pull next work to search
(defun zc-core/yank-symbol-at-point ()
  "Pull next word from buffer into search string."
  (interactive)
  (let (query)
    (with-ivy-window
      (let ((tmp (symbol-at-point)))
        (setq query tmp)))
    (when query
      (insert (format "%s" query)))))

;; Evaluate the last expression and replace with the result
(defun eval-last-sexp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (condition-case nil
      (let ((value (eval (preceding-sexp))))
        (evil-jump-item)
        (kill-sexp)
        (insert (format "%S" value)))
    (error (message "Invalid expression"))))

;; Returns secret value with the given key
(defun zc-core/load-secrets (pkg key)
  "Return the secret value with KEY from PKG."
  (interactive)
  (let ((value) (package (assoc pkg zc-secret-sources)))
    (if (not (null package))
        (progn
          (require pkg (cdr package))
          (setq value (symbol-value key))
          (unload-feature pkg)
          (identity value))
      (user-error "No secret package [%s] found" pkg))))
