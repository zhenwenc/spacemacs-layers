;;; funcs.el --- Yasnippet Layer functions File for My Spacemacs

(defvar smartparens-mode-original-value)

(defun zc-yasnippet/disable-sp-hippie-advice (&rest _)
  (setq smartparens-mode-original-value smartparens-mode)
  (setq smartparens-mode nil)
  t) ; We should still return t.

(defun zc-yasnippet/reenable-sp-hippie-advice (&rest _)
  (when (boundp 'smartparens-mode-original-value)
    (setq smartparens-mode smartparens-mode-original-value)
    (makunbound 'smartparens-mode-original-value)))
(advice-add 'hippie-expand :after #'zc-yasnippet/reenable-sp-hippie-advice
            ;; Set negative depth to make sure we go after
            ;; `sp-auto-complete-advice'.
            '((depth . -100)))

;; -----------------------------------------------------------------------------
;; Snippet conditions

(defun zc-yasnippet/line-match-p (REGEXP)
  "Return t if line matches the given pattern."
  (string-match-p REGEXP (thing-at-point 'line t)))

(defun zc-yasnippet/buffer-match-p (REGEXP)
  "Return t if buffer matches the given pattern."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward-regexp REGEXP nil t))))

