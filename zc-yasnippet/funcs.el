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

(defun zc-yasnippet/point-in-angle-pair-p ()
  "Return t if point is inside <>, nil otherwise."
  (equal "<" (plist-get (sp-get-enclosing-sexp) :op)))

(defun zc-yasnippet/point-after-whitespace-p ()
  "Return t if point is after space, tab or newline, nil otherwise."
  (looking-back "[\s\n\t]+" 1))

(defun zc-yasnippet/key-after-whitespace-p (key)
  "Return t if KEY is after space, tab or newline, nil otherwise."
  (looking-back (concat "[\s\n\t]+" (regexp-quote key)) 1))

(defun zc-yasnippet/react-buffer-p ()
  "Return t if buffer name ends with tsx or jsx."
  (let ((ext (file-name-extension (buffer-name))))
    (string-match-p "jsx\\|tsx" ext)))

(defun zc-yasnippet/react-jsx-expand-prop-p (key)
  (and (zc-yasnippet/react-buffer-p)
       (zc-yasnippet/key-after-whitespace-p key)
       (zc-yasnippet/point-in-angle-pair-p)))

(defun zc-yasnippet/react-jsx-expand-tag-p (key)
  (and (zc-yasnippet/react-buffer-p)
       (zc-yasnippet/key-after-whitespace-p key)))
