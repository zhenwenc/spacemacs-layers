;;; packages.el --- highlight Layer packages File for My Spacemacs

(defconst zc-highlight-packages
  '(highlight-symbol))

(defun zc-highlight/init-highlight-symbol ()
  (use-package highlight-symbol
    :defer t
    :init
    (setq highlight-symbol-idle-delay 0.35)
    (define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode
      (lambda ()
        (when (not (memq major-mode
                         (list
                          'magit-status-mode
                          'magit-diff-mode
                          'magit-revision-mode
                          'magit-auto-revert-mode
                          'magit-reflog-mode
                          'magit-log-mode
                          'magit-popup-mode
                          'tide-references-mode
                          'minibuffer-inactive-mode
                          'spacemacs-buffer-mode)))
          (highlight-symbol-mode 1))))
    (global-highlight-symbol-mode 1)))
