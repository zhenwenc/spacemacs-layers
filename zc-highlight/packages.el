;;; packages.el --- highlight Layer packages File for My Spacemacs

(defconst zc-highlight-packages
  '(highlight-thing))

(defun zc-highlight/init-highlight-thing ()
  (use-package highlight-thing
    :defer t
    :init
    (progn
      (setq highlight-thing-delay-seconds 0.3)
      (setq highlight-thing-case-sensitive-p t)
      (set-face-background 'hi-yellow "#3e4446")
      (set-face-foreground 'hi-yellow nil)
      (global-highlight-thing-mode 1)
      )))

;; =======================================================================================
;; NOTE: highlight-symbol uses font-lock, which problematic in many
;;       mayjor modes, esp. minibuffers.

;; (defun zc-highlight/init-highlight-symbol ()
;;   (use-package highlight-symbol
;;     :defer t
;;     :init
;;     (setq highlight-symbol-idle-delay 0.35)
;;     (define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode
;;       (lambda ()
;;         (when (not (memq major-mode
;;                          (list
;;                           'magit-status-mode
;;                           'magit-diff-mode
;;                           'magit-revision-mode
;;                           'magit-auto-revert-mode
;;                           'magit-reflog-mode
;;                           'magit-log-mode
;;                           'magit-popup-mode
;;                           'tide-references-mode
;;                           'minibuffer-inactive-mode
;;                           'spacemacs-buffer-mode)))
;;           (highlight-symbol-mode 1))))
;;     (global-highlight-symbol-mode 1)))
