;;; packages.el --- Scala Layer packages File for Spacemacs

(eval-when-compile
  (require 'use-package nil t))

(defconst zc-scala-packages
  '(scala-mode2
    sbt-mode
    ensime
    noflet
    ))

(defun zc-scala/post-init-scala-mode2 ()
  (use-package scala-mode2
    :defer t
    :config
    (progn
      ;; Show line number
      (add-hook 'scala-mode-hook (lambda () (linum-mode t)))
      ;; Automatically replace arrows with unicode ones when enabled
      (when scala-use-unicode-arrows
        (define-key scala-mode-map (kbd ">") 'scala/unicode-gt)
        (define-key scala-mode-map (kbd "-") 'scala/unicode-hyphen)))))

(defun zc-scala/post-init-ensime ()
  (add-hook 'scala-mode-hook 'scala/maybe-start-ensime)
  (add-hook 'ensime-mode-hook 'ensime-set-company-backend)

  (setq ensime-auto-generate-config t)
  (setq ensime-prefer-noninteractive t)
  (setq ensime-implicit-gutter-icons nil)

  (with-eval-after-load 'ensime
    (define-key ensime-mode-map (kbd "M-n") nil)
    (define-key ensime-mode-map (kbd "M-p") nil)

    (define-key ensime-mode-map (kbd "M-n") 'ensime-forward-note)
    (define-key ensime-mode-map (kbd "M-N") 'ensime-backward-note)

    (define-key ensime-mode-map (kbd "M-.") 'ensime-edit-definition-with-fallback)
    (define-key ensime-mode-map (kbd "M-,") 'ensime-pop-stack-with-fallback))

  (spacemacs/set-leader-keys-for-major-mode 'scala-mode "ii" 'ensime-import-type-at-point))
