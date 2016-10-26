;;; packages.el --- zc-core Layer packages Files for Spacemacs
;;; Commentary:
;;; Code:

;; iedit mode


(eval-when-compile
  (require 'use-package nil t))

(defconst zc-core-packages
  '(wgrep-ag
    goto-chg
    nginx-mode
    swiper
    ivy
    (cb-buffers :location local)
    (smart-ops :location local)
    (sp-generic-prog :location local)
    evil))

(defun zc-core/init-cb-buffers ()
  (use-package cb-buffers))

(defun zc-core/post-init-evil ()
  (with-eval-after-load 'evil
    (setq evil-insert-state-cursor 'bar)
    (setq evil-emacs-state-cursor 'bar)))

(defun zc-core/init-wgrep-ag ()
  (use-package wgrep-ag
    :defer t))

;; Goto Last Change
;; http://ensime.github.io/editors/emacs/hacks/#goto-last-change
(defun zc-core/init-goto-chg ()
  (use-package goto-chg
    :commands goto-last-change
    ;; complementary to
    ;; C-x r m / C-x r l
    ;; and C-<space> C-<space> / C-u C-<space>
    :bind (("M-[" . goto-last-change)
           ("M-]" . goto-last-change-reverse))))

(defun zc-core/init-nginx-mode ()
  (use-package nginx-mode
    :defer t
    :mode ("nginx\\.conf\\'" "/docker-nginx/.*\\.tmpl\\'")))

(defun zc-core/post-init-ivy ()
  (use-package ivy
    :config
    (progn
      ;; Prevent minibuffer close
      (set-variable 'ivy-on-del-error-function '(lambda())))))

(defun zc-core/post-init-ivy ()
  (use-package ivy
    :config
    (progn
      ;; Prevent minibuffer close
      (set-variable 'ivy-on-del-error-function '(lambda())))))

(defun zc-core/post-init-swiper ()
  (use-package swiper
    :config
    (progn
      (define-key swiper-map (kbd "C-w") 'core/swiper-symbol-at-point))))
