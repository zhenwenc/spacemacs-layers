;;; packages.el --- zc-core Layer packages Files for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(require 's)
(require 'dash)
(require 'f)
(require 'noflet)

(defconst zc-core-packages
  '(wgrep-ag
    goto-chg
    ivy
    nginx-mode
    swiper
    ivy
    neotree
    (cb-vars :location local)
    (cb-buffers :location local)
    (smart-ops :location local)))

(defun zc-core/init-cb-buffers ()
  (use-package cb-buffers
    :bind
    (("C-c k b" . cb-buffers-maybe-kill-all))
    :bind*
    (("C-<backspace>" . cb-buffers-maybe-kill))))

(defun zc-core/init-smart-ops ()
  (use-package smart-ops
    :diminish smart-ops-mode
    :functions (smart-ops-init)
    :config (smart-ops-init)))

(defun zc-core/post-init-neotree ()
  (use-package neotree
    :config
    (progn
      (setq neo-theme 'arrow))))

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
      ;; Unset the fucking key binding
      (global-set-key (kbd "<f6>") #'eyebrowse-switch-to-window-config-2)

      ;; To match ISearch behaviour
      (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

      ;; Prevent minibuffer close
      (set-variable 'ivy-on-del-error-function '(lambda())))))
