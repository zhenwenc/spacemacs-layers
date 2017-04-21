;;; packages.el --- zc-core Layer packages Files for Spacemacs
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'f)

(defconst zc-core-packages
  '(wgrep-ag
    goto-chg
    nginx-mode
    neotree
    pbcopy
    term
    (cb-buffers :location local)
    ))

(defconst zc-core/secret-sources
  '((zc-db-secrets . "~/dotfiles/secret/db-secrets.el.gpg")))

(defun zc-core/init-cb-buffers ()
  (use-package cb-buffers
    :bind
    (("C-c k b" . cb-buffers-maybe-kill-all))
    :bind*
    (("C-<backspace>" . cb-buffers-maybe-kill))))

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

(defun zc-core/post-init-projectile ()
  (use-package projectile
    :config
    (progn
      (dolist (dir '("vendor" "target", "node_modules"))
        (add-to-list 'projectile-globally-ignored-directories dir)))))

(defun zc-core/init-pbcopy ()
  (use-package pbcopy
    :if (and (spacemacs/system-is-mac) (not (display-graphic-p)))
    :init (turn-on-pbcopy)))

(defun zc-core/post-init-term ()
  (with-eval-after-load 'term
    (define-key term-raw-map (kbd "s-v") 'term-paste)))
