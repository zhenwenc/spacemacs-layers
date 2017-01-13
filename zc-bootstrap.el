;;; zc-bootstrap.el --- File for bootstrapping layers.  -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:

;; This configuration boostrapping file is greatly adapted from
;; https://github.com/chrisbarrett/spacemacs-layers

;;; Code:

(require 'package)

(defconst zc-bootstrap-packages
  '(s
    use-package
    evil
    dash
    dash-functional
    f
    let-alist
    hydra)
  "Packages required for bootstrapping my configuration.")

(defconst zc-bootstrap-package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/"))
  "An alist of package archives required during bootstrap.")

(defconst zc-bootstrap-preload-lisp-files
  (list
   (concat user-layers-directory "cb-use-package-extensions.el")
   (concat user-layers-directory "cb-vars.el"))
  "Aggressively load these packages.  They contain utilities needed in layer definitions.")

(defvar zc-bootstrap/package-installation-attempts 2
  "Abort package installation after this number of failed attempts.")

(defun zc-bootstrap/enable-debugging ()
  "Show a backtrace if I've stuffed up something in my configuration."
  (setq debug-on-error t)
  (setq debug-on-quit t))

(defun zc-bootstrap/initialize-packages ()
  (dolist (archive zc-bootstrap-package-archives)
    (add-to-list 'package-archives archive))
  (package-initialize)
  (unless (file-exists-p (concat user-emacs-directory "elpa"))
    (package-refresh-contents))
  (dolist (pkg zc-bootstrap-packages)
    (zc-bootstrap--install-package pkg)))

(defun zc-bootstrap/load-preloadable-lisp-files ()
  (add-to-list 'load-path user-layers-directory)
  (dolist (el zc-bootstrap-preload-lisp-files)
    (load el)))

(defun zc-bootstrap/disable-debugging ()
  (setq debug-on-error nil)
  (setq debug-on-quit nil))

(defun zc-bootstrap/configure-ensime-use-stable ()
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(ensime . "melpa-stable") t))

(defun zc-bootstrap--mk-package-dir-regexp (pkg)
  (rx-to-string `(and ,(symbol-name pkg)
                      "-" (repeat 8 digit) "." (repeat 3 4 digit) (? "/"))))

(defun zc-bootstrap--install-package (pkg &optional attempts cur)
  (cond
   ((null attempts)
    (zc-bootstrap--install-package pkg zc-bootstrap/package-installation-attempts 1))
   ((< attempts cur)
    (error "Unable to install %s after %s attempt(s)" pkg attempts))
   (t
    (if (equal 1 cur)
        (message "--> Installing package %s..." pkg)
      (message "--> Installing package %s... (attempt %s/%s)" pkg cur attempts))
    (condition-case err
        (cond
         ((require 'paradox nil t)
          (paradox-require pkg))
         ((package-installed-p pkg)
          (require pkg))
         (t
          (package-install pkg)
          (require pkg)))
      (error
       (let ((archives (concat package-user-dir "/archives")))
         (when (file-directory-p archives)
           (message "--> Cleaning package archives...")
           (delete-directory archives t)))

       (dolist (entry (directory-files package-user-dir t))
         (when (string-match-p (zc-bootstrap--mk-package-dir-regexp pkg) (file-name-nondirectory entry))
           (message "--> Deleting existing package at %s..." entry)
           (delete-directory entry t)))

       (package-refresh-contents)
       (package-initialize)
       (zc-bootstrap--install-package pkg attempts (1+ cur)))))))

(provide 'zc-bootstrap)

;;; zc-bootstrap.el ends here
