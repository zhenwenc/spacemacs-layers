;;; packages.el --- zc-sql layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Requirements:
;; - The database passwords are stored in a file encrypted by GnuPG,
;;   install via 'brew install gnupg'

;; Known bugs:
;; ------------------------------------------------------------------
;; sql-connect fails in first invocation: "Attempt to set a constant symbol: nil":
;; - http://emacs.1067599.n8.nabble.com/bug-19452-24-4-sql-connect-fails-in-first-invocation-quot-Attempt-to-set-a-constant-symbol-nil-quot-td344566.html#a405288

;;; Code:

(defconst zc-sql-packages
  '(sql))

(defconst zc-sql-connection-alist
  '((local-mysql
     (sql-product 'mysql)
     (sql-port 3306)
     (sql-server "127.0.0.1")
     (sql-user "root")
     (sql-password nil)
     (sql-database "backend_test"))
    (bonc-test2
     (sql-product 'mysql)
     (sql-port 3306)
     (sql-server "127.0.0.1")
     (sql-user "root")
     (sql-password nil)
     (sql-database "backend_test")
     (sql-default-directory "/ssh:bonc-test2.movio.co:"))
    (bonc-uat2
     (sql-product 'mysql)
     (sql-port 3306)
     (sql-server "127.0.0.1")
     (sql-user "root")
     (sql-database "backend_test")
     (sql-default-directory "/ssh:bonc-uat2.movio.co:"))
    ))

(defun zc-sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'sql 'zc-sql/start-connection "sql")
      (global-set-key (kbd "<f10>") 'zc-sql/start-connection))))

;;; packages.el ends here
