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

;; TODO:
;; * Rewrite sql-send-paragraph function:
;;   - execute on empty line should send the SQL statement above if there is any;
;;   - execute on SQL comment should send the SQL statement above if there is any;
;;   - execute on SQL statement body should send the paragraph.
;; * Default result set limitation:
;;   - set buffer local variable for "... LIMIT x" if statement doesn't contains LIMIT.

;;; Code:

(defconst zc-sql-packages
  '(sql))

(defun zc-sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'sql 'zc-sql/start-connection "sql")

      (evil-define-key 'normal sql-mode-map
        (kbd "RET") 'sql-send-paragraph)

      (spacemacs/set-leader-keys-for-major-mode 'sql-mode
        "ns" 'zc-sql/attach-to-sqli-buffer
        "nS" 'zc-sql/attach-to-sqli-buffer-auto
        "nn" 'zc-sql/attach-to-new-sqli-buffer
        "nt" 'zc-sql/show-attached-sqli-buffer

        "sb" 'sql-send-buffer
        "sp" 'sql-send-paragraph
        "ss" 'sql-send-string
        "sr" 'sql-send-region

        "dd" 'sql-list-all
        "dt" 'sql-list-table)

      (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode
        "bb" 'zc-sql/switch-to-sqli-buffer))
    ))

;;; packages.el ends here
