;;; packages.el --- Java functions File for Spacemacs


(defun zc-java/setup-ensime-eldoc ()
  "Setup ENSIME eldoc."
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode))

(defun zc-java/setup-indentation-style ()
  "Setup Java indentation style."
  (subword-mode)

  ;; Using Google's style
  ;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
  (google-set-c-style)
  (google-make-newline-indent)

  ;; Customize indentation
  ;; Use (c-show-syntactic-information) to check syntax element
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/c_002doffsets_002dalist.html
  (setq c-basic-offset 4)

  (c-set-offset 'statement-cont '+)
  (c-set-offset 'inexpr-class '0)
  (c-set-offset 'annotation-var-cont '0)
  )

(defun zc-java/ensime-print-errors-at-point (&optional arg)
  (interactive "P")
  (let ((msgs (append (ensime-errors-at (point)))))
    (when msgs
      (let ((msg (mapconcat 'identity msgs "\n")))
        (when (equal arg '(16))
          (ensime--make-result-overlay
              (format "%S" msg)
            :where (point)
            :duration 'command))
        (message "%s" msg)))
    (ensime-event-sig :errors-at-point-printed)))
