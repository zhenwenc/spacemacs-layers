;;; funcs.el --- TypeScript Layer functions File for My Spacemacs

(defun zc-web/tsfmt-format-buffer ()
  "Format buffer with tsfmt."
  (interactive)
  (if (executable-find "tsfmt")
      (let*  ((tmpfile (make-temp-file "~fmt-tmp" nil ".ts"))
              (coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (outputbuf (get-buffer-create "*~fmt-tmp.ts*")))
        (unwind-protect
            (progn
              (with-current-buffer outputbuf (erase-buffer))
              (write-region nil nil tmpfile)
              (if (zerop (apply 'call-process "tsfmt" nil outputbuf nil
                                (list (format
                                       "--baseDir='%s' --"
                                       default-directory)
                                      tmpfile)))
                  (let ((p (point)))
                    (save-excursion
                      (with-current-buffer (current-buffer)
                        (erase-buffer)
                        (insert-buffer-substring outputbuf)))
                    (goto-char p)
                    (message "formatted.")
                    (kill-buffer outputbuf))
                (progn
                  (message "Formatting failed!")
                  (display-buffer outputbuf)))
              (progn
                (delete-file tmpfile)))))
    (error "tsfmt not found. Run \"npm install -g typescript-formatter\"")))

(defun zc-web/format ()
  "Call formatting tool specified in `typescript-fmt-tool'."
  (interactive)
  (cond
   ((eq typescript-fmt-tool 'typescript-formatter)
    (call-interactively 'zc-web/tsfmt-format-buffer))
   ((eq typescript-fmt-tool 'tide)
    (call-interactively 'tide-format))
   (t (error (concat "%s isn't valid typescript-fmt-tool value."
                     " It should be 'tide or 'typescript-formatter."
                     (symbol-name typescript-fmt-tool))))))

(defun zc-web/fmt-before-save-hook ()
  (add-hook 'before-save-hook 'zc-web/format t t))

(defun zc-web/jump-to-type-def ()
  (interactive)
  (tide-jump-to-definition t))

(defun zc-web/print-error-at-point ()
  "Return the flycheck errors at current point."
  (interactive)
  (-if-let (msg (get-text-property (point) 'help-echo))
      (message msg)
    (message "No warning or error found.")))
