;;; funcs.el --- TypeScript Layer functions File for My Spacemacs

(defun zc-web/add-node-modules-bin-to-path ()
  "Use binaries from node_modules, where available."
  (when-let (root (projectile-project-p))
    (make-local-variable 'exec-path)
    (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))


;; Formatter

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



(defun zc-web/print-error-at-point ()
  "Return the flycheck errors at current point."
  (interactive)
  (-if-let (msg (get-text-property (point) 'help-echo))
      (message msg)
    (message "No warning or error found.")))


;; Emmet

(defun zc-web/buffer-contains-react-p ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward "React" nil t))))

(defun zc-web/emmet-mode-maybe ()
  (cond
   ((derived-mode-p 'zc-web-html-mode 'html-mode 'nxml-mode)
    (emmet-mode +1))

   ((and (derived-mode-p 'zc-web-js-mode 'zc-web-ts-mode)
         (zc-web/buffer-contains-react-p))
    (emmet-mode +1))

   ((-contains? '("jsx" "tsx") (file-name-extension (buffer-file-name)))
    (emmet-mode +1))))

(defun zc-web/emmet-expand ()
  (interactive)
  (if (bound-and-true-p yas-minor-mode)
      (call-interactively 'emmet-expand-yas)
    (call-interactively 'emmet-expand-line)))
