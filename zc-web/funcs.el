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

(defun zc-web/emmet-expand-yas ()
  (interactive)
  (if (bound-and-true-p yas-minor-mode)
      (unless (call-interactively 'yas-expand)
        (call-interactively 'emmet-expand-line))
    (call-interactively 'emmet-expand-line)))


;; Smartparens

(defun zc-web/sp-skip-asterisk (ms mb me)
  "Non-nil if we should ignore this asterisk as a delimiter."
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

(defun zc-web/sp-comment-expand (&rest _ignored)
  "Expand Javascript comment block."
  (save-excursion
    (previous-line)
    (insert "*"))
  (insert "* ")
  (save-excursion
    (insert "\n")
    (indent-according-to-mode))
  (indent-according-to-mode))

(defun zc-web/sp-jsx-expand-tag (id action _context)
  "Expand JSX tag <> to self-closing form </> if point is not after a word."
  (when (and (eq action 'insert)
             (not (sp--looking-back-p
                   (concat "\\(\\sw\\|\\s_\\)" (regexp-quote id)))))
    (save-excursion (insert "/"))))

(defun zc-web/sp-jsx-rewrap-tag (&rest _ignored)
  "Rewrap the self-closing JSX tag <_/> to <_>|</_> if point is followed by />."
  (interactive)
  (if (sp--looking-at-p "/>")
    (let ((tag (zc-web/sp-jsx-get-tag-name))
          (beg (save-excursion (sp-backward-whitespace))))
      (delete-region beg (re-search-forward ">"))
      (insert ">\n")
      (save-excursion
        (insert "\n</" tag ">")
        (indent-according-to-mode))
      (indent-according-to-mode))
    (self-insert-command 1)))

(defun zc-web/sp-jsx-get-tag-name (&rest _ignored)
  "Return the JSX tag name inclosed in <> pair."
  (let* ((beg (save-excursion (re-search-backward "<")))
         (end (save-excursion (re-search-forward ">")))
         (str (buffer-substring beg end))
         (sub (replace-regexp-in-string "/\\|<\\|>" "" str)))
    (string-trim (car (split-string sub " ")))))

(defun zc-web/sp-point-after-bol-and-chevron-p (&rest _ignored)
  "Return t if point is after begining of line followed by <, nil otherwise."
  (save-excursion
    (sp--looking-back-p "^\\s-*<")))
