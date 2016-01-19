;;; funcs.el --- Scala Layer functions File for My Spacemacs

;;; Ensime

(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")
(autoload 'projectile-project-p "projectile")

;; -----------------------------------------------------------------------------
;; Ensime custom config

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition` with ctags if ENSIME is not available."
  (interactive)
  (if (ensime-connection-or-nil)
      (ensime-edit-definition)
    (helm-gtags-find-tag-from-here)))

(defun ensime-pop-stack-with-fallback ()
  "Variant of `pop-tag-mark` with ctags if ENSIME is not available."
  (interactive)
  (if (ensime-connection-or-nil)
      (pop-tag-mark)
    (helm-gtags-pop-stack)))

;; -----------------------------------------------------------------------------
;; Automatically replace arrows with unicode ones when enabled

(defconst scala-unicode-arrows-alist
  '(("=>" . "⇒")
    ("->" . "→")
    ("<-" . "←")))

(defun scala/replace-arrow-at-point ()
  "Replace the arrow at point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
  (let* ((end (point))
         (start (max (- end 2) (point-min)))
         (x (buffer-substring start end))
         (arrow (assoc x scala-unicode-arrows-alist)))
    (when arrow
      (undo-boundary)
      (backward-delete-char 2)
      (insert (cdr arrow)))))

(defun scala/unicode-gt ()
  "Insert a `>' to the buffer. If it's part of an right arrow (`->' or `=>'),
replace it with the corresponding unicode arrow."
  (interactive)
  (insert ">")
  (scala/replace-arrow-at-point))

(defun scala/unicode-hyphen ()
  "Insert a `-' to the buffer. If it's part of an left arrow (`<-'),
replace it with the unicode arrow."
  (interactive)
  (insert "-")
  (scala/replace-arrow-at-point))

;; -----------------------------------------------------------------------------

(defun scala/configure-ensime ()
  "Ensure the file exists before starting `ensime-mode'."
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode +1))
   ((buffer-file-name)
    (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

(defun scala/maybe-start-ensime ()
  (when (buffer-file-name)
    (let ((ensime-buffer (scala/ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name)))
          (is-source-file (s-matches? (rx (or "/src/" "/test/")) (buffer-file-name))))

      (when (and is-source-file (null ensime-buffer))
        (noflet ((ensime-config-find (&rest _) file))
          (save-window-excursion
            (ensime)))))))

(defun scala/ensime-buffer-for-file (file)
  "Find the Ensime server buffer corresponding to FILE."
  (let ((default-directory (file-name-directory file)))
    (-when-let (project-name (projectile-project-p))
      (--first (-when-let (bufname (buffer-name it))
                 (and (s-contains? "inferior-ensime-server" bufname)
                      (s-contains? (file-name-nondirectory project-name) bufname)))
               (buffer-list)))))

(defun scala/enable-eldoc ()
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode +1))

(defun spacemacs/ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun spacemacs/ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))

;;; Interactive commands

(defun spacemacs/scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.
`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.
Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (insert ".")
  (company-complete))

;;; Flyspell

(defun scala/flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun scala/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'scala/flyspell-verify))
