;;; funcs.el --- Scala Layer functions File for My Spacemacs

(require 'f nil t)

(autoload 'ensime "ensime")
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

;; TODO Investigate if this function might cause any performance impact 
(defun ensime-set-company-backend ()
  "Company backend for ctags if ENSIME is not available."
  (set (make-local-variable 'company-backends)
       '(ensime-company
         (company-keywords company-dabbrev-code company-gtags company-yasnippet))))

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
;; SBT
;; - ref:
;; https://github.com/chrisbarrett/spacemacs-layers/blob/master/cb-scala/funcs.el

(defun scala/maybe-project-root ()
  (or (locate-dominating-file default-directory ".ensime") (projectile-project-p)))

;; TODO Fix this function
(defun scala/sbt-gen-ensime (dir)
  (interactive (list
                (read-directory-name "Directory: " nil nil t (scala/maybe-project-root))))

  (let ((default-directory (f-slash dir)))
    (-when-let (buf (scala/inf-ensime-buffer))
      (message "Killing an existing Ensime server.")
      (kill-buffer buf))

    (message "Initialising Ensime at %s." default-directory)

    (let* ((bufname (format "*sbt gen-ensime [%s]*" (f-filename dir)))
           (proc (start-process "gen-ensime" bufname "sbt" "gen-ensime"))

           (kill-process-buffer
            (lambda ()
              (when (get-buffer bufname)
                (-when-let (windows (--filter (equal (get-buffer bufname)
                                                     (window-buffer it))
                                              (window-list)))
                  (-each windows 'delete-window))
                (kill-buffer bufname))))

           (kill-proc-buffer
            (lambda (_ status)
              (when (s-matches? "finished" status)
                (scala/fix-ensime-file)
                (ignore-errors
                  (funcall kill-process-buffer))
                (ensime)
                (message "Ensime ready.")))))

      (set-process-sentinel proc kill-proc-buffer)
      (display-buffer bufname)
      (recenter-top-bottom -1)
      (redisplay))))

(defun scala/fix-ensime-file (&optional file)
  "Fix malformed scalariform settings in FILE."
  (interactive)
  (require 'ensime)
  (let* ((ensime-prefer-noninteractive t)
         (file (or file (ensime-config-find)))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (scala/fix-dot-ensime)
      (let ((modified? (buffer-modified-p)))
        (save-buffer 0)
        (if modified?
            (message "Fixed ensime file")
          (message "No changes were needed"))))
    (kill-buffer buf)))

(defun scala/fix-dot-ensime ()
  (let ((invalid-formatter-rx
         (rx ":alignSingleLineCaseStatements" (group ".") "maxArrowIndent")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp invalid-formatter-rx nil t)
        (replace-match "_" t t nil 1)))))
