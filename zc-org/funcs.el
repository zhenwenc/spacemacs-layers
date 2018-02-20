(defun zc-org/babel-foreach-result (fn)
  "Visit every Source-Block and evaluate `fn'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun zc-org/babel-remove-result-all ()
  (interactive)
  (zc-org/babel-foreach-result 'org-babel-remove-result-one-or-many))

(defun zc-org/babel-execute-all ()
  (interactive)
  (zc-org/babel-foreach-result 'org-ctrl-c-ctrl-c))
