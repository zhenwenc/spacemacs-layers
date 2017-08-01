;;; funcs.el

;; Toggle display of Eshell

(defun zc-eshell/bring (&optional new)
  "Display an eshell buffer, creating a new one if needed.
With prefix argument ARG, always create a new shell."
  (interactive "P")
  (cond
   (new
    (zc-eshell--new))
   ((derived-mode-p 'eshell-mode)
    (zc-eshell--hide))
   ((zc-eshell--buffer)
    (zc-eshell--show))
   (t
    (zc-eshell--new))))

(defun zc-eshell--hide ()
  (let ((reg (zc-eshell--mk-register-name)))
    (if (get-register reg)
        (or (ignore-errors (jump-to-register reg t) t)
            (bury-buffer))
      (bury-buffer)
      (when (< 1 (length (window-list)))
        (delete-window)))))

(defun zc-eshell--new ()
  (window-configuration-to-register (zc-eshell--mk-register-name))
  (save-window-excursion
    (eshell t))
  (zc-eshell--show))

(defun zc-eshell--show ()
  (window-configuration-to-register (zc-eshell--mk-register-name))
  (pop-to-buffer (zc-eshell--buffer)))

(defun zc-eshell--buffer ()
  (let ((current-frame (zc-shell--current-frame)))
    (--first (with-current-buffer it
               (and (derived-mode-p 'eshell-mode)
                    (s-matches? "eshell" (buffer-name it))
                    (equal current-frame (window-frame (get-buffer-window it)))))
             (buffer-list))))

(defun zc-shell--current-frame ()
  (window-frame (get-buffer-window (current-buffer))))

(defun zc-eshell--mk-register-name ()
  (-let [(&alist 'window-id id) (frame-parameters (zc-shell--current-frame))]
    (intern (format "zc-eshell-%s" id))))



(defun zc-eshell/setup ()
  (vi-tilde-fringe-mode -1)
  (local-set-key (kbd "C-c RET") 'eshell-toggle-direct-send))
