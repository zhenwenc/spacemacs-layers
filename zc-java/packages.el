;;; packages.el --- zc-java layer packages file for Spacemacs.

;;; Code:

(defconst zc-java-packages
  '(eclim))

(defun zc-java/post-init-eclim ()
  (use-package eclim
    :defer t

    :init
    (progn
      )

    :config
    (progn
      (setq eclim-eclipse-dirs "~/eclipse/Eclipse.app/Contents/Eclipse"
            eclim-executable "~/eclipse/Eclipse.app/Contents/Eclipse/eclim"
            eclimd-default-workspace "~/code/workspace"
            eclimd-wait-for-process nil)

      (setq help-at-pt-display-when-idle t
            help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer))

    :evil-bind
    (:map
     eclim-mode-map
     :state normal
     ("RET" . eclim-java-show-documentation-for-current-element)

     :map
     eclim-java-show-documentation-map
     :state normal
     ("q" . eclim-quit-window))
    ))

;;; packages.el ends here
