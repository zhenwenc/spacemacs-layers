;;; packages.el --- zc-ensime layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Frederick Cai <fredc@Fredericks-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst zc-ensime-packages
  '(ensime
    sbt-mode
    (ensime-diminished-modeline :location local)))

(defun zc-ensime/init-ensime ()
  (use-package ensime
    :defer t
    :config
    (progn
      (setq ensime-startup-snapshot-notification nil)
      (setq ensime-startup-notification nil)
      (setq ensime-auto-generate-config nil)
      (setq ensime-implicit-gutter-icons nil)
      (setq ensime-startup-dirname (f-join spacemacs-cache-directory "ensime"))

      (setq ensime-goto-test-config-defaults
            (list :test-class-names-fn #'ensime-goto-test--test-class-names
                  :test-class-suffixes '("Test" "Tests"
                                         "IntTest" "IntTests" "IntegrationTest" "IntegrationTests"
                                         "Spec" "Specs" "Specification" "Specifications"
                                         "Prop" "Props" "Property" "Properties"
                                         "Check" "Checks")
                  :impl-class-name-fn #'ensime-goto-test--impl-class-name
                  :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
                  :is-test-dir-fn #'ensime-goto-test--is-test-dir))

      (spacemacs/register-repl 'ensime #'ensime-inf-switch "ensime")

      (evil-define-key 'insert ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)

      (evil-define-key 'normal ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack
        (kbd "RET") 'ensime-inspect-type-at-point)

      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "M-.") 'ensime-inspector-browse-source
        (kbd "K") 'ensime-inspector-browse-doc
        (kbd "q") 'ensime-popup-buffer-quit-function
        (kbd ",") 'ensime-inspector-backward-page
        (kbd ".") 'ensime-inspector-forward-page
        (kbd "^") 'ensime-inspector-backward-page)

      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'zc-ensime/ensime-refactor-cancel
        (kbd "c") 'zc-ensime/ensime-refactor-accept
        (kbd "RET") 'zc-ensime/ensime-refactor-accept)

      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button
        (kbd "n") 'forward-button
        (kbd "N") 'backward-button)

      ;; Never ever let ensime check the whole project
      (define-key ensime-mode-map
        (kbd "C-c C-c a") 'ensime-show-all-errors-and-warnings)

      ;; HACK: Fix errors with ensime eldoc function.
      (with-eval-after-load 'ensime-inspector
        (defun ensime-type-at-point (&optional arg)
          "Echo the type at point to the minibuffer.
      A prefix argument will add the type to the kill ring."
          (interactive "P")
          (let* ((type (ensime-rpc-get-type-at-point))
                 (fullname (ensime-type-full-name-with-args type)))
            (when arg
              (kill-new fullname))
            (message fullname))))
      ))

  (use-package ensime-company
    :after ensime
    :config
    (progn
      ;; HACK: Reset company idle delay.
      ;; (advice-add 'ensime-company-enable :after #'scala/set-company-variables)

      ;; HACK: Prevent ensime from clobbering company settings.
      (with-eval-after-load 'ensime-company
        (defun ensime-company-enable ()
          (set (make-local-variable 'company-backends) '(ensime-company))
          (company-mode)
          (yas-minor-mode-on)
          (set (make-local-variable 'company-idle-delay) 0))))))

(defun zc-ensime/init-ensime-diminished-modeline ()
  (use-package ensime-diminished-modeline
    :after ensime))

;;; packages.el ends here
