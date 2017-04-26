;;; zc-web-modes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Frederick Cai

;; Author: Frederick Cai <frederick.cai@gmail.com>

;;; Code:

(require 'web-mode)

;;;###autoload
(define-derived-mode zc-web-js-mode web-mode "JS"
  "Derived mode for editing JavaScript files.")

(provide 'zc-web-modes)

;;; zc-web-modes.el ends here
