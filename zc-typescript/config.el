;;; config.el --- Typescript Layer Configuration File for Spacemacs

(defvar typescript-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar typescript-fmt-tool 'typescript-formatter
  "The name of the tool to be used
for TypeScript source code formatting.
Currently avaliable 'tide (default)
and 'typescript-formatter .")

(spacemacs|define-jump-handlers typescript-mode)
