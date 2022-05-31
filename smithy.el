;;; smithy.el --- Major mode for editing Smithy IDL files

;; Copyright (C) 2022 Matt Nemitz

;; Author: Matt Nemitz <matt.nemitz@gmail.com>
;; Version: 1.0
;; Keywords: tools, languages, smithy, IDL, amazon

;;; Commentary:

;; This package provides a major mode for editing Smithy IDL files
;; Currently only supports basic syntax highlighting

;;; Code:

;; We leverage the C syntax table to get strings, comments, parentheses and braces
;; matching and highlighting properly
(require 'cc-langs)


(defvar smithy-highlights
      (let* (
            ;; define several category of keywords
             (x-keywords '("namespace" "use" "metadata" "apply" "member"))
             (x-types '("service" "operation" "resource" "map" "structure" "union" "set" "list"))
             (x-simple-type-names'("blob"
                                   "boolean"
                                   "document"
                                   "string"
                                   "byte"
                                   "short"
                                   "integer"
                                   "long"
                                   "float"
                                   "double"
                                   "bigInteger"
                                   "bigDecimal"
                                   "timestamp"))
             (x-node-keywords '("true" "false" "null"))
            ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-traits-regexp "@\\w+\\(\\.\\w+\\)*\\(#\\w+\\)?")
             (x-types-regexp (regexp-opt x-types 'words))
             (x-node-keywords-regexp (regexp-opt x-node-keywords 'words))
             (x-control-statement-regexp "$version:")
             (x-simple-type-names-regexp (regexp-opt x-simple-type-names'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-simple-type-names-regexp . 'font-lock-builtin-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          (,x-node-keywords-regexp . 'font-lock-keyword-face)
          (,x-control-statement-regexp . 'font-lock-preprocessor-face)
          (,x-traits-regexp . 'font-lock-function-name-face))))
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          


(defvar smithy-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?- ". 12b" table)
    table))

(define-derived-mode smithy-mode fundamental-mode "smithy"
  "major mode for editing smithy code."
  :syntax-table smithy-syntax-table
  (setq comment-start "// ")
  (setq comment-end "")
  (when (>= emacs-major-version 26.1)
    (display-line-numbers-mode))
  (setq font-lock-defaults '(smithy-highlights)))


(provide 'smithy)

;;; smithy.el ends here
