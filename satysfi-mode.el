;;; satysfi-mode.el -- major mode for editing SATySFi documents
;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'rx)

(defvar satysfi-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "/" st)
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?# "'" st)
    st)
  "Syntax table for `satysfi-mode'.")

(defconst satysfi-syntax-propertize-rules
  '(((rx (group (any "-|") ?>)) ;; TODO: handling of > should be context-aware
     (1 "."))
    ((rx (group ?') (group ?<))
     (1 "'")
     (2 "(>"))
    ((rx (group ?#) (1+ ?`))
     (1 "|"))
    ((rx (group ?`) (0+ ?`))
     (1 "|"))))

(defun satysfi-syntax-propertize (beg end)
;;  (while (< beg end)
    (let ((ppss (syntax-ppss beg)))
;;      (if (eq (nth 3 ppss) t) ;; if this is inside ` string
;;          (satysfi--syntax-propertize-string-literal ppss beg end)
        (funcall (syntax-propertize-rules satysfi-syntax-propertize-rules)
                 beg end)));;))

(defun satysfi--syntax-propertize-string-literal (ppss beg end)
  )


(defconst satysfi-mode-program-keywords-regexp
   (regexp-opt '("let" "let-rec" "let-mutable" "let-inline" "let-block" "let-math" "in" "and"
                 "match" "with" "when" "as" "if" "then" "else" "fun"
                 "type" "constraint" "val" "direct" "of"
                 "module" "struct" "sig" "end"
                 "before" "while" "do"
                 "controls" "cycle")
               'symbols))

(defconst satysfi-mode-header-keywords-regexp
  (concat (regexp-opt '("@import" "@require") t)
          ":"))

(defvar satysfi-mode-font-lock-keywords
  `(,satysfi-mode-program-keywords-regexp
    (,satysfi-mode-header-keywords-regexp 1 font-lock-builtin-face))
  "Font-lock keywords for `satysfi-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(saty\\|satyh\\)\\'" . satysfi-mode))

;;;###autoload
(define-derived-mode satysfi-mode prog-mode "SATySFi"
  "Major mode for editing SATySFi document."
  (set-syntax-table satysfi-mode-syntax-table)
  (setq-local syntax-propertize-function #'satysfi-syntax-propertize)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local font-lock-defaults '(satysfi-mode-font-lock-keywords nil nil))
  (run-mode-hooks 'satysfi-mode-hook))

(provide 'satysfi-mode)
