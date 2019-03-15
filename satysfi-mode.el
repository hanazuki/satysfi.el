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

(eval-when-compile
  (defconst satysfi-syntax-propertize-rules
    '(((rx (group (any "-|") ?>)) ;; TODO: handling of > should be context-aware
       (1 "."))
      ((rx (group ?') (group ?<))
       (1 "'")
       (2 "(>"))
      ((rx (group ?$) ?{)
       (1 "'")))))

(defun satysfi-syntax-propertize (beg end)
  (funcall (syntax-propertize-rules satysfi-syntax-propertize-rules)
           beg end)

  (catch 'exit
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((ppss (syntax-ppss)))
          (if (eq (nth 3 ppss) t) ;; if this is inside ` string
              (let ((open-marker
                     (save-excursion
                       (goto-char (nth 8 ppss))
                       (looking-at (rx (? ?#) (group (+ ?`))))
                       (match-string 1))))
                (unless (search-forward open-marker end t)
                  (throw 'exit t))
                (unless (looking-at-p "#")
                  (forward-char -1))
                (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
                (forward-char +1))

            (forward-comment (buffer-size))
            (unless (re-search-forward (rx (any "#`")) end t)
              (throw 'exit t))
            (goto-char (match-beginning 0))

            (cond ((looking-at (rx (? ?#) (+ ?`)))
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
                   (goto-char (match-end 0)))
                  (t (forward-char 1)))))))))

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
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults '(satysfi-mode-font-lock-keywords nil nil))
  (run-mode-hooks 'satysfi-mode-hook))

(provide 'satysfi-mode)
