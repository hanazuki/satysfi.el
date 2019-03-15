;;; satysfi-mode.el -- major mode for editing SATySFi documents
;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'rx)
(require 'seq)

(defvar satysfi-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "/" st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
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
       (1 "'"))
      ((rx (group ?!) (any "([<{"))
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
            (unless (re-search-forward (rx (any "#`<>")) end t)
              (throw 'exit t))
            (goto-char (match-beginning 0))

            (cond
             ((looking-at (rx (? ?#) (+ ?`)))
              (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
              (goto-char (match-end 0)))
             ((eq (following-char) ?<)
              (let ((ctx (satysfi--current-context)))
                (if (or (eq ctx 'block) (eq ctx 'inline))
                    (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "(>"))))
              (forward-char 1))
             ((eq (following-char) ?>)
              (let ((ctx (satysfi--current-context)))
                (if (eq ctx 'block)
                    (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ")<"))))
              (forward-char 1))
             (t (forward-char 1)))))))))

;; Compute the lexing context at the point.
;; From lexer.mll:
;;
;;   The SATySFi lexer is stateful; the transitions are:
;;   | to \ from |program|block |inline|active |  math  |
;;   |-----------|-------|------|------|-------|--------|
;;   |  program  | (   ) |      |      | (   ) | !(   ) |
;;   |           | (| |) |      |      | (| |) | !(| |) |
;;   |           | [   ] |      |      | [   ] | ![   ] |
;;   |  block    | '<  > | <  > | <  > | <     | !<   > |
;;   |  inline   | {   } | {  } | {  } | {     | !{   } |
;;   |  active   |       | +x ; | \x ; |       |        |
;;   |           |       | #x ; | #x ; |       |        |
;;   |  math     | ${  } |      | ${ } |       | {    } |
;;
;;   Note that the active-block and active-inline transitions are one-way.
;;
(defun satysfi--current-context ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) 'string)
     ((nth 4 ppss) 'comment)
     (t
      (seq-reduce
       (lambda (st p)
         (let ((ch (char-after p)))
           (or
            (cond
             ((eq st 'program)
              (cond
               ((eq ch ?\<) 'block)
               ((eq ch ?\{) (if (eq (char-before p) ?$) 'math 'inline))))
             ((eq st 'block)
              (cond
               ((eq ch ?\() (if (satysfi--active p) 'program))
               ((eq ch ?\[) (if (satysfi--active p) 'program))
               ((eq ch ?\<) 'block)
               ((eq ch ?\{) 'inline)))
             ((eq st 'inline)
              (cond
               ((eq ch ?\() (if (satysfi--active p) 'program))
               ((eq ch ?\[) (if (satysfi--active p) 'program))
               ((eq ch ?\<) 'block)
               ((eq ch ?\{) (if (eq (char-before p) ?$) 'math 'inline))))
             ((and (eq st 'math) (eq (char-before p) ?!))
              (cond
               ((eq ch ?\() 'program)
               ((eq ch ?\[) 'program)
               ((eq ch ?\<) 'block)
               ((eq ch ?\{) 'inline))))
            st)))
       (nth 9 ppss)  ; positions of open parentheses
       'program)))))

;; Check if pos is just after an activating command
;; BUGS: text\command(this context should be program);
(defun satysfi--active (pos)
  (catch 'exit
    (while t
      (let* ((p (condition-case nil
                    (scan-sexps pos -1)
                  (scan-error (throw 'exit nil))))
             (c (char-after p)))
        (unless (eq (scan-sexps p +1)
                    (save-excursion
                      (goto-char pos)
                      (forward-comment (- (buffer-size)))
                      (point)))
          (throw 'exit nil))
        (cond
         ((or (eq c ?\() (eq c ?\[))
          (setq pos p))
         ((or (eq c ?#) (eq c ?\\) (eq c ?+))
          (throw 'exit t))
         (t (throw 'exit nil)))))))

(defun satysfi-current-context ()
  (interactive)
  (print (satysfi--current-context)))


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
  (setq-local comment-start "%")
  (setq-local comment-end "")

  (set-syntax-table satysfi-mode-syntax-table)
  (setq-local syntax-propertize-function #'satysfi-syntax-propertize)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults '(satysfi-mode-font-lock-keywords nil nil))
  (run-mode-hooks 'satysfi-mode-hook))

(provide 'satysfi-mode)
