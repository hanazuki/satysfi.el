;;; satysfi-mode.el --- major mode for editing SATySFi documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Kasumi Hanazuki

;; Author: Kasumi Hanazuki <kasumi@rollingapple.net>
;; Keywords: satysfi, languages
;; URL: https://github.com/hanazuki/satysfi.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'rx)
(require 'seq)

; Optional dependency
(eval-when-compile
  (require 'paren))

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
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?# "'" st)
    st)
  "Syntax table for `satysfi-mode'.")

(eval-when-compile
  (defconst satysfi-syntax-propertize-rules
    '(((rx (group ?') (group ?<))
       (1 "'")
       (2 "(>"))
      ((rx (group ?$) ?{)
       (1 "'"))
      ((rx (group ?!) (any "([<{"))
       (1 "'")))))

(defun satysfi-syntax-propertize (beg end)
  (funcall (syntax-propertize-rules satysfi-syntax-propertize-rules)
           beg end)

  (save-excursion
    (catch 'exit
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

            (while (and
                    (< (point) end)
                    (forward-comment 1)))
            (unless (and
                     (< (point) end)
                     (re-search-forward (rx (any "#`<>")) end t))
              (throw 'exit t))
            (goto-char (match-beginning 0))

            (cond
             ((and (looking-at-p (rx (? ?#) (+ ?`))))
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

;; Check if pos is an active position in block or inline context
(defun satysfi--active (pos)
  (save-excursion
    (goto-char pos)
    (catch 'exit
      (let* ((ppss (syntax-ppss))
             (bos (nth 1 ppss)))
        ;; never active at toplevel, which is in program context
        (unless bos
          (throw 'exit nil))

        (condition-case nil
            (while t
              (forward-comment (- (buffer-size)))
              (cond
               ((memq (preceding-char) '(?\) ?\]))
                ;; backtrack to matching paren (unless the closing paren is escaped)
                (let ((ppss0 (syntax-ppss (1- (point)))))
                  (if (eq (nth 0 ppss) (nth 0 ppss0))
                      (throw 'exit nil)
                    (goto-char (nth 1 ppss0)))))
               ((looking-back (rx (or "?*" "?:")) 2)
                (forward-char -2))
               (t
                ;; Check if the last token is a command
                (throw 'exit
                       (and
                        (looking-back
                         (rx (any "\\+#") (1+ (or (syntax word) (syntax symbol))))
                         (- (point) bos))
                        (match-string 0)))))))))))


(defun satysfi-current-context ()
  (interactive)
  (message (symbol-name (satysfi--current-context))))

(defun satysfi-current-activation ()
  (interactive)
  (message (satysfi--active (point))))


(defvar satysfi-mode-program-keywords-regexp
  (regexp-opt '("let" "let-rec" "let-mutable" "let-inline" "let-block" "let-math" "in" "and"
                "match" "with" "when" "as" "if" "then" "else" "fun"
                "type" "constraint" "val" "direct" "of"
                "module" "struct" "sig" "end"
                "before" "while" "do"
                "controls" "cycle")
              'symbols))

(defvar satysfi-mode-header-keywords-regexp
  (concat (regexp-opt '("@import" "@require") t)
          ":"))

(defvar satysfi-mode-commands-regexp
  (rx (group (any "\\+#") (1+ (or (syntax word) (syntax symbol))))))

(defun satysfi-mode--match-contextual-keywords (contexts keywords-regexp)
  (letrec ((re (symbol-value keywords-regexp))
           (matcher
            (lambda (limit)
              (and
               (re-search-forward re limit t)
               (or
                (memq (save-match-data (satysfi--current-context)) contexts)
                (funcall matcher limit))))))
    matcher))


(defvar satysfi-mode-font-lock-keywords
  `((,(satysfi-mode--match-contextual-keywords '(program) 'satysfi-mode-program-keywords-regexp) 1 font-lock-keyword-face)
    (,(satysfi-mode--match-contextual-keywords '(program) 'satysfi-mode-header-keywords-regexp) 1 font-lock-keyword-face)
    (,(satysfi-mode--match-contextual-keywords '(block inline) 'satysfi-mode-commands-regexp) 1 font-lock-builtin-face))
  "Font-lock keywords for `satysfi-mode'.")

(defgroup satysfi nil
  "SATySFi"
  :prefix "satysfi-"
  :group 'languages)

(defcustom satysfi-basic-offset 2
  "Amount of basic offset"
  :type 'integer
  :group 'satysfi)

(defun satysfi-mode-indent-line ()
  "Indent current line as SATySFi code."
  (interactive)
  (let ((indent (satysfi-mode-find-indent (point))))
    (when indent
      (indent-line-to (max 0 indent)))))

(defun satysfi-mode-find-indent (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (pcase (satysfi--current-context)
      ('string nil)  ; TODO: what about indentation in multiline string literals?
      ('comment nil)  ; this should never happen, though
      (`,ctx
       (satysfi-mode-find-base-indent)))))

(defun satysfi-mode-find-base-indent ()
  (save-excursion
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (if (not (nth 1 ppss))
          0  ; no indent for toplevel
        (let ((open-indentaion
               (save-excursion
                 (goto-char (nth 1 ppss))
                 (current-indentation)))
              (content-alignment
               (save-excursion
                 (goto-char (nth 1 ppss))
                 (if (looking-at-p (rx "(|"))
                     (forward-char 2)
                   (forward-char 1))
                 (forward-comment 1)
                 (if (eq (line-number-at-pos) (line-number-at-pos (nth 1 ppss)))  ; content exists after open paren
                     (current-column)))))
          (cond
           ((or (looking-at-p (rx "|)"))
                (eq (syntax-class (syntax-after (point))) 5))  ; 5 for close parenthesis
            open-indentaion)
           (content-alignment  ; TODO: make vertical alignment configurable?
            content-alignment)
           (t  (+ satysfi-basic-offset open-indentaion))))))))

(defun satysfi-mode-show-paren-data ()
  (save-excursion
    (cond
     ((and (eq (preceding-char) ?\() (eq (following-char) ?|))
      (forward-char -1))
     ((and (eq (preceding-char) ?|) (eq (following-char) ?\)))
      (forward-char +1))
     ((or (looking-at-p "'<") (looking-at-p "${"))
      (forward-char +1)))

    (pcase (show-paren--default)
      (`(,here-beg ,here-end ,there-beg ,there-end ,mismatch)
       (pcase (cons (satysfi-mode--extend-paren here-beg here-end)
                    (satysfi-mode--extend-paren there-beg there-end))
         (`((,here-beg . ,here-end) . (,there-beg . ,there-end))
          (let ((here (buffer-substring here-beg here-end))
                (there (buffer-substring there-beg there-end)))
            (when (or (and (string= here "(|") (not (string= there "|)")))
                      (and (not (string= here "(|")) (string= there "|)"))
                      (and (string= here "|)") (not (string= there "(|")))
                      (and (not (string= here "|)")) (string= there "(|")))
              (setq mismatch t))
            (list here-beg here-end there-beg there-end mismatch))))))))

(defun satysfi-mode--extend-paren (beg end)
  (let ((str (buffer-substring beg end)))
    (pcase str
      ("("
       (pcase (char-after end)
         (?| (setq end (1+ end)))))
      (")"
       (pcase (char-before beg)
         (?| (setq beg (1- beg)))))
      ("<"
       (pcase (char-before beg)
         (?' (setq beg (1- beg)))))
      ("{"
       (pcase (char-before beg)
         (?$ (setq beg (1- beg))))))
    (cons beg end)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(saty\\|satyh\\)\\'" . satysfi-mode))

;;;###autoload
(define-derived-mode satysfi-mode prog-mode "SATySFi"
  "Major mode for editing SATySFi document."
  (set-syntax-table satysfi-mode-syntax-table)
  (setq-local syntax-propertize-function #'satysfi-syntax-propertize)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults '(satysfi-mode-font-lock-keywords))

  ;; for indent
  (setq-local indent-line-function #'satysfi-mode-indent-line)

  ;; for paren
  (setq-local show-paren-data-function #'satysfi-mode-show-paren-data)

  ;; for comment
  (setq-local comment-start "%")
  (setq-local comment-end "")

  (run-mode-hooks 'satysfi-mode-hook))

(provide 'satysfi-mode)

;;; satysfi-mode.el ends here
