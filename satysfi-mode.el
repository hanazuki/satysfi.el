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

(require 'cl-lib)
(require 'rx)
(require 'seq)

; Optional dependency
(eval-when-compile
  (require 'paren))

(defconst satysfi-mode--syntax-alist
  '((?\n . ">")
    (?#  . "'")
    (?%  . "<")
    (?\( . "()")
    (?\) . ")(")
    (?_  . "_")
    (?\[ . "(]")
    (?\\ . "/")
    (?\] . ")[")
    (?\{ . "(}")
    (?\} . "){")))

(defvar satysfi-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; reset all symbols
    (loop for c from ?! to ?/
          do (modify-syntax-entry c "." st))
    (loop for c from ?: to ?@
          do (modify-syntax-entry c "." st))
    (loop for c from ?\[ to ?`
          do (modify-syntax-entry c "." st))
    (loop for c from ?\{ to ?~
          do (modify-syntax-entry c "." st))

    (loop for (c . syntax) in satysfi-mode--syntax-alist
          do (modify-syntax-entry c syntax st))

    st)
  "Syntax table for `satysfi-mode'.")

(eval-when-compile
  (defconst satysfi-syntax-propertize-rules
    '(((rx (group ?') ?<)
       (1 "'"))
      ((rx (group ?$) ?{)
       (1 "'"))
      ((rx (group ?!) (any "([<{"))
       (1 "'"))
      ((rx (or (syntax word) (syntax symbol)) (group (1+ "-")))
       (1 "_")))))

(defun satysfi-syntax-propertize (beg end)
  (remove-list-of-text-properties beg end '(satysfi-lexing-context satysfi-active-command))
  (funcall (syntax-propertize-rules satysfi-syntax-propertize-rules) beg end)

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
                     (re-search-forward (rx (any "#`<>([{")) end t))
              (throw 'exit t))
            (goto-char (match-beginning 0))

            (pcase (following-char)
              ((guard (looking-at-p (rx (? ?#) (+ ?`))))
               (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
               (goto-char (match-end 0)))
              (?<
               (let ((ctx (car (satysfi-mode--lexing-context (point)))))
                 (cond
                  ((memq ctx '(block inline))
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "(>"))
                   (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi-mode--lexing-context-transition ctx (point))))
                  ((eq (preceding-char) ?')
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "(>"))
                   (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi-mode--lexing-context-transition ctx (point))))))
               (forward-char 1))
              (?>
               (if (eq (car (satysfi-mode--lexing-context (point))) 'block)
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ")<")))
               (forward-char 1))
              ((or ?\( ?\[ ?\{)
               (let ((ctx (car (satysfi-mode--lexing-context (point)))))
                 (when (or (memq ctx '(program block inline))
                           (and (eq ctx 'math) (eq (preceding-char) ?!)))
                   (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi-mode--lexing-context-transition ctx (point)))))
               (forward-char 1))
              (_
               (forward-char 1)))))))))

;; Compute lexing context transition at pos.
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
(defun satysfi-mode--lexing-context-transition (current-context pos)
  (let ((ch (char-after pos)))
    (pcase current-context
      ('program
       (pcase ch
         (?\< 'block)
         (?\{ (if (eq (char-before pos) ?$) 'math 'inline))))
      ('block
          (pcase ch
            (?\( (if (satysfi--active-p pos) 'program))
            (?\[ (if (satysfi--active-p pos) 'program))
            (?\< 'block)
            (?\{ 'inline)))
      ('inline
        (pcase ch
          (?\( (if (satysfi--active-p pos) 'program))
          (?\[ (if (satysfi--active-p pos) 'program))
          (?\< 'block)
          (?\{ (if (eq (char-before pos) ?$) 'math 'inline))))
      ('math
       (when (eq (char-before pos) ?!)
         (pcase ch
           (?\( 'program)
           (?\[ 'program)
           (?\< 'block)
           (?\{ 'inline)))))))

;; Returns lexing context at pos and the position of
;; enclosing open parenthesis.
(defun satysfi-mode--lexing-context (pos)
  ; find innermost paren with lexing context transition
  (let ((ppss (syntax-ppss pos)))
    (cond
     ((nth 3 ppss) (cons 'string (nth 8 ppss)))
     ((nth 4 ppss) (cons 'comment (nth 8 ppss)))
     (t
      (catch 'exit
        (dolist (p (reverse (nth 9 (syntax-ppss pos))))
          (let ((ctx (get-text-property p 'satysfi-lexing-context)))
            (when ctx
              (throw 'exit (cons ctx p)))))
        (cons 'program 0))))))


;; Check if pos is an active position in block or inline context
;; Returns active command if the position is active
(defun satysfi--active-p (pos &optional skip-block-inline)
  (save-excursion
    (goto-char pos)
    (catch 'exit
      (let* ((ppss (syntax-ppss))
             (bos (nth 1 ppss)))
        ;; never active at toplevel, which is in program context
        (unless bos
          (throw 'exit nil))

        (when skip-block-inline
          (forward-comment (- (buffer-size)))
          (while (memq (preceding-char) '(?\> ?\}))
            ;; backtrack to matching paren (unless the closing paren is escaped)
            (let ((ppss0 (syntax-ppss (1- (point)))))
              (if (eq (nth 0 ppss) (nth 0 ppss0))
                  (throw 'exit nil)
                (goto-char (nth 1 ppss0))))))

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
                    (match-string 0))))))))))

(defun satysfi-mode--active-command (pos)
  (let* ((tmp (satysfi-mode--lexing-context pos))
         (ctx (car tmp))
         (pos (cdr tmp)))
    (if (memq ctx '(block inline))
        (satysfi--active-p pos t))))

(defun satysfi-current-context ()
  (interactive)
  (message "%s" (satysfi-mode--lexing-context (point))))

(defun satysfi-current-activation ()
  (interactive)
  (message (satysfi--active-p (point))))

(defun satysfi-current-command ()
  (interactive)
  (message (satysfi-mode--active-command (point))))

(defvar satysfi-mode-program-keywords-regexp
  (regexp-opt '("let" "let-rec" "let-mutable" "let-inline" "let-block" "let-math" "in" "and"
                "match" "with" "when" "as" "if" "then" "else" "fun"
                "type" "constraint" "val" "direct" "of"
                "module" "struct" "sig" "end" "open"
                "before" "while" "do"
                "controls" "cycle"
                "command" "inline-cmd" "block-cmd" "math-cmd"
                "not" "mod" "true" "false")
              'symbols))

(defvar satysfi-mode-header-keywords-regexp
  (concat (regexp-opt '("@import" "@require") t)
          ":"))

(defvar satysfi-mode-block-commands-regexp
  (rx (| bol (not-char "\\")) (0+ "\\\\") (group "+" (1+ (or (syntax word) (syntax symbol))))))

(defvar satysfi-mode-inline-commands-regexp
  (rx (| bol (not-char "\\")) (0+ "\\\\") (group "\\" (1+ (or (syntax word) (syntax symbol))))))

(defun satysfi-mode--match-contextual-keywords (contexts keywords-regexp)
  (letrec ((re (symbol-value keywords-regexp))
           (matcher
            (lambda (limit)
              (and
               (re-search-forward re limit t)
               (or
                (memq (save-match-data (car (satysfi-mode--lexing-context (point)))) contexts)
                (funcall matcher limit))))))
    matcher))


(defvar satysfi-mode-font-lock-keywords
  `((,(satysfi-mode--match-contextual-keywords '(program) 'satysfi-mode-program-keywords-regexp) 1 satysfi-mode-program-keyword-face)
    (,(satysfi-mode--match-contextual-keywords '(program) 'satysfi-mode-header-keywords-regexp) 1 satysfi-mode-header-keyword-face)
    (,(satysfi-mode--match-contextual-keywords '(block inline) 'satysfi-mode-block-commands-regexp) 1 satysfi-mode-block-command-face)
    (,(satysfi-mode--match-contextual-keywords '(block inline) 'satysfi-mode-inline-commands-regexp) 1 satysfi-mode-inline-command-face)
    (,(satysfi-mode--match-contextual-keywords '(math) 'satysfi-mode-inline-commands-regexp) 1 satysfi-mode-math-command-face))
  "Font-lock keywords for `satysfi-mode'.")

(defun satysfi-mode-syntactic-face (state)
  (if (nth 3 state) satysfi-mode-string-face satysfi-mode-comment-face))

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
      (let ((orig-column (current-column))
            (orig-indent (current-indentation)))
        (indent-line-to (max 0 indent))
        (when (< orig-indent orig-column)
          (move-to-column (+ (current-indentation) (- orig-column orig-indent))))))))

(defun satysfi-mode-find-indent (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (pcase (satysfi-mode--lexing-context (point))
      (`(,'string . ,_) (message "!") nil)  ; TODO: what about indentation in multiline string literals?
      (`(,'comment . ,_) nil)  ; this should never happen, though
      (`(,_ . ,paren-pos)
       (let* ((tmp (satysfi-mode-find-base-indent))
              (indent (car tmp))
              (chain (cdr tmp))
              (command (satysfi-mode--active-command (point)))
              (indent-fun (assoc command satysfi-mode-find-command-indent-function-alist)))
         (if (and chain indent-fun)
             (save-restriction
               (narrow-to-region (1+ paren-pos) (line-end-position))
               (funcall (cdr indent-fun) indent))
           indent))))))

(defun satysfi-mode-find-base-indent ()
  (save-excursion
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (if (not (nth 1 ppss))
          ;; no indent for toplevel
          (cons
           (if (fboundp 'prog-first-column) (prog-first-column) 0)
           nil)
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
                 ;; check if content exists after open paren
                 (let ((line (line-number-at-pos))
                       (column (current-column)))
                   (forward-comment 1)
                   (if (and
                        (eq (line-number-at-pos) line)
                        (not (eq (current-column) (line-end-position))))
                     (current-column))))))
          (cond
           (content-alignment  ; TODO: make vertical alignment configurable?
            (cons content-alignment t))
           ((or (looking-at-p (rx "|)"))
                (eq (syntax-class (syntax-after (point))) 5))  ; 5 for close parenthesis
            (cons open-indentaion nil))
           (t (cons (+ satysfi-basic-offset open-indentaion) t))))))))

(defvar satysfi-mode-find-command-indent-function-alist
  '(("+listing" . satysfi-mode-find-itemize-indent)
    ("\\listing" . satysfi-mode-find-itemize-indent)))

(defun satysfi-mode-find-itemize-indent (first-column)
  (let ((current-column
         (lambda ()
           (if (= (line-number-at-pos (point)) 1)
               (+ (1- first-column) (current-column))
             (current-column)))))
    (save-excursion
      (back-to-indentation)
      (or
       (catch 'exit
         (if (looking-at (rx (1+ ?*)))
             (let ((level (- (match-end 0) (match-beginning 0))))
               (while (not (bobp))
                 (forward-line -1)
                 (back-to-indentation)
                 (when (looking-at (rx (1+ ?*)))
                   (let ((l (- (match-end 0) (match-beginning 0))))
                     (cond
                      ((= l level)
                       (throw 'exit (funcall current-column)))
                      ((< l level)
                       (goto-char (match-end 0))
                       (skip-syntax-forward "-")
                       (throw 'exit (funcall current-column))))))))

           (while (not (bobp))
             (forward-line -1)
             (back-to-indentation)
             (unless (eolp)
               (skip-chars-forward "*")
               (skip-syntax-forward "-")
               (throw 'exit (funcall current-column))))))
       first-column))))

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

(defvar satysfi-mode-block-command-face
  font-lock-function-name-face)
(defvar satysfi-mode-inline-command-face
  font-lock-function-name-face)
(defvar satysfi-mode-math-command-face
  font-lock-function-name-face)
(defvar satysfi-mode-program-keyword-face
  font-lock-keyword-face)
(defvar satysfi-mode-header-keyword-face
  font-lock-keyword-face)
(defvar satysfi-mode-string-face
  font-lock-string-face)
(defvar satysfi-mode-comment-face
  font-lock-comment-face)

;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(saty\\|satyh\\)\\'" . satysfi-mode))

;;;###autoload
(define-derived-mode satysfi-mode prog-mode "SATySFi"
  "Major mode for editing SATySFi document."
  (set-syntax-table satysfi-mode-syntax-table)
  (setq-local syntax-propertize-function #'satysfi-syntax-propertize)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults
              `(satysfi-mode-font-lock-keywords
                nil
                nil
                nil
                (font-lock-syntactic-face-function . satysfi-mode-syntactic-face)))

  ;; for indent
  (setq-local indent-line-function #'satysfi-mode-indent-line)

  ;; for paren
  (setq-local show-paren-data-function #'satysfi-mode-show-paren-data)

  ;; for comment
  (setq-local comment-start "%")
  (setq-local comment-end "")

  ;; for electric
  (setq-local electric-indent-chars '(?\n ?\} ?\> ?\) ?\] ?*))

  (run-mode-hooks 'satysfi-mode-hook))

(provide 'satysfi-mode)

;;; satysfi-mode.el ends here
