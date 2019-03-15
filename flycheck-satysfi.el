(require 'flycheck)

(defvar flycheck-satysfi-typecheck-only
  nil)

(flycheck-define-checker satysfi
  "A syntax checker for SATySFi."
  :command ("satysfi"
            "-o" "/dev/null"  ; TODO: Windows?
            (option-flag "-t" flycheck-satysfi-typecheck-only)
            source-inplace)
  :error-patterns
  ((error line-start
          "! [" (0+ nonl) "] at \"" (file-name) "\", line " line ", characters " column (? (: "-" (1+ digit))) ":"
          (message (0+ (: "\n" (not-char "!") (0+ nonl))))
          line-end))
  :modes satysfi-mode)

;;;###autoload
(defun flycheck-satysfi-setup ()
  "Setup Flycheck checker for SATySFi."
  (interactive)
  (add-to-list 'flycheck-checkers 'satysfi))

(provide 'flycheck-satysfi)
