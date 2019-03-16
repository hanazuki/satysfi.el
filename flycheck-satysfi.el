;;; flycheck-satysfi.el --- Flycheck checker definition for SATySFi  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Kasumi Hanazuki

;; Author: Kasumi Hanazuki <kasumi@rollingapple.net>
;; Package-Requires: (flycheck)
;; Keywords: satysfi, flycheck
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

(require 'flycheck)

(defvar flycheck-satysfi-typecheck-only
  t)
(defvar flycheck-satysfi-use-bytecompiler
  t)

(defun flycheck-satysfi-parse-errors (output checker buffer)
  ;; Errors and warnings are marked with ! at BoL.
  ;; Messages follow with 4-space indentation.
  (let ((errors)
        (error-regexp
         (rx bol "! [" (group (0+ nonl)) "] at \"" (group (0+ nonl))
             "\", line " (group (1+ digit)) ", character" (? "s") " " (group (1+ digit))
             )))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward error-regexp nil t)
        (let* ((type (match-string 1))
               (file (match-string 2))
               (line (match-string 3))
               (column (match-string 4))
               (message (concat type ": "))
               (level (cond
                       ((string-match-p "Error" type) 'error)
                       ((string-match-p "Warning" type) 'warning)
                       (t 'info))))
          (forward-line +1)
          (while (looking-at-p "    ")
            (setq message (concat message (buffer-substring (+ (point) 4) (1+ (line-end-position)))))
            (forward-line +1))
          (push (flycheck-error-new-at
                 (flycheck-string-to-number-safe line)
                 (1+ (flycheck-string-to-number-safe column))
                 level
                 message
                 :filename file
                 :checker checker
                 :buffer buffer)
                errors)))
      (reverse errors))))

(flycheck-define-checker satysfi
  "A syntax checker for SATySFi."
  :command ("satysfi"
            "-o" "/dev/null"  ; TODO: Windows?
            (option-flag "-t" flycheck-satysfi-typecheck-only)
            (option-flag "-t" flycheck-satysfi-use-bytecompiler)
            source-inplace)
  :error-parser flycheck-satysfi-parse-errors
  :modes satysfi-mode)

;;;###autoload
(defun flycheck-satysfi-setup ()
  "Setup Flycheck checker for SATySFi."
  (interactive)
  (add-to-list 'flycheck-checkers 'satysfi))

(provide 'flycheck-satysfi)

;;; flycheck-satysfi.el ends here
