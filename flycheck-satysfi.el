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

;;; flycheck-satysfi.el ends here
