# satysfi.el
Emacs major mode for editing [SATySFi](https://github.com/gfngfn/SATySFi) document.

```elisp
(add-to-list 'auto-mode-alist '("\\.\\(saty\\|satyh\\)\\'" . satysfi-mode))
(autoload 'satysfi-mode "satysfi-mode" "Major mode for editing SATySFi document." t)
(autoload 'flycheck-satysfi-setup "flycheck-satysfi" "Setup Flycheck checker for SATySFi." t)

(add-hook 'satysfi-mode-hook
          (lambda ()
            (flycheck-satysfi-setup)
            (flycheck-mode)))
```

Features implemented so far:
- Parentheses matching (<kbd>C-M-f</kbd> and <kbd>C-M-b</kbd>)
- Contextual syntax highlighting (eg. only highlight `let` in program segments)
- Structure-based auto indentation
- [Flycheck](https://github.com/flycheck/flycheck) integration

To-Dos:
- Auto indentation in `\listing{}`
- Automatic parentheses pairing
- Auto indentation in program segments
