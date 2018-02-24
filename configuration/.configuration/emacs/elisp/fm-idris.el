(defun my-idris-repl-hook()
  (setq show-trailing-whitespace nil)
  (linum-mode false))
(add-hook 'idris-repl-mode-hook 'my-idris-repl-hook)

(defun my-idris-hook()
  (unbind-key "C-c C-SPC" idris-mode-map)
  (bind-key "C-c C-SPC" 'comment-or-uncomment-region idris-mode-map))
(add-hook 'idris-mode-hook 'my-idris-hook)
