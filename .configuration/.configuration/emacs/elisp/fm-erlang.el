(add-to-list 'load-path (concat (getenv "ERLANG_HOME") "/elisp"))
;; (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.12/emacs/")

(setq edts-inhibit-package-check t)

(defun my-erlang-mode-hook ()
  (setq erlang-indent-level 2)
  (bind-key "C-<up>" 'erlang-beginning-of-function)
  (subword-mode t)
  (setq erlang-electric-commands (remove 'erlang-electric-gt erlang-electric-commands))
  (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                      :inherit font-lock-function-name-face
                      :underline t)
  (setq-local whitespace-style '(face lines-tail))
  (setq-local whitespace-line-column 80)
  (whitespace-mode t))

(use-package erlang-start
  :init
  (progn
    (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)))

(defconst erl-root (expand-file-name "~/.emacs.d/elpa"))
(add-subdirs-to-load-path erl-root)

(add-to-list 'load-path erl-root)
(require 'erlang-start)

(add-to-list 'load-path "~/src/edts")
(use-package edts-start
  :config (progn
            (edts-log-set-level 'debug)
            (unbind-key "M--" auto-highlight-symbol-mode-map)
            (bind-key "M-n" 'edts-code-next-issue edts-mode-map)
            (bind-key "M-p" 'edts-code-previous-issue edts-mode-map)
            (unbind-key "M-," edts-mode-map)
            (bind-key "M-*" 'edts-find-source-unwind edts-mode-map)
            (unbind-key "M-<left>" auto-highlight-symbol-mode-map)
            (unbind-key "M-<right>" auto-highlight-symbol-mode-map))
;;  :defer t
  :init
  (progn
    (setq edts-inhibit-package-check t)))



