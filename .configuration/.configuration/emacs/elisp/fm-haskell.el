(add-to-list 'load-path "~/src/ghc-mod/elisp")

(defun my-haskell-hook ()
  (subword-mode 1)
  (turn-on-haskell-indentation)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4)
  (intero-mode))

(use-package hindent)
(use-package haskell-mode
  :commands haskell-mode
  :init (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode)))


(add-hook 'haskell-mode-hook 'my-haskell-hook)
(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
