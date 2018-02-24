(add-to-list 'load-path "~/src/ghc-mod/elisp")

(defun my-haskell-hook ()
  (subword-mode 1)
  (turn-on-haskell-indentation)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4)
  ;;(ghc-init)
  ;; (setq ghc-debug t)
  ;; (fm-unbind-key "M-t" 'ghc-insert-template haskell-mode-map)
  ;; (fm-unbind-key "M-s" 'ghc-sort-key haskell-mode-map)
  ;; (bind-key "C-c C-s" 'ghc-insert-template haskell-mode-map)
  ;; (fm-unbind-key "C-M-d" 'ghc-browse-document haskell-mode-map)
  ;; (unbind-key "C-M-i")
  ;; (bind-key "C-<tab>" 'ghc-complete)

  ;; (setq ghc-display-error 'minibuffer)
  (intero-mode))

;;(add-hook 'haskell-mode 'intero-mode)

                                        ;(require 'haskell-mode-autoloads)
(use-package hindent
  )
(use-package haskell-mode
  :commands haskell-mode
  :init (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode)))


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook 'my-haskell-hook)

(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)
