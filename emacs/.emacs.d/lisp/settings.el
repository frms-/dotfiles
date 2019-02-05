(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("7e0fe06c91f0902eb8c68737c13a0868f6b800165753d406df0cffcfaf99dc7a" default)))
 '(fill-column 101)
 '(flycheck-hlint-ignore-rules (quote ("\"eta reduce\"")))
 '(global-hl-fill-column-mode t)
 '(intero-blacklist (quote ("/home/frms/src/fp-course")))
 '(package-selected-packages
   (quote
    (diminish counsel-projectile projectile counsel swiper ivy which-key hl-fill-column adoc-mode zenburn-theme lsp-haskell lsp-mode neotree groovy-mode dockerfile-mode js2-mode iedit ledit intero-mode use-package hindent intero 0blayout autopair browse-kill-ring haskell-mode yaml-mode win-switch scala-mode popup-switcher markdown-mode magit lusty-explorer json-mode idris-mode ibuffer-vc hlinum go-mode erlang csv-mode)))
 '(safe-local-variable-values
   (quote
    ((intero-targets "generate:lib" "generate:test:tests")
     (erlang-indent-level . 2)
     (intero-targets "reports:lib" "reports:exe:report" "reports:test:report-test")
     (encoding . utf-8)
     (allout-layout . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:underline nil))))
 '(compilation-error ((t nil)))
 '(flycheck-error ((t nil)))
 '(flycheck-error-list-error ((t (:inherit error :underline nil))))
 '(flycheck-error-list-highlight ((t (:inherit highlight :underline nil))))
 '(flycheck-error-list-id-with-explainer ((t (:inherit flycheck-error-list-id :box (:line-width 1 :style released-button) :underline nil))))
 '(flycheck-error-list-warning ((t (:inherit warning :underline nil))))
 '(hl-fill-column-face ((t (:background "gray26" :foreground "red1"))))
 '(linum-highlight-face ((t (:background "gold" :foreground "black"))))
 '(success ((t (:foreground "#7F9F7F" :underline nil :weight bold)))))
