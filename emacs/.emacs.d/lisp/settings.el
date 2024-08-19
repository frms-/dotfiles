(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(counsel-projectile-switch-project-action
   '(4
     ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
     ("D" counsel-projectile-switch-project-action-dired "open project in dired")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
     ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure "run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
     ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
     ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
     ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
     ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
     ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
     ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))
 '(custom-safe-themes
   '("9b4ae6aa7581d529e20e5e503208316c5ef4c7005be49fdb06e5d07160b67adc" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "7e0fe06c91f0902eb8c68737c13a0868f6b800165753d406df0cffcfaf99dc7a" default))
 '(fci-rule-color "#3E4451")
 '(fill-column 101)
 '(flycheck-hlint-ignore-rules '("\"eta reduce\""))
 '(global-hl-fill-column-mode t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ignored-local-variable-values '((apheleia-formatter . nixfmt)))
 '(intero-blacklist '("/home/frms/src/fp-course"))
 '(js-indent-level 2)
 '(lsp-haskell-server-args '("-d" "-l" "/tmp/hls.log"))
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-ui-doc-delay 0.4)
 '(magit-blame-styles
   '((headings
      (heading-format . "%-20a %C %s %H
"))
     (margin
      (margin-format " %s%f" " %C %a" " %H")
      (margin-width . 42)
      (margin-face . magit-blame-margin)
      (margin-body-face magit-blame-dimmed))
     (highlight
      (highlight-face . magit-blame-highlight))
     (lines
      (show-lines . t)
      (show-message . t))))
 '(magit-prefer-remote-upstream t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/org/todo"))
 '(package-selected-packages
   '(wgrep-deadgrep deadgrep uuid restclient-jq jq-mode restclient wgrep nasm-mode rainbow-blocks rainbow-delimiters org-indent-mode jenkinsfile-mode dash dash-functional flycheck-rust rust-mode atom-dark-theme flycheck direnv flycheck-pos-tip flycheck-color-mode-line nix-sandbox lsp-ivy company flx nix-mode git-timemachine htmlize org orgalist all-the-icons doom-modeline org-bullets spacemacs-common spacemacs-light spacemacs-theme yasnippet company-lsp lsp-ui atom-one-dark-theme avy-zap avy ace-window terraform-mode nord-theme lua-mode default-text-scale control-mode typescript-mode evil-tutor diminish counsel-projectile projectile counsel swiper ivy which-key hl-fill-column adoc-mode zenburn-theme lsp-haskell lsp-mode neotree groovy-mode dockerfile-mode js2-mode iedit ledit intero-mode use-package hindent intero 0blayout autopair browse-kill-ring haskell-mode yaml-mode win-switch scala-mode popup-switcher markdown-mode magit lusty-explorer json-mode idris-mode ibuffer-vc hlinum go-mode erlang csv-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (intero-targets "generate:lib" "generate:test:tests")
     (erlang-indent-level . 2)
     (intero-targets "reports:lib" "reports:exe:report" "reports:test:report-test")
     (encoding . utf-8)
     (allout-layout . t)))
 '(suggest-key-bindings nil)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(warning-suppress-types '((comp) (comp) (comp) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 138))))
 '(aw-leading-char-face ((t (:inherit aw-mode-line-face :foreground "orange" :height 2.0))))
 '(button ((t (:underline t))))
 '(compilation-error ((t nil)))
 '(deadgrep-meta-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-error ((t nil)))
 '(flycheck-error-list-error ((t (:inherit error :underline nil))))
 '(flycheck-error-list-highlight ((t (:inherit highlight :underline nil))))
 '(flycheck-error-list-id-with-explainer ((t (:inherit flycheck-error-list-id :box (:line-width 1 :style released-button) :underline nil))))
 '(flycheck-error-list-warning ((t (:inherit warning :underline nil))))
 '(flycheck-fringe-warning ((t (:inherit bold :foreground "gold"))))
 '(flycheck-warning ((t (:underline "gold"))))
 '(font-lock-comment-face ((t nil)))
 '(hl-fill-column-face ((t (:background "gray26" :foreground "red1"))))
 '(linum-highlight-face ((t (:background "gold" :foreground "black"))))
 '(success ((t (:foreground "#7F9F7F" :underline nil :weight bold)))))
