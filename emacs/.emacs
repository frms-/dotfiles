(defconst emacs-start-time (current-time))

(defconst initial-gc-cons-threshold gc-cons-threshold)

(setq gc-cons-threshold 64000000)

(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold initial-gc-cons-threshold)
                             (garbage-collect)))

(eval-when-compile
  (setq package-enable-at-startup nil
        package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/"))))
(require 'package)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (let ((default-directory  "~/.emacs.d/lisp/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'use-package)
(setq init-file-debug nil)
(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t))

(setq inhibit-startup-screen t
      split-height-threshold 60
      split-width-threshold 100
      window-combination-resize t
      message-log-max 16384
      read-process-output-max (* 1024 1024) ;; 1mb
      show-paren-delay 0
      scroll-step 1
      org-log-done 'time
      display-time-24hr-format t
      display-time-day-and-date t
      abbrev-file-name "~/.emacs.d/data/abbrev_defs"
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      auto-save-default t
      tags-add-tables nil
      large-file-warning-threshold nil
      bookmark-default-file "~/.emacs.d/data/bookmarks"
      kill-ring-max 1024
      bookmark-save-flag 1
      reb-re-syntax 'string
      indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-eldoc-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode t)
(transient-mark-mode t)
(global-display-line-numbers-mode)
(show-paren-mode t)
(global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)
(display-time)
(savehist-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(set 'frame-title-format '(myltiple-frames "%f" ("" "%f")))
(put 'narrow-to-region 'disabled nil)
(set-face-underline 'font-lock-warning-face "yellow")
(put 'upcase-region 'disabled nil)

(global-set-key [(ctrl meta w)] `delete-trailing-whitespace)
(global-set-key [C-left] 'shrink-window-horizontally)
(global-set-key [C-right] 'enlarge-window-horizontally)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<f5>") (lambda () (interactive)(find-file "~/.emacs")))
(global-set-key (kbd "<f6>") (lambda () (interactive)(find-file "~/org/todo")))
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-*") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-o") 'split-line)

(use-package diminish
  :demand t)

(use-package windmove
  :defer t
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))

(use-package newcomment
  :defer t
  :bind ("C-c C-<SPC>" . comment-or-uncomment-region))

(use-package conf-mode
  :defer t
  :mode  ("\\.tf$" . conf-mode))

(use-package funs
  :commands (x-settings
             insert-org-mode-magic-comment)
  :bind (("C-c k"   . delete-current-line)
         ("C-M-o"   . open-line-below)
         ("C-z"     . maybe-suspend-frame)
         ("C-x C-c" . ask-save-buffers-kill-terminal)
         ("C-,"     . scroll-up-one-line)
         ("C-."     . scroll-down-one-line)
         ("<f11>"   . fm-full-screen-toogle)))

(use-package dabbrev
  :bind ("M-/" . dabbrev-expand)
  :config (setq dabbrev-case-fold-search t
                dabbrev-case-fold-search nil))

(use-package vc-hooks :defer t :config (setq vc-follow-symlinks nil
                                             vc-make-backup-files t))
(use-package cua-base :defer t
  :bind ("C-<return>" . cua-rectangle-mark-mode)
  :init (setq cua-enable-cua-keys nil))
(use-package csv-mode :ensure :defer t :mode "\\.csv\\'")
(use-package zenburn-theme :ensure :disabled)
(use-package atom-one-dark-theme :ensure :disabled)
(use-package spacemacs-common
    :ensure spacemacs-theme
    :init (load-theme 'spacemacs-light t))

(use-package dired :defer)
(use-package dired-x :after dired)
(use-package hlinum :ensure t)

(use-package compile
  :bind (("C-c c" . compile)
         ("C-c r" . recompile)))

(use-package align
  :bind ("C-x a r" . align-regexp))

(use-package browse-kill-ring
  :ensure t
  :bind ("M-y" . browse-kill-ring)
  :config (progn
	    (browse-kill-ring-default-keybindings)
	    (setq browse-kill-ring-quit-action 'save-and-restore)))

(use-package uniquify
  :defer t
  :init(setq
           uniquify-buffer-name-style 'post-forward
           uniquify-separator ":"))

(use-package gtags
  :disabled
  :defer t
  :commands gtags-mode
  :config (progn
	    (add-hook
             'gtags-mode-hook
             (lambda ()
               (local-set-key (kbd "M-.") 'gtags-find-tag)
               (local-set-key (kbd "C-x 4.") 'gtags-find-tag-other-window)
               (local-set-key (kbd "M-;") 'gtags-find-file)
               (local-set-key (kbd "M-,") 'gtags-find-rtag)
               (local-set-key (kbd "M-*") 'gtags-pop-stack)))
	    (add-hook
             'gtags-select-mode-hook
             (lambda ()
               (local-set-key (kbd "RET") 'gtags-select-tag)
               (local-set-key (kbd "M-*") 'gtags-pop-stack)))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (setq ibuffer-expert nil
                      ibuffer-show-empty-groups nil
                      ibuffer-default-sorting-mode 'major-mode)
                (ibuffer-auto-mode 1))))

(use-package ibuffer-vc
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :config (advice-add 'ibuffer-vc-generate-filter-groups-by-vc-root
                      :filter-return
                      #'(lambda (ibuffer-filter-groups)
                          (append '(("*" (name . "^\\*")) ) ibuffer-filter-groups)))

  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))



(use-package win-switch
  :bind ("C-x o" . win-switch-dispatch)
  :disabled
  :config
  (progn (setq win-switch-idle-time 1.4)
         (setq win-switch-other-window-first nil)))

(use-package ace-window
  :ensure
  :bind* ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background nil
        aw-dispatch-always t))

(use-package avy
  :bind* ("C-:" . avy-goto-char-timer)
  :config (avy-setup-default))

(use-package avy-zap
  :bind ("M-z" . avy-zap-to-char-dwim))

(use-package haskell-mode
  :commands haskell-mode
  :defines haskell-indentation-ifte-offset
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))

  :config
  (add-hook 'haskell-mode-hook
            (lambda()
              (subword-mode 1)
              (haskell-indentation-mode)
              (setq tab-width 4
                    haskell-indentation-layout-offset 4
                    haskell-indentation-left-offset 4
                    haskell-indentation-ifte-offset 4))))

(use-package hindent
  :hook (haskell-mode . hindent-mode))

(use-package lsp-haskell)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-enable-snippet nil)
  :hook ((erlang-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (haskell-mode . lsp))
  :config (setq lsp-log-io t
                yas-global-mode t
                lsp-enable-file-watchers t
                lsp-file-watch-threshold 2000)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :commands lsp)

(use-package flycheck
  :config
  (setq flycheck-display-errors-function 'ignore)
  (flycheck-pos-tip-mode)
  (advice-add 'keyboard-quit :before #'flycheck-pos-tip-hide-messages))

(use-package yasnippet
  :defer t
  :after lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom)

  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions]
  ;;   #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references]
  ;;   #'lsp-ui-peek-find-references)

  (defun quit-lsp-ui-doc-frame ()
    (when (and lsp-ui-doc-mode (lsp-ui-doc--frame-visible-p))
      (lsp-ui-doc-hide)))

  (advice-add 'keyboard-quit :before #'quit-lsp-ui-doc-frame)
  (advice-add 'isearch-exit :before #'quit-lsp-ui-doc-frame))

(use-package erlang
  :defer t
  :defines whitespace-style whitespace-line-column
  :bind (("C-<up>" . erlang-beginning-of-function)
         (:map erlang-mode-map
               ("TAB" . company-indent-or-complete-common)
               ))
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :config (add-hook 'erlang-mode-hook
                    (lambda()
                      (setq erlang-indent-level 2
                            erlang-electric-commands
                            (remove 'erlang-electric-gt erlang-electric-commands))
                      (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                                          :inherit font-lock-function-name-face
                                          :underline t)
                      (setq-local whitespace-style '(face lines-tail))
                      (whitespace-mode t)
                      (subword-mode))))

(use-package misc :bind ("M-F" . forward-to-word))

(use-package ido
  :disabled
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window)
         ("C-x C-f" . ido-find-file))
  :config
  (ido-mode)
  (setq ido-create-new-buffer 'always
        ido-file-extensions-order '(".erl" ".hrl" ".hs" ".emacs"  ".sh")))

(use-package ivy
  :diminish
  :demand t
  :bind (("C-x b" . ivy-switch-buffer))
  ;; There already is a way to do this. Just type C-u C-j to exit with the current input ignoring candidates.
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffert t
          ivy-dymanic-exhibit-delay-ms 200
          ivy-height 20
          ivy-wrap t)))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :bind (("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)))

(use-package swiper
  :after ivy
  :bind (:map swiper-map
              ("M-%" . swiper-query-replace))
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package grep
  :defer t
  :bind ("<f9>" . rgrep)
  :config (add-hook 'grep-setup-hook
                    (lambda()
                      (progn
                        (add-to-list 'grep-find-ignored-directories ".eunit")
                        (add-to-list 'grep-find-ignored-directories ".ct_run.pay*")))))

(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :demand
  :config (winner-mode 1))


(use-package iedit
  :bind ("C-;" . iedit-mode)
  :ensure)

(use-package neotree
  :config (progn
            (add-hook 'neo-after-create-hook
                      (lambda (window) (display-line-numbers-mode -1)))
            (setq neo-theme 'ascii
                  neo-smart-open t))
  :defer t
  :ensure
  :bind ("<f8>" . neotree-toggle))

(use-package magit
  :defer 3
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package projectile
  :defer 5
  :diminish
  :config
  (projectile-global-mode)
  (defun my-projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'my-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'my-projectile-invalidate-cache)))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-idle-timer t))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(use-package control-mode
  :disabled
  :bind (("C-x C-z" . global-control-mode)))

(use-package default-text-scale
  ;; font size
  :defer t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M-+" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset)))

(use-package company-dabbrev
  :config
  (setq company-dabbrev-downcase nil))
(use-package company)
(use-package org-bullets
  :ensure
  :config
  :disabled
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package doom-modeline
  :ensure t
  :disabled
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :ensure)

(use-package direnv
  :config  (direnv-mode))

(add-hook 'read-only-mode-hook 'view-mode)

(add-hook 'after-init-hook
	  (lambda ()
	    (let ((delta (float-time (time-subtract (current-time) emacs-start-time))))
	      (message "Loading %s...done (%.3fs)" ".emacs" delta))
	    (x-settings (selected-frame))))

(add-hook 'after-make-frame-functions (lambda (frame)
					(x-settings frame)
                                        (load-theme 'spacemacs-light t)))
(add-hook 'find-file-hook
          (lambda()
            (font-lock-add-keywords nil '(("\\<\\(fixme\\|todo\\|note\\|bug
\\|qwerty\\|FIXME\\|TODO\\|NOTE\\|BUG\\|QWERTY
\\|Fixme\\|Todo\\|Note\\|Bug\\|Qwerty\\)" 1 font-lock-warning-face prepend)))))

(add-hook 'after-change-major-mode-hook
          (lambda()
            (when (not buffer-read-only)
              (setq show-trailing-whitespace t))))

(setq custom-file "~/.emacs.d/lisp/settings.el")
(load custom-file)
