(defconst emacs-start-time (current-time))

(defconst initial-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold initial-gc-cons-threshold)
                             (garbage-collect)))

(eval-when-compile
  (setq package-enable-at-startup nil
        package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/"))))
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

(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t))

(setq inhibit-startup-screen t
      split-height-threshold 60
      split-width-threshold 100
      message-log-max 16384
      show-paren-delay 0
      scroll-step 1
      org-log-done 'time
      display-time-24hr-format t
      display-time-day-and-date t
      abbrev-file-name "~/.emacs.d/data/abbrev_defs"
      auto-save-file-namqe-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      version-control t
      kept-new-versions 8
      kept-old-versions 4
      auto-save-default t
      bookmark-default-file "~/.emacs.d/data/bookmarks"
      kill-ring-max 1024
      bookmark-save-flag 1
      reb-re-syntax 'string
      default-frame-alist '((cursor-color . "white")))

(setq-default show-trailing-whitespace t
	      indent-tabs-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode t)
(transient-mark-mode t)
(global-linum-mode)
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
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-*") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-o") 'split-line)

(use-package windmove
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))

(use-package newcomment
  :bind ("C-c C-<SPC>" . comment-or-uncomment-region)
  :defer t)

(use-package conf-mode
  :defer t
  :mode  ("\\.tf$" . conf-mode))

(use-package funs
  :commands (x-settings
             insert-org-mode-magic-comment)
  :bind (("M-s"     . swap-windows)
         ("C-c k"   . delete-current-line)
         ("C-M-o"   . open-line-below)
         ("C-z"     . maybe-suspend-frame)
         ("C-x C-c" . ask-save-buffers-kill-terminal)
         ("C-,"     . scroll-up-one-line)
         ("C-."     . scroll-down-one-line)
         ("C-;"     . scroll-other-window-up-one-line)
         ("C-:"     . scroll-other-window-down-one-line)
         ("<f2>"    . close-mru-non-selected-window)
         ("<f11>"    . fm-full-screen-toogle)))

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
(use-package zenburn-theme :ensure)
(use-package dired :defer)
(use-package dired-x :after dired)
(use-package hlinum :ensure t)

(use-package compile
  :bind (("C-c c" . compile) ("C-c r" . recompile)))

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
  :ensure t
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (progn (setq win-switch-idle-time 1.4)
         (setq win-switch-other-window-first nil)))

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

(use-package intero
  :defer t
  :hook (haskell-mode . intero-mode)
  :config (use-package flycheck
            :config (setq flycheck-display-errors-function
                          #'flycheck-display-error-messages-unless-error-buffer)
            (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
  :ensure t)

(use-package erlang
  :defer t
  :defines whitespace-style whitespace-line-column
  :bind ("C-<up>" . erlang-beginning-of-function)
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :interpreter "escript"
  :config (progn
            (setq erlang-indent-level 2
                  erlang-electric-commands
                  (remove 'erlang-electric-gt erlang-electric-commands))
            (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                                :inherit font-lock-function-name-face
                                :underline t)
            (setq-local whitespace-style '(face lines-tail))
            (setq-local whitespace-line-column 80)
            (whitespace-mode t)
            (subword-mode t)))

(use-package misc :bind ("M-F" . forward-to-word))

(use-package ido
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window)
         ("C-x C-f" . ido-find-file))
  :config
  (ido-mode)
  (setq ido-create-new-buffer 'always
        ido-file-extensions-order '(".erl" ".hrl" ".hs" ".emacs"  ".sh")))

(use-package edts-start
  :disabled
  :load-path "~/src/edts"
  :config (progn
            (edts-log-set-level 'debug)
            (unbind-key "M--" auto-highlight-symbol-mode-map)
            (bind-key "M-n" 'edts-code-next-issue edts-mode-map)
            (bind-key "M-p" 'edts-code-previous-issue edts-mode-map)
            (unbind-key "M-," edts-mode-map)
            (bind-key "M-*" 'edts-find-source-unwind edts-mode-map)
            (unbind-key "M-<left>" auto-highlight-symbol-mode-map)
            (unbind-key "M-<right>" auto-highlight-symbol-mode-map))
  :init (setq edts-inhibit-package-check t))

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
  :bind (("C-c C-;" . iedit-mode))
  :ensure)

(add-hook 'after-init-hook
	  (lambda ()
	    (let ((delta (float-time (time-subtract (current-time) emacs-start-time))))
	      (message "Loading %s...done (%.3fs)" ".emacs" delta))
	    (x-settings (selected-frame))))


(add-hook 'after-make-frame-functions (lambda (frame)
					(x-settings frame)
                                        (load-theme 'zenburn t)))
(add-hook 'find-file-hook
          (lambda()
            (font-lock-add-keywords nil '(("\\<\\(fixme\\|todo\\|note\\|bug
\\|qwerty\\|FIXME\\|TODO\\|NOTE\\|BUG\\|QWERTY
\\|Fixme\\|Todo\\|Note\\|Bug\\|Qwerty\\)" 1 font-lock-warning-face prepend)))))

(setq custom-file "~/.emacs.d/lisp/settings.el")
(load custom-file)
