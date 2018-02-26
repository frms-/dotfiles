(defconst emacs-start-time (current-time))

(progn
  (defconst initial-gc-cons-threshold  gc-cons-threshold)
  (setq gc-cons-threshold 64000000)
  (add-hook 'after-init-hook (lambda ()
                               (setq gc-cons-threshold initial-gc-cons-threshold)
                               (garbage-collect))))

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'package)
  (setq package-enable-at-startup nil
        package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t))

(load-library "funs")

;;; Settings
(setq inhibit-startup-screen t
      split-height-threshold 60
      split-width-threshold 100
      message-log-max 16384
      show-paren-delay 0
      scroll-step 1
      display-time-24hr-format t
      display-time-day-and-date t
      vc-follow-symlinks nil
      dabbrev-case-fold-search t
      dabbrev-case-fold-search nil
      abbrev-file-name "~/.emacs.d/data/abbrev_defs"
      auto-save-file-namqe-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      make-bakup-files t
      backup-by-copying t
      version-control t
      vc-make-backup-files t
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      auto-save-default t
      bookmark-default-file "~/.emacs.d/data/bookmarks"
      kill-ring-max 1024
      bookmark-save-flag 1
      reb-re-syntax 'string
      flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer
      default-frame-alist '((cursor-color . "white")))

(setq-default show-trailing-whitespace t
              cursor-type 'bar
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
(set-cursor-color "red")
(add-to-list 'auto-mode-alist '("\\.tf$" . conf-mode))
(put 'upcase-region 'disabled nil)

(global-set-key [C-tab] 'hippie-expand)
(global-set-key [(ctrl meta w)] `delete-trailing-whitespace)
(global-set-key [C-left] 'shrink-window-horizontally)
(global-set-key [C-right] 'enlarge-window-horizontally)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'recompile)
(global-set-key [f12] 'next-error)
(global-set-key (kbd "<f11>") 'fm-full-screen-toogle)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-SPC") 'comment-or-uncomment-region)
(global-set-key "\M-K" 'delete-current-line)
(global-set-key (kbd "<f5>") (lambda () (interactive)(find-file "~/.emacs")))
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "<f9>") 'rgrep)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-o") 'split-line)

(use-package cua-base
  :defer t
  :config (progn (cua-mode t)
                 (setq cua-enable-cua-keys nil)))
(use-package csv-mode :ensure :defer t)
(use-package zenburn-theme :ensure)
(use-package dired)
(use-package dired-x :after dired)
(use-package hlinum
  :ensure
  :config (hlinum-activate))

(use-package browse-kill-ring
  :ensure
  :defer t
  :config (progn
	    (browse-kill-ring-default-keybindings)
	    (setq browse-kill-ring-quit-action 'save-and-restore)))

(use-package uniquify
  :config (setq
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

(use-package ibuffer-vc
  :bind ("C-x C-b" . ibuffer)
  :ensure
  :pin melpa
  :defer 5
  :config (add-hook 'ibuffer-mode-hook
                    (lambda ()
                      (ibuffer-switch-to-saved-filter-groups "default")
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))
                      (ibuffer-auto-mode 1)
                      (setq ibuffer-expert nil
                            ibuffer-show-empty-groups nil
                            ibuffer-default-sorting-mode 'major-mode))))

(use-package win-switch
  :ensure
  :bind ("C-x C-o" . win-switch-dispatch)
  :config (progn
            (setq win-switch-idle-time 1.4)
            (setq win-switch-other-window-first nil)))

(use-package hindent :ensure :defer t)

(use-package haskell-mode
  :commands haskell-mode
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  :config
  (add-hook 'haskell-mode-hook
            (lambda()
              (subword-mode 1)
              (turn-on-haskell-indentation)
              (setq tab-width 4
                    haskell-indentation-layout-offset 4
                    haskell-indentation-left-offset 4
                    haskell-indentation-ifte-offset 4)
              (intero-mode))))

(use-package erlang :ensure :defer t :pin melpa)
(use-package erlang-start
  :after erlang
  :defer t
  :config
  (add-hook 'erlang-mode-hook
            (lambda()
              (setq erlang-indent-level 2
                    erlang-electric-commands
                    (remove 'erlang-electric-gt erlang-electric-commands))
                (bind-key "C-<up>" 'erlang-beginning-of-function)
                (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                                    :inherit font-lock-function-name-face
                                    :underline t)
                (setq-local whitespace-style '(face lines-tail))
                (setq-local whitespace-line-column 80)
                (whitespace-mode t)
                (subword-mode t))))

(use-package misc :bind ("M-F" . forward-to-word))

(use-package ido
  :config (progn
            (ido-mode t)
            (setq ido-create-new-buffer 'always
                  ido-file-extensions-order '(".erl" ".hrl" ".hs" ".emacs"  ".sh"))))

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
  :config (add-hook 'grep-setup-hook
                    (lambda()
                      (progn
                        (add-to-list 'grep-find-ignored-directories ".eunit")
                        (add-to-list 'grep-find-ignored-directories ".ct_run.pay*")))))

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
            (font-lock-add-keywords nil
                                    '(("\\<\\(fixme\\|todo\\|note\\|bug\\|qwerty\\|FIXME\\|TODO\\|NOTE\\|BUG\\|QWERTY\\|Fixme\\|Todo\\|Note\\|Bug\\|Qwerty\\)" 1 font-lock-warning-face prepend)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes
   (quote
    ("7e0fe06c91f0902eb8c68737c13a0868f6b800165753d406df0cffcfaf99dc7a" default)))
 '(package-selected-packages
   (quote
    (use-package hindent intero 0blayout autopair browse-kill-ring haskell-mode yaml-mode win-switch scala-mode popup-switcher markdown-mode magit lusty-explorer json-mode js2-mode idris-mode ibuffer-vc hlinum go-mode  erlang  csv-mode)))
 '(safe-local-variable-values (quote ((encoding . utf-8) (allout-layout . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:background "gold" :foreground "black")))))




