(defconst emacs-start-time (current-time))
(defconst dot-root (expand-file-name "~/.configuration/emacs/elisp"))
(defconst emacs-dot-d (expand-file-name "~/.emacs.d"))

(defun add-subdirs-to-load-path (base-dir)
  (when (file-directory-p base-dir)
    (let ((dirs (list base-dir)) (filter "/\\.\\{1,2\\}$"))
      (while dirs
	(let ((files (directory-files (pop dirs) t "[[:alnum:]]+$")))
	  (dolist (file files)
	    (when (file-directory-p file)
	      (add-to-list 'load-path file)
	      (push file dirs))))))))

(add-to-list 'load-path dot-root)
(add-subdirs-to-load-path dot-root)
(add-subdirs-to-load-path emacs-dot-d)

(delete-dups load-path)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'cl)

(load-library "funs")
(load-library "settings")

(eval-when-compile
  (progn (require 'use-package)
         (setq use-package-compute-statistics t)))

(use-package cua-base
  :defer t
  :config (progn (cua-mode t)
                 (setq cua-enable-cua-keys nil)))

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

(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)

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


(defun x-settings (frame)
  (when (display-graphic-p frame)
    (hl-line-mode t))
  (unless (display-graphic-p frame)
    (global-linum-mode -1)))

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
 '(horizontal-scroll-bar-mode nil)
 '(package-selected-packages
   (quote
    (use-package hindent intero 0blayout autopair browse-kill-ring haskell-mode yaml-mode win-switch scala-mode popup-switcher markdown-mode magit lusty-explorer json-mode js2-mode idris-mode ibuffer-vc hlinum go-mode evil erlang django-theme csv-mode)))
 '(safe-local-variable-values (quote ((encoding . utf-8) (allout-layout . t))))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:background "gold" :foreground "black")))))




