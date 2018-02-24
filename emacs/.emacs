(defconst emacs-start-time (current-time))
(defconst dot-root (expand-file-name "~/.configuration/emacs/elisp"))
(defconst emacs-dot-d (expand-file-name "~/.emacs.d"))

;; (condition-case err
;;     (let ((path "~/.configuration/emacs/elisp/lib"))
;;       (add-to-list 'custom-theme-load-path path)
;;       (load-theme 'zenburn t))
;;   (error (message "Failed to load zenburn theme: %s" err)))

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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package zenburn-theme
  :ensure)

;; (require 'diminish)
;; (require 'bind-key)

(require 'cl)

(use-package dired-x)

(load-library "funs")
(load-library "settings")
;(load-library "fm-haskell")
;;(load-library "fm-erlang")

(use-package haskell-mode
  :ensure)

(use-package browse-kill-ring
  :ensure
  :config (progn
	    (browse-kill-ring-default-keybindings)
	    (setq browse-kill-ring-quit-action 'save-and-restore)))

(use-package autopair
  :ensure
  :config (progn
	    (setq autopair-autowrap t)
	    (add-hook 'c-mode-hook  #'(lambda () autopair-mode))
	    (add-hook 'java-mode-hook #'(lambda () autopair-mode))))

(use-package uniquify
  :init (setq
	 uniquify-buffer-name-style 'post-forward
	 uniquify-separator ":"))

(use-package gtags
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
                (ibuffer-switch-to-saved-filter-groups "default")
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))
                (ibuffer-auto-mode 1)
                (setq ibuffer-expert nil)
                (setq ibuffer-show-empty-groups nil)
                (setq ibuffer-default-sorting-mode 'major-mode))))

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

(setq default-frame-alist '((cursor-color . "white")))

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
 '(edts-inhibit-package-check t)
 '(edts-man-root "/home/frms/.emacs.d/edts/doc/R16B03-1")
 '(horizontal-scroll-bar-mode nil)
 '(package-selected-packages
   (quote
    (autopair browse-kill-ring haskell-mode yaml-mode win-switch scala-mode popup-switcher markdown-mode magit lusty-explorer json-mode js2-mode intero idris-mode ibuffer-vc hlinum go-mode evil erlang django-theme csv-mode)))
 '(safe-local-variable-values (quote ((encoding . utf-8) (allout-layout . t))))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:background "gold" :foreground "black")))))

(defun my-grep-hook ()
  (add-to-list 'grep-find-ignored-directories ".eunit")
  (add-to-list 'grep-find-ignored-directories ".ct_run.pay*"))

(add-hook 'grep-setup-hook 'my-grep-hook)
(put 'upcase-region 'disabled nil)

