;;; Settings
;;; Depends: functions.el
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq split-height-threshold 60)
(setq split-width-threshold 100)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode t)
(setq message-log-max 16384)
(transient-mark-mode t)
(global-linum-mode t)
;;(require 'hlinum)
(hlinum-activate)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq-default show-trailing-whitespace t)
(setq-default cursor-type 'bar)
(setq scroll-step 1)
(global-hl-line-mode t)
(setq-default indent-tabs-mode nil)
(line-number-mode t)
(column-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq vc-follow-symlinks nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-fold-search nil)
(set 'frame-title-format '(myltiple-frames "%f" ("" "%f"))) ;; Show filename in titlebar

(if (cua-mode t)
    (setq cua-enable-cua-keys nil))
(setq abbrev-file-name
      "~/.configuration/emacs/data/abbrev_defs")

(put 'narrow-to-region 'disabled nil)
(set-face-underline 'font-lock-warning-face "yellow")
(savehist-mode t)
(global-set-key [C-tab] 'hippie-expand)
(global-set-key [(ctrl meta w)] `delete-trailing-whitespace)
(global-set-key [C-left] 'shrink-window-horizontally)
(global-set-key [C-right] 'enlarge-window-horizontally)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
;;(global-set-key "\C-x\C-b" 'buffer-menu); do buffer selection in active window
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
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-bakup-files t
      backup-by-copying t
      version-control t
      vc-make-backup-files t
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      auto-save-default t)

(setq bookmark-default-file "~/.configuration/emacs/data/bookmarks"
      bookmark-save-flag 1)

(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".erl" ".hrl" ".hs" ".emacs"  ".sh"))
(ido-mode t)
(set-cursor-color "red")
(setq kill-ring-max 1024)
(add-to-list 'auto-mode-alist '("\\.tf$" . conf-mode))
(setq reb-re-syntax 'string)
;(magit-auto-revert-mode -1)
