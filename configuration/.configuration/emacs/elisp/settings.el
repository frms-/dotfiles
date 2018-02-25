;;; Settings
;;; Depends: functions.el
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
      abbrev-file-name "~/.configuration/emacs/data/abbrev_defs"
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
      bookmark-default-file "~/.configuration/emacs/data/bookmarks"
      kill-ring-max 1024
      bookmark-save-flag 1
      reb-re-syntax 'string
      default-frame-alist '((cursor-color . "white")))

(setq-default show-trailing-whitespace t
              cursor-type 'bar
              indent-tabs-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode t)
(transient-mark-mode t)
(global-linum-mode t)
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
