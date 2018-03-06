;; miscellaneous functions

(defun nuke-all-buffers ()
  "kill all buffers, leaving scratch only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)))

(defun fm-full-screen-toogle ()
  "toggle full-screen-mode"
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defmacro fm/require-configure (library &optional &rest body)
  `(condition-case err
       (progn
	 (require ,library)
	 ,@body)
     (error (warn "Failed to load and configure %s: %s" ,library err))))

(defun swap-windows ()
  "Visit buffer of other window in this window and visit this buffer in other window."
  (interactive)
  (other-window 1)
  (switch-to-buffer (other-buffer (current-buffer) t))
  (other-window -1)
  (switch-to-buffer (other-buffer)))

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((p (point)))
    (beginning-of-line)
    (kill-line arg)
    (end-of-line)
    (goto-char (min p (point)))))

(defun open-line-below (n)
  "Insert a newline at the end of the current line and leave point unchanged.
With arg N, insert N newlines."
  (interactive "*p")
  (cl-flet ((move (n)
	       (if (< n 0)
		   (beginning-of-line)
		 (end-of-line))))
    (save-excursion
      (move n)
      (newline (abs n)))))

(defun java-string-to-sql ()
  (interactive)
  (let ((from (if mark-active
                  (region-beginning)
                (point-min)))
        (to (if mark-active
                (region-end)
              (point-max))))
    (replace-regexp "^[ \t=+]*\"" "" nil from to)
    (replace-regexp "\"[ ;+]*$" "" nil from to))
  (highlight-regexp "\?")
  (delete-trailing-whitespace))

(defun maybe-suspend-frame ()
  (interactive)
  (if (display-graphic-p (selected-frame))
      (message "suspend-frame deactivated in graphic display")
    (suspend-frame)))

(defun x-settings (frame)
  (when (display-graphic-p frame)
    (global-hl-line-mode t)))

(defun ask-save-buffers-kill-terminal ()
  (interactive)
  (when (y-or-n-p "Really exit emacs? ")
    (save-buffers-kill-terminal)))

(defun fm-unbind-key (key-name function-name &optional keymap)
  (unbind-key key-name keymap))

(defun my-colours-set ()
  (interactive)
  (set-background-color "grey15")
  (set-foreground-color '"grey75")
  (set-cursor-color "red"))

(defun my-colours-disable-all-themes ()
  (interactive)
  (mapc (lambda (theme) (disable-theme theme))
        custom-enabled-themes))

(defun my-colours-theme (theme)
  (interactive
   (list (intern (completing-read
                  "Set custom theme: "
                  '(("solarized-dark" 1) ("solarized-light" 2) ("zenburn" 3)
                    ("default" 4) ("my-solarized" 5))
                  nil t))))
    (my-colours-disable-all-themes)
    (cond ((eq theme 'default)
           (my-colours-set))
          ((eq theme 'my-solarized)
           (load-theme 'solarized-dark t)
           (my-colours-set))
          (t
           (load-theme theme t nil))))

;; Define C-, and C-. as scroll-up and scroll-down
(defun scroll-up-one-line ()
  (interactive)
  (scroll-up 1))
(defun scroll-down-one-line ()
  (interactive)
  (scroll-down 1))
(defun scroll-other-window-up-one-line ()
  (interactive)
  (scroll-other-window 1))
(defun scroll-other-window-down-one-line ()
  (interactive)
  (scroll-other-window -1))

(defun close-mru-non-selected-window ()
  (interactive)
  (let ((window (get-mru-window (selected-window) t t)))
    (delete-window window)))

(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(defun x-settings (frame)
  (when (display-graphic-p frame)
    (hl-line-mode t))
  (unless (display-graphic-p frame)
    (global-linum-mode -1)))

(defun insert-org-mode-magic-comment ()
  (interactive)
  (save-excursion
    (progn
      (goto-char (point-min))
      (insert "-*- mode: org -*-\n"))))

(provide 'funs)
