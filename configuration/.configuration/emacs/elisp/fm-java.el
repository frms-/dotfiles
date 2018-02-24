;; Make java mode support Java 1.5 annotations.
(condition-case nil
    (progn
      (require 'java-mode-indent-annotations)
      (add-hook 'java-mode-hook
		(lambda ()
		  (java-mode-indent-annotations-setup))))
  (error (message "Failed to load java-mode-indent-annotations")))

;; Show lines longer then 100 columns
(add-hook
 'java-mode-hook
 (lambda ()
   (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|NOTE\\|BUG\\)" 1 font-lock-warning-face prepend)))))

(add-hook 'java-mode-hook
	  (lambda ()
		 (gtags-mode 1)))
