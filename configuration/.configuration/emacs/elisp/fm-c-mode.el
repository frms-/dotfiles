(add-hook 'c-mode-hook
	  (lambda ()
	    (gtags-mode t)
	    (setq c-basic-offset 4)))
