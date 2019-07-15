;;; php.el --- C/C++ configs
;;; Commentary:
;;; Code:

(mode:initialize "php")

;; Download php-mode
(use-package php-mode
	:ensure t
	:config
		(php-mode)
)

(use-package geben
	:ensure t
	:config
	(progn
		(setq-default geben-temporary-file-directory
			(concat dir-cache "geben"))))



;;; php.el ends here

