;;; php.el --- C/C++ configs
;;; Commentary:
;;; Code:

(mode:initialize "php")

;; php-mode
(use-package php-mode
	:ensure t
	:config
		(php-mode)
		(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
		(setq php-template-compatibility nil)
		(setq php-lineup-cascaded-calls t)
		;@todo: download documentation locally
)

; Xdebug
; Read more: https://github.com/ahungry/geben
(use-package geben
	:ensure t
	:config
	(progn
		(setq-default geben-temporary-file-directory
			(concat dir-cache "geben"))))



;;; php.el ends here
