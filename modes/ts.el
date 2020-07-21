;;; ts.el --- TypeScript configs
;;; Commentary:
;;; Code:

; Initializes the mode
(mode:initialize "ts")


;; typescript-mode
(use-package typescript-mode
	:ensure t
	:config
		(typescript-mode)
)


;;; ts.el ends here
