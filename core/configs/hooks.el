;;; hooks.el --- Custom hooks.
;;; Commentary:
;;; Code:

(add-hook 'find-file-hook 'core:load-mode)			; Init mode when a file is loaded.

(provide 'hooks)

;;; hooks.el ends here
