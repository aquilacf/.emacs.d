;;; hooks.el --- Custom hooks.
;;; Commentary:
;;; Code:

; Not in use yet.
;(add-hook 'find-file-hook 'core:load-mode)			; Init mode when a file is loaded.

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

(provide 'hooks)

;;; hooks.el ends here
