;; Remove security vulnerability @todo: is it actually needed?
(eval-after-load "enriched"
	'(defun enriched-decode-display-prop (start end &optional param)
		(list start end)))


;; Set paths
(setq root (expand-file-name user-emacs-directory))
(setq dir-settings (concat root "configs/settings"))
(setq dir-snippets (concat root "snippets/"))
;(setq dir-backups (concat root "backups/"))
;(setq dir-autosaves (concat root "autosaves/"))
(setq dir-modes (concat root "modes/"))



;; Function that loads all files in directory @todo: accept .elc files
(defun load-directory (dir)
	(let ((load-it (lambda (f)
			(load-file (concat (file-name-as-directory dir) f)))))
			(mapc load-it (directory-files dir nil "\\.el$")))
)

;; Initialize mode @todo: setq dir-mode-X
(defun mode:initialize (mode)
	(let (dir-mode)
		(setq dir-mode (concat dir-modes mode))
		(unless (file-directory-p dir-mode)
			(make-directory dir-mode)))
)




; Download @todo: fix p
(defun core:download (url p)
 "Download any file"
 (url-retrieve url
		  (lambda (s)
		    (re-search-forward "\r?\n\r?\n")
		    (write-region (point) (point-max) p)))
 )



;; Other configs
(load-directory dir-settings)



;; Turn off backups and autosaves. @todo: Improve this
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-trailing-lines nil) ; Prevent from deleting EOF line
(add-hook 'before-save-hook 'delete-trailing-whitespace)



