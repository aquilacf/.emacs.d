;; -*- lexical-binding: t -*-
;;; core.el --- Core of the framework.
;;; Commentary: This should be loaded first and once.
;;; Code:


;; Remove security vulnerability
(eval-after-load "enriched"
	'(defun enriched-decode-display-prop (start end &optional param)
		(list start end)))


;; Set paths
(setq root (expand-file-name user-emacs-directory))
(setq dir-settings (concat root "configs/settings/"))
(setq dir-snippets (concat root "snippets/"))
;(setq dir-backups (concat root "backups/"))
;(setq dir-autosaves (concat root "autosaves/"))
(setq dir-modes (concat root "modes/"))


;; Global functions

;; Function that loads all files in directory @todo: accept .elc files
(defun load-directory (dir)
	(let ((load-it (lambda (f)
			(load-file (concat (file-name-as-directory dir) f)))))
			(mapc load-it (directory-files dir nil "\\.el$")))
)


;; Initialize mode.
(defun mode:initialize (mode)
  (let (dir-mode)
    (setq dir-mode (concat dir-modes mode "/"))
    (set (intern (concat "dir-mode-" mode)) dir-mode) ; This line creates a dynamic variable dir-mode-'X' where 'X' is the mode. Tip from: https://www.rosettacode.org/wiki/Dynamic_variable_names#Emacs_Lisp
		(unless (file-directory-p dir-mode)
			(make-directory dir-mode))) ; @todo Is this really necessary?
)


;; Download a file from the internet
(defun core:download (url p)
  "Download any file from the internet.  It doesn't override the existing file."

  (unless (file-exists-p p)
    (url-retrieve url
		  (lambda (s)
		    (re-search-forward "\r?\n\r?\n")
		    (write-region (point) (point-max) p)))
    )
)




;; Load other less important configs
(load-directory dir-settings)


;; Turn off backups and autosaves. @todo: Improve this
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-trailing-lines nil) ; Prevent from deleting EOF line
(add-hook 'before-save-hook 'delete-trailing-whitespace)

