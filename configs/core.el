;; -*- lexical-binding: t -*-
;;; core.el --- Core of the framework.
;;; Commentary: This should be loaded first and once.
;;; Code:

;; Remove security vulnerability
(eval-after-load "enriched"
	'(defun enriched-decode-display-prop (start end &optional param)
		(list start end)))



;; Sets

(setq root (expand-file-name user-emacs-directory))
(setq dir-settings (concat root "configs/settings/"))
;(setq dir-snippets (concat root "snippets/"))
(setq dir-configs (concat root "configs/"))
;(setq dir-backups (concat root "backups/"))
;(setq dir-autosaves (concat root "autosaves/"))
(setq dir-modes (concat root "modes/"))
(setq dir-cache (concat root "cache/"))
(setq custom-file (concat dir-cache "priv.custom.el")) ; Remove custom-set-variables from init.el

;; Turn off backups and autosaves. @todo: Improve this
(setq make-backup-files nil)
(setq auto-save-default nil)



;; Core functions

; Function that loads all files in directory.
(defun core:load-directory (dir)
  "Load all elips files in dir, .elc version will be loaded over .el. Do not delete .el files."

  (let ((load-it (lambda (f)
                    (load (concat (file-name-as-directory dir) (file-name-base f))))
        ))
    (mapc load-it (directory-files dir nil "\\.el$"))
  )
)

;; Download a file from the internet.
(defun core:download (url p)
  "Download any file from the internet.  It doesn't override the existing file."

  (unless (file-exists-p p)
    (url-retrieve url
      (lambda (s)
        (re-search-forward "\r?\n\r?\n")
        (write-region (point) (point-max) p)))
    )
)

;; Initialize modes.
(defun core:load-mode ()
  "Initialize mode based on the file extension. Mode most exist in modes/.el."

  (let (f)
    (setq f (concat dir-modes (file-name-extension buffer-file-name)))

    (if (file-exists-p (concat f ".el"))
      (load f)
    )
  )
)

;; Compile project
(defun core:compile()
  "Compile all .el files in the project after first run."
  (let (f)
    (setq f (concat dir-cache "priv.compiled"))
    (unless (file-exists-p f)
      ;@todo: add the function body here
      ;(byte-recompile-directory root)
      (write-region nil "" f))
    )
)



;; Mode functions

; Initialize mode.
(defun mode:initialize (mode)
  (let (dir-mode)
    (setq dir-mode (concat dir-modes mode "/"))
    (set (intern (concat "dir-mode-" mode)) dir-mode) ; This line creates a dynamic variable dir-mode-'X' where 'X' is the mode. Tip from: https://www.rosettacode.org/wiki/Dynamic_variable_names#Emacs_Lisp
		(unless (file-directory-p dir-mode)
			(make-directory dir-mode)))
)



;; Init functions

;; Initialize use-packages.
(defun init-packages ()
  "Initialize main packages."
  (load (concat dir-configs "packages"))
)



;; Hooks

; Init mode when a file is loaded.
(add-hook 'find-file-hook 'core:load-mode)



;; Load other less important configs
(core:load-directory dir-settings)

;;; core.el ends here
