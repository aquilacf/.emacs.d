;;; interface.el --- Interface definitions.
;;; Commentary:
;;; Code:

;; Interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode nil))
(if (fboundp 'tool-bar-mode) (tool-bar-mode nil))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode nil))
(transient-mark-mode t)
(xterm-mouse-mode t) ; enable mouse

; Enable line numbers >v26
(global-display-line-numbers-mode)

; Set unicode

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ; Treat clipboard input as UTF-8 string first.
(set-buffer-file-coding-system 'utf-8-unix)


;; No welcome screen
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode
      inhibit-splash-screen t)


;; Startup motivational message
(defun display-startup-echo-area-message()
  "Message that will display on the footer when opening EMACS.")
;	(message "Let the hacking begin!"))


;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

												; Defaults for identation/trailing:
(setq-default 	delete-trailing-lines t			; 	Delete useless empty lines at bottom.
				delete-trailing-whitespace t	; 	Delete useless whitespaces.
				indent-tabs-mode t				;	Tabs, not spaces.
				tab-always-indent nil			;	Force tab insertion. @todo add shit-tab for 'compete
				tab-width 4)					;	Tab with.
												; Editorconfig can override this.


(provide 'interface)

;;; interface.el ends here
