
;; Interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(transient-mark-mode t)
(xterm-mouse-mode t) ; enable mouse

; Enable line numbers >v26
(global-display-line-numbers-mode)

; Set unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ; Treat clipboard input as UTF-8 string first.


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
