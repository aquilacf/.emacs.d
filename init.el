;;; init.el --- Summary
;;; Commentary:
;;; init.el --------------------------------------------------------------------------------------------
;;; Code:

;; User Info
(setq user-full-name "Áquila Freitas")
(setq user-mail-address "aquilacf@protonmail.com")


;; Interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(transient-mark-mode t)
(xterm-mouse-mode t) ; enable mouse

;; No welcome screen
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode
      inhibit-splash-screen t)

;; Startup motivational message
(defun display-startup-echo-area-message()
  "Message that will display on the footer when opening EMACS."
	(message "Let the hacking begin!"))


;; Other configs
;; Key bindings
;; Scroll without moving cursor
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-3>") (kbd "C-y"))
(global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off backups and autosaves. @todo: remove this
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default truncate-lines t)

;; Requires >v24.4
;; Install package repositories
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Start package manager
(eval-when-compile
  (require 'use-package))

;; Enable line numbers >v26
(global-display-line-numbers-mode)


;; Monokai theme >v26
(use-package monokai-theme
	:ensure t
	:init
		(load-theme 'monokai t))

;; GIT gutter
(use-package git-gutter
	:ensure t
	:init
		(custom-set-variables '(git-gutter:added-sign " +")
				      '(git-gutter:deleted-sign " -")
				      '(git-gutter:modified-sign " \u2502")
				      '(git-gutter:separator-sign nil)

				      '(git-gutter:lighter " GitG")
				      '(git-gutter:window-width 2)
				      '(git-gutter:update-interval 1)
				      '(git-gutter:visual-line t))
		'(git-gutter:hide-gutter t)
		(global-git-gutter-mode t)

	:config
		(set-face-background 'git-gutter:added nil)
		(set-face-background 'git-gutter:deleted nil)
		(set-face-background 'git-gutter:modified nil)
		(set-face-background 'git-gutter:unchanged nil)

		(set-face-foreground 'git-gutter:added "green")
		(set-face-foreground 'git-gutter:deleted "red")
		(set-face-foreground 'git-gutter:modified "yellow")
		(set-face-foreground 'git-gutter:unchanged nil))



;; Flycheck
(use-package flycheck
	:ensure t
	:init
		(global-flycheck-mode))



;;; init.el --------------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign " +")
 '(git-gutter:deleted-sign " -")
 '(git-gutter:lighter " Git")
 '(git-gutter:modified-sign " │")
 '(git-gutter:separator-sign nil)
 '(git-gutter:update-interval 1)
 '(git-gutter:visual-line t)
 '(git-gutter:window-width 2)
 '(package-selected-packages (quote (flycheck use-package monokai-theme git-gutter))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
