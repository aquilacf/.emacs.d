;;; packages.el --- Packages core.
;;; Commentary:
;;; Code:

;; GIT gutter
(use-package git-gutter
	:ensure t
	:init
		(custom-set-variables '(git-gutter:added-sign " +")
								'(git-gutter:deleted-sign " -")
								'(git-gutter:modified-sign " \u2502")
								'(git-gutter:separator-sign nil)
								'(git-gutter:unchanged-sign nil)

								'(git-gutter:lighter " GitG")
								'(git-gutter:window-width 2)
								'(git-gutter:update-interval 0)
								'(git-gutter:visual-line t)
		)
		'(git-gutter:hide-gutter t)
		(global-git-gutter-mode t) ; Enable git-gutter minot-mode for all projects.

	:config
		(set-face-background 'git-gutter:added nil)
		(set-face-background 'git-gutter:deleted nil)
		(set-face-background 'git-gutter:modified nil)
		(set-face-background 'git-gutter:unchanged nil)

		(set-face-foreground 'git-gutter:added "green")
		(set-face-foreground 'git-gutter:deleted "red")
		(set-face-foreground 'git-gutter:modified "yellow")
		(set-face-foreground 'git-gutter:unchanged nil)
)


;; .editorconfig
(use-package editorconfig
	:ensure t
	:config
		(editorconfig-mode 1) ; Enable editorconfig minor-mode for all projects.
)


;; Eglot @todo: decide about this
;(use-package eglot
;  :ensure t)
