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
		(global-git-gutter-mode)

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
		(editorconfig-mode) ; Enable editorconfig minor-mode for all projects.
)


;; Eglot @todo: decide about this
;(use-package eglot
;  :ensure t)


;; Monokai theme >v26
(use-package monokai-theme
	:ensure t
	:init
		(load-theme 'monokai t)
)



;; Flycheck
; (use-package flycheck
; 	:ensure t
; 	:init
; 	(global-flycheck-mode))





(use-package use-package-ensure-system-package
  :ensure t)

;; Format code
(use-package format-all
	:after use-package-ensure-system-package
	:ensure-system-package
		(
			(yarn 		. yarn)
			(prettier 	. "yarn global add prettier @prettier/plugin-php prettier-plugin-pkg prettier-plugin-toml")
		)
	:ensure t
)


;; Major Modes

(use-package typescript-mode
	:ensure t
	:mode ("\\.ts[x]?\\'" . typescript-mode)
	;:ensure-system-package (prettier . prettier)
	:ensure-system-package
		(tsc . "yarn global add typescript")
	:hook
		(typescript-mode . format-all-mode)
)

(use-package yaml-mode
	:ensure t
	:hook
		(yaml-mode . format-all-mode)
)

(use-package toml-mode
	:ensure t
)


(use-package json-mode
	:ensure t
	:hook
		(json-mode . format-all-mode)
)

(use-package graphql-mode
	:ensure t
	:hook
		(graphql-mode . format-all-mode)
)

(use-package markdown-mode
	:ensure t
	:hook
		(markdown-mode . format-all-mode)
)

(use-package php-mode
	:ensure t
	:config
		(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
		(setq php-template-compatibility nil)
		(setq php-lineup-cascaded-calls t)
	:hook
		(php-mode . format-all-mode)
)

(use-package terraform-mode
	:ensure t
	:ensure-system-package (terraform . terraform)
	:hook
		; @todo: https://github.com/lassik/emacs-format-all-the-code/pull/84
		(terraform-mode . format-all-mode)
)





;; Projectile
(use-package projectile
	:ensure t
	:init
		(custom-set-variables '(projectile-known-projects-file (concat dir-cache "projectile-bookmarks.eld")))
	:config
		(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
		(projectile-mode)
)





(projectile-register-project-type 'npm '("package.json")
				  :compile "yarn install"
				  :test "yarn test"
				  :run "yarn start"
				  :test-suffix ".spec")




;; Company @todo: finish this
;(use-package company
; ; :ensure t
;  :init (global-company-mode)
;  :config
;  (setq company-dabbrev-downcase nil
;	company-tooltip-align-annotations t
;	;; Navigation with M-<n>
;	company-show-numbers t))





; @todo treemacs


; (use-package treemacs-projectile
;   :after treemacs projectile
;   :ensure t)
