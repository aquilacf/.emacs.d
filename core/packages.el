;;; packages.el --- Packages core.
;;; Commentary: My packages.
;;; Code:

(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)


(use-package monokai-theme	:init (load-theme 'monokai t))
(use-package which-key		:config (which-key-mode))						; @todo
(use-package yasnippet		:config (yas-global-mode))
(use-package editorconfig	:config (editorconfig-mode))
(use-package smartparens	:config (smartparens-global-mode t))


(use-package format-all
	:after use-package-ensure-system-package
	:ensure-system-package
	(
		(yarn 		. yarn)
		(prettier 	. "yarn global add prettier @prettier/plugin-php prettier-plugin-pkg prettier-plugin-toml")
	)
)


(use-package git-gutter
	:custom
	(
		(git-gutter:added-sign " +")
		(git-gutter:deleted-sign " -")
		(git-gutter:modified-sign " \u2502")
		(git-gutter:separator-sign nil)
		(git-gutter:unchanged-sign nil)
		(git-gutter:lighter nil)
		(git-gutter:window-width 2)
		(git-gutter:visual-line t)
		(git-gutter:update-interval 2)

	)
	:config
		(set-face-attribute 'git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
		(set-face-attribute 'git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
		(set-face-attribute 'git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
		(set-face-attribute 'git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified)
		(global-git-gutter-mode)
)


;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

(use-package typescript-mode
	:custom
		(lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/tsserver.log"))
	:mode
		("\\.ts[x]?\\'" . typescript-mode)
	:ensure-system-package
	(
		(typescript-language-server . "yarn global add typescript-language-server")
		(tsc . "yarn global add typescript")
	)
	:hook
		(typescript-mode . format-all-mode)
)

(use-package yaml-mode		:hook (yaml-mode . format-all-mode))
(use-package toml-mode		:hook (yaml-mode . format-all-mode))
(use-package json-mode		:hook (json-mode . format-all-mode))
(use-package graphql-mode	:hook (graphql-mode . format-all-mode))
(use-package markdown-mode	:hook (markdown-mode . format-all-mode))

(use-package php-mode
	:config
		(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
		(setq php-template-compatibility nil)
		(setq php-lineup-cascaded-calls t)
	:hook
		(php-mode . format-all-mode)
)

(use-package terraform-mode
	:ensure-system-package
	(
		(terraform . terraform)
		(terraform-ls . "brew install hashicorp/tap/terraform-ls")
	)
	:hook (terraform-mode . format-all-mode)
)



(use-package projectile
	:custom
		(projectile-known-projects-file (concat dir-cache "projectile-bookmarks.eld"))
	:config
		(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
		(projectile-mode)
)

(projectile-register-project-type 'npm '("package.json")
				  :compile "yarn install"
				  :test "yarn test"
				  :run "yarn start"
				  :test-suffix ".spec")


(use-package company)					; @todo


(use-package lsp-mode					; @todo
	:commands (lsp lsp-deferred)
	:ensure-system-package
		(bash-language-server . "yarn global add bash-language-server")
	:hook
	(
		(typescript-mode 	. lsp-deferred)
		(js-mode 			. lsp-deferred)
		(sh-mode			. lsp-deferred)
	)
	:custom
	(
		(lsp-auto-guess-root t)
		(lsp-eldoc-render-all nil)
		(lsp-session-file (concat dir-cache ".lsp-session-v1"))
		(lsp-keymap-prefix "s-l")
		(lsp-prefer-flymake t)
		(lsp-enable-indentation nil)
		(lsp-semantic-highlighting t)

		(lsp-bash-explainshell-endpoint t)
		(lsp-bash-highlight-parsing-errors t)
		(lsp-bash-glob-pattern t)
	)
	:config
	(lsp-register-client
		(make-lsp-client	:new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
							:major-modes '(terraform-mode)
							:server-id 'terraform-ls
		)
	)
	(add-hook 'terraform-mode-hook #'lsp-deferred)
)
