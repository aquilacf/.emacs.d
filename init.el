;;; init.el --- EMACS
;;; Commentary:
;;; Code:

; Load core
(load (concat user-emacs-directory "configs/core"))


;; Requires >26.1
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

;; Start package manager @todo: is this really necessary? it's causing error process sentinel: args out of range with find-file-hook
;(eval-when-compile
;	(require 'use-package)
	(init-packages)
;)

;;; init.el ends here
