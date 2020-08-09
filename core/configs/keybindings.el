;;; keybindings.el --- Frequent key bindings.
;;; Commentary:
;;; Code:

;; Scroll without moving cursor
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-3>") (kbd "C-y"))
(global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(global-set-key (kbd "RET") 'newline-and-indent) ; Not perfect.
(global-set-key (kbd "C-S-k") (lambda() (interactive) (kill-line 0)))	; Kill line backwards. Won't work on Mac Terminal.


(global-unset-key (kbd "C-t"))				; Reserved for tmux.


(defun utilities:read-kbd ()

	"Scan keybindings."

	(interactive)
	(message (key-description (vector (read-key "Type a key: "))))

)
