;;; cpp.el --- C/C++ configs
;;; Commentary:
;;; Code:


(concat user-emacs-directory "cpp/cpplint.py")





(url-retrieve "https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py"
	      (lambda (s)
		(re-search-forward "\r?\n\r?\n")
		(write-region (point) (point-max) (concat user-emacs-directory "cpp/cpplint.py"))))







;;; cpp.el ends here

