;;; cpp.el --- C/C++ configs
;;; Commentary:
;;; Code:

; Download
(defun download()
  "Download cpplint.py if does not exist."
  (message "Downloading cpplint.py...")
  (url-retrieve "https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py"
		  (lambda (s)
		    (re-search-forward "\r?\n\r?\n")
		    (write-region (point) (point-max) (concat user-emacs-directory "cpp/cpplint.py"))))
  )


; Download cpplint
(unless (file-exists-p "cpp/cpplint.py") (download))







;;; cpp.el ends here

