;;; pydor.el --- Provide command to run the Python doctest at point

;; Author: P.C.J. Swinkels <swinkels.pieter@yahoo.com>
;; Created: 27 July 2023

;; Version: 0.0.0

;; URL: https://github.com/swinkels/pydor

;;; Commentary:

;; Needs to be filled in, for now only present to satisfy the lint...

;;; Code:

(provide 'pydor)

(defun pydor--delimits-multiline-docstring(line)
  (not (eq (string-match-p "^[[:blank:]]*\"\"\"" line) nil)))

(defun pydor--current-line()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun pydor--move-to-previous-line()
  (eq (forward-line -1) 0))

(defun pydor--find-delimiter-multiline-docstring()
  (save-excursion
    (while (and (not (pydor--delimits-multiline-docstring (pydor--current-line)))
                (pydor--move-to-previous-line))
      nil)
    (if (pydor--delimits-multiline-docstring (current-line))
        (line-number-at-pos)
      -1)))

(defun pydor-execute-doctest()
  (interactive)
  (let ((lineno (pydor--find-delimiter-multiline-docstring)))
    (if (> lineno 0)
        (compile
         (concat "python use_finder.py "
                 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                 " "
                 (number-to-string lineno)))
      (message "Did not find a doctest"))))

;;; pydor.el ends here
