;;; pydor.el --- Provide command to run the Python doctest at point

;; Author: P.C.J. Swinkels <swinkels.pieter@yahoo.com>
;; Created: 27 July 2023

;; Version: 0.0.0

;; URL: https://github.com/swinkels/pydor

;;; Commentary:

;; Needs to be filled in, for now only present to satisfy the lint...

;;; Code:

(provide 'pydor)

(defvar pydor--install-directory
  (file-name-directory (if load-file-name load-file-name buffer-file-name))
  "*Path to the directory that contains the current package.

This directory also contains the Python script that finds the
doctests and runs the selected one. This allows us to call that
script from anywhere")

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
    (if (pydor--delimits-multiline-docstring (pydor--current-line))
        (line-number-at-pos)
      -1)))

(defun pydor--build-execute-doctest-spec()
  (let ((lineno (pydor--find-delimiter-multiline-docstring)))
    (when (> lineno 0)
        (concat "python " pydor--install-directory "use_finder.py "
                (buffer-file-name)
                " "
                (number-to-string lineno)))))

(defun pydor-execute-doctest()
  (interactive)
  (let ((call-script-command (pydor--build-execute-doctest-spec)))
    (if call-script-command
        (compile call-script-command)
      (message "Did not find a doctest"))))

;;; pydor.el ends here
