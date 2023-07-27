;;; pydor.el --- Provide command to run the Python doctest at point

;; Version: 0.0.0
;; Package-Requires:

(provide 'pydor)

(defun delimits-multiline-docstring(line)
  (not (eq (string-match-p "^[[:blank:]]*\"\"\"" line) nil)))

(defun current-line()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun move-to-previous-line()
  (eq (forward-line -1) 0))

(defun find-delimiter-multiline-docstring()
  (save-excursion
    (while (and (not (delimits-multiline-docstring (current-line)))
                (move-to-previous-line))
      nil)
    (if (delimits-multiline-docstring (current-line))
        (line-number-at-pos)
      -1)))

(defun execute-doctest()
  (interactive)
  (let ((lineno (find-delimiter-multiline-docstring)))
    (if (> lineno 0)
        (compile
         (concat "python use_finder.py "
                 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                 " "
                 (number-to-string lineno)))
      (message "Did not find a doctest"))))

;;; pydor.el ends here