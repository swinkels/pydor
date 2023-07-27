(defun delimits-multiline-docstring(line)
  (not (eq (string-match-p "^[[:blank:]]*\"\"\"" line) nil)))

(delimits-multiline-docstring "hello")
(delimits-multiline-docstring "\"\"\"Return `None`.")
(delimits-multiline-docstring "    \"\"\"Return `None`.")

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
        (compile (concat "python use_finder.py module_under_test " (number-to-string (- lineno 1))))
      (message "Did not find a doctest"))))
