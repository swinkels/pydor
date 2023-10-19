(require 'pydor)

;;; pydor-test.el --- Tests for pydor

;; expand-file-name works relative to the working directory. When the tests run,
;; the working directory is the root of the repo
(setq repo-root-directory (file-name-as-directory (expand-file-name ".")))

(ert-deftest test-delimits-multiline-docstring()
  (should (not (pydor--delimits-multiline-docstring "hello")))
  (should (pydor--delimits-multiline-docstring "\"\"\"Return `None`."))
  (should (pydor--delimits-multiline-docstring "    \"\"\"Return `None`.")))

(ert-deftest test-build-spec-for-single-module-in-another-directory()
  (find-file "test/test-python-modules/single_module.py")
  (beginning-of-buffer)
  (next-line 8)
  (message "repo-root-directory: %s" repo-root-directory)
  (let ((spec-components (split-string (pydor--build-execute-doctest-spec))))
    (should (equal (nth 0 spec-components) "python"))
    (should (equal (nth 1 spec-components)
                   (file-name-concat repo-root-directory "execute_doctests.py")))
    (should (equal (nth 2 spec-components)
                   (file-name-concat repo-root-directory
                                     "test"
                                     "test-python-modules"
                                     "single_module.py")))
    (should (equal (nth 3 spec-components) "7"))))

(ert-deftest test-build-pythonpath-parameters-when-specified()
  (let ((pydor-pythonpath-directories
         '("/home/user/repo/""/home/user/repo/src")))
    (should (equal
             " --pythonpath /home/user/repo/ --pythonpath /home/user/repo/src"
             (pydor--build-pythonpath-parameters)))))

(ert-deftest test-build-pythonpath-parameters-when-missing()
  (let ((pydor-pythonpath-directories nil))
    (should (equal "" (pydor--build-pythonpath-parameters))))
  )

(ert-deftest test-build-verbose-parameter-when-specified()
  (let ((pydor-run-verbose t))
    (should (equal " --verbose" (pydor--build-verbose-parameter)))))

(ert-deftest test-build-verbose-parameter-when-missing()
  (let ((pydor-run-verbose nil))
    (should (equal "" (pydor--build-verbose-parameter)))))
