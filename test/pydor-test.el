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
  (next-line 6)
  (message "repo-root-directory: %s" repo-root-directory)
  (let ((spec-components (split-string (pydor--build-execute-doctest-spec))))
    (should (equal (nth 0 spec-components) "python"))
    (should (equal (nth 1 spec-components)
                   (file-name-concat repo-root-directory "use_finder.py")))
    (should (equal (nth 2 spec-components) "single_module"))
    (should (equal (nth 3 spec-components) "5"))))
