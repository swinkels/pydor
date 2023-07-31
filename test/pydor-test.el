(require 'pydor)

;;; pydor-test.el --- Tests for pydor

(ert-deftest test-delimits-multiline-docstring()
  (should (not (pydor--delimits-multiline-docstring "hello")))
  (should (pydor--delimits-multiline-docstring "\"\"\"Return `None`."))
  (should (pydor--delimits-multiline-docstring "    \"\"\"Return `None`.")))
