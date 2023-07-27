(require 'pydor)

;;; pydor-test.el --- Tests for pydor

(ert-deftest test-delimits-multiline-docstring()
  (should (not (delimits-multiline-docstring "hello")))
  (should (delimits-multiline-docstring "\"\"\"Return `None`."))
  (should (delimits-multiline-docstring "    \"\"\"Return `None`.")))
