import doctest
import importlib
import sys

def execute_doctest(module_path, lineno):
    finder = doctest.DocTestFinder()

    module = importlib.import_module(module_path)
    doctests = finder.find(module)
    for dt in doctests:
        print(f"Found doctest at line {dt.lineno}")
        if dt.lineno == lineno:
            break
    else:
        print("Did not find a matching doctest")
        return None

    runner = doctest.DocTestRunner(checker=None, verbose=None, optionflags=doctest.FAIL_FAST)
    runner.run(dt)


if __name__ == '__main__':
    execute_doctest("module_under_test", int(sys.argv[2]))
