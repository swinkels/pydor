import doctest
import importlib
import pathlib
import sys

def execute_doctest(module_path: str, lineno: str):

    path = pathlib.Path(module_path)

    # We add the directory that contains the module under test to the list of
    # directories Python searches for modules. This lets us easily import that
    # module. It might not be the proper way to import it, but it gets the job
    # done.
    directory = path.parent
    sys.path.insert(0, directory.as_posix())

    module_name = path.stem
    module = importlib.import_module(module_name)

    doctests = doctest.DocTestFinder().find(module)
    for dt in doctests:
        print(f"Found doctest at line {dt.lineno}")
        # doctest starts line numbering at zero so we have to correct for that
        if dt.lineno == int(lineno) - 1:
            break
    else:
        print("Did not find a matching doctest")
        return None

    print(f"Run doctest at line {dt.lineno}")
    runner = doctest.DocTestRunner(checker=None, verbose=None, optionflags=doctest.FAIL_FAST)
    runner.run(dt)


if __name__ == '__main__':
    execute_doctest(sys.argv[1], int(sys.argv[2]))
