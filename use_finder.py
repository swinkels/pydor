import argparse
import doctest
import importlib
import logging
import pathlib
import sys
from typing import Optional

logger = logging.getLogger(__name__)


def execute_doctest(module_path: str, lineno: str):
    """Execute the tests in the docstring at the given location.

    :param module_path: path to the module that contains the docstring
    :param lineno: number of the line that contains the triple quotes that
       start the docstring - line numbering starts at 1

    """
    path = pathlib.Path(module_path)

    package_path = get_package_path(
        path, lambda directory: (directory / "__init__.py").exists()
    )
    print(f"Package path: {package_path}")
    module_name = get_module_name(pathlib.Path.cwd(), sys.path, path, package_path)
    if module_name is None:
        print(
            f"Unable to import module: no sys.path entry completes package path "
            f"{package_path}"
        )
        sys.exit(1)
    print(f"Module name: {module_name}")

    module = importlib.import_module(module_name)

    doctests = doctest.DocTestFinder().find(module)
    for dt in doctests:
        # doctest starts line numbering at 0 whereas Emacs starts line
        # numbering at 1: here we correct for that
        print(f"Found doctest at line {dt.lineno + 1}")
        if dt.lineno + 1 == int(lineno):
            break
    else:
        print("Did not find a matching doctest")
        return None

    print(f"Run doctest at line {dt.lineno + 1}")
    runner = doctest.DocTestRunner(
        checker=None,
        verbose=None,
        optionflags=doctest.FAIL_FAST | doctest.NORMALIZE_WHITESPACE,
    )
    runner.run(dt)


def get_package_path(
    module_path: pathlib.Path, contains_init=lambda d: (d / "__init__.py").exists()
):
    """Return the part of the given module path that is inside a package.

    For example, assume you're developing a package that starts at

        /home/user/my_project/src

    then the call

        get_package_path("/home/user/my_project/src/my_package/my_module.py")

    will return

        pathlib.Path("my_package/my_module.py")

    Starting with the directory that contains the module, this function checks
    the parent directories whether they contain an `__init__.py` file. If so,
    it considers that directory to be part of the package path and continues
    with the parent directory. If that's not the case, the package path is
    complete.

    In the previous example, directory `my_package` apparently contains such an
    `__init__.py` but directory `src` doesn't.

    Parameter `contains_init` should hold a function that checks whether a
    given `pathlib.Path` directory contains a `__init__.py`. By default this
    boils down to a call to `pathlib.Path.exists`, but by making it a parameter
    it is much easier to unit test the current function.

    """
    package_path = pathlib.Path(module_path.name)

    path = module_path.parent
    while True:
        if not contains_init(path):
            break

        package_path = pathlib.Path(path.name) / package_path
        if path.parent == path:
            # the current path is the root so we should stop
            break

        path = path.parent

    return package_path


def get_module_name(
    cwd: pathlib.Path,
    sys_path: list[str],
    module_path: pathlib.Path,
    package_path: pathlib.Path,
) -> Optional[str]:
    """Return the module name for the given module and package paths.

    For example, you have a module path of

        /home/user/my_project/src/my_package/my_module.py

    and a package path of

        my_package/my_module.py

    Assuming

        /home/user/my_project/src

    is one of the directories in the given system path, this function will
    return "my_package.my_module".

    """
    module_name: Optional[str] = None

    for path in map(pathlib.Path, sys_path):
        # if the path is relative, make the path absolute with respect to the
        # given current working directory
        if not path.is_absolute():
            path = cwd / path

        if module_path.is_relative_to(path):
            logger.debug("Found candidate path %s", path)

            relative_module_path = module_path.relative_to(path)
            logger.debug("Found relative path %s", relative_module_path)

            if relative_module_path == package_path:
                module_name = ".".join(relative_module_path.parts[:-1])
                leading_dot = "." if module_name else ""
                module_name += f"{leading_dot}{relative_module_path.stem}"
                logger.debug("Found module name %s", module_name)
                break

    return module_name


def parse_args(argv):
    parser = argparse.ArgumentParser(
        prog=f"python {argv[0]}",
        description="Run the tests in the docstring in MODULE_PATH at LINENO",
        exit_on_error=False,
    )
    parser.add_argument("module_path", help="absolute path to the module")
    parser.add_argument(
        "lineno", help="line number in a line of the docstring (1-based)", type=int
    )
    parser.add_argument(
        "--pythonpath",
        help="add DIRECTORY to PYTHONPATH",
        metavar="DIRECTORY",
        action="append",
        default=[],
    )

    return parser.parse_args(argv[1:])


if __name__ == "__main__":
    namespace = parse_args(sys.argv)
    sys.path = namespace.pythonpath + sys.path
    execute_doctest(namespace.module_path, namespace.lineno)
