import argparse
import doctest
import importlib
import importlib.util
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

    logger.debug("Module has path %s", module_path)

    package_path = get_package_path(
        path, lambda directory: (directory / "__init__.py").exists()
    )
    if package_path is None:
        module = import_standalone_module(path)
    else:
        module = import_module_from_package(path, package_path)

    if module is None:
        sys.exit(1)

    doctests = doctest.DocTestFinder().find(module)
    for dt in doctests:
        if dt.lineno is None:
            # There are scenarios where the doctest.DocTestFinder cannot
            # retrieve the line number of a doctest. The lineno attribute of
            # such a doctest will be None so we cannot determine whether that
            # doctest is the one we're looking for.
            continue
        # doctest starts line numbering at 0 whereas Emacs starts line
        # numbering at 1: here we correct for that
        if dt.lineno + 1 == int(lineno):
            break
    else:
        logger.error("Unable to find matching doctests")
        sys.exit(1)

    logger.info("Run doctests in docstring at line %d of %s", dt.lineno + 1, path)
    runner = doctest.DocTestRunner(
        checker=None,
        verbose=None,
        optionflags=doctest.FAIL_FAST | doctest.NORMALIZE_WHITESPACE,
    )
    runner.run(dt)


def import_module_from_package(
    module_to_import: pathlib.Path, module_in_package: pathlib.Path
):
    """Import the module as part of a package and return the imported module.

    If this function cannot import the module, it returns None or raises an
    exception.

    :param module_to_import: path to the module to be imported
    :param module_in_package: relative path to the module inside an importable
        package

    """
    module_name = get_module_name(
        pathlib.Path.cwd(), sys.path, module_to_import, module_in_package
    )
    if module_name is None:
        logger.error(
            "Unable to import module: no sys.path entry completes package path"
        )
        return None

    logger.info("Import module %s", module_name)

    return importlib.import_module(module_name)


def import_standalone_module(module_to_import: pathlib.Path):
    """Import the standalone module by its path and return the imported module.

    This function imports the module as if it's not part of a package, hence
    the term "standalone". If it cannot import the module, it returns None or
    raises an exception.

    :param module_to_import: path to the module to be imported

    """
    logger.info("Import standalone module %s", module_to_import)

    module_name = module_to_import.stem
    spec = importlib.util.spec_from_file_location(module_name, module_to_import)
    if spec is None or spec.loader is None:
        logger.error(
            "Unable to import module: no loader can be created for the given module"
        )
        return None

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)

    return module


def get_package_path(
    module_path: pathlib.Path, contains_init=lambda d: (d / "__init__.py").exists()
):
    """Return the part of the given module path that is inside a package.

    If no part of the given module path is inside a package, this functions
    returns None.

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
    if not contains_init(module_path.parent):
        # the module does not seem to be part of a package
        return None

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
        logger.debug(
            "Check if module seems importable from sys.path directory %s", path
        )
        if not path.is_absolute():
            path = cwd / path

        if not module_path.is_relative_to(path):
            logger.debug("No, module path is not below sys.path directory")
            continue

        relative_module_path = module_path.relative_to(path)

        if relative_module_path != package_path:
            logger.debug(
                "Package path of module is not adjacent to this sys.path directory"
            )
        else:
            module_name = ".".join(relative_module_path.parts[:-1])
            leading_dot = "." if module_name else ""
            module_name += f"{leading_dot}{relative_module_path.stem}"
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
    parser.add_argument(
        "--verbose",
        help="increase verbosity of logging",
        dest="log_level",
        action="store_const",
        const=logging.DEBUG,
        default=logging.INFO,
    )

    return parser.parse_args(argv[1:])


if __name__ == "__main__":
    # we setup logging before we call our code so all of our log messages look
    # the same
    logging.basicConfig(format="%(message)s", level=logging.INFO)

    namespace = parse_args(sys.argv)

    # one of the parameters sets the default log level so we have to update the
    # log level set by logging.basicConfig
    logging.getLogger().setLevel(namespace.log_level)

    sys.path = namespace.pythonpath + sys.path
    execute_doctest(namespace.module_path, namespace.lineno)
