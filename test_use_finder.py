import functools
import logging
import pathlib
import unittest

from use_finder import get_module_name, get_package_path, parse_args

logger = logging.getLogger(__name__)


class TestGetModuleName(unittest.TestCase):
    def setUp(self):
        self.cwd = pathlib.Path("/home/user/my_project")
        self.sys_paths = ["", "/home/user/my_project/src"]

    def test_module_path_extends_a_sys_path(self):
        module_path = pathlib.Path(
            "/home/user/my_project/src/my_package/my_subpackage/my_module.py"
        )
        package_path = pathlib.Path("my_package/my_subpackage/my_module.py")
        path = get_module_name(self.cwd, self.sys_paths, module_path, package_path)
        self.assertEqual(path, "my_package.my_subpackage.my_module")

    def test_module_path_does_not_extend_a_sys_path(self):
        module_path = pathlib.Path("/home/user/my_project/tests/test_my_module.py")
        package_path = pathlib.Path("my_package/my_subpackage/my_module.py")
        path = get_module_name(self.cwd, self.sys_paths, module_path, package_path)
        self.assertIsNone(path)

    def test_module_path_is_in_the_current_working_directory(self):
        module_path = pathlib.Path("/home/user/my_project/my_module.py")
        package_path = pathlib.Path("my_module.py")
        path = get_module_name(self.cwd, self.sys_paths, module_path, package_path)
        self.assertEqual(path, "my_module")


def stub_contains_init(
    directories_with_init: list[pathlib.Path], directory: pathlib.Path
):
    return directory in directories_with_init


class TestGetPackagePath(unittest.TestCase):
    def test_with_trailing_package_directories(self):
        contains_init = functools.partial(
            stub_contains_init,
            [
                pathlib.Path("/home/user/my_project/src/my_package/my_subpackage"),
                pathlib.Path("/home/user/my_project/src/my_package"),
            ],
        )
        module_path = pathlib.Path(
            "/home/user/my_project/src/my_package/my_subpackage/my_module.py"
        )
        self.assertEqual(
            pathlib.Path("my_package/my_subpackage/my_module.py"),
            get_package_path(module_path, contains_init),
        )


class TestParseArgs(unittest.TestCase):
    def test_with_additional_pythonpath_directories(self):
        namespace = parse_args(
            [
                "use_finder.py",
                "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
                "30",
                "--pythonpath",
                "/home/user/my_project/src",
                "--pythonpath",
                "/home/user/my_project",
            ]
        )

        self.assertEqual(
            namespace.module_path,
            "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
        )
        self.assertEqual(namespace.lineno, 30)

        self.assertEqual(len(namespace.pythonpath), 2)
        self.assertEqual(namespace.pythonpath[0], "/home/user/my_project/src")
        self.assertEqual(namespace.pythonpath[1], "/home/user/my_project")

    def test_without_additional_pythonpath_directories(self):
        namespace = parse_args(
            [
                "use_finder.py",
                "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
                "30",
            ]
        )

        self.assertEqual(
            namespace.module_path,
            "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
        )
        self.assertEqual(namespace.lineno, 30)

        self.assertEqual(len(namespace.pythonpath), 0)

    def test_default_verbosity_is_info(self):
        namespace = parse_args(
            [
                "use_finder.py",
                "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
                "30"
            ]
        )

        self.assertEqual(namespace.log_level, logging.INFO)

    def test_increased_verbosity_is_debug(self):
        namespace = parse_args(
            [
                "use_finder.py",
                "/home/user/my_project/src/my_package/my_subpackage/my_module.py",
                "30",
                "--verbose"
            ]
        )

        self.assertEqual(namespace.log_level, logging.DEBUG)
