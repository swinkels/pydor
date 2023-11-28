"""Provide doctests.

>>> h = "Hello World!"
>>> assert h == "hello world!"

"""

import doctest


def return_none():
    """Return `None`.

    >>> v = return_none()
    >>> assert v is None, "function should have returned None"

    >>> v = return_none()
    >>> assert v is not None, "function should have returned None"

    """
    return None


docstring = """Return `None`.

>>> v = return_none()
>>> assert v is None, "function should have returned None"

"""

# parser = doctest.DocTestParser()
# s = parser.get_doctest(docstring, globals(), "<no name>", "<filename>", 11)

# assert len(s.examples) == 2

# runner = doctest.DocTestRunner()
# runner.run(s)
