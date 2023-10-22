unit-tests:
	# Execute unit tests in test/
	eask test ert-runner -L . -L test

lint:
	eask lint package

python-static-analysis:
	flake8 execute_doctests.py test_execute_doctests.py
	black --check execute_doctests.py test_execute_doctests.py
	isort --check execute_doctests.py test_execute_doctests.py

python-unit-tests:
	python -m unittest test_execute_doctests.py
