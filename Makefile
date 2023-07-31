unit-tests:
	# Execute unit tests in test/
	eask test ert-runner -L . -L test

lint:
	eask lint package
