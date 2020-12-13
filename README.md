# Test System for py4cl2

`py4cl2` can be found at https://github.com/digikar99/py4cl2

# Running Tests

For running the tests, ensure you use python3 (by configuring `(py4cl2:config-var 'py4cl2:pycmd)`) and have the following installed on the python side:

- numpy
- networkx

The system is configured (in [package.lisp](package.lisp)) to avoid testing the loading of networkx on ECL and ABCL.

To overcome this behavior, simply remove `:ecl` or `:abcl` from `cl:*features*` during system-read time.

Once these are loaded, run `(asdf:test-system "py4cl2-tests")`.
