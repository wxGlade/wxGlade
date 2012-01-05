#!/usr/bin/env  python
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY
#
# Copyright (c) 2012 Carsten Grohmann

# import general python modules
import imp
import os
import unittest


def run_tests():
    """\
    Create test suites and run all tests
    """
    suites = []

    # get a list of all test modules
    modules = os.listdir('./tests')
    if '__init__.py' in modules:
        modules.remove('__init__.py')

    # try to import all files as modules
    for module_name in modules:
        if not module_name.endswith('.py'):
            continue
        module_name = os.path.splitext(module_name)[0]
        fp, path, info = imp.find_module(module_name, ['./tests'])
        try:
            module = imp.load_module(module_name, fp, path, info)

        finally:
            # Make sure fp is closed properly
            if fp:
                fp.close()

        # search all testcases in the loaded module
        suites.append(unittest.findTestCases(module))

    # summarise all suites and run tests
    all_tests = unittest.TestSuite(suites)
    unittest.TextTestRunner(verbosity=2).run(all_tests)


if __name__ == '__main__':
    run_tests()
