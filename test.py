#!/usr/bin/env  python
"""
Create a test suites and run all tests

@see: L{wxglade.tests}

@copyright: 2012 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import gettext
import imp
import os
import unittest
from optparse import OptionParser

t = gettext.translation(domain="wxglade", localedir="locale", fallback=True)
t.install("wxglade")


def run_tests(gui_tests=False):
    """\
    Create test suites and run all tests

    @param gui_tests: Test GUI components or test internal functionality
    @type gui_tests:  Boolean
    """
    suites = []

    # get a list of all test modules
    modules = os.listdir('./tests')
    if '__init__.py' in modules:
        modules.remove('__init__.py')

    # try to import all files as modules
    for module_name in modules:
        if (not module_name.endswith('.py')) or \
           (gui_tests and not module_name.endswith('_gui.py')) or \
           (not gui_tests and module_name.endswith('_gui.py')):
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
    # evaluate command line options first
    parser = OptionParser(
        usage="%prog [options]  Test wxGlade components",
        )
    parser.add_option(
        '-g',
        '--gui',
        dest='gui_tests',
        default=False,
        action='store_true',
        help=_('Test GUI components instead of non-GUI components'),
        )

    (options, args) = parser.parse_args()

    run_tests(options.gui_tests)
