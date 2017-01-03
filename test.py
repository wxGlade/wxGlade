#!/usr/bin/env  python2
"""
Create a test suites and run all tests

@see: L{wxglade.tests}

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import gettext, logging, imp, os, sys, unittest
from optparse import OptionParser

t = gettext.translation(domain="wxglade", localedir="locale", fallback=True)
t.install("wxglade")

import wxglade, compat


def run_tests():
    "Create test suites and run all tests"

    # evaluate command line options first
    parser = OptionParser(
        usage="%prog [options]  Test wxGlade components",
    )
    parser.set_defaults(kind='cli')
    parser.add_option(
        '-g',
        '--gui',
        action='store_const',
        dest='kind',
        const='gui',
        help=_('Test GUI components instead of non-GUI components'),
    )
    parser.add_option(
        '-c',
        '--compile',
        action='store_const',
        dest='kind',
        const='compile',
        help=_('Compile generated C++ source code'),
    )

    options = parser.parse_args()[0]

    suites = []

    # disable logging first because the initialisation logs path details and
    # other details
    logging.disable(logging.WARNING)
    wxglade.init_stage1()
    wxglade.init_localization()
    wxglade.init_stage2(options.kind == 'gui')

    # select proper wxversion
    if options.kind == 'gui' and compat.IS_CLASSIC:
        # import proper wx-module using wxversion
        if not hasattr(sys, "frozen") and 'wx' not in sys.modules:
            try:
                import wxversion
                wxversion.ensureMinimal('2.8')
            except ImportError:
                print(_('Please install missing Python module "wxversion".'))
                sys.exit(1)
            except wxversion.VersionError:
                print(_('The requested wxPython version is not found. Disable GUI tests.'))
                sys.exit()

    if options.kind == 'gui':
        modules = ['test_gui.py']
        import wx
        i = wx.Locale(wx.LANGUAGE_DEFAULT)
    elif options.kind == 'compile':
        modules = ['test_compile.py']
    else:                               # options.kind == 'cli'
        modules = ['test_external.py', 'test_codegen.py', 'test_bugs.py', ]


    # try to import all files as modules
    for module_name in modules:

        module_name = os.path.splitext(module_name)[0]
        fp, path, info = imp.find_module(module_name, ['./tests'])
        try:
            module = imp.load_module(module_name, fp, path, info)
        finally:
            # Make sure fp is closed properly
            if fp:
                fp.close()

        # search all test cases in the loaded module
        suites.append(unittest.findTestCases(module))

    # summarise all suites and run tests
    all_tests = unittest.TestSuite(suites)
    unittest.TextTestRunner(verbosity=2).run(all_tests)


if __name__ == '__main__':
    run_tests()
