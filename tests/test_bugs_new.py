"""
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from testsupport_new import WXGladeGUITest

import common
import unittest


class TestBugs(WXGladeGUITest):
    "Test for different reported bugs"

    def test_bug163(self):
        "Test bug #163 - Don't convert first char of a XRC extraproperty to upper case."
        self.load_and_generate('bug163', test_GUI=False)

    def test_bug165(self):
        "Test bug #165 - Can't rename notebook widget class - internal error on Preview"
        self.load_and_generate('bug165', test_GUI=True, preview=False)

    def test_bug166(self):
        "Test bug #166 - UnicodeDecodeError when saving project using non ASCII characters in menu items"
        self.load_and_generate('bug166', test_GUI=False)

    @unittest.skip("wxg and .py do not match")
    def test_bug167(self):
        """Test bug #167 - ascii codec error - UnicodeDecodeError will be raised if existing files will be changed
        (and not overwritten) and those files contains non-ASCII characters."""
        self.load_and_generate('bug167', test_GUI=False)
        self.load_and_generate('bug167_utf8', test_GUI=False)

    def Xtest_bug179(self):
        "Test bug #179 - Main file is generated without custom extensions"
        codegen = common.code_writers['C++']
        source = self._load_file('bug179.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app        = self._load_file('Bug179_main.c++')
        result_frame_cpp  = self._load_file('Bug179_Frame.c++')
        result_frame_h    = self._load_file('Bug179_Frame.hpp')

        self._generate_code('C++', source, self.curr_dir)

        app_filename = codegen._generate_app_filename()
        main_cpp = self._with_curr_dir(app_filename)
        generated_app    = self.vFiles[main_cpp].getvalue()
        generated_frame_cpp  = self.vFiles[self._with_curr_dir('Bug179_Frame.c++')].getvalue()
        generated_frame_h    = self.vFiles[self._with_curr_dir('Bug179_Frame.hpp')].getvalue()
        self._compare(result_app,    generated_app, app_filename)
        self._compare(result_frame_cpp,  generated_frame_cpp , 'Bug179_Frame.c++')
        self._compare(result_frame_h,    generated_frame_h,    'Bug179_Frame.hpp')

    def test_bug183(self):
        "Test bug #183 - Preview failure for class names with Perl scope separator."
        self.load_and_generate('bug183', test_GUI=False)

    def test_bug184(self):
        "Test bug #184 - Perl code generation: System colour constants named incorrectly."
        self.load_and_generate('bug184', test_GUI=False)

    def test_bug186(self):
        "Test bug #186 - Fix C++ code issue with Ids assigned to variables"
        self.load_and_generate('bug186', test_GUI=False)

    def test_bug188_toolbar_depencencies(self):
        "Test bug #188 - Missing dependencies with wxToolBox widgets"
        self.load_and_generate('bug188_included_toolbar', test_GUI=False)
        self.load_and_generate('bug188_standalone_toolbar', test_GUI=False)

    def test_bars_wo_parent(self):
        "Test AttributeError during code generation of toplevel menubars"
        self.load_and_generate('bars_wo_parent', test_GUI=False)

    def test_bug189_XMLParsingError(self):
        "Test bug #189 - XmlParsingError : An internal error occurred while Generate Code"
        self.load_and_generate('bug183', test_GUI=False)

    def test_bug194(self):
        """Test bug #194 - LB_EXTENDED for ListBox they never show up in generated code"""
        self.load_and_generate('bug194', test_GUI=False)

if __name__ == '__main__':
    unittest.main(exit=False)
