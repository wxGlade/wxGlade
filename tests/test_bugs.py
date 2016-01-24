"""
@copyright: 2014-2016 Carsten Grohmann

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common

# import test base class
from tests import WXGladeBaseTest


class TestCodeGen(WXGladeBaseTest):
    """\
    Test code generation
    """

    def test_bug163(self):
        """\
        Test bug #163 - Don't convert first char of a XRC extraproperty to
        upper case.

        That's the test case for SF bug #163.
        """
        self._test_all('bug163')

    def test_bug165(self):
        """\
        Test bug #165 - Can't rename notebook widget class - internal error
        on Preview

        That's the test case for SF bug #165.
        """
        self._test_all('bug165')

    def test_bug166(self):
        """\
        Test bug #166 - UnicodeDecodeError when saving project using non
        ASCII characters in menu items

        That's the test case for SF bug #166.
        """
        self._test_all('bug166')

    def test_bug167(self):
        """\
        Test bug #167 - ascii codec error - UnicodeDecodeError will be raised
        if existing files will be changed (and not overwritten) and those files
        contains non-ASCII characters.

        That's the test case for SF bug #167.
        """
        self._test_all('bug167')
        self._test_all('bug167_utf8')

    def test_bug179(self):
        """\
        Test bug #179 - Main file is generated without custom extensions

        That's the test case for SF bug #179.
        """
        codegen = common.code_writers['C++']
        source = self._load_file('bug179.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app        = self._load_file('Bug179_main.c++')
        result_frame_cpp  = self._load_file('Bug179_Frame.c++')
        result_frame_h    = self._load_file('Bug179_Frame.hpp')
        self._generate_code('C++', source, './')

        app_filename = '%s' % codegen._generate_app_filename()
        main_cpp = './%s' % app_filename
        generated_app    = self.vFiles[main_cpp].getvalue()
        generated_frame_cpp  = self.vFiles['./Bug179_Frame.c++'].getvalue()
        generated_frame_h    = self.vFiles['./Bug179_Frame.hpp'].getvalue()
        self._compare(result_app,    generated_app, app_filename)
        self._compare(result_frame_cpp,  generated_frame_cpp , 'Bug179_Frame.c++')
        self._compare(result_frame_h,    generated_frame_h,    'Bug179_Frame.hpp')
