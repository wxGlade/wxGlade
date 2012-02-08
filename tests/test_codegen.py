"""
@copyright: 2012 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import project modules
import common


class TestCodeGen(WXGladeBaseTest):
    """\
    Test code generation
    """

    def test_CPP_wxCalendarCtrl(self):
        """\
        Test CPP code generation with a small wxCalendarCtrl example

        The test also tests for Sourceforge bug #2782306

        @see: L{wxglade.widgets.calendar_ctrl.calendar_ctrl}
        """
        source = self._load_file('CPP_wxCalendarCtrl.wxg')
        result_cpp = self._load_file('CPP_wxCalendarCtrl.cpp')
        result_h = self._load_file('CPP_wxCalendarCtrl.h')

        # generate and compare C++ code
        self._generate_code('C++', source, 'CPP_wxCalendarCtrl')
        generated_cpp = self.vFiles['CPP_wxCalendarCtrl.cpp'].getvalue()
        generated_h = self.vFiles['CPP_wxCalendarCtrl.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'C++ source')
        self._compare(result_h, generated_h, 'C++ header')

    def test_CPP_Preferences(self):
        """\
        Test C++ code generation with preferences dialog

        @see: L{wxglade.codegen.cpp_codegen}
        """
        source = self._load_file('CPP_Preferences.wxg')
        result_cpp = self._load_file('CPP_Preferences.cpp')
        result_h = self._load_file('CPP_Preferences.h')

        # generate and compare C++ code
        self._generate_code('C++', source, 'CPP_Preferences')
        generated_cpp = self.vFiles['CPP_Preferences.cpp'].getvalue()
        generated_h = self.vFiles['CPP_Preferences.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'C++ source')
        self._compare(result_h, generated_h, 'C++ header')

    def test_Lisp_quote_path(self):
        """\
        Test codegen.lisp_codegen.quote_path()

        @see: L{wxglade.codegen.lisp_codegen.quote_path()}
        """
        quote_path = common.code_writers['lisp'].quote_path
        examples = [
            ('icon.png',                 '"icon.png"'),
            ('/usr',                     '"/usr"'),
            ('/usr/shar/icons/iso.png',  '"/usr/shar/icons/iso.png"'),
            (r'C:\Temp',                r'"C:\\Temp"'),
            ('/tmp/wx""glade',          r'"/tmp/wx\"\"glade"'),
            ]
        for unquoted, tquoted in examples:
            aquoted = quote_path(unquoted)
            self.assertEqual(
                aquoted,
                tquoted,
                "Lisp quotation for '%s' returned '%s' expected '%s'" % (
                    unquoted,
                    aquoted,
                    tquoted,
                    )
                )

    def test_Lisp_Preferences(self):
        """\
        Test Lisp code generation with preferences dialog

        @see: L{wxglade.codegen.lisp_codegen}
        """
        self._generate_and_compare(
            'lisp',
            'Preferences.wxg',
            'Lisp_Preferences.lisp'
            )

    def test_Lisp_wxBitmapButton(self):
        """\
        Test Lisp code generation with small wxBitmapButton example

        @see: L{wxglade.widgets.bitmap_button.lisp_codegen}
        """
        self._generate_and_compare(
            'lisp',
            'Lisp_wxBitmapButton.wxg',
            'Lisp_wxBitmapButton.lisp'
            )

    def test_Perl_Preferences(self):
        """\
        Test Perl code generation with preferences dialog

        @see: L{wxglade.codegen.pl_codegen}
        """
        self._generate_and_compare(
            'perl',
            'Perl_Preferences.wxg',
            'Perl_Preferences.pl'
            )

    def test_Python_Preferences(self):
        """\
        Test Python code generation with preferences dialog

        @see: L{wxglade.codegen.py_codegen}
        """
        self._generate_and_compare(
            'python',
            'Preferences.wxg',
            'Python_Preferences.py'
            )

    def test_GridEvents(self):
        """\
        Test Perl code generation with a grid widgets and handling events

        @see: L{wxglade.codegen.pl_codegen}
        """
        self._generate_and_compare(
            'perl',
            'GridEvents.wxg',
            'GridEvents.pl'
            )
        self._generate_and_compare(
            'python',
            'GridEvents.wxg',
            'GridEvents.py'
            )
        self._generate_and_compare(
            'XRC',
            'GridEvents.wxg',
            'GridEvents.xrc'
            )
        # check C++
        source = self._load_file('GridEvents.wxg')
        result_cpp = self._load_file('GridEvents.cpp')
        result_h = self._load_file('GridEvents.h')
        self._generate_code('C++', source, 'GridEvents')
        generated_cpp = self.vFiles['GridEvents.cpp'].getvalue()
        generated_h = self.vFiles['GridEvents.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'C++ source')
        self._compare(result_h, generated_h, 'C++ header')
