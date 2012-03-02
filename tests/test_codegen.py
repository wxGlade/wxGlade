"""
@copyright: 2012 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import project modules
import common

class MockCodeObject(object):
    """\
    Mock object for L{wxglade.xml_parse.CodeObject}
    """
    preview = False
    properties = {}


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
        Test code generation with a grid widgets and handling events
        """
        self._generate_and_compare(
            'lisp',
            'GridEvents.wxg',
            'GridEvents.lisp'
            )
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

    def test_Gauge(self):
        """\
        Test code generation for a wxGauge
        """
        self._generate_and_compare(
            'lisp',
            'Gauge.wxg',
            'Gauge.lisp'
            )
        self._generate_and_compare(
            'perl',
            'Gauge.wxg',
            'Gauge.pl'
            )
        self._generate_and_compare(
            'python',
            'Gauge.wxg',
            'Gauge.py'
            )
        self._generate_and_compare(
            'XRC',
            'Gauge.wxg',
            'Gauge.xrc'
            )
        # check C++
        source = self._load_file('Gauge.wxg')
        result_cpp = self._load_file('Gauge.cpp')
        result_h = self._load_file('Gauge.h')
        self._generate_code('C++', source, 'Gauge')
        generated_cpp = self.vFiles['Gauge.cpp'].getvalue()
        generated_h = self.vFiles['Gauge.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'C++ source')
        self._compare(result_h, generated_h, 'C++ header')

    def test_generate_code_id(self):
        """\
        Test id code generation of all code generators

        @see: L{wxglade.codegen.cpp_codegen.generate_code_id}
        @see: L{wxglade.codegen.lisp_codegen.generate_code_id}
        @see: L{wxglade.codegen.pl_codegen.generate_code_id}
        @see: L{wxglade.codegen.py_codegen.generate_code_id}
        """
        # create dummy code object first
        obj = MockCodeObject()
        obj.preview = False
        obj.properties = {'id': 'wxID_ANY'}

        gen_id = common.code_writers['python'].generate_code_id
        cn = common.code_writers['python'].cn

        # check preview mode first
        obj.preview = True
        name, value = gen_id(obj)
        self.failUnless(
            (name, value) == ('', '-1'),
            """Expect "('s', '-1')" got "('%s', '%s')"!""" % (name, value)
            )

        # now we check non-preview only
        obj.preview = False

        # test for Python
        for test_id, target_decl, target_value in [
            ['wxID_ANY', '', cn('wxID_ANY')],                  # id => "wxID_ANY"
            ['=wxID_ANY', '', cn('wxID_ANY')],                 # id => "=wxID_ANY" (ugly!)
            ['', '', cn('wxID_ANY')],                          # id => "" 
            ['self.myid', '', 'self.myid'],                    # id => "self.myid"  (predefined class member)
            ['self.myid=1', 'self.myid = 1\n', 'self.myid'],   # id => "self.myid=1" 
            ['self.myid=?', 'self.myid = %s\n' % cn('wxNewId()') , 'self.myid'],             # id => "self.myid=?" 
            ['myid', '', 'myid'],                              # id => "myid" (global value and not a class member)
            ['myid = 1', 'global myid; myid = 1\n', 'myid'],   # id => "myid=1" (declare new global variable and store value)
            ]:
            obj.properties['id'] = test_id
            act_decl, act_value = gen_id(obj)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For Python expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)

                )

        # test for C++
        gen_id = common.code_writers['C++'].generate_code_id
        for test_id, target_decl, target_value in [
            ['wxID_ANY', '', 'wxID_ANY'],                      # id => "wxID_ANY"
            ['=wxID_ANY', '', 'wxID_ANY'],                     # id => "=wxID_ANY" (ugly!)
            ['', '', 'wxID_ANY'],                              # id => "" 
            ['myid', '', 'myid'],                              # id => "myid"  (predefined variable)
            ['myid=1', 'myid = 1', 'myid'],                    # id => "myid=1" 
            ['myid=?', 'myid = wxID_HIGHEST + 1000', 'myid'],  # id => "myid=?" 
            ['myid=?', 'myid = wxID_HIGHEST + 1001', 'myid'],  # id => "myid=?" 
            ]:
            obj.properties['id'] = test_id
            act_decl, act_value = gen_id(obj)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For C++ expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)
                )

        # test for Perl
        lang = 'Perl'
        gen_id = common.code_writers['perl'].generate_code_id
        cn = common.code_writers['perl'].cn
        for test_id, target_decl, target_value in [
            ['wxID_ANY', '', cn('wxID_ANY')],                  # id => "wxID_ANY"
            ['=wxID_ANY', '', cn('wxID_ANY')],                 # id => "=wxID_ANY" (ugly!)
            ['', '', cn('wxID_ANY')],                          # id => "" 
            ['myid', '', 'myid'],                              # id => "myid"  (predefined variable)
            ['myid=1', 'use constant myid => 1;\n', 'myid'],   # id => "myid=1" 
            ['myid=?', 'use constant myid => %s;\n' % cn('wxNewId()') , 'myid'],  # id => "myid=?" 
            ]:
            obj.properties['id'] = test_id
            act_decl, act_value = gen_id(obj)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For Perl expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)

                )

        # test for Lisp
        gen_id = common.code_writers['lisp'].generate_code_id
        cn = common.code_writers['lisp'].cn
        for test_id, target_decl, target_value in [
            ['wxID_ANY', '', cn('wxID_ANY')],               # id => "wxID_ANY"
            ['=wxID_ANY', '', cn('wxID_ANY')],              # id => "=wxID_ANY" (ugly!)
            ['', '', cn('wxID_ANY')],                         # id => "" 
            ['myid', '', 'myid'],                           # id => "myid"  (predefined variable)
            ['myid=1', 'global myid; myid = 1\n', 'myid'],  # id => "myid=1" 
            ['myid=?', 'global myid; myid = %s\n' % cn('wxNewId()') , 'myid'],  # id => "myid=?" 
            ]:
            obj.properties['id'] = test_id
            act_decl, act_value = gen_id(obj)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For Lisp expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)
                )
