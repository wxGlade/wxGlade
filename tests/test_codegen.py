"""
@copyright: 2012 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import cStringIO
import re

# import test base class
from tests import WXGladeBaseTest

# import project modules
import common
import misc


class MockCodeObject(object):
    """\
    Mock object for L{xml_parse.CodeObject}
    """
    preview = False
    properties = {}
    in_sizers = False
    in_windows = False
    name = ''
    klass = ''


class MockSourceFileContent(object):
    """\
    Mock object for L{codegen.BaseSourceFileContent}
    """
    spaces = {}


class TestCodeGen(WXGladeBaseTest):
    """\
    Test code generation
    """

    def tearDown(self):
        """\
        Cleanup
        """
        common.code_writers['python'].use_new_namespace = 1
        WXGladeBaseTest.tearDown(self)

    def test_CPP_wxCalendarCtrl(self):
        """\
        Test CPP code generation with a small wxCalendarCtrl example

        The test also tests for Sourceforge bug #2782306

        @see: L{wxglade.widgets.calendar_ctrl.calendar_ctrl}
        """
        self._generate_and_compare_cpp(
            'CPP_wxCalendarCtrl.wxg',
            'CPP_wxCalendarCtrl'
            )

    def test_CPP_Preferences(self):
        """\
        Test C++ code generation with preferences dialog

        @see: L{codegen.cpp_codegen}
        """
        self._generate_and_compare_cpp(
            'CPP_Preferences.wxg',
            'CPP_Preferences'
            )

    def test_Lisp_quote_path(self):
        """\
        Test codegen.lisp_codegen.quote_path()

        @see: L{codegen.lisp_codegen.LispCodeWriter.quote_path()}
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

        @see: L{codegen.lisp_codegen}
        """
        self._generate_and_compare(
            'lisp',
            'Lisp_Preferences.wxg',
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

        @see: L{codegen.pl_codegen}
        """
        self._generate_and_compare(
            'perl',
            'Perl_Preferences.wxg',
            'Perl_Preferences.pl'
            )

    def test_Python_Preferences(self):
        """\
        Test Python code generation with preferences dialog

        @see: L{codegen.py_codegen}
        """
        self._generate_and_compare(
            'python',
            'Preferences.wxg',
            'Python_Preferences.py'
            )

    def test_CalendarCtrl(self):
        """\
        Test code generation for a CalendarCtrl
        """
        self._generate_and_compare(
            'perl',
            'CalendarCtrl.wxg',
            'CalendarCtrl.pl'
            )
        self._generate_and_compare(
            'python',
            'CalendarCtrl.wxg',
            'CalendarCtrl.py'
            )
        self._generate_and_compare(
            'XRC',
            'CalendarCtrl.wxg',
            'CalendarCtrl.xrc'
            )
        self._generate_and_compare_cpp(
            'CalendarCtrl.wxg',
            'CalendarCtrl'
            )

    def test_FontColour(self):
        """\
        Test code generation for fonts and colours
        """
        self._test_all('FontColour')

    def test_Grid(self):
        """\
        Test code generation with a grid widgets and handling events
        """
        self._test_all('Grid')

    def test_Gauge(self):
        """\
        Test code generation for a wxGauge
        """
        self._test_all('Gauge')

    def test_HyperlinkCtrl(self):
        """\
        Test code generation for a HyperlinkCtrl for wxWidgets 2.6 and 2.8
        """
        # test for wxWidgets 2.6.X
        self._test_all('HyperlinkCtrl_26')

        # test for wxWidgets 2.8.X
        self._test_all('HyperlinkCtrl_28')

    def test_generate_code_id(self):
        """\
        Test id code generation of all code generators

        @see: L{codegen.cpp_codegen.CPPCodeWriter.generate_code_id()}
        @see: L{codegen.lisp_codegen.LispCodeWriter.generate_code_id()}
        @see: L{codegen.pl_codegen.PerlCodeWriter.generate_code_id()}
        @see: L{codegen.py_codegen.PythonCodeWriter.generate_code_id()}
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
            act_decl, act_value = gen_id(None, test_id)
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
        for test_id, target_decl, target_value in [
            ['wxID_ANY', '', 'wxID_ANY'],                      # id => "wxID_ANY"
            ['=wxID_ANY', '', 'wxID_ANY'],                     # id => "=wxID_ANY" (ugly!)
            ['', '', 'wxID_ANY'],                              # id => ""
            ['myid', '', 'myid'],                              # id => "myid"  (predefined variable)
            ['myid=1', 'myid = 1', 'myid'],                    # id => "myid=1"
            ['myid=?', 'myid = wxID_HIGHEST + 1002', 'myid'],  # id => "myid=?"
            ['myid=?', 'myid = wxID_HIGHEST + 1003', 'myid'],  # id => "myid=?"
            ]:
            act_decl, act_value = gen_id(None, test_id)
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
            act_decl, act_value = gen_id(None, test_id)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For Perl expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)
                )

        # test for Lisp
        gen_id = common.code_writers['lisp'].generate_code_id
        cn = common.code_writers['lisp'].cn
        for test_id, target_decl, target_value in [
            ['wxID_ANY',  '', cn('wxID_ANY')],              # id => "wxID_ANY"
            ['=wxID_ANY', '', cn('wxID_ANY')],              # id => "=wxID_ANY" (ugly!)
            ['', '', cn('wxID_ANY')],                       # id => ""
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
            act_decl, act_value = gen_id(None, test_id)
            self.failUnless(
                (act_decl, act_value) == (target_decl, target_value),
                """For Lisp expected "('%s', '%s')" got "('%s', '%s')"!""" % \
                    (target_decl, target_value, act_decl, act_value)
                )

    def test_Python_Ogg1(self):
        """\
        Test Python code generation with overwriting a single existing file

        @see: L{codegen.py_codegen.PythonCodeWriter}
        """
        self._generate_and_compare(
            'python',
            'PyOgg1.wxg',
            'PyOgg1.py'
            )

        # load XML input file
        source = self._load_file('PyOgg1.wxg')
        expected = self._load_file('PyOgg1_oldnamespace.py')
        source = self._modify_attrs(source,
            name='app',
            overwrite='1',
            path='PyOgg1_oldnamespace.py',
            use_new_namespace='0',
            )

        # generate code
        self._generate_code('python', source, 'PyOgg1_oldnamespace.py')
        generated = self.vFiles['PyOgg1_oldnamespace.py'].getvalue()
        self._compare(expected, generated)

    def test_Python_Ogg2(self):
        """\
        Test Python code generation with overwriting two existing files

        @see: L{codegen.py_codegen.PythonCodeWriter}
        """
        source = self._load_file('PyOgg2.wxg')
        source = self._modify_attrs(source,
            overwrite='0',
            )
        result_app    = self._load_file('PyOgg2_app.py')
        result_dialog = self._load_file('PyOgg2_MyDialog.py')
        result_frame  = self._load_file('PyOgg2_MyFrame.py')
        self._generate_code('python', source, './')
        generated_app    = self.vFiles['./PyOgg2_app.py'].getvalue()
        generated_dialog = self.vFiles['./PyOgg2_MyDialog.py'].getvalue()
        generated_frame  = self.vFiles['./PyOgg2_MyFrame.py'].getvalue()
        self._compare(result_app,    generated_app, 'PyOgg2_app.py')
        self._compare(result_dialog, generated_dialog, 'PyOgg2_MyDialog.py')
        self._compare(result_frame,  generated_frame , 'PyOgg2_MyFrame.py')

    def test_Python_Ogg3(self):
        """\
        Test Python code generation with overwriting a single existing file

        @see: L{codegen.py_codegen.PythonCodeWriter}
        """
        source = self._load_file('PyOgg2.wxg')
        expected = self._load_file('PyOgg3.py')

        # rename all occurencies of PyOgg2 to PyOgg3
        source = source.replace('PyOgg2', 'PyOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source,
            option='0',
            path='PyOgg3.py'
            )

        # generate and compare code
        self._generate_code('python', source, 'PyOgg3.py')
        generated = self.vFiles['PyOgg3.py'].getvalue()
        self._compare(expected, generated, 'PyOgg3.py')

    def test_Lisp_Ogg1(self):
        """\
        Test Lisp code generation with overwriting a single existing file

        @see: L{codegen.py_codegen.LispCodeWriter}
        """
        self._generate_and_compare(
            'lisp',
            'LispOgg1.wxg',
            'LispOgg1.lisp'
            )

    def test_Lisp_Ogg2(self):
        """\
        Test Lisp code generation with overwriting two existing files

        @see: L{codegen.py_codegen.LispCodeWriter}
        """
        source = self._load_file('LispOgg2.wxg')
        source = self._modify_attrs(source,
            overwrite='0',
            )
        result_app    = self._load_file('LispOgg2_app.lisp')
        result_dialog = self._load_file('LispOgg2_MyDialog.lisp')
        result_frame  = self._load_file('LispOgg2_MyFrame.lisp')
        self._generate_code('lisp', source, './')
        generated_app    = self.vFiles['./LispOgg2_app.lisp'].getvalue()
        generated_dialog = self.vFiles['./LispOgg2_MyDialog.lisp'].getvalue()
        generated_frame  = self.vFiles['./LispOgg2_MyFrame.lisp'].getvalue()
        self._compare(result_app,    generated_app, 'LispOgg2_app.lisp')
        self._compare(result_dialog, generated_dialog, 'LispOgg2_MyDialog.lisp')
        self._compare(result_frame,  generated_frame , 'LispOgg2_MyFrame.lisp')

    def test_Lisp_Ogg3(self):
        """\
        Test Lisp code generation with overwriting a single existing file

        @see: L{codegen.py_codegen.LispCodeWriter}
        """
        source = self._load_file('LispOgg2.wxg')
        expected = self._load_file('LispOgg3.lisp')

        # rename all occurencies of LispOgg2 to LispOgg3
        source = source.replace('LispOgg2', 'LispOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source,
            option='0',
            path='LispOgg3.lisp'
            )

        # generate and compare code
        self._generate_code('lisp', source, 'LispOgg3.lisp')
        generated = self.vFiles['LispOgg3.lisp'].getvalue()
        self._compare(expected, generated, 'LispOgg3.lisp')

    def test_Perl_Ogg1(self):
        """\
        Test Perl code generation with overwriting a single existing file

        @see: L{codegen.pl_codegen.PerlCodeWriter}
        """
        self._generate_and_compare(
            'perl',
            'PlOgg1.wxg',
            'PlOgg1.pl'
            )

    def test_Perl_Ogg2(self):
        """\
        Test Perl code generation with overwriting two existing files

        @see: L{codegen.pl_codegen.PerlCodeWriter}
        """
        source = self._load_file('PlOgg2.wxg')
        source = self._modify_attrs(source,
            overwrite='0',
            )
        result_app    = self._load_file('PlOgg2_app.pl')
        result_dialog = self._load_file('PlOgg2_MyDialog.pm')
        result_frame  = self._load_file('PlOgg2_MyFrame.pm')
        self._generate_code('perl', source, './')
        generated_app    = self.vFiles['./PlOgg2_app.pl'].getvalue()
        generated_dialog = self.vFiles['./PlOgg2_MyDialog.pm'].getvalue()
        generated_frame  = self.vFiles['./PlOgg2_MyFrame.pm'].getvalue()
        self._compare(result_app,    generated_app, 'PlOgg2_app.pl')
        self._compare(result_dialog, generated_dialog, 'PlOgg2_MyDialog.pm')
        self._compare(result_frame,  generated_frame , 'PlOgg2_MyFrame.pm')

    def test_Perl_Ogg3(self):
        """\
        Test Perl code generation with overwriting a single existing file

        @see: L{codegen.pl_codegen.PerlCodeWriter}
        """
        source = self._load_file('PyOgg2.wxg')
        expected = self._load_file('PyOgg3.py')

        # rename all occurencies of PyOgg2 to PyOgg3
        source = source.replace('PyOgg2', 'PyOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source,
            option='0',
            path='PyOgg3.py'
            )

        # generate and compare code
        self._generate_code('python', source, 'PyOgg3.py')
        generated = self.vFiles['PyOgg3.py'].getvalue()
        self._compare(expected, generated, 'PyOgg3.py')

    def test_CPP_Ogg1(self):
        """\
        Test C++ code generation with overwriting a single existing file

        @see: L{codegen.cpp_codegen.PythonCodeWriter}
        """
        self._generate_and_compare_cpp(
            'CPPOgg1.wxg',
            'CPPOgg1'
            )

    def test_CPP_Ogg2(self):
        """\
        Test C++ code generation with overwriting two existing files

        @see: L{codegen.cpp_codegen.CPPCodeWriter}
        """
        source = self._load_file('CPPOgg2.wxg')
        source = self._modify_attrs(source,
            overwrite='0',
            )
        result_app        = self._load_file('CPPOgg2_main.cpp')
        result_dialog_cpp = self._load_file('CPPOgg2_MyDialog.cpp')
        result_dialog_h   = self._load_file('CPPOgg2_MyDialog.h')
        result_frame_cpp  = self._load_file('CPPOgg2_MyFrame.cpp')
        result_frame_h    = self._load_file('CPPOgg2_MyFrame.h')
        self._generate_code('C++', source, './')
        generated_app    = self.vFiles['./main.cpp'].getvalue()
        generated_dialog_cpp = self.vFiles['./CPPOgg2_MyDialog.cpp'].getvalue()
        generated_dialog_h   = self.vFiles['./CPPOgg2_MyDialog.h'].getvalue()
        generated_frame_cpp  = self.vFiles['./CPPOgg2_MyFrame.cpp'].getvalue()
        generated_frame_h    = self.vFiles['./CPPOgg2_MyFrame.h'].getvalue()
        self._compare(result_app,    generated_app, 'main.cpp')
        self._compare(result_dialog_cpp, generated_dialog_cpp, 'CPPOgg2_MyDialog.cpp')
        self._compare(result_dialog_h,   generated_dialog_h,   'CPPOgg2_MyDialog.h')
        self._compare(result_frame_cpp,  generated_frame_cpp , 'CPPOgg2_MyFrame.cpp')
        self._compare(result_frame_h,    generated_frame_h,    'CPPOgg2_MyFrame.h')

    def test_CPP_Ogg3(self):
        """\
        Test C++ code generation with overwriting a single existing file

        @see: L{codegen.cpp_codegen.CPPCodeWriter}
        """
        source = self._load_file('CPPOgg2.wxg')
        result_cpp = self._load_file('CPPOgg3.cpp')
        result_h   = self._load_file('CPPOgg3.h')

        # rename all occurencies of CPPOgg2 to CPPOgg3
        source = source.replace('CPPOgg2', 'CPPOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source,
            option='0',
            path='CPPOgg3'
            )

        # generate and compare code
        self._generate_code('C++', source, 'CPPOgg3')
        generated_cpp = self.vFiles['CPPOgg3.cpp'].getvalue()
        generated_h   = self.vFiles['CPPOgg3.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'CPPOgg3.cpp')
        self._compare(result_h,   generated_h,   'CPPOgg3.h')

    def test_PerlSourceFileContent_regexp(self):
        """\
        Test some regular expessions used in L{codegen.pl_codegen.SourceFileContent}
        """
        import codegen.pl_codegen
        lang = 'Perl'
        source = codegen.pl_codegen.SourceFileContent
        re = source.rec_block_start
        for line, expected in [
            ('# begin wxGlade: extracode',              ('', None, 'extracode')),
            ('    # begin wxGlade: ::extracode',        ('    ', None, 'extracode')),
            ('#begin wxGlade: dependencies',            ('', None, 'dependencies')),
            ('#begin wxGlade: ::dependencies',          ('', None, 'dependencies')),
            ('    # begin wxGlade: wxGladePreferencesUI::new',              ('    ', 'wxGladePreferencesUI', 'new')),
            ('    # begin wxGlade: wxGladePreferencesUI::__do_layout',      ('    ', 'wxGladePreferencesUI', '__do_layout')),
            ('    # begin wxGlade: wxGladePreferencesUI::__set_properties', ('    ', 'wxGladePreferencesUI', '__set_properties')),
            ]:
            result = re.match(line)
            self.failUnless(
                result,
                '%s: Line "%s" does not fit pattern!' % (lang, line)
                )
            self.failUnless(
                result.groups() == expected,
                '%s: Unexpected result for line "%s":\n   got: "%s"\nexpect: "%s"' % (lang, line, result.groups(), expected)
                )

    def test_PythonSourceFileContent_regexp(self):
        """\
        Test some regular expessions used in L{codegen.py_codegen.SourceFileContent}
        """
        import codegen.py_codegen
        lang = 'Python'
        source = codegen.py_codegen.SourceFileContent
        re = source.rec_block_start
        for line, expected in [
            ('# begin wxGlade: extracode',      ('', None, 'extracode')),
            ('    # begin wxGlade: extracode',  ('    ', None, 'extracode')),
            ('#begin wxGlade: dependencies',    ('', None, 'dependencies')),
            ('    # begin wxGlade: PyOgg3_MyDialog.__init__',        ('    ', 'PyOgg3_MyDialog', '__init__')),
            ('    # begin wxGlade: PyOgg3_MyFrame.__do_layout',      ('    ', 'PyOgg3_MyFrame', '__do_layout')),
            ('    # begin wxGlade: PyOgg3_MyFrame.__set_properties', ('    ', 'PyOgg3_MyFrame', '__set_properties')),
            ]:
            result = re.match(line)
            self.failUnless(
                result,
                '%s: Line "%s" does not fit pattern!' % (lang, line)
                )
            self.failUnless(
                result.groups() == expected,
                '%s: Unexpected result for line "%s":\n   got: "%s"\nexpect: "%s"' % (lang, line, result.groups(), expected)
                )
        re = source.rec_event_handler
        for line, expected in [
            ('   def myEVT_GRID_CELL_LEFT_CLICK(self, event):  # wxGlade: MyFrame.<event_handler>',   ('myEVT_GRID_CELL_LEFT_CLICK', 'MyFrame')),
            ('   def startConverting(self, event):  # wxGlade: PyOgg1_MyDialog.<event_handler>',      ('startConverting', 'PyOgg1_MyDialog')),
            ]:
            result = re.match(line)
            self.failUnless(
                result,
                '%s: Line "%s" does not fit pattern!' % (lang, line)
                )
            self.failUnless(
                result.groups() == expected,
                '%s: Unexpected result for line "%s":\n   got: "%s"\nexpect: "%s"' % (lang, line, result.groups(), expected)
                )

    def test_content_notfound(self):
        """\
        Test replacement of not found blocks with a warning message

        @see: L{codegen.BaseCodeWriter._content_notfound()}
        """
        import codegen
        base_codegen = codegen.BaseCodeWriter()
        base_codegen.previous_source = MockSourceFileContent()

        # this is to be more sure to replace the right tags
        base_codegen.nonce = '12G34'
        base_codegen.comment_sign = "#"

        source_sample = """\
<12G34wxGlade replace %(pattern)s>
        """

        expected_sample = """\
%(indent)s# Content of this block not found. Did you rename this class?

        """

        for pattern, indent in [
            ('MissingClass method', '  '),
            ('MissingClass method', '  '),
            ('MissingClass Method', '---'),
            ('MissingClass method', '\t'),
            ('MissingClass method', ''),
            ('MissingClass .',      ''),
            ]:
            source = source_sample % {
                'pattern': pattern,
                'indent':  indent,
                }
            expected = expected_sample % {
                'pattern': pattern,
                'indent':  indent,
                }

            if indent:
                base_codegen.previous_source.spaces['MissingClass'] = indent
            else:
                base_codegen.previous_source.spaces = {}

            result = base_codegen._content_notfound(
                source,
                )

            self.failUnless(
                result == expected,
                'Unexpected result for pattern "%s":\n   got: "%s"\nexpect: "%s"' % (pattern, result, expected)
                )

    def test_add_app(self):
        """\
        Test the generation of application start code

        @see: L{codegen.py_codegen.PythonCodeWriter.add_app()}
        """
        for language, prefix, suffix in [
            ['python', 'Py',    '.py'],
            ['perl',   'Pl',    '.pl'],
            ['C++',    'CPP',   '.cpp'],
            ['lisp',   'Lisp',  '.lisp']
            ]:
            for multiple_files in [0,  1]:
                for klass in ['MyStartApp', None]:
                    # prepare code writer
                    codewriter = common.code_writers[language]

                    # path must be a directory for multiple files
                    if multiple_files:
                        path = './'
                    else:
                        path = 'myoutputfile'

                    for use_gettext in [0, 1]:
                        codewriter.initialize({
                            'indent_amount': '4',
                            'indent_symbol': 'space',
                            'language': language,
                            'name': 'myapp',
                            'option': multiple_files,
                            'overwrite': 1,
                            'path': path,
                            'use_gettext': use_gettext,
                            'use_new_namespace': 1,
                            })

                        # clear output_file
                        if codewriter.output_file:
                            codewriter.output_file.close()
                        codewriter.output_file = cStringIO.StringIO()

                        # generate application start code
                        codewriter.add_app({
                            'class': klass,
                            'top_window': 'appframe',
                            },
                            'MyAppFrame')

                        if use_gettext:
                            fn_gettext = '_gettext'
                        else:
                            fn_gettext = ''
                        if klass:
                            simple = '_detailed'
                        else:
                            simple = '_simple'

                        if language == 'C++':
                            app_filename = './main.cpp'
                        else:
                            app_filename = './myapp%s' % suffix

                        if multiple_files:
                            generated = self.vFiles[app_filename].getvalue()
                            multiple = '_multi'
                        else:
                            generated = codewriter.output_file.getvalue()
                            multiple = '_single'

                        filename = '%sAddApp%s%s%s%s' % \
                            (prefix, multiple, fn_gettext, simple, suffix)
                        expected = self._load_file(filename)


                        self._compare(
                            expected,
                            generated,
                            '%s (%s)' % (misc.capitalize(language), filename),
                            )

                        # close open virtual files
                        for name in self.vFiles.keys():
                            self.vFiles[name].close()
                            del self.vFiles[name]

    def test_no_suitable_writer(self):
        """\
        Test the generation of a warning if no suitable writer has been found
        for a specific widget.
        
        This tests only the case of a known widget but the language specific
        widget handler is missing.
        
        This test doesn't cover the missing of a whole widget.
        
        @see: L{codegen.BaseCodeWriter._add_object_init()}
        """
        for lang, ext in [
            ('python', '.py'),
            ('perl', '.pl'),
            ('lisp', '.lisp'),
            ('XRC', '.xrc'),
            ('C++', ''),
            ]:
            codegen = common.code_writers.get(lang)
            handler = codegen.obj_builders['wxButton']
            del codegen.obj_builders['wxButton']
            
            # don' use _generate_and_compare() resp.
            # _generate_and_compare_cpp() because a failure wouldn't restore
            # the temporarily removed widget
            if lang == 'C++':
                inname = 'no_suitable_writer.wxg'
                outname = 'no_suitable_writer'
                name_h = '%s.h' % outname
                name_cpp = '%s.cpp' % outname

                # load XML input file
                source = self._load_file(inname)
                result_cpp = self._load_file(name_cpp)
                result_h = self._load_file(name_h)

                # generate and compare C++ code
                self._generate_code('C++', source, outname)
                generated_cpp = self.vFiles[name_cpp].getvalue()
                generated_h = self.vFiles[name_h].getvalue()
                
                # restore deleted handler
                codegen.obj_builders['wxButton'] = handler
                
                # compare generated and expected code
                self._compare(result_cpp, generated_cpp, 'C++ source')
                self._compare(result_h, generated_h, 'C++ header')

            else:
                # load XML input file
                inname = 'no_suitable_writer.wxg'
                outname = 'no_suitable_writer%s' % ext
                source = self._load_file(inname)
                expected = self._load_file(outname)

                # generate code
                self._generate_code(lang, source, outname)
                generated = self.vFiles[outname].getvalue()
                
                # restore deleted handler
                codegen.obj_builders['wxButton'] = handler
                
                # compare generated and expected code
                self._compare(expected, generated)                
            

    def test_add_class_inplace(self):
        """\
        Test appending of a new class to an existing file without overwriting.
        """
        # Test Lisp code generator
        #=========================
        # load XML input file
        source = self._load_file('add_class_inplace_extended.wxg')
        expected = self._load_file('add_class_inplace_extended.lisp')

        # generate code
        self._generate_code('lisp', source, 'add_class_inplace_orig.lisp')
        generated = self.vFiles['add_class_inplace_orig.lisp'].getvalue()
        self._compare(expected, generated)

        # Test Perl code generator
        #=========================
        # load XML input file
        source = self._load_file('add_class_inplace_extended.wxg')
        expected = self._load_file('add_class_inplace_extended.pl')

        # generate code
        self._generate_code('perl', source, 'add_class_inplace_orig.pl')
        generated = self.vFiles['add_class_inplace_orig.pl'].getvalue()
        self._compare(expected, generated)

        # Test Python code generator
        #===========================
        # load XML input file
        source = self._load_file('add_class_inplace_extended.wxg')
        expected = self._load_file('add_class_inplace_extended.py')

        # generate code
        self._generate_code('python', source, 'add_class_inplace_orig.py')
        generated = self.vFiles['add_class_inplace_orig.py'].getvalue()
        self._compare(expected, generated)

        # Test XRC code generator
        #========================
        # load XML input file
        source = self._load_file('add_class_inplace_extended.wxg')
        expected = self._load_file('add_class_inplace_extended.xrc')

        # generate code
        self._generate_code('XRC', source, 'add_class_inplace_orig.xrc')
        generated = self.vFiles['add_class_inplace_orig.xrc'].getvalue()
        self._compare(expected, generated)

        # Test C++ code generator
        #========================
        # load XML input file
        source = self._load_file('add_class_inplace_extended.wxg')
        expected_cpp = self._load_file('add_class_inplace_extended.cpp')
        expected_h   = self._load_file('add_class_inplace_extended.h')

        # generate code
        self._generate_code('C++', source, 'add_class_inplace_orig.xrc')
        gen_cpp = self.vFiles['add_class_inplace_orig.cpp'].getvalue()
        gen_h   = self.vFiles['add_class_inplace_orig.h'].getvalue()
        self._compare(expected_cpp, gen_cpp, 'C++ source')
        self._compare(expected_h, gen_h, 'C++ header')

    def test_remove_class_inplace(self):
        """\
        Test class removal in an existing file without overwriting.
        """
        # Test Lisp code generator
        #=========================
        # load XML input file
        source = self._load_file('remove_class_inplace.wxg')
        expected = self._load_file('remove_class_inplace_expected.lisp')

        # generate code
        self._generate_code('lisp', source, 'remove_class_inplace_input.lisp')
        generated = self.vFiles['remove_class_inplace_input.lisp'].getvalue()
        self._compare(expected, generated)

        # Test Perl code generator
        #=========================
        # load XML input file
        source = self._load_file('remove_class_inplace.wxg')
        expected = self._load_file('remove_class_inplace_expected.pl')

        # generate code
        self._generate_code('perl', source, 'remove_class_inplace_input.pl')
        generated = self.vFiles['remove_class_inplace_input.pl'].getvalue()
        self._compare(expected, generated)

        # Test Python code generator
        #===========================
        # load XML input file
        source = self._load_file('remove_class_inplace.wxg')
        expected = self._load_file('remove_class_inplace_expected.py')

        # generate code
        self._generate_code('python', source, 'remove_class_inplace_input.py')
        generated = self.vFiles['remove_class_inplace_input.py'].getvalue()
        self._compare(expected, generated)

        # Test XRC code generator
        #========================
        # load XML input file
        source = self._load_file('remove_class_inplace.wxg')
        expected = self._load_file('remove_class_inplace_expected.xrc')

        # generate code
        self._generate_code('XRC', source, 'remove_class_inplace_input.xrc')
        generated = self.vFiles['remove_class_inplace_input.xrc'].getvalue()
        self._compare(expected, generated)

        # Test C++ code generator
        #========================
        # load XML input file
        source = self._load_file('remove_class_inplace.wxg')
        expected_cpp = self._load_file('remove_class_inplace_expected.cpp')
        expected_h   = self._load_file('remove_class_inplace_expected.h')

        # generate code
        self._generate_code('C++', source, 'remove_class_inplace_input.xrc')
        gen_cpp = self.vFiles['remove_class_inplace_input.cpp'].getvalue()
        gen_h   = self.vFiles['remove_class_inplace_input.h'].getvalue()
        self._compare(expected_cpp, gen_cpp, 'C++ source')
        self._compare(expected_h, gen_h, 'C++ header')

    def test_format_classattr(self):
        """\
        Test formatting names as class attributes
        
        @see: L{codegen.BaseCodeWriter._format_classattr()}
        """
        details = {}
        details['python'] = [
                ('', ''),
                ('self.mywidget', 'self.mywidget'),
                ('1myclass', 'self.1myclass'),
                ('_myclass', 'self._myclass'),
                ('myclass',  'self.myclass'),
                ('label1',   'self.label1'),
                ('label_1',  'self.label_1'),
                ('_label',   'self._label'),
                ('_label_1', 'self._label_1'),
                ('20, 30', '(20, 30)'),
                ]
        details['perl'] = [
                ('', ''),
                ('$self->{mywidget}', '$self->{mywidget}'),
                ('1myclass', '$self->{1myclass}'),
                ('_myclass', '$self->{_myclass}'),
                ('$mybox',   '$mybox'),
                ('myclass',  '$self->{myclass}'),
                ('label1',   '$self->{label1}'),
                ('label_1',  '$self->{label_1}'),
                ('_label',   '$self->{_label}'),
                ('_label_1', '$self->{_label_1}'),
                ('20, 30', '20, 30'),
                ]
        details['lisp'] = [
                ('', ''),
                ('slot-mywidget', 'slot-mywidget'),
                ('1myclass', 'slot-1myclass'),
                ('_myclass', 'slot--myclass'),
                ('myclass',  'slot-myclass'),
                ('my_class', 'slot-my-class'),
                ('label1',   'slot-label1'),
                ('label_1',  'slot-label-1'),
                ('_label',   'slot--label'),
                ('_label_1', 'slot--label-1'),
                ('20, 30', '(20, 30)'),
                ]
        details['C++'] = [
                ('', ''),
                ('mywidget', 'mywidget'),
                ('1myclass', '1myclass'),
                ('_myclass', '_myclass'),
                ('myclass',  'myclass'),
                ('my_class', 'my_class'),
                ('label1',   'label1'),
                ('label_1',  'label_1'),
                ('_label',   '_label'),
                ('_label_1', '_label_1'),
                ('20, 30', '20, 30'),
                ]
        for lang in ['python', 'perl', 'lisp', 'C++']:
            codegen = common.code_writers.get(lang)
            ret = codegen._format_classattr(None)
            self.failUnlessEqual(
                '',
                ret,
                '%s: Unexpected result got: "%s" expect: "%s"' % (
                    lang.capitalize(),
                    ret,
                    ''
                    )
                )
            for name, expected in details[lang]:
                obj = MockCodeObject()
                obj.name = name
                if name == '20, 30':
                    obj.klass = 'spacer'
                else:
                    obj.klass = 'wxButton'
                ret = codegen._format_classattr(obj)
                self.failUnlessEqual(
                    expected,
                    ret,
                    '%s: Unexpected result got: "%s" expect: "%s"' % (
                        lang.capitalize(),
                        ret,
                        expected,
                        )
                    )

    def test_sizer_references(self):
        """\
        Test storing references to sizers in class attributes
        """
        # don't store sizer references
        self._test_all('Sizers_no_classattr')
        
        # store sizer references
        self._test_all('Sizers_classattr')

    def test_cn(self):
        """\
        Test formatting of names with cn().

        @see: L{codegen.cpp_codegen.CPPCodeWriter.cn()}
        @see: L{codegen.lisp_codegen.LispCodeWriter.cn()}
        @see: L{codegen.pl_codegen.PerlCodeWriter.cn()}
        @see: L{codegen.py_codegen.PythonCodeWriter.cn()}
        """
        details = {}
        details['python_new'] = [  # new namespace
            ('wxID_OK',        'wx.ID_OK'),
            ('wxALL',          'wx.ALL'),
            ('wxBLUE',         'wx.BLUE'),
            ('wxALIGN_CENTER', 'wx.ALIGN_CENTER'),
            ('wxFrame',        'wx.Frame'),
            ('EVT_BUTTON',     'wx.EVT_BUTTON'),
                ]
        details['python_old'] = [  # old namespace
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wxBLUE',         'wxBLUE'),
            ('wxALIGN_CENTER', 'wxALIGN_CENTER'),
            ('wxFrame',        'wxFrame'),
            ('EVT_BUTTON',     'EVT_BUTTON'),
            ]
        details['perl'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wxBLUE',         'wxBLUE'),
            ('wxALIGN_CENTER', 'wxALIGN_CENTER'),
            ('wxFrame',        'Wx::Frame'),
            ('EVT_BUTTON',     'Wx::EVT_BUTTON'),
            ]
        details['lisp'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wxBLUE',         'wxBLUE'),
            ('wxALIGN_CENTER', 'wxALIGN_CENTER'),
            ('wxFrame',        'wxFrame'),
            ('EVT_BUTTON',     'wxEVT_BUTTON'),
            ]
        details['C++'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wxBLUE',         'wxBLUE'),
            ('wxALIGN_CENTER', 'wxALIGN_CENTER'),
            ('wxFrame',        'wxFrame'),
            ('EVT_BUTTON',     'EVT_BUTTON'),
            ]

        for lang in ['python_new', 'python_old', 'perl', 'lisp', 'C++']:
            if lang.startswith('python'):
                if lang == 'python_old':
                    common.code_writers['python'].use_new_namespace = 0
                elif lang == 'python_new':
                    common.code_writers['python'].use_new_namespace = 1
                cn = common.code_writers['python'].cn
            else:
                cn = common.code_writers[lang].cn

            for unformatted, formatted in details[lang]:
                ret = cn(unformatted)
                self.failUnlessEqual(
                    formatted,
                    ret,
                    '%s: Unexpected result got: "%s" expect: "%s"' % (
                        lang,
                        ret,
                        formatted,
                        )
                    )
                # test for prevent double formatting
                ret = cn(formatted)
                self.failUnlessEqual(
                    formatted,
                    ret,
                    '%s: Unexpected result got: "%s" expect: "%s"' % (
                        lang,
                        ret,
                        formatted,
                        )
                    )

    def test_ComplexExample(self):
        """\
        Test code generation for a complex example
        """
        self._test_all('ComplexExample')

    def test_StylelessDialog(self):
        """\
        Test code generation for a style less dialog
        """
        self._test_all('styleless-dialog')

    def test_quote_str(self):
        """\
        Test string quotation for Perl and Python
        
        @see: L{codegen.pl_codegen.PerlCodeWriter.quote_str()}
        @see: L{codegen.py_codegen.PythonCodeWriter.quote_str()}
        """
        details = {}
        details['python'] = [
                (None, '""'),
                ('', '""'),
                ('My 1st and simple ascii test!', '_("My 1st and simple ascii test!")'),
                ('\xe2\x99\xa5 Love this song', '_(u"\\u2665 Love this song")'),
                ('\xe2\x99\xa5 Love \xe2\x99\xa5', '_(u"\\u2665 Love \\u2665")'),
                ]
        details['perl'] = [
                (None, '""'),
                ('', '""'),
                ('My 1st and simple ascii test!', '_T("My 1st and simple ascii test!")'),
                ('\xe2\x99\xa5 Love this song', '_T("\\N{U+2665} Love this song")'),
                ('\xe2\x99\xa5 Love \xe2\x99\xa5', '_T("\\N{U+2665} Love \\N{U+2665}")'),
                ('\xe2\x99\xa52 Love \xe2\x99\xa5', '_T("\u26652 Love \N{U+2665}")'),
                ]
        for lang in ['python', 'perl']:
            codegen = common.code_writers.get(lang)
            codegen._use_gettext = True
            for unformatted, formatted in details[lang]:
                ret = codegen.quote_str(unformatted)
                self.failUnlessEqual(
                    formatted,
                    ret,
                    '%s: Unexpected result got: "%s" expect: "%s"' % (
                        lang,
                        ret,
                        formatted,
                        )
                    )