"""\
Code genera Commandline / non-graphical tests

@copyright: 2012-2016 Carsten Grohmann

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os

# import test base class
from tests import WXGladeBaseTest

# import project modules
import common
import config
import compat
import errors
import misc
import xrc2wxg


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

    def setUp(self):
        WXGladeBaseTest.setUp(self)
        xrc2wxg._write_timestamp = False

    def tearDown(self):
        WXGladeBaseTest.tearDown(self)

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

    def test_Preferences_dialog(self):
        """\
        Test code generation with preferences dialog
        """
        for language, prefix, ext in self.language_constants:
            if language == 'XRC':
                continue
            inname = '%s_Preferences.wxg' % prefix

            if language == 'C++':
                outname = '%s_Preferences' % prefix
                self._generate_and_compare_cpp(
                    inname,
                    outname
                )
            else:
                outname = '%s_Preferences%s' % (prefix, ext)
                self._generate_and_compare(
                    language,
                    inname,
                    outname
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
        Test code generation for a HyperlinkCtrl
        """
        # test for wxWidgets 2.8.X
        self._test_all('HyperlinkCtrl_28')

    def test_generate_code_id(self):
        """\
        Test id code generation of all code generators

        @see: L{codegen.cpp_codegen.CPPCodeWriter.generate_code_id()}
        @see: L{codegen.lisp_codegen.LispCodeWriter.generate_code_id()}
        @see: L{codegen.perl_codegen.PerlCodeWriter.generate_code_id()}
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

    def test_Python_Ogg2(self):
        """\
        Test Python code generation with overwriting two existing files

        @see: L{codegen.py_codegen.PythonCodeWriter}
        """
        source = self._load_file('PyOgg2.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app    = self._load_file('PyOgg2_app.py')
        result_dialog = self._load_file('PyOgg2_MyDialog.py')
        result_frame  = self._load_file('PyOgg2_MyFrame.py')

        self._generate_code('python', source, self.curr_dir)

        generated_app    = self.vFiles[self._with_curr_dir('PyOgg2_app.py')].getvalue()
        generated_dialog = self.vFiles[self._with_curr_dir('PyOgg2_MyDialog.py')].getvalue()
        generated_frame  = self.vFiles[self._with_curr_dir('PyOgg2_MyFrame.py')].getvalue()
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

        # rename all occurrences of PyOgg2 to PyOgg3
        source = source.replace('PyOgg2', 'PyOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source, option='0', path='PyOgg3.py')

        # generate and compare code
        self._generate_code('python', source, 'PyOgg3.py')
        generated = self.vFiles['PyOgg3.py'].getvalue()
        self._compare(expected, generated, 'PyOgg3.py')

    def test_Lisp_Ogg1(self):
        """\
        Test Lisp code generation with overwriting a single existing file

        @see: L{codegen.py_codegen.LispCodeWriter}
        """
        self._generate_and_compare('lisp', 'LispOgg1.wxg', 'LispOgg1.lisp')

    def test_Lisp_Ogg2(self):
        """\
        Test Lisp code generation with overwriting two existing files

        @see: L{codegen.py_codegen.LispCodeWriter}
        """
        source = self._load_file('LispOgg2.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app    = self._load_file('LispOgg2_app.lisp')
        result_dialog = self._load_file('LispOgg2_MyDialog.lisp')
        result_frame  = self._load_file('LispOgg2_MyFrame.lisp')

        self._generate_code('lisp', source, self.curr_dir)

        generated_app    = self.vFiles[self._with_curr_dir('LispOgg2_app.lisp')].getvalue()
        generated_dialog = self.vFiles[self._with_curr_dir('LispOgg2_MyDialog.lisp')].getvalue()
        generated_frame  = self.vFiles[self._with_curr_dir('LispOgg2_MyFrame.lisp')].getvalue()
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

        # rename all occurrences of LispOgg2 to LispOgg3
        source = source.replace('LispOgg2', 'LispOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source, option='0', path='LispOgg3.lisp')

        # generate and compare code
        self._generate_code('lisp', source, 'LispOgg3.lisp')
        generated = self.vFiles['LispOgg3.lisp'].getvalue()
        self._compare(expected, generated, 'LispOgg3.lisp')

    def test_Perl_Ogg1(self):
        """\
        Test Perl code generation with overwriting a single existing file

        @see: L{codegen.perl_codegen.PerlCodeWriter}
        """
        self._generate_and_compare('perl', 'PlOgg1.wxg', 'PlOgg1.pl')

    def test_Perl_Ogg2(self):
        """\
        Test Perl code generation with overwriting two existing files

        @see: L{codegen.perl_codegen.PerlCodeWriter}
        """
        source = self._load_file('PlOgg2.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app    = self._load_file('PlOgg2_app.pl')
        result_dialog = self._load_file('PlOgg2_MyDialog.pm')
        result_frame  = self._load_file('PlOgg2_MyFrame.pm')

        self._generate_code('perl', source, self.curr_dir)

        generated_app    = self.vFiles[self._with_curr_dir('PlOgg2_app.pl')].getvalue()
        generated_dialog = self.vFiles[self._with_curr_dir('PlOgg2_MyDialog.pm')].getvalue()
        generated_frame  = self.vFiles[self._with_curr_dir('PlOgg2_MyFrame.pm')].getvalue()
        self._compare(result_app,    generated_app, 'PlOgg2_app.pl')
        self._compare(result_dialog, generated_dialog, 'PlOgg2_MyDialog.pm')
        self._compare(result_frame,  generated_frame , 'PlOgg2_MyFrame.pm')

    def test_Perl_Ogg3(self):
        """\
        Test Perl code generation with overwriting a single existing file

        @see: L{codegen.perl_codegen.PerlCodeWriter}
        """
        source = self._load_file('PlOgg2.wxg')
        expected = self._load_file('PlOgg3.pl')

        # rename all occurrences of PlOgg2 to PlOgg3
        source = source.replace('PlOgg2', 'PlOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source, option='0', path='PlOgg3.pl')

        # generate and compare code
        self._generate_code('perl', source, 'PlOgg3.pl')
        generated = self.vFiles['PlOgg3.pl'].getvalue()
        self._compare(expected, generated, 'PlOgg3.pl')

    def test_CPP_Ogg1(self):
        """\
        Test C++ code generation with overwriting a single existing file

        @see: L{codegen.cpp_codegen.PythonCodeWriter}
        """
        self._generate_and_compare_cpp('CPPOgg1.wxg', 'CPPOgg1')

    def test_CPP_Ogg2(self):
        """\
        Test C++ code generation with overwriting two existing files

        @see: L{codegen.cpp_codegen.CPPCodeWriter}
        """
        codegen = common.code_writers['C++']
        source = self._load_file('CPPOgg2.wxg')
        source = self._modify_attrs(source, overwrite='0')
        result_app        = self._load_file('CPPOgg2_main.cpp')
        result_dialog_cpp = self._load_file('CPPOgg2_MyDialog.cpp')
        result_dialog_h   = self._load_file('CPPOgg2_MyDialog.h')
        result_frame_cpp  = self._load_file('CPPOgg2_MyFrame.cpp')
        result_frame_h    = self._load_file('CPPOgg2_MyFrame.h')

        self._generate_code('C++', source, self.curr_dir)

        app_filename = codegen._generate_app_filename()
        main_cpp = self._with_curr_dir(app_filename)
        generated_app    = self.vFiles[main_cpp].getvalue()
        generated_dialog_cpp = self.vFiles[self._with_curr_dir('CPPOgg2_MyDialog.cpp')].getvalue()
        generated_dialog_h   = self.vFiles[self._with_curr_dir('CPPOgg2_MyDialog.h')].getvalue()
        generated_frame_cpp  = self.vFiles[self._with_curr_dir('CPPOgg2_MyFrame.cpp')].getvalue()
        generated_frame_h    = self.vFiles[self._with_curr_dir('CPPOgg2_MyFrame.h')].getvalue()
        self._compare(result_app,    generated_app, app_filename)
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

        # rename all occurrences of CPPOgg2 to CPPOgg3
        source = source.replace('CPPOgg2', 'CPPOgg3')

        # set option="0" for writing into a single file
        source = self._modify_attrs(source, option='0', path='CPPOgg3')

        # generate and compare code
        self._generate_code('C++', source, 'CPPOgg3')
        generated_cpp = self.vFiles['CPPOgg3.cpp'].getvalue()
        generated_h   = self.vFiles['CPPOgg3.h'].getvalue()
        self._compare(result_cpp, generated_cpp, 'CPPOgg3.cpp')
        self._compare(result_h,   generated_h,   'CPPOgg3.h')

    def test_PerlSourceFileContent_regexp(self):
        """\
        Test some regular expressions used in L{codegen.perl_codegen.SourceFileContent}
        """
        import codegen.perl_codegen
        lang = 'Perl'
        source = codegen.perl_codegen.SourceFileContent
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

        @see: L{codegen.BaseLangCodeWriter._content_notfound()}
        """
        import codegen
        base_codegen = codegen.BaseLangCodeWriter()
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
        for language, prefix, suffix in self.language_constants:
            if language == 'XRC':
                continue
            for multiple_files in [0, 1]:
                for klass in ['MyStartApp', None]:
                    for use_gettext in [0, 1]:
                        for top_window in ['appframe', '']:
                            for appname in ['myapp', '']:

                                # run test
                                self._test_add_app(language, prefix, suffix,
                                                   multiple_files, klass,
                                                   use_gettext, top_window,
                                                   appname)

                                # close open virtual files
                                for name in self.vFiles.keys():
                                    self.vFiles[name].close()
                                    del self.vFiles[name]

    def _test_add_app(self, language, prefix, suffix, multiple_files,
                      klass, use_gettext, top_window, appname):

        # prepare code writer
        codewriter = common.code_writers[language]

        # path must be a directory for multiple files
        if multiple_files:
            path = './'
        else:
            path = 'myoutputfile'

        codewriter.new_project(indent_amount='4', indent_symbol='space', language=language, name=appname,
                               option=multiple_files, overwrite=1, path=path, use_gettext=use_gettext)
        # clear output_file
        if codewriter.output_file:
            codewriter.output_file.close()
        codewriter.output_file = compat.StringIO()

        # generate application start code
        codewriter.add_app({'class': klass,
                            'top_window': top_window, },
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
            app_filename = self._with_curr_dir(codewriter._generate_app_filename())
        else:
            app_filename = self._with_curr_dir('myapp%s' % suffix)

        # top window and application name are mandatory
        if top_window and appname:
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

        # top window is not set
        else:
            generated = ''
            if multiple_files:
                if self.vFiles:
                    generated = self.vFiles[app_filename].getvalue()
            else:
                generated = codewriter.output_file.getvalue()

            if generated:
                self.fail(
                    'Unexpected application startup code '
                    'for %s (multi=%s, gettext=%s, '
                    'top window=%s):\n%s' % (
                        misc.capitalize(language),
                        multiple_files, use_gettext,
                        top_window, generated)
                )

    def test_no_suitable_writer(self):
        """\
        Test the generation of a warning if no suitable writer has been found
        for a specific widget.
        
        This tests only the case of a known widget but the language specific
        widget handler is missing.
        
        This test doesn't cover the missing of a whole widget.
        
        @see: L{codegen.BaseLangCodeWriter._add_object_init()}
        """
        for language, dummy, ext in self.language_constants:
            codegen = common.code_writers.get(language)
            handler = codegen.obj_builders['wxButton']
            del codegen.obj_builders['wxButton']

            # don' use _generate_and_compare() resp.
            # _generate_and_compare_cpp() because a failure wouldn't restore
            # the temporarily removed widget
            if language == 'C++':
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
                self._generate_code(language, source, outname)
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
        
        @see: L{codegen.BaseLangCodeWriter._format_classattr()}
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
                    misc.capitalize(lang),
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
                        misc.capitalize(lang),
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
        @see: L{codegen.perl_codegen.PerlCodeWriter.cn()}
        @see: L{codegen.py_codegen.PythonCodeWriter.cn()}
        """
        details = {}
        details['python'] = [
            ('wxID_OK',        'wx.ID_OK'),
            ('wxALL',          'wx.ALL'),
            ('wxBLUE',         'wx.BLUE'),
            ('wxALIGN_CENTER', 'wx.ALIGN_CENTER'),
            ('wxFrame',        'wx.Frame'),
            ('EVT_BUTTON',     'wx.EVT_BUTTON'),
                ]
        details['perl'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wxBLUE',         'wxBLUE'),
            ('wxALIGN_CENTER', 'wxALIGN_CENTER'),
            ('wxFrame',        'Wx::Frame'),
            ('EVT_BUTTON',     'Wx::Event::EVT_BUTTON'),
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

        for lang in ['python', 'perl', 'lisp', 'C++']:
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

    def test_cn_f(self):
        """\
        Test formatting of names with cn_f().

        @see: L{wcodegen.BaseLanguageMixin.cn_f()}
        """
        details_wxStaticText = [
            ((2, 8), 'wxALL', 'wxALL'),
            ((2, 8), 'wxALL|wxALL', 'wxALL'),
            ((2, 8), '0', '0'),
            ((2, 8), 'wxALL|wxLEFT|wxRIGHT|wxTOP|wxBOTTOM', 'wxALL'),
            ((2, 8), 'wxALL|wxLEFT|wxTOP|wxBOTTOM', 'wxALL'),
            ((2, 8), 'wxLEFT|wxTOP|wxBOTTOM', 'wxBOTTOM|wxLEFT|wxTOP'),
            ((2, 8), 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM', 'wxALL'),
            ((2, 8), 'wxALL|wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL', 'wxALIGN_CENTER|wxALL|wxEXPAND'),
            ((2, 8), 'wxNonexistingFlag|wxALL', 'wxALL|wxNonexistingFlag'),
            ((2, 8), '', ''),
            ((2, 8), 'wxST_ELLIPSIZE_MIDDLE', ''),
            ((3, 0), 'wxALL', 'wxALL'),
            ((3, 0), 'wxALL|wxALL', 'wxALL'),
            ((3, 0), '0', '0'),
            ((3, 0), 'wxALL|wxLEFT|wxRIGHT|wxTOP|wxBOTTOM', 'wxALL'),
            ((3, 0), 'wxALL|wxLEFT|wxTOP|wxBOTTOM', 'wxALL'),
            ((3, 0), 'wxLEFT|wxTOP|wxBOTTOM', 'wxBOTTOM|wxLEFT|wxTOP'),
            ((3, 0), 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM', 'wxALL'),
            ((3, 0), 'wxALL|wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL', 'wxALIGN_CENTER|wxALL|wxEXPAND'),
            ((3, 0), 'wxNonexistingFlag|wxALL', 'wxALL|wxNonexistingFlag'),
            ((3, 0), '', ''),
            ((3, 0), 'wxALL|wxNO_3D', 'wxALL'),
            ((3, 0), 'wxST_ELLIPSIZE_MIDDLE', 'wxST_ELLIPSIZE_MIDDLE'),
            ]
        handler = common.code_writers['C++'].obj_builders['wxStaticText']
        for for_version, unformatted, formatted in details_wxStaticText:
            common.code_writers['C++'].for_version = for_version
            ret = handler.cn_f(unformatted)
            self.failUnlessEqual(
                formatted,
                ret,
                'Unexpected result for wx%s%s got: "%s" expect: "%s"' % (
                    for_version[0],
                    for_version[1],
                    ret,
                    formatted,
                )
            )

        details_wxCheckBox = [
            ('wxALL', 'wxALL'),
            ('wxCHK_ALLOW_3RD_STATE_FOR_USER', 'wxCHK_3STATE|wxCHK_ALLOW_3RD_STATE_FOR_USER'),
            ('wxALL|wxCHK_ALLOW_3RD_STATE_FOR_USER', 'wxALL|wxCHK_3STATE|wxCHK_ALLOW_3RD_STATE_FOR_USER'),
            ('wxALL|wxCHK_3STATE|wxCHK_ALLOW_3RD_STATE_FOR_USER|wxCHK_2STATE', 'wxALL|wxCHK_2STATE'),
            ('wxALL|wxCHK_2STATE|wxCHK_ALLOW_3RD_STATE_FOR_USER', 'wxALL|wxCHK_2STATE'),
            ('wxALL|wxCHK_3STATE|wxCHK_2STATE', 'wxALL|wxCHK_2STATE'),
            ]
        handler = common.code_writers['C++'].obj_builders['wxCheckBox']
        common.code_writers['C++'].for_version = (3, 0)
        for unformatted, formatted in details_wxCheckBox:
            ret = handler.cn_f(unformatted)
            self.failUnlessEqual(
                formatted,
                ret,
                'Unexpected result for wx30 got: "%s" expect: "%s"' % (
                    ret,
                    formatted,
                )
            )

    def test_unsupported_flags(self):
        """\
        Test code generation with unsupported flags

        @see: L{test_cn_f()}
        """
        self._test_all('no_supported_flags')

    def test_cn_class(self):
        """\
        Test formatting of names with cn_class().

        @see: L{wcodegen.BaseLanguageMixin.cn_class()}
        @see: L{wcodegen.CppMixin.cn_class()}
        @see: L{wcodegen.PythonMixin.cn_class()}
        """
        details = {}
        details['python'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wx.ALL',         'wx.ALL'),
            ('wx::UI::Report', 'wx_UI_Report'),
        ]
        details['perl'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wx::UI::Report', 'wx::UI::Report'),
        ]
        details['lisp'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wx::UI::Report', 'wx::UI::Report'),
        ]
        details['C++'] = [
            ('wxID_OK',        'wxID_OK'),
            ('wxALL',          'wxALL'),
            ('wx::UI::Report', 'wx_UI_Report'),
        ]

        for lang in ['python', 'perl', 'lisp', 'C++']:
            cn_class = common.code_writers[lang].cn_class

            for unformatted, formatted in details[lang]:
                ret = cn_class(unformatted)
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
                ret = cn_class(formatted)
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
        self._test_all('ComplexExample_30', ['lisp'])

        # Lisp code has to raise an exception
        source = self._load_file('ComplexExample_30.wxg')
        self.failUnlessRaises(
            errors.WxgLispWx3NotSupported,
            self._generate_code,
            'lisp',
            source,
            'ComplexExample_30.lisp',
            )

    def test_quote_str(self):
        """\
        Test string quotation for Perl and Python
        
        @see: L{codegen.perl_codegen.PerlCodeWriter.quote_str()}
        @see: L{codegen.py_codegen.PythonCodeWriter.quote_str()}
        """
        details = {}
        details[('UTF-8', 'python')] = [
            (None, '""'),
            ('', '""'),

            ('My 1st and simple ascii test!',  '_("My 1st and simple ascii test!")'),
            ('\xe2\x99\xa5 Love this song',    '_(u"\\u2665 Love this song")'),
            ('\xe2\x99\xa5 Love \xe2\x99\xa5', '_(u"\\u2665 Love \\u2665")'),
            ('\xe2\x99\xa5 Love \\n',          '_(u"\\u2665 Love \\n")'),
            ('Yes!\\nWe \xe2\x99\xa5 it!',     '_(u"Yes!\\nWe \\u2665 it!")'),

            (r'double quote: "',             '_("double quote: \\"")'),
            (r'newline \n',                  '_("newline \\n")'),
            (r'tab \t',                      '_("tab \\t")'),
            ( 'tailing single backslash \\', '_("tailing single backslash \\\\")'),

            (r'escaped newline \\n',      r'_("escaped newline \\n")'),
            (r'escaped tab \\t',          r'_("escaped tab \\t")'),
            (r'escaped double quote "',   r'_("escaped double quote \"")'),

            ('euro \xe2\x82\xac and newline \\n',          '_(u"euro \\u20ac and newline \\n")'),
            ('euro \xe2\x82\xac and tab \\t',              '_(u"euro \\u20ac and tab \\t")'),
            ('euro \xe2\x82\xac and quoted newline \\\\n', '_(u"euro \u20ac and quoted newline \\\\n")'),
            ('euro \xe2\x82\xac and quoted tab \\\\t',     '_(u"euro \u20ac and quoted tab \\\\t")'),

            ('rom\xc3\xa2n\xc4\x83', '_(u"rom\\u00e2n\\u0103")')
        ]
        details[('UTF-8', 'perl')] = [
            (None, '""'),
            ('', '""'),

            ('My 1st and simple ascii test!',   '_T("My 1st and simple ascii test!")'),
            ('\xe2\x99\xa5 Love this song',     '_T("\\N{U+2665} Love this song")'),
            ('\xe2\x99\xa5 Love \xe2\x99\xa5',  '_T("\\N{U+2665} Love \\N{U+2665}")'),
            ('\xe2\x99\xa5 Love \xe2\x99\xa5',  '_T("\\N{U+2665} Love \\N{U+2665}")'),
            ('Yes!\\nWe \xe2\x99\xa5 it!',      '_T("Yes!\\nWe \\N{U+2665} it!")'),

            (r'double quote: "',             '_T("double quote: \\"")'),
            (r'newline \n',                  '_T("newline \\n")'),
            (r'tab \t',                      '_T("tab \\t")'),
            ( 'tailing single backslash \\', '_T("tailing single backslash \\\\")'),

            (r'escaped newline \\n',      r'_T("escaped newline \\n")'),
            (r'escaped tab \\t',          r'_T("escaped tab \\t")'),
            (r'escaped double quote \"',  r'_T("escaped double quote \\\"")'),

            ('euro \xe2\x82\xac and newline \\n',          '_T("euro \N{U+20ac} and newline \\n")'),
            ('euro \xe2\x82\xac and tab \\t',              '_T("euro \N{U+20ac} and tab \\t")'),
            ('euro \xe2\x82\xac and quoted newline \\\\n', '_T("euro \N{U+20ac} and quoted newline \\\\n")'),
            ('euro \xe2\x82\xac and quoted tab \\\\t',     '_T("euro \N{U+20ac} and quoted tab \\\\t")'),
        ]

        # Lisp doesn't support unicode!
        details[('ascii', 'lisp')] = [
            (None, '""'),
            ('', '""'),

            (r'double quote: "',             '(_"double quote: \\"")'),
            (r'newline \n',                  '(_"newline \\n")'),
            (r'tab \t',                      '(_"tab \\t")'),
            ( 'tailing single backslash \\', '(_"tailing single backslash \\\\")'),

            (r'escaped newline \\n',     r'(_"escaped newline \\n")'),
            (r'escaped tab \\t',         r'(_"escaped tab \\t")'),
            (r'escaped double quote "',  r'(_"escaped double quote \"")'),
        ]
        details[('UTF-8', 'C++')] = [
            (None, 'wxEmptyString'),
            ('', 'wxEmptyString'),

            ('My 1st and simple ascii test!',  '_("My 1st and simple ascii test!")'),
            ('\xe2\x99\xa5 Love this song',    '_("\xe2\x99\xa5 Love this song")'),
            ('\xe2\x99\xa5 Love \xe2\x99\xa5', '_("\xe2\x99\xa5 Love \xe2\x99\xa5")'),
            ('\xe2\x99\xa5 Love \\n',          '_("\xe2\x99\xa5 Love \\n")'),
            ('Yes!\\nWe \xe2\x99\xa5 it!',     '_("Yes!\\nWe \xe2\x99\xa5 it!")'),

            (r'double quote: "',             '_("double quote: \\"")'),
            (r'newline \n',                  '_("newline \\n")'),
            (r'tab \t',                      '_("tab \\t")'),
            ( 'tailing single backslash \\', '_("tailing single backslash \\\\")'),

            (r'escaped newline \\n',      r'_("escaped newline \\n")'),
            (r'escaped tab \\t',          r'_("escaped tab \\t")'),
            (r'escaped double quote "',   r'_("escaped double quote \"")'),

            ('euro \xe2\x82\xac and newline \\n',          '_("euro \xe2\x82\xac and newline \\n")'),
            ('euro \xe2\x82\xac and tab \\t',              '_("euro \xe2\x82\xac and tab \\t")'),
            ('euro \xe2\x82\xac and quoted newline \\\\n', '_("euro \xe2\x82\xac and quoted newline \\\\n")'),
            ('euro \xe2\x82\xac and quoted tab \\\\t',     '_("euro \xe2\x82\xac and quoted tab \\\\t")'),
        ]
        details[('ISO-8859-1', 'python')] = [
            ('\xc4nderung', '_(u"\u00c4nderung")'),

        ]
        details[('ISO-8859-1', 'perl')] = [
            ('\xc4nderung', '_T("\N{U+00c4}nderung")'),

            ]
        for encoding, language in details:
            codegen = common.code_writers.get(language)
            codegen.app_encoding = encoding
            codegen._use_gettext = True
            for unformatted, formatted in details[(encoding, language)]:
                # XXX quote_str expects a string as from an XML file/string
                ret = codegen.quote_str(unformatted)
                self.failUnlessEqual( formatted, ret, 
                                      '%s: Unexpected result got: "%s" expect: "%s"' % (language, ret, formatted) )

                if language in ['python', 'perl']:
                    try:
                        formatted.encode('ascii')
                    except UnicodeDecodeError:
                        self.fail( '%s: ASCII string expected for "%s"' % (language, formatted) )

    def test_is_supported(self):
        """\
        Test check if the widget is supported with the requested version

        @see: L{wcodegen.BaseWidgetWriter.is_widget_supported()}
        @see: L{config.widget_config}
        """
        import wcodegen
        builder = wcodegen.PythonWidgetCodeWriter('test_nonexisting')
        builder.config = {}

        # no restrictions -> True
        self.assertTrue(builder.is_widget_supported(3, 0))

        builder.config['supported_by'] = ['wx31', 'wx2']

        # wx1 is not listed -> False
        self.assertFalse(builder.is_widget_supported(1))

        # wx2 is listed -> True
        self.assertTrue(builder.is_widget_supported(2))

        # wx3 is not listed -> False
        self.assertFalse(builder.is_widget_supported(3))

        # wx31 is listed -> True
        self.assertTrue(builder.is_widget_supported(3, 1))

        # wx32 is not listed -> False
        self.assertFalse(builder.is_widget_supported(3, 2))

        self.assertRaises(AssertionError, builder.is_widget_supported, (2, 6))

    def test_StylelessDialog(self):
        """\
        Test code generation for a style less dialog
        """
        self._test_all('styleless-dialog')

    def test_AllWidgets(self):
        """\
        Test code generation for all widgets
        """
        self._test_all('AllWidgets_28')
        self._test_all('AllWidgets_30', ['lisp'])

    def test_copy_py_codegen(self):
        """\
        Test creation of an independent instance of the Python code generator
        """
        py_codegen = common.code_writers['python']
        new_codegen = common.code_writers['python'].copy()

        # set new target version
        new_codegen.for_version = (3, 0)

        # check for different code generator objects
        self.failIfEqual(
            id(py_codegen),
            id(new_codegen),
            'Code generator objects are not different objects'
        )

        # for_version object and content should be different
        self.failIfEqual(
            id(py_codegen.for_version),
            id(new_codegen.for_version),
            'for_version are not different objects'
        )
        self.failIfEqual(
            py_codegen.for_version,
            new_codegen.for_version,
            'for_version content is not different'
        )

        # logging instance should be the same
        self.failUnlessEqual(
            id(py_codegen._logger),
            id(new_codegen._logger),
            'Logging instance _logger is not the same object'
        )

        # obj_builders should be the different
        self.failIfEqual(
            id(py_codegen.obj_builders),
            id(new_codegen.obj_builders),
            'obj_builders should be different'
        )
        self.failIfEqual(
            py_codegen.obj_builders,
            new_codegen.obj_builders,
            'obj_builders content is not different'
        )

        # widget generator objects should be different
        old_gen = py_codegen.obj_builders['wxDialog']
        new_gen = new_codegen.obj_builders['wxDialog']
        self.failIfEqual(
            id(old_gen),
            id(new_gen),
            'widget generator objects for wxDialog are not different'
        )

        # code generators used by widget generators should be different
        self.failIfEqual(
            id(old_gen.codegen),
            id(new_gen.codegen),
            'code generators used by widget generators are not different'
        )

        # for_version too ...
        self.failIfEqual(
            id(old_gen.codegen.for_version),
            id(new_gen.codegen.for_version),
            'for_version used by widget generators are not different'
        )

    def test_WxgLispWx3NotSupported(self):
        """\
        Test code generation for Lisp and wxWidgets 3.0 or newer.

        That's not supported anymore.

        @see: L{errors.WxgLispWx3NotSupported}
        """
        # load XML input file
        source = self._load_file('Lisp_Preferences.wxg')
        source = self._modify_attrs(source, for_version='3.0')

        # generate code and check for raising exception
        self.failUnlessRaises(
            errors.WxgLispWx3NotSupported,
            self._generate_code,
            'lisp',
            source,
            'Lisp_Preferences.lisp',
        )

    def test_OutputFileAndDirectory(self):
        """\
        Test check for output file and directory.

        @see: L{errors.WxgOutputPathIsDirectory}
        @see: L{errors.WxgOutputDirectoryNotExist}
        """
        # load XML input file
        source = self._load_file('Python_Preferences.wxg')

        # Single output file out_path shouldn't be a directory
        #=====================================================

        # generate code and check for raising exception
        self.failUnlessRaises(
            errors.WxgOutputPathIsDirectory,
            self._generate_code,
            'python',
            source,
            os.path.normpath('/tmp'),
            )
        self.failUnlessRaises(
            errors.WxgOutputDirectoryNotExist,
            self._generate_code,
            'python',
            source,
            os.path.normpath('/non-existing/result.py'),
            )
        self.failUnlessRaises(
            errors.WxgOutputDirectoryNotWritable,
            self._generate_code,
            'python',
            source,
            os.path.normpath('/non-writable/result.py'),
            )

        # Multiple output file out_path should be a writable directory
        #=============================================================
        source = self._modify_attrs(source, option=1)
        self.failUnlessRaises(
            errors.WxgOutputDirectoryNotExist,
            self._generate_code,
            'python',
            source,
            os.path.normpath('/non-existing'),
            )
        self.failUnlessRaises(
            errors.WxgOutputDirectoryNotWritable,
            self._generate_code,
            'python',
            source,
            os.path.normpath('/non-writable'),
            )

    def test_WxgXRCMultipleFilesNotSupported(self):
        """\
        Test for multi file XRC projects.

        @see: L{errors.WxgXRCMultipleFilesNotSupported}
        """
        # load XML input file
        source = self._load_file('Python_Preferences.wxg')

        # check to multiple files
        source = self._modify_attrs(source, option=1)

        # generate code and check for raising exception
        self.failUnlessRaises(
            errors.WxgXRCMultipleFilesNotSupported,
            self._generate_code,
            'XRC',
            source,
            'Preferences.xrc',
            )

    def test_WxgTemplateCodegenNotPossible(self):
        """\
        Test for code generation from a template

        @see: L{errors.WxgTemplateCodegenNotPossible}
        """
        # load XML input file
        source = self._load_file('Python_Preferences.wxg')

        # check to multiple files
        source = self._modify_attrs(source, is_template=1)

        # generate code and check for raising exception
        self.failUnlessRaises(
            errors.WxgTemplateCodegenNotPossible,
            self._generate_code,
            'python',
            source,
            'Python_Preferences.py',
            )

    def test_xrc2wxg(self):
        "Test converting XRC files into WXG files"
        fullpath = os.path.join(self.caseDirectory, 'import_test.xrc')
        obuffer = compat.StringIO()

        xrc2wxg.convert(fullpath, obuffer)

        generated = obuffer.getvalue().decode('UTF-8')
        expected = self._load_file('import_test.wxg')

        self._compare(expected, generated, "wxg")

    def test_Statusbar_wo_labels(self):
        "Test code generation for a wxStatusBar with fields but w/o labels"
        self._test_all('Statusbar_wo_labels')

    def test_Format_flags(self):
        "Test code cn_f() for XRC code generator"
        self._generate_and_compare(
            'XRC',
            'Format_flags.wxg',
            'Format_flags.xrc'
        )

    def test_get_bitmap_code(self):
        "Test bitmap code generation"
        details = {}
        details['C++'] = [
            (None, 'wxNullBitmap'),
            ('icon.xpm', 'wxBitmap(wxT("icon.xpm"), wxBITMAP_TYPE_ANY)'),
            ('code:SenselessStatement', 'SenselessStatement'),
            ('empty:10,10', 'wxBitmap(10, 10)'),
            ('empty: 10, 10', 'wxBitmap(10, 10)'),
            ('empty:FooBar', 'wxBitmap(16, 16)'),
            ('empty:10', 'wxBitmap(16, 16)'),
            ('empty:A, B', 'wxBitmap(16, 16)'),
            ('var:AlreadyDefinedVariable', 'wxBitmap(AlreadyDefinedVariable, wxBITMAP_TYPE_ANY)'),
            ('art:wxART_PRINT,wxART_OTHER', 'wxArtProvider::GetBitmap(wxART_PRINT, wxART_OTHER, wxDefaultSize)'),
            ('art:wxART_PRINT,wxART_OTHER,16,16', 'wxArtProvider::GetBitmap(wxART_PRINT, wxART_OTHER, wxSize(16, 16))'),
            ('art:ART_PRINT,ART_OTHER', 'wxArtProvider::GetBitmap(ART_PRINT, ART_OTHER, wxDefaultSize)'),
            ('art:"gtk-cdrom",wxART_MENU', 'wxArtProvider::GetBitmap("gtk-cdrom", wxART_MENU, wxDefaultSize)'),
            ]
        details['lisp'] = [
            (None, 'wxNullBitmap'),
            ('icon.xpm', '(wxBitmap_CreateLoad "icon.xpm" wxBITMAP_TYPE_ANY)'),
            ('code:SenselessStatement', 'SenselessStatement'),
            ('empty:10,10', 'wxBitmap_Create(10 10)'),
            ('empty: 10, 10', 'wxBitmap_Create(10 10)'),
            ('empty:FooBar', 'wxBitmap_Create(16 16)'),
            ('empty:10', 'wxBitmap_Create(16 16)'),
            ('empty:A, B', 'wxBitmap_Create(16 16)'),
            ('var:AlreadyDefinedVariable', '(wxBitmap_CreateLoad AlreadyDefinedVariable wxBITMAP_TYPE_ANY)'),
            ('art:wxART_PRINT,wxART_OTHER', 'wxArtProvider_GetBitmap(wxART_PRINT wxART_OTHER wxDefaultSize)'),
            ('art:wxART_PRINT,wxART_OTHER,16,16', 'wxArtProvider_GetBitmap(wxART_PRINT wxART_OTHER wxSize_Create(16 16))'),
            ('art:ART_PRINT,ART_OTHER', 'wxArtProvider_GetBitmap(ART_PRINT ART_OTHER wxDefaultSize)'),
            ('art:"gtk-cdrom",wxART_MENU', 'wxArtProvider_GetBitmap("gtk-cdrom" wxART_MENU wxDefaultSize)'),
            ]
        details['perl'] = [
            (None, 'wxNullBitmap'),
            ('icon.xpm', 'Wx::Bitmap->new("icon.xpm", wxBITMAP_TYPE_ANY)'),
            ('code:SenselessStatement', 'SenselessStatement'),
            ('empty:10,10', 'Wx::Bitmap->new(10, 10)'),
            ('empty: 10, 10', 'Wx::Bitmap->new(10, 10)'),
            ('empty:FooBar', 'Wx::Bitmap->new(16, 16)'),
            ('empty:10', 'Wx::Bitmap->new(16, 16)'),
            ('empty:A, B', 'Wx::Bitmap->new(16, 16)'),
            ('var:AlreadyDefinedVariable', 'Wx::Bitmap->new(AlreadyDefinedVariable, wxBITMAP_TYPE_ANY)'),
            ('art:wxART_PRINT,wxART_OTHER', 'Wx::ArtProvider::GetBitmap(wxART_PRINT, wxART_OTHER, wxDefaultSize)'),
            ('art:wxART_PRINT,wxART_OTHER,16,16', 'Wx::ArtProvider::GetBitmap(wxART_PRINT, wxART_OTHER, Wx::Size->new(16, 16))'),
            ('art:ART_PRINT,ART_OTHER', 'Wx::ArtProvider::GetBitmap(ART_PRINT, ART_OTHER, wxDefaultSize)'),
            ('art:"gtk-cdrom",wxART_MENU', 'Wx::ArtProvider::GetBitmap("gtk-cdrom", wxART_MENU, wxDefaultSize)'),
            ]
        details['python'] = [
            (None, 'wx.NullBitmap'),
            ('icon.xpm', 'wx.Bitmap("icon.xpm", wx.BITMAP_TYPE_ANY)'),
            ('code:SenselessStatement', 'SenselessStatement'),
            ('empty:10,10', 'wx.EmptyBitmap(10, 10)'),
            ('empty: 10, 10', 'wx.EmptyBitmap(10, 10)'),
            ('empty:FooBar', 'wx.EmptyBitmap(16, 16)'),
            ('empty:10', 'wx.EmptyBitmap(16, 16)'),
            ('empty:A, B', 'wx.EmptyBitmap(16, 16)'),
            ('var:AlreadyDefinedVariable', 'wx.Bitmap(AlreadyDefinedVariable, wx.BITMAP_TYPE_ANY)'),
            ('art:wxART_PRINT,wxART_OTHER', 'wx.ArtProvider.GetBitmap(wx.ART_PRINT, wx.ART_OTHER, wx.DefaultSize)'),
            ('art:wxART_PRINT,wxART_OTHER,16,16', 'wx.ArtProvider.GetBitmap(wx.ART_PRINT, wx.ART_OTHER, (16, 16))'),
            ('art:ART_PRINT,ART_OTHER', 'wx.ArtProvider.GetBitmap(ART_PRINT, ART_OTHER, wx.DefaultSize)'),
            ('art:"gtk-cdrom",wxART_MENU', 'wx.ArtProvider.GetBitmap("gtk-cdrom", wx.ART_MENU, wx.DefaultSize)'),
            ]

        for lang in ['C++', 'lisp', 'perl', 'python']:
            codegen = common.code_writers.get(lang)
            get_bitmap_code = codegen.obj_builders['wxBitmapButton'].generate_code_bitmap
            for bitmap_file, expected in details[lang]:
                stmt = get_bitmap_code(bitmap_file)
                self.failUnlessEqual(
                    expected,
                    stmt,
                    '%s: Unexpected result got: "%s" expect: "%s"' % (
                        misc.capitalize(lang),
                        stmt,
                        expected,
                    )
                )

    def test_CustomWidget(self):
        "Test code generation for CustomWidget"
        self._test_all('CustomWidget')

    def test_missing_application_attributes(self):
        "Test code generation w/ missing <application> attributes"
        self._test_all('app_wo_attrs')

    def test_get_scope(self):
        "Test splitting scope (get_class() and get_scope())"
        details = {}
        details['C++'] = [
            ('wxALL',           '',         'wxALL'),
            ('wx::ALL',         'wx',       'ALL'),
            ('ui::dlgs::About', 'ui::dlgs', 'About'),
        ]
        details['lisp'] = [
            ('wxALL',         '', 'wxALL'),
            ('wx.ALL',        '', 'wx.ALL'),
            ('ui.dlgs.About', '', 'ui.dlgs.About'),
        ]
        details['python'] = [
            ('wxALL',         '',        'wxALL'),
            ('wx.ALL',        'wx',      'ALL'),
            ('ui.dlgs.About', 'ui.dlgs', 'About'),
        ]
        details['perl'] = [
            ('wxALL',           '',         'wxALL'),
            ('wx::ALL',         'wx',       'ALL'),
            ('ui::dlgs::About', 'ui::dlgs', 'About'),
        ]
        for lang in ['C++', 'lisp', 'perl', 'python']:
            codegen = common.code_writers.get(lang)
            for full_name, scope, klass in details[lang]:
                expected_scope = codegen.get_scope(full_name)
                expected_class = codegen.get_class(full_name)
                self.failUnlessEqual(
                    expected_scope, scope,
                    '%s: Unexpected scope got: "%s" expect: "%s"' % (
                        lang, expected_scope, scope,)
                )
                self.failUnlessEqual(
                    expected_class, klass,
                    '%s: Unexpected class got: "%s" expect: "%s"' % (
                        lang, expected_class, klass,)
                )
