"""
@copyright: 2012-2014 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import general python modules
import StringIO
import sys
import wx

# import project modules
import config
import common
import main


class TestGui(WXGladeBaseTest):
    """\
    Test GUI functionality
    
    Since Python created an own instance for each test, we use class variables
    for persistent storing L{app} and L{frame}.
    
    @cvar app: Reference to a wx.App object. he object is persistent after
               the creation in L{setUp()}.
    @cvar frame: Reference to L{main.wxGladeFrame}. The object is persistent
                 after the creation in L{setUp()}.
    @ivar orig_stdout: Original fd for stdout.
    """

    app = None
    frame = None
    orig_stdout = None

    def mockMessageBox(self, message, caption, *args, **kwargs):
        """\
        Mock object for wx.MessageBox
        """
        self._messageBox = [message, caption]

    @classmethod
    def setUpClass(cls):
        WXGladeBaseTest.setUpClass()

        # create an simply application
        cls.app = wx.PySimpleApp()
        wx.InitAllImageHandlers()
        wx.ArtProvider.PushProvider(main.wxGladeArtProvider())
        cls.frame = main.wxGladeFrame()

        # hide all windows
        cls.frame.Hide()
        cls.frame.hide_all()

    def setUp(self):
        # redirect stdout
        self.orig_stdout = sys.stdout
        sys.stdout = StringIO.StringIO()

        # initialise base class
        WXGladeBaseTest.setUp(self)

        # inject mock object for wxMessageBox
        wx.MessageBox = self.mockMessageBox
        self._messageBox = []

        # show dialog "Code generation completed successfully"
        config.preferences.show_completion = True

    def tearDown(self):
        # restore original stdout
        if self.orig_stdout:
            sys.stdout = self.orig_stdout

        # initialise base class
        WXGladeBaseTest.tearDown(self)

    def _FindWindowByName(self, name):
        """\
        Search and return a widget with the given name in the top window
        widget tree.
        """
        app = wx.GetApp()
        top = app.GetTopWindow()
        return top.FindWindowByName(name)

    def _generate_code(self):
        """\
        Search button with label "Generate code" and press it
        """
        # search wx.Button "Generate code" 
        btn_codegen = self._FindWindowByName("BtnGenerateCode")
        self.failUnless(
            btn_codegen,
            'Button with label "Generate code" not found'
            )
        # press button to generate code
        self._press_button(btn_codegen)

    def _press_button(self, button):
        """\
        Simulate pressing the button by sending a button clicked event
        """
        event = wx.CommandEvent(
            wx.wxEVT_COMMAND_BUTTON_CLICKED,
            button.GetId()
            )
        button.GetEventHandler().ProcessEvent(event)

    def _set_lang(self, lang):
        """\
        Set "Language" and simulate clicking radio button
        
        @param lang: Language to set
        @type lang:  StringIO
        """
        radiobox = common.app_tree.app.codewriters_prop.options
        radiobox.SetStringSelection(lang)
        event = wx.CommandEvent(
            wx.wxEVT_COMMAND_RADIOBOX_SELECTED,
            radiobox.GetId()
            )
        radiobox.GetEventHandler().ProcessEvent(event)

    def test_NotebookWithoutTabs(self):
        """\
        Test loading Notebook without tabs
        """
        self._messageBox = None
        infile = StringIO.StringIO(
            self._load_file('Notebook_wo_tabs.wxg')
            )
        self.frame._open_app(
            infilename=infile,
            use_progress_dialog=False,
            is_filelike=True,
            add_to_history=False,
            )
        err_msg = u'Error loading file None: Notebook widget' \
                  ' "notebook_1" does not have any tabs! ' \
                  '_((line: 17, column:  20))'
        err_caption = u'Error'
        self.failUnless(
            [err_msg, err_caption] == self._messageBox,
            'Expected wxMessageBox(message="%s", caption="%s") '
            'got wxMessageBox(message="%s", caption="%s")' % (
                err_msg,
                err_caption,
                self._messageBox[0],
                self._messageBox[1],
                )
            )

    def test_NotebookWithTabs(self):
        """\
        Test loading Notebook with tabs
        """
        self._messageBox = None
        infile = StringIO.StringIO(
            self._load_file('Notebook_w_tabs.wxg')
            )
        self.frame._open_app(
            infilename=infile,
            use_progress_dialog=False,
            is_filelike=True,
            add_to_history=False,
            )
        self.failIf(
            self._messageBox,
            'Loading test wxg file caused an error message: %s' %
            self._messageBox
            )

    def load_and_generate(self, basename):
        """\
        Load a wxGlade document and generate code for all languages.

        @param basename: Base name of the wxg file
        @type basename:  String
        """
        source = self._load_file('%s.wxg' % basename)
        source = self._modify_attrs(
            source,
            path='',
        )
        infile = StringIO.StringIO(source)
        self.frame._open_app(
            infilename=infile,
            use_progress_dialog=False,
            is_filelike=True,
            add_to_history=False,
        )
        # generate code
        self._generate_code()
        # first test should fail because no output file is given
        err_msg = u'You must specify an output file\n' \
                  'before generating any code'
        err_caption = u'Error'
        self.failUnless(
            [err_msg, err_caption] == self._messageBox,
            'Expected wxMessageBox(message="%s", caption="%s") '
            'got wxMessageBox(message="%s", caption="%s")' % (
                err_msg,
                err_caption,
                self._messageBox[0],
                self._messageBox[1],
            )
        )
        self._messageBox = None
        # now test full code generation
        for ext, language in [
            ['.lisp', 'lisp'],
            ['.pl', 'perl'],
            ['.py', 'python'],
            ['.xrc', 'XRC'],
            ['', 'C++'],
        ]:
            filename = '%s%s' % (basename, ext)

            # check for language first
            self.failUnless(
                language in common.code_writers,
                "No codewriter loaded for %s" % language
            )

            # prepare and open wxg
            source = self._prepare_wxg(language, source)
            infile = StringIO.StringIO(source)
            self.frame._open_app(
                infilename=infile,
                use_progress_dialog=False,
                is_filelike=True,
                add_to_history=False,
            )

            # set "Output path", language and generate code
            common.app_tree.app.output_path = filename
            self._set_lang(language)
            self._generate_code()

            success_msg = u'Code generation completed successfully'
            success_caption = u'Information'
            self.failUnless(
                [success_msg, success_caption] == self._messageBox,
                'Expected wxMessageBox(message="%s", caption="%s") '
                'got wxMessageBox(message="%s", caption="%s")' % (
                    success_msg,
                    success_caption,
                    self._messageBox[0],
                    self._messageBox[1],
                )
            )
            self._messageBox = None

            if language == 'C++':
                name_h = '%s.h' % filename
                name_cpp = '%s.cpp' % filename
                result_cpp = self._load_file(name_cpp)
                result_h = self._load_file(name_h)
                generated_cpp = self.vFiles[name_cpp].getvalue()
                generated_h = self.vFiles[name_h].getvalue()
                self._compare(result_cpp, generated_cpp, 'C++ source')
                self._compare(result_h, generated_h, 'C++ header')
            else:
                expected = self._load_file(filename)
                generated = self.vFiles[filename].getvalue()
                self._compare(expected, generated)

    def test_CodeGeneration_FontColour(self):
        """\
        Test GUI code generation using "FontColour.wxg"
        """
        self.load_and_generate('FontColour')

    def test_StylelessDialog(self):
        """\
        Test code generation for a style less dialog
        """
        source = self._load_file('styleless-dialog.wxg')

        # now test full code generation
        for filename, language in [
            ['styleless-dialog.lisp', 'lisp'],
            ['styleless-dialog.pl',   'perl'],
            ['styleless-dialog.py',   'python'],
            ['styleless-dialog.xrc',  'XRC'],
            ['styleless-dialog',      'C++'],
            ]:

            # check for language first
            self.failUnless(
                language in common.code_writers,
                "No codewriter loaded for %s" % language
                )

            # prepare and open wxg
            source = self._prepare_wxg(language, source)
            infile = StringIO.StringIO(source)
            self.frame._open_app(
                infilename=infile,
                use_progress_dialog=False,
                is_filelike=True,
                add_to_history=False,
                )

            # set "Output path", language and generate code
            common.app_tree.app.output_path = filename
            self._set_lang(language)
            self._generate_code()

            success_msg = u'Code generation completed successfully'
            success_caption = u'Information'
            self.failUnless(
                [success_msg, success_caption] == self._messageBox,
                'Expected wxMessageBox(message="%s", caption="%s") '
                'got wxMessageBox(message="%s", caption="%s")' % (
                    success_msg,
                    success_caption,
                    self._messageBox[0],
                    self._messageBox[1],
                    )
                )
            self._messageBox = None

            if language == 'C++':
                name_h = '%s.h' % filename
                name_cpp = '%s.cpp' % filename
                result_cpp = self._load_file(name_cpp)
                result_h = self._load_file(name_h)
                generated_cpp = self.vFiles[name_cpp].getvalue()
                generated_h = self.vFiles[name_h].getvalue()
                self._compare(result_cpp, generated_cpp, 'C++ source')
                self._compare(result_h, generated_h, 'C++ header')
            else:
                expected = self._load_file(filename)
                generated = self.vFiles[filename].getvalue()
                self._compare(expected, generated)

    def test_CodeGeneration_AllWidgets_28(self):
        """\
        Test GUI code generation using "AllWidgets_28.wxg"
        """
        self.load_and_generate('AllWidgets_28')

    def test_CodeGeneration_ComplexExample(self):
        """\
        Test GUI code generation using "ComplexExample.wxg"
        """
        self.load_and_generate('ComplexExample')

    def test_StylesMixin(self):
        """\
        StyleMixin: Test converting of styles
        """
        import edit_windows
        sm = edit_windows.StylesMixin('wxHyperlinkCtrl')

        # test converting of a combined attribute
        style_names = ['wxHL_DEFAULT_STYLE', 'wxHL_ALIGN_LEFT',
                       'wxHL_ALIGN_RIGHT', 'wxHL_ALIGN_CENTRE',
                       'wxHL_CONTEXTMENU']
        ret = sm.style_names[:]
        ret.sort()
        expected = style_names
        expected.sort()
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.__init__() failed: got "%s" expect: "%s"' % (
                ret, expected)
        )

        # test setting new styles
        sm.set_style('|'.join(style_names))
        ret = sm.style_set
        expected = set(['wxHL_DEFAULT_STYLE', 'wxHL_ALIGN_LEFT',
                        'wxHL_ALIGN_RIGHT'])
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.set_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )
        sm.set_style([True, False, True, False, False])
        ret = sm.style_set
        expected = set(['wxHL_ALIGN_LEFT', 'wxHL_ALIGN_CENTRE'])
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.set_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )
        sm.set_style('wxHL_DEFAULT_STYLE|wxHL_CONTEXTMENU')
        ret = sm.style_set
        expected = set(['wxHL_DEFAULT_STYLE',])
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected)
        )

        # test generating a flag list
        ret = sm.get_style()
        expected = [False, False, False, False, True]
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.get_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )

        # returning styles as a string concatenated with '|'
        ret = sm.get_string_style()
        expected = 'wxHL_DEFAULT_STYLE'
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.get_style_string(): got "%s" expect: "%s"' %
            (ret, expected)
        )

        # test setting styles via style dictionary
        from ordereddict import OrderedDict
        styles = OrderedDict()
        styles[_('Border')] = ['wxALL', 'wxLEFT']
        styles[_('Alignment')] = ['wxEXPAND', 'wxALIGN_RIGHT']

        sm = edit_windows.StylesMixin('wxHyperlinkCtrl', styles)
        ret = sm.style_names[:]
        ret.sort()
        expected = ['wxALL', 'wxLEFT', 'wxEXPAND', 'wxALIGN_RIGHT']
        expected.sort()
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.__init__() failed: got "%s" expect: "%s"' % (
                ret, expected)
        )

        # check handling of empty ('', None) styles
        for value, desc in (('', "''"), (None, 'None')):
            sm.set_style(value)
            ret = sm.style_set
            expected = set()
            self.failUnlessEqual(
                ret,
                expected,
                'StylesMixin.set_style(%s) failed: got "%s" expect: "%s"' % (
                    desc, ret, expected)
            )
            ret = sm.get_style()
            expected = [False, False, False, False]
            self.failUnlessEqual(
                ret,
                expected,
                'StylesMixin.set_style(%s) failed: got "%s" expect: "%s"' % (
                    desc, ret, expected)
            )
            ret = sm.get_int_style()
            expected = 0
            self.failUnlessEqual(
                ret,
                expected,
                'StylesMixin.set_style(%s) failed: got "%s" expect: "%s"' % (
                    desc, ret, expected)
            )