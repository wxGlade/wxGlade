"""
@copyright: 2012-2014 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import general python modules
import cStringIO
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
        """\
        XXX
        """
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
        #sys.stdout = cStringIO.StringIO()

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

        # initialse base class
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
        infile = cStringIO.StringIO(
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
        infile = cStringIO.StringIO(
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
        infile = cStringIO.StringIO(source)
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
            infile = cStringIO.StringIO(source)
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
            infile = cStringIO.StringIO(source)
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

    def test_StylesMixin(self):
        """\
        StyleMixin: Test converting of styles
        """
        import edit_windows
        sm = edit_windows.StylesMixin()

        # test converting attribute names to values
        style_labels = ('wxMINIMIZE_BOX', 'wxMAXIMIZE_BOX',
                        'wxRESIZE_BORDER')
        sm.gen_style_pos(style_labels)
        style_values = [1024, 512, 64]
        self.failUnlessEqual(
            sm._style_values,
            style_values,
            'StylesMixin.gen_style_pos() failed: got "%s" expect: "%s"' % (
                sm._style_values,
                style_values
            )
        )

        # test converting a single style
        style_labels = (
            'wxDEFAULT_FRAME_STYLE',
            'wxMINIMIZE_BOX', 'wxMAXIMIZE_BOX', 'wxRESIZE_BORDER',
            'wxSYSTEM_MENU', 'wxCAPTION', 'wxCLOSE_BOX', 'wxCLIP_CHILDREN',
        )
        style_values = [541072960, 1024, 512, 64, 2048, 536870912,
                        4096, 4194304,
        ]
        sm.gen_style_pos(style_labels)
        self.failUnlessEqual(
            sm._style_values,
            style_values,
            'StylesMixin.gen_style_pos() failed: got "%s" expect: "%s"' % (
                sm._style_values,
                style_values
            )
        )
        sm.style = 2048
        ret = sm.get_style()
        expected = [0, 0, 0, 0, 1, 0, 0, 0]
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.StylesMixin.get_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )

        # test converting of a combined attribute
        style_labels = ('#section#Border', 'wxALL',
                        'wxLEFT', 'wxRIGHT', 'wxTOP', 'wxBOTTOM')
        style_values = [240, 16, 32, 64, 128]
        sm.gen_style_pos(style_labels)
        sm.combined_attr = 'wxALL'
        self.failUnlessEqual(
            sm._style_values,
            style_values,
            'StylesMixin.gen_style_pos() failed: got "%s" expect: "%s"' % (
                sm._style_values,
                style_values
            )
        )
        sm.style = 240
        ret = sm.get_style()
        expected = [1, 0, 0, 0, 0]
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected)
        )

        # test attributes with 0 values like wxALIGN_LEFT or compatibility
        # reasons
        style_labels = ('wxALL', 'wxLEFT', 'wxNO_FULL_REPAINT_ON_RESIZE')
        style_values = [240, 16, 0]
        sm.gen_style_pos(style_labels)
        ret = sm.get_style()
        self.failUnlessEqual(
            sm._style_values,
            style_values,
            'StylesMixin.gen_style_pos() failed: got "%s" expect: "%s"' % (
                sm._style_values,
                style_values
            )
        )
        sm.style = 240
        ret = sm.get_style()
        expected = [1, 0, 0]
        self.failUnlessEqual(
            ret,
            expected,
            'StylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected)
        )
        sm.style = None
        sm.set_style('wxLEFT|wxNO_FULL_REPAINT_ON_RESIZE')
        expected = 16
        self.failUnlessEqual(
            sm.style,
            expected,
            'StylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected)
        )
