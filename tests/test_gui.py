"""
@copyright: 2012 Carsten Grohmann

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


class TestGui(WXGladeBaseTest):
    """\
    Test GUI functionality
    
    Since Python created an own instance for each test, we use class varaibles
    for persistent storing L{app} and L{frame}.
    
    @cvar app: Reference to a wx.App object. he object is persistent after the creation in L{setUp()}.
    @cvar frame: Reference to L{main.wxGladeFrame}. The object is persistent after the creation in L{setUp()}.
    @ivar orig_stdout: Original fd for stdout.
    """

    init_stage1 = True
    init_use_gui = True

    app = None
    frame = None
    orig_stdout = None

    def mockMessageBox(self, message, caption, *args, **kwargs):
        """\
        Mock object for wx.MessageBox
        """
        self._messageBox = [message, caption]

    def setUp(self):
        # redirect stdout
        self.orig_stdout = sys.stdout
        sys.stdout = cStringIO.StringIO()

        # initialse base class
        WXGladeBaseTest.setUp(self)

        import main
        import wx

        # inject mock object for wxMessageBox
        self._messageBox = []
        wx.MessageBox = self.mockMessageBox

        # create an simply application
        if not TestGui.app:
            TestGui.app = wx.PySimpleApp()
            wx.InitAllImageHandlers()
            wx.ArtProvider.PushProvider(main.wxGladeArtProvider())

        if not TestGui.frame:
            self.frame = main.wxGladeFrame()

            # hide all windows
            self.frame.Hide()
            self.frame.hide_all()
            TestGui.frame = self.frame

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
        radiobox= common.app_tree.app.codewriters_prop.options
        radiobox.SetStringSelection(lang)
        event = wx.CommandEvent(
            wx.wxEVT_COMMAND_RADIOBOX_SELECTED,
            radiobox.GetId()
            )
        radiobox.GetEventHandler().ProcessEvent(event)

    def testNotebookWithoutTabs(self):
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
                  '_((line: 18, column:  20))'
        err_caption = u'Error'
        self.failUnless(
            [err_msg, err_caption] == self._messageBox,
            '''Expected wxMessageBox(message=%s, caption=%s)''' % (
                err_msg,
                err_caption
                )
            )

    def testNotebookWithTabs(self):
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
            'Loading test wxg file caused an error message: %s' % \
                self._messageBox
            )

    def testCodeGeneration(self):
        """\
        Test GUI code generation
        """
        source = self._load_file('FontColour.wxg')
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
        #print self._messageBox
        err_msg = u'You must specify an output file\n' \
                   'before generating any code'
        err_caption = u'Error'
        self.failUnless(
            [err_msg, err_caption] == self._messageBox,
            '''Expected wxMessageBox(message=%s, caption=%s)''' % (
                err_msg,
                err_caption
                )
            )
        self._messageBox = None

        # now test full code generation
        for filename, language in [
            ['FontColour.lisp', 'lisp'],
            ['FontColour.pl',   'perl'],
            ['FontColour.py',   'python'],
            ['FontColour.xrc',  'XRC'],
            ['FontColour',      'C++'],
            ]:

            # check for langage first
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
                '''Expected wxMessageBox(message=%s, caption=%s)''' % (
                    success_msg,
                    success_caption
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

            # check for langage first
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
                '''Expected wxMessageBox(message=%s, caption=%s)''' % (
                    success_msg,
                    success_caption
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
