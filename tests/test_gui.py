"""\
Graphical tests

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import general python modules
import glob
import os.path
import sys
import wx
import wx.xrc

# import project modules
import config
import common
import compat
import main
import xrc2wxg


class TestGui(WXGladeBaseTest):
    """\
    Test GUI functionality
    
    Since Python created an own instance for each test, we use class variables
    for persistent storing L{app} and L{frame}.
    
    @cvar app: Reference to a wx.App object. he object is persistent after
               the creation in L{setUp()}.
    @cvar frame: Reference to L{main.wxGladeFrame}. The object is persistent
                 after the creation in L{setUp()}.
    @cvar nolog: wxWidgets Null logger to suppress error messages
    @ivar orig_stdout: Original fd for stdout.
    """

    app = None
    frame = None
    nolog = None
    orig_stdout = None

    def mockMessageBox(self, message, caption, *args, **kwargs):
        """\
        Mock object for wx.MessageBox
        """
        self._messageBox = [message, caption]

    @classmethod
    def setUpClass(cls):
        WXGladeBaseTest.setUpClass()
        xrc2wxg._write_timestamp = False

        # create an simply application
        cls.app = wx.PySimpleApp()
        compat.wx_ArtProviderPush(main.wxGladeArtProvider())
        cls.frame = main.wxGladeFrame()

        # suppress wx error messages
        cls.nolog = wx.LogNull()

        # hide all windows
        #cls.frame.Hide()
        #cls.frame.hide_all()

    @classmethod
    def tearDownClass(cls):
        cls.nolog = None

    def setUp(self):
        # redirect stdout
        self.orig_stdout = sys.stdout
        sys.stdout = compat.StringIO()

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

    def _generate_code(self):
        """\
        Search button with label "Generate code" and press it
        """
        # search wx.Button "Generate code"
        btn_codegen = wx.FindWindowByName('BtnGenerateCode')
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
        @type lang:  str
        """
        radiobox = common.app_tree.app.codewriters_prop.options
        radiobox.SetStringSelection(lang)
        event = wx.CommandEvent(
            wx.wxEVT_COMMAND_RADIOBOX_SELECTED,
            radiobox.GetId()
        )
        radiobox.GetEventHandler().ProcessEvent(event)

    def _set_output_path(self, path):
        """\
        Set the output path

        @type path: str
        """
        common.app_tree.app.outpath_prop.set_value(path)
        common.app_tree.app.set_output_path(path)
        self._process_wx_events()

    def _open_wxg_file(self, content=None, filename=None):
        """\
        Open a wxGlade project

        @param content: Content
        @type content:  Unicode | StringIO | None

        @param filename: File name
        @type filename:  str
        """
        self.failUnless(content or filename)
        self.failUnless(isinstance(content, (compat.unicode, compat.StringIO)) or content is None)
        if isinstance(content, compat.StringIO):
            self.failUnless( isinstance(content.getvalue(), compat.unicode) )

        if filename:
            content = compat.StringIO(self._load_file(filename))
        elif isinstance(content, compat.StringIO):
            self.failUnless( isinstance(content.getvalue(), compat.unicode) )
        else:
            content = compat.StringIO(content)

        self.frame._open_app(filename_or_filelike=content, use_progress_dialog=False, add_to_history=False)
        tree = common.app_tree
        root = tree.GetRootItem()
        first, cookie = tree.GetFirstChild(root)
        if first.IsOk():
            tree.expand()
            self._process_wx_events()
            tree.SelectItem(first)
            self._process_wx_events()
            node = tree.GetPyData(first)
            tree.show_toplevel(node)
        self._process_wx_events()

    def _process_wx_events(self):
        "Process wx events, because we don't start the main loop"
        for i in range(3):
            wx.SafeYield()
            self.app.ProcessPendingEvents()

    def test_NotebookWithoutTabs(self):
        "Test loading Notebook without tabs"
        self._messageBox = None
        self._open_wxg_file(filename='Notebook_wo_tabs.wxg')
        err_msg = u'Error loading from a file-like object: Notebook ' \
                  u'widget "notebook_1" does not have any tabs! ' \
                  u'(line: 17, column: 20)'
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
        "Test loading Notebook with tabs"
        self._messageBox = None
        self._open_wxg_file(filename='Notebook_w_tabs.wxg')
        self.failIf( self._messageBox, 'Loading test wxg file caused an error message: %s' % self._messageBox )

    def load_and_generate(self, basename, excluded=None):
        """\
        Load a wxGlade document and generate code for all languages.

        @param basename: Base name of the wxg file
        @type basename:  str
        @param excluded: Languages to exclude from test
        @type excluded:  list[str]
        """
        source = self._load_file('%s.wxg' % basename)
        source = self._modify_attrs(source, path='')
        self._open_wxg_file(source)

        # generate code
        self._generate_code()

        # first test should fail because no output file is given
        err_msg = u'You must specify an output file\nbefore generating any code'
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

        # now test full code generation
        self._messageBox = None
        for language, dummy, ext in self.language_constants:
            if excluded and language in excluded:
                continue

            filename = '%s%s' % (basename, ext)

            # check for language first
            self.failUnless( language in common.code_writers, "No codewriter loaded for %s" % language )

            # prepare and open wxg
            source = self._prepare_wxg(language, source)
            self._open_wxg_file(source)

            # set "Output path", language and generate code
            self._set_output_path(filename)
            self._set_lang(language)
            self._process_wx_events()
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
                name_h = '%s.h' % os.path.splitext(filename)[0]
                name_cpp = filename
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
        'Test GUI code generation using "FontColour.wxg"'
        self.load_and_generate('FontColour')

    def test_StylelessDialog(self):
        "Test code generation for a style less dialog"
        source = self._load_file('styleless-dialog.wxg')

        # now test full code generation
        for language, dummy, ext in self.language_constants:
            filename = 'styleless-dialog%s' % ext

            # check for language first
            self.failUnless( language in common.code_writers, "No codewriter loaded for %s" % language )

            # prepare and open wxg
            source = self._prepare_wxg(language, source)
            self._open_wxg_file(source)

            # set "Output path", language and generate code
            self._set_output_path(filename)
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
                name_h = '%s.h' % os.path.splitext(filename)[0]
                name_cpp = filename
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

    def test_CodeGeneration_AllWidgets_30(self):
        """\
        Test GUI code generation using "AllWidgets_30.wxg"
        """
        self.load_and_generate('AllWidgets_30', ['lisp'])

    def test_CodeGeneration_ComplexExample(self):
        """\
        Test GUI code generation using "ComplexExample.wxg"
        """
        self.load_and_generate('ComplexExample')

    def test_CodeGeneration_CustomWidget(self):
        """\
        Test GUI code generation using "CustomWidget.wxg"
        """
        self.load_and_generate('CustomWidget')

    def test_StylesMixin(self):
        """\
        StyleMixin: Test converting of styles
        """
        import edit_windows, new_properties
        class ESM(edit_windows.EditStylesMixin):
            def __init__(self, klass='', styles=[]):
                new_properties.PropertyOwner.__init__(self)
                edit_windows.EditStylesMixin.__init__(self, klass, styles)

        #esm = edit_windows.EditStylesMixin('wxHyperlinkCtrl')
        esm = ESM('wxHyperlinkCtrl')

        # test converting of a combined attribute
        style_names = ['wxHL_DEFAULT_STYLE', 'wxHL_ALIGN_LEFT',
                       'wxHL_ALIGN_RIGHT', 'wxHL_ALIGN_CENTRE',
                       'wxHL_CONTEXTMENU']
        ret = esm.style_names[:]
        ret.sort()
        expected = style_names
        expected.sort()
        self.failUnlessEqual(ret, expected, 'EditStylesMixin.__init__() failed: got "%s" expect: "%s"'%(ret, expected))

        # test setting new styles
        #esm.set_style('|'.join(style_names))
        esm.properties["style"].set('|'.join(style_names))
        ret = esm.style_set
        expected = set(['wxHL_DEFAULT_STYLE', 'wxHL_ALIGN_LEFT',
                        'wxHL_ALIGN_RIGHT'])
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.set_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )
        esm.set_style([True, False, True, False, False])
        ret = esm.style_set
        expected = set(['wxHL_ALIGN_LEFT', 'wxHL_ALIGN_CENTRE'])
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.set_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )
        esm.set_style('wxHL_DEFAULT_STYLE|wxHL_CONTEXTMENU')
        ret = esm.style_set
        expected = set(['wxHL_DEFAULT_STYLE',])
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected)
        )

        # test generating a flag list
        ret = esm.get_style()
        expected = [False, False, False, False, True]
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.get_style(): got "%s" expect: "%s"' % (
                ret, expected)
        )

        # returning styles as a string concatenated with '|'
        ret = esm.get_string_style()
        expected = 'wxHL_DEFAULT_STYLE'
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.get_style_string(): got "%s" expect: "%s"' %
            (ret, expected)
        )

        # test setting styles via style dictionary
        from collections import OrderedDict
        styles = OrderedDict()
        styles[_('Border')] = ['wxALL', 'wxLEFT']
        styles[_('Alignment')] = ['wxEXPAND', 'wxALIGN_RIGHT']

        esm = edit_windows.EditStylesMixin('wxHyperlinkCtrl', styles)
        ret = esm.style_names[:]
        ret.sort()
        expected = ['wxALL', 'wxLEFT', 'wxEXPAND', 'wxALIGN_RIGHT']
        expected.sort()
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.__init__() failed: got "%s" expect: "%s"' % (
                ret, expected)
        )

        # check handling of empty ('', None) styles
        for value, desc in (('', "''"), (None, 'None')):
            esm.set_style(value)
            ret = esm.style_set
            expected = set()
            self.failUnlessEqual(
                ret,
                expected,
                'EditStylesMixin.set_style(%s) failed: got "%s" expect: "%s"' % (
                    desc, ret, expected)
            )
            ret = esm.get_style()
            expected = [False, False, False, False]
            self.failUnlessEqual(
                ret,
                expected,
                'EditStylesMixin.get_style() failed: got "%s" expect: "%s"' % (
                    ret, expected)
            )
            ret = esm.get_int_style()
            expected = 0
            self.failUnlessEqual(
                ret,
                expected,
                'EditStylesMixin.get_int_style() failed: got "%s" expect: '
                '"%s"' % (ret, expected)
            )

        # check handling of unsupported style
        esm = edit_windows.EditStylesMixin('wxStaticText')
        esm.codegen.for_version = (2, 8)

        # set un-supported style
        esm.set_style('wxST_ELLIPSIZE_MIDDLE')

        ret = esm.style_set
        expected = set(('wxST_ELLIPSIZE_MIDDLE',))
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.set_style("wxST_ELLIPSIZE_MIDDLE") failed: got '
            '"%s" expect: "%s"' % (ret, expected))

        ret = esm.get_int_style()
        expected = 0
        self.failUnlessEqual(
            ret,
            expected,
            'EditStylesMixin.get_int_style() failed: got "%s" expect: "%s"' % (
                ret, expected)
        )

    def test_missing_application_attributes(self):
        """\
        Test load wxg file w/ missing <application> attributes and generate
        code
        """
        fullpath = os.path.join(self.caseDirectory, 'app_wo_attrs_gui.xrc')
        obuffer = compat.StringIO()

        xrc2wxg.convert(fullpath, obuffer)

        generated = obuffer.getvalue().decode('UTF-8')
        expected = self._load_file('app_wo_attrs_gui.wxg')

        self._compare(expected, generated, "wxg")

        self._messageBox = None
        self._open_wxg_file(generated)
        self.failIf(
            self._messageBox,
            'Loading test wxg file caused an error message: %s' %
            self._messageBox
        )

    def test_load_xrc(self):
        """\
        Test loading XRC files
        """
        res = wx.xrc.EmptyXmlResource()
        for filename in glob.glob(os.path.join(self.caseDirectory, '*.xrc')):
            self.failUnless(
                    res.Load(filename),
                    'Loading XRC file %s failed' % os.path.relpath(
                            filename, self.caseDirectory)
            )