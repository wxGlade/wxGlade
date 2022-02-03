"""\
Graphical tests

@copyright: 2012-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from testsupport_new import WXGladeGUITest

import wx, wx.xrc
import xrc2wxg
import common, compat
import glob, os, sys, unittest


class TestGui(WXGladeGUITest):
    "Test GUI functionality"

    def test_NotebookWithoutTabs(self):
        "Test loading Notebook without tabs"
        self._messageBox = None
        infilename = self._get_casefile_path('Notebook_wo_tabs.wxg')
        self.frame._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        self._assert_error_message('widget "notebook_1" does not have')

    def test_NotebookWithTabs(self):
        "Test loading Notebook with tabs"
        self._messageBox = None
        infilename = self._get_casefile_path('Notebook_w_tabs.wxg')
        self.frame._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        self.assertFalse( self._messageBox, 'Loading test wxg file caused an error message: %s' % self._messageBox )

    def test_StylelessDialog(self):
        "Test code generation for a style less dialog"
        self.load_and_generate('styleless-dialog')

    def test_CodeGeneration_AllWidgets_28(self):
        'Test GUI code generation using "AllWidgets_28"'
        self.load_and_generate('AllWidgets_28', preview = compat.IS_CLASSIC)

    @unittest.skipIf(wx.VERSION[:2]<(3,0), "not supported")
    def test_CodeGeneration_AllWidgets_30(self):
        'Test GUI code generation using "AllWidgets_30"'
        self.load_and_generate('AllWidgets_30', ['lisp'])

    def test_CodeGeneration_ComplexExample(self):
        'Test GUI code generation using "ComplexExample"'
        self.load_and_generate('ComplexExample')#, excluded=["wxg"])

    def test_CodeGeneration_ComplexExample30(self):
        'Test GUI code generation using "ComplexExample"'
        self.load_and_generate('ComplexExample_30', excluded=["lisp"])
        #self.load_and_generate('ComplexExample_30', included=["lisp"])
        # Lisp code has to raise an exception
        common.app_tree.root.properties["language"].set("lisp")
        self._process_wx_events()
        common.app_tree.root.generate_code()
        self._assert_error_message("Generating Lisp code")

    def test_CodeGeneration_CustomWidget(self):
        'Test GUI code generation using "CustomWidget"'
        self.load_and_generate('CustomWidget')

    def test_Statusbar_wo_labels(self):
        "Test code generation for a wxStatusBar with fields but w/o labels"
        self.load_and_generate('Statusbar_wo_labels')

    def test_Lisp_wxBitmapButton(self):
        """Test Lisp code generation with small wxBitmapButton example

        @see: L{wxglade.widgets.bitmap_button.lisp_codegen}"""
        self.load_and_generate( 'Lisp_wxBitmapButton', included=['lisp'], test_GUI=False )

    def test_CalendarCtrl(self):
        "Test code generation for a CalendarCtrl"
        self.load_and_generate('CalendarCtrl', test_GUI=False)

    def test_FontColour(self):
        "Test code generation for fonts and colours"
        self.load_and_generate('FontColour', test_GUI=True)

    def test_Font(self):
        "Test code generation for fonts"
        self.load_and_generate('FontTest28', test_GUI=True)
        self.load_and_generate('FontTest', test_GUI=False)

    def test_Frame_Size(self):
        "Generate Fit() code for the second frame"
        self.load_and_generate('Frame_Size', test_GUI=False)

    def test_sizes(self):
        # revision 1.1: test frame 'min_size' and managed 'max_size'
        self.load_and_generate('Sizes_FrameMin_ManagedMax', test_GUI=True)

    def test_event_binding(self):
        "bind mouse events for/to button, panel, frame; will not work if bound to frame"
        self.load_and_generate('Event_Binding', test_GUI=False)

    def test_Grid(self):
        "Test code generation with a grid widgets and handling events"
        self.load_and_generate('Grid', test_GUI=False)

    def test_ListCtrl_Report(self):
        "Test code generation for ListCtrl with some columns and rows"
        self.load_and_generate('ListCtrl_Report', test_GUI=True)

    def test_Gauge(self):
        "Test code generation for a wxGauge"
        self.load_and_generate('Gauge', test_GUI=False)

    def test_HyperlinkCtrl(self):
        "Test code generation for a HyperlinkCtrl"
        # test for wxWidgets 2.8.X
        self.load_and_generate('HyperlinkCtrl_28', test_GUI=False)

    def test_Preferences(self):
        "Test code generation for some variants of the preferences dialog; also tests backwards compatibility"
        import config
        restore = (config.preferences.default_border, config.preferences.default_border_size)

        # without default border
        config.preferences.default_border = False
        config.preferences.default_border_size = 0

        self.load_and_generate('Python_Preferences', included=["python"], test_GUI=True)
        self.load_and_generate('Perl_Preferences', included=["perl"], test_GUI=False, preview=False)
        self.load_and_generate('CPP_Preferences', included=["C++"], test_GUI=True)
        self.load_and_generate('Lisp_Preferences', included=["lisp"], test_GUI=True)

        # with default border
        config.preferences.default_border = True
        config.preferences.default_border_size = 5

        self.load_and_generate('Python_Preferences', included=["python"], test_GUI=True)
        self.load_and_generate('Perl_Preferences', included=["perl"], test_GUI=False, preview=False)
        self.load_and_generate('CPP_Preferences', included=["C++"], test_GUI=True)
        self.load_and_generate('Lisp_Preferences', included=["lisp"], test_GUI=True)

        config.preferences.default_border, config.preferences.default_border_size = restore

    def test_sizer_references(self):
        "Test storing references to sizers in class attributes"
        # don't store sizer references
        self.load_and_generate('Sizers_no_classattr', test_GUI=False)
        # store sizer references
        self.load_and_generate('Sizers_classattr', test_GUI=False)

    def test_bases_etc(self):
        # test changes in data structure and code generation from 0.9 to 1.0
        self.load_and_generate('BasesEtc_w_sizers', test_GUI=True)  # test files generated with v0.9.5 and re-ordered
        self.load_and_generate('BasesEtc', test_GUI=True)           # test files generated with 0.9.9pre

    def test_grid_custom_base(self):
        # test custom base for grid and also "Mark code block" deactivated
        self.load_and_generate('test_grid_custom_base', test_GUI=True)

    def test_keep_code_migration(self):
        # test migration from 0.9 to 1.0: __do_layout and __set_properties should be removed
        # C++: "virtual" will not be added here:  "public: void on_button_plot(wxCommandEvent &event);"
        # copy old files
        for ext in ("py","cpp","pl","lisp", "h"):
            old_filename = self._get_casefile_path('matplotlib_example_old.%s'%ext)
            generate_filename = self._get_outputfile_path('matplotlib_example.%s'%ext)
            self._copy_and_modify(old_filename, generate_filename)
        # the standard test will do the rest
        self.load_and_generate('matplotlib_example', test_GUI=True)

    def test_class_migration(self):
        # test migration from 0.9 to 1.0
        self.load_and_generate('test_no_custom_class09', test_GUI=True)

    def test_Python_Ogg1(self):
        "Test Python code generation with overwriting a single existing file, preserving manually added code"
        # set up filenames, copy the old file to the output path and modify it to trigger re-writing
        infilename = self._get_casefile_path('PyOgg1.wxg')
        generate_filename = self._get_outputfile_path('PyOgg1.py')
        expected_filename = self._get_casefile_path('PyOgg1.py')
        self._copy_and_modify(expected_filename, generate_filename, b"SetSize((500, 300))", b"SetSize((300, 300))")

        # load and set up project
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.properties["overwrite"].set(False)  # overwrite is 0 in the file
        common.app_tree.root.properties["output_path"].set(generate_filename)
        common.app_tree.root.properties["language"].set("python")
        # generate, compare and check for overwriting
        self._process_wx_events()
        common.app_tree.root.generate_code()
        self._compare_files(expected_filename, generate_filename, check_mtime=True)

    def test_Python_Ogg2(self):
        "Test Python code generation with overwriting two existing files, preserving manually added code"
        infilename = self._get_casefile_path('PyOgg2.wxg')

        generate_app    = self._get_outputfile_path('PyOgg2_app.py')
        generate_dialog = self._get_outputfile_path('PyOgg2_MyDialog.py')
        generate_frame  = self._get_outputfile_path('PyOgg2_MyFrame.py')

        expected_app    = self._get_casefile_path('PyOgg2_app.py')
        expected_dialog = self._get_casefile_path('PyOgg2_MyDialog.py')
        expected_frame  = self._get_casefile_path('PyOgg2_MyFrame.py')

        self._copy_and_modify(expected_dialog, generate_dialog, b"SetSize((500, 300))", b"SetSize((300, 300))")
        self._copy_and_modify(expected_frame,  generate_frame,  b"SetSize((400, 300))", b"SetSize((300, 300))")

        # load and set up project
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.properties["overwrite"].set(False)  # overwrite is 0 in the file
        common.app_tree.root.properties["output_path"].set(self.outDirectory)
        common.app_tree.root.properties["language"].set("python")
        # generate, compare and check for overwriting
        self._process_wx_events()
        common.app_tree.root.generate_code()
        self._compare_files(expected_app,    generate_app, check_mtime=True)
        self._compare_files(expected_dialog, generate_dialog, check_mtime=True)
        self._compare_files(expected_frame,  generate_frame, check_mtime=True)

    def test_all_Ogg1(self):
        "Test Python code generation with overwriting a single existing file, preserving manually added code"
        # XXX overwriting is only working if ALL files are there; if e.g. .h is missing, it fails!
        # set up filenames, copy the old file to the output path and modify it to trigger re-writing
        for language, P, E1, E2 in self.language_constants:
            if language=="XRC": continue
            infilename = self._get_casefile_path(P+'Ogg1.wxg')
            generate_filename = self._get_outputfile_path(P+'Ogg1'+E1)
            expected_filename = self._get_casefile_path(P+'Ogg1'+E1)
            self._copy_and_modify(expected_filename, generate_filename, b"(500, 300)", b"(300, 300)")
            if language=="C++":
                generate_filename_h = self._get_outputfile_path(P+'Ogg1.h')
                expected_filename_h = self._get_casefile_path(P+'Ogg1.h')
                self._copy_and_modify(expected_filename_h, generate_filename_h)
    
            # load and set up project
            common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
            common.app_tree.root.properties["overwrite"].set(False)  # overwrite is 0 in the file already
            common.app_tree.root.properties["output_path"].set(generate_filename)
            common.app_tree.root.properties["language"].set(language)
            # generate, compare and check for overwriting
            self._process_wx_events()
            common.app_tree.root.generate_code()
            self._compare_files(expected_filename, generate_filename, check_mtime=True)
            if language=="C++":
                self._compare_files(expected_filename_h, generate_filename_h, check_mtime=True)

    def test_all_Ogg2(self):
        "Test Python code generation with overwriting a single existing file, preserving manually added code"
        # XXX overwriting is only working if ALL files are there; if e.g. .h is missing, it fails!
        # set up filenames, copy the old file to the output path and modify it to trigger re-writing
        for language, P, E1, E2 in self.language_constants:
            infilename = self._get_casefile_path(P+'Ogg2.wxg')
            generate_filename = self._get_outputfile_path(P+'Ogg2'+E1)
            expected_filename = self._get_casefile_path(P+'Ogg2'+E1)
            self._copy_and_modify(expected_filename, generate_filename, b"(500, 300)", b"(300, 300)")
            if language=="C++":
                generate_filename_h = self._get_outputfile_path(P+'Ogg2.h')
                expected_filename_h = self._get_casefile_path(P+'Ogg2.h')
                self._copy_and_modify(expected_filename_h, generate_filename_h)
    
            # load and set up project
            common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
            common.app_tree.root.properties["overwrite"].set(False)  # overwrite is 0 in the file
            common.app_tree.root.properties["output_path"].set(generate_filename)
            common.app_tree.root.properties["language"].set(language)
            # generate, compare and check for overwriting
            self._process_wx_events()
            common.app_tree.root.generate_code()
            self._compare_files(expected_filename, generate_filename, check_mtime=True)
            if language=="C++":
                self._compare_files(expected_filename_h, generate_filename_h, check_mtime=True)

    def test_all_Ogg2(self):
        "Test code generation with overwriting multiples existing files, preserving manually added code"
        for language, P, E1, E2 in self.language_constants:
            if language=="C++":
                APP = "Ogg2_main"
            else:
                APP = 'Ogg2_app'
            infilename = self._get_casefile_path(P+'Ogg2.wxg')
            if not infilename: continue

            generate_app    = self._get_outputfile_path(P+APP+E1)
            generate_dialog = self._get_outputfile_path(P+'Ogg2_MyDialog'+E2)
            generate_frame  = self._get_outputfile_path(P+'Ogg2_MyFrame'+E2)

            expected_app    = self._get_casefile_path(P+APP+E1)
            expected_dialog = self._get_casefile_path(P+'Ogg2_MyDialog'+E2)
            expected_frame  = self._get_casefile_path(P+'Ogg2_MyFrame'+E2)
    
            self._copy_and_modify(expected_dialog, generate_dialog, b"(500, 300)", b"(300, 300)")
            self._copy_and_modify(expected_frame,  generate_frame,  b"(400, 300)", b"(300, 300)")
            if language=="C++":
                generate_filename_dialog_h = self._get_outputfile_path(P+'Ogg2_MyDialog.h')
                expected_filename_dialog_h = self._get_casefile_path(P+'Ogg2_MyDialog.h')
                generate_filename_frame_h = self._get_outputfile_path(P+'Ogg2_MyFrame.h')
                expected_filename_frame_h = self._get_casefile_path(P+'Ogg2_MyFrame.h')
                self._copy_and_modify(expected_filename_dialog_h, generate_filename_dialog_h)
                self._copy_and_modify(expected_filename_frame_h,  generate_filename_frame_h)

            # load and set up project
            common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
            common.app_tree.root.properties["overwrite"].set(False)  # overwrite is 0 in the file
            common.app_tree.root.properties["output_path"].set(self.outDirectory)
            common.app_tree.root.properties["language"].set(language)
            # generate, compare and check for overwriting
            self._process_wx_events()
            if language=="C++":
                common.app_tree.root.app_filename = P+APP
            common.app_tree.root.generate_code()
            check_mtime = language!="perl"
            self._compare_files(expected_app,    generate_app,    check_mtime=check_mtime)
            self._compare_files(expected_dialog, generate_dialog, check_mtime=check_mtime)
            self._compare_files(expected_frame,  generate_frame,  check_mtime=check_mtime)

            if language=="C++":
                self._compare_files(expected_filename_dialog_h, generate_filename_dialog_h, check_mtime=True)
                self._compare_files(expected_filename_frame_h, generate_filename_frame_h, check_mtime=True)

    def test_all_Tool_Menu_EventBinding(self):
        self.load_and_generate('Tool_Menu_EventBinding', excluded=["lisp"], test_GUI=False)

    def test_OutputFileAndDirectory(self):
        "Test check for output file and directory"
        infilename = self._get_casefile_path('Python_Preferences.wxg')
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)

        # Single output file out_path shouldn't be a directory, non-existing file or non-writable directory
        common.app_tree.root.properties["output_path"].set(self.outDirectory)
        common.app_tree.root.generate_code()
        self._assert_error_message( "path is directory" )

        common.app_tree.root.properties["output_path"].set( os.path.join(self.outDirectory,"non-existing/result.py") )
        common.app_tree.root.generate_code()
        self._assert_error_message( "directory does not exist" )

        #common.app_tree.root.properties["output_path"].set( os.path.join(self.outDirectory,"non-writable/result.py") )
        #common.app_tree.root.generate_code()
        #self._assert_error_message( "" )

        # Multiple output file out_path should be a writable directory
        common.app_tree.root.properties["multiple_files"].set(1)
        common.app_tree.root.properties["output_path"].set( os.path.join(self.outDirectory,"non-existing") )
        common.app_tree.root.generate_code()
        self._assert_error_message( "directory does not exist" )

        #common.app_tree.root.properties["output_path"].set( os.path.join(self.outDirectory,"non-writable") )
        #common.app_tree.root.generate_code()
        #self._assert_error_message( "can not be a directory when generating a single file" )

    def test_PythonSubclass(self):
        "check for correct import: from package.name import name"
        infilename = self._get_casefile_path('PythonSubclass.wxg')
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.filename = self._get_outputfile_path('PythonSubclass.wxg')
        common.app_tree.root.generate_code()

        expected_filename = self._get_casefile_path("PythonSubclass.py")
        generated_filename = self._get_outputfile_path("PythonSubclass.py")
        self._compare_files(expected_filename, generated_filename)
        expected_filename = self._get_casefile_path("PythonSubclass_PythonSubclass.py")
        generated_filename = self._get_outputfile_path("PythonSubclass/PythonSubclass.py")
        self._compare_files(expected_filename, generated_filename)

    def test_WxgXRCMultipleFilesNotSupported(self):
        "Test for multi file XRC projects."
        infilename = self._get_casefile_path('Python_Preferences.wxg')
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.properties["multiple_files"].set(1)
        common.app_tree.root.properties["language"].set("XRC")
        common.app_tree.root.generate_code()
        self._assert_error_message( "XRC code cannot be split into multiple files" )

    def test_WxgTemplateCodegenNotPossible(self):
        "Test for code generation from a template"
        infilename = self._get_casefile_path('Python_Preferences.wxg')
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.properties["is_template"].set(True)
        common.app_tree.root.generate_code()
        self._assert_error_message( "Code generation from a template is not possible" )

    def test_SizersSize(self):
        HAVE_WRAP_SIZER = hasattr(wx, "WrapSizer")  # only for 3.0
        if HAVE_WRAP_SIZER:
            self.load_and_generate('SizersSizeTests', test_GUI=True)
        else:
            self.load_and_generate('SizersSizeTests_nowrap', test_GUI=True)

    def test_Menu(self):
        self.load_and_generate('MenuTest', excluded=["lisp"], test_GUI=True)

    def test_Menu_lambda_handlers_keep_code(self):
        # copy .py file to test "Keep user code"
        old_filename = self._get_casefile_path('MenuHandlers_Lambda.py')
        generate_filename = self._get_outputfile_path("MenuHandlers_Lambda.py")
        self._copy_and_modify(old_filename, generate_filename)
        # the standard test will do the rest
        self.load_and_generate('MenuHandlers_Lambda', included=["python"], test_GUI=True)

    def _assert_styles(self, got, expected, msg=None):
        if isinstance(got,      str): got      = got.split("|")
        if isinstance(expected, str): expected = expected.split("|")
        if isinstance(got,      (list,set)): got      = sorted(got)
        if isinstance(expected, (list,set)): expected = sorted(expected)
        msg = msg or "Style names"
        self.assertEqual(got, expected, '%s do not match: got "%s" expected %s'%(msg, got, expected))

    def test_StylesMixin(self):
        "StyleMixin: Test converting of styles"
        # XXX actually generate a file and simulate editing

        #common.palette._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        common.app_tree.root.clear()
        common.app_tree.root.init()
        import widgets.frame.frame
        frame = widgets.frame.frame.builder(common.root, 0, "wxFrame", "MyFrame", "frame")

        import widgets.hyperlink_ctrl.hyperlink_ctrl
        hyperlink = widgets.hyperlink_ctrl.hyperlink_ctrl.builder(frame, 0, "Hyperlink")
        self._process_wx_events()
        # check available style names
        sp = hyperlink.properties["style"]
        style_names = ['wxHL_ALIGN_CENTRE','wxHL_ALIGN_LEFT','wxHL_ALIGN_RIGHT','wxHL_CONTEXTMENU','wxHL_DEFAULT_STYLE',
                       'wxBORDER_NONE']
        self._assert_styles(sp._names, style_names, "Style names for HyperlinkCtrl")

        # test setting all styles and check whether some are combined  (not too useful, as no exclusions are defined)
        sp.set( style_names )
        #self._assert_styles('wxHL_ALIGN_LEFT|wxHL_ALIGN_RIGHT|wxHL_DEFAULT_STYLE', sp.get_string_value(),
                            #"Full style flags for HyperlinkCtrl")
        self._assert_styles(style_names, sp.get_string_value(), "Full style flags for HyperlinkCtrl")

        return
        sp.set( "HL_ALIGN_LEFT" )
        self.assertEqual( ret, expected, 'EditStylesMixin.set_style(): got "%s" expect: "%s"' % (ret, expected) )
        esm.set_style([True, False, True, False, False])
        ret = esm.style_set
        expected = set(['wxHL_ALIGN_LEFT', 'wxHL_ALIGN_CENTRE'])
        self.assertEqual( ret, expected, 'EditStylesMixin.set_style(): got "%s" expect: "%s"' % (ret, expected) )
        
        esm.set_style('wxHL_DEFAULT_STYLE|wxHL_CONTEXTMENU')
        ret = esm.style_set
        expected = set(['wxHL_DEFAULT_STYLE',])
        self.assertEqual( ret, expected, 'EditStylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected) )

        # test generating a flag list
        ret = esm.get_style()
        expected = [False, False, False, False, True]
        self.assertEqual( ret, expected, 'EditStylesMixin.get_style(): got "%s" expect: "%s"' % (ret, expected) )

        # returning styles as a string concatenated with '|'
        ret = esm.get_string_style()
        expected = 'wxHL_DEFAULT_STYLE'
        self.assertEqual( ret, expected, 'EditStylesMixin.get_style_string(): got "%s" expect: "%s"' % (ret, expected) )

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
        self.assertEqual( ret, expected, 'EditStylesMixin.__init__() failed: got "%s" expect: "%s"' % ( ret, expected) )

        # check handling of empty ('', None) styles
        for value, desc in (('', "''"), (None, 'None')):
            esm.set_style(value)
            ret = esm.style_set
            expected = set()
            self.assertEqual(ret, expected, 'EditStylesMixin.set_style(%s) failed: got "%s" expect: "%s"' % (desc, ret, expected))
            ret = esm.get_style()
            expected = [False, False, False, False]
            self.assertEqual( ret, expected, 'EditStylesMixin.get_style() failed: got "%s" expect: "%s"' % (ret, expected))
            ret = esm.get_int_style()
            expected = 0
            self.assertEqual( ret, expected, 'EditStylesMixin.get_int_style() failed: got "%s" expect: ' '"%s"' % (ret, expected) )

        # check handling of unsupported style
        esm = edit_windows.EditStylesMixin('wxStaticText')
        esm.codegen.for_version = (2, 8)

        # set un-supported style
        esm.set_style('wxST_ELLIPSIZE_MIDDLE')

        ret = esm.style_set
        expected = set(('wxST_ELLIPSIZE_MIDDLE',))
        self.assertEqual( ret, expected,
            'EditStylesMixin.set_style("wxST_ELLIPSIZE_MIDDLE") failed: got "%s" expect: "%s"' % (ret, expected))

        ret = esm.get_int_style()
        expected = 0
        self.assertEqual( ret, expected, 'EditStylesMixin.get_int_style() failed: got "%s" expect: "%s"' % (ret, expected))

    def test_missing_application_attributes(self):
        #"Test load wxg file w/ missing <application> attributes and generate code"
        "convert .xrc file with missing <application> attributes to .wxg and load it"
        # this will fail on Python 3.8 as the ordering of elements is not defined
        # convert .xrc to .wxg
        infilename  = self._get_casefile_path('app_wo_attrs_gui.xrc')
        generated_filename = self._get_outputfile_path('app_wo_attrs_gui.wxg')
        xrc2wxg.convert(infilename, generated_filename)
        # compare
        expected_filename = self._get_casefile_path('app_wo_attrs_gui.wxg')
        self._compare_files(expected_filename, generated_filename)
        # open the .wxg file; there should be no problem
        self._messageBox = None
        self.frame._open_app(generated_filename, use_progress_dialog=False, add_to_history=False)
        self.assertFalse(self._messageBox,'Loading test wxg file caused an error message: %s'%self._messageBox)

    def test_toplevels_no_size(self):
        "Test frame, panel, dialog without size"
        # previous versions wrote only the last of the three panels
        self.load_and_generate('toplevels_no_size', test_GUI=True)

    def test_toplevels_code_pre_post(self):
        "Test frame, dialog without extracode_pre, extracode_post"
        self.load_and_generate('toplevel_extracode', test_GUI=False)

    def test_unsupported_flags(self):
        "Test code generation with unsupported flags"
        self.load_and_generate('no_supported_flags', test_GUI=False)

    def test_panel_class(self):
        "Test code generation for a standalone panel class"
        self.load_and_generate('PanelClass', excluded=("lisp",), test_GUI=False)

    @unittest.skipIf(wx.VERSION[:2]<(3,0), "not all supported (import_test.xrc fails")
    def test_load_xrc(self):
        "Test loading XRC files"
        res = wx.xrc.EmptyXmlResource()
        for filename in glob.glob(os.path.join(self.caseDirectory, '*.xrc')):
            self.assertTrue( res.Load(filename),
                             'Loading XRC file %s failed' % os.path.relpath(filename, self.caseDirectory) )

    def test_import_xrc(self):
        "Test importing XRC files: just import_test.xrc at the moment"
        infilename = self._get_casefile_path("import_test.xrc")
        common.main.import_xrc(infilename)
        # save file to wxg and check
        generated_filename = self._get_outputfile_path("import_test.wxg")
        compare_filename = self._get_casefile_path("import_test.wxg")
        common.main._save_app(generated_filename)
        self._compare_files(compare_filename, generated_filename)

    def stop(self):
        print("XXX")  # nothing to do


if __name__ == '__main__':
    unittest.main(exit=False)
