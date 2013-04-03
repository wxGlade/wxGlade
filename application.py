"""
Application class to store properties of the application being created
 
@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import codecs
import locale
import os
import re
import wx

from widget_properties import *
import common
import config
import errors
import math
import misc

class FileDirDialog:
    """\
    Custom class which displays a FileDialog or a DirDialog, according to the
    value of the L{Application.codegen_opt} of its parent (instance of
    L{Application}).
    
    @ivar default_extension: The default extension will be added to all
                             file names without extension.
    @type default_extension: String
    
    @ivar file_message: Message to show on the file dialog
    @type file_message: String
    
    @ivar dir_message: Message to show on the directory dialog
    @type dir_message: String
    
    @ivar file_style: Style for the file dialog
    @ivar dir_style:  Style for the directory dialog
    @ivar value:      Value returned by file or directory dialog on success
    @ivar parent:     Parent instance of L{Application}
    @ivar prev_dir:   Previous directory
    """
    def __init__(self, parent, wildcard=_("All Files|*"),
                 file_message=_("Choose a file"), dir_message=None, file_style=0):
        self.prev_dir = config.preferences.codegen_path or ""
        self.wildcard = wildcard
        self.file_message = file_message
        self.dir_message = dir_message
        self.file_style = file_style
        self.dir_style = wx.DD_DEFAULT_STYLE|wx.DD_NEW_DIR_BUTTON
        self.parent = parent
        self.value = None
        self.default_extension = None

    def ShowModal(self):
        """\
        Show a FileDialog or a DirDialog as a modal dialog. The selected
        file or directory is stored in L{value} on success.
        
        @return: C{ID_OK} or C{ID_CANCEL}
        @see: L{get_value()}
        """
        if self.parent.codegen_opt == 0:
            self.value = misc.FileSelector(
                self.file_message,
                self.prev_dir,
                wildcard=self.wildcard,
                flags=self.file_style
                )
            # check for file extension and add default extension if missing
            if self.value and self.default_extension:
                ext = os.path.splitext(self.value)[1].lower()
                if not ext:
                    self.value = "%s%s" % (self.value, self.default_extension)
        else:
            self.value = misc.DirSelector(
                self.dir_message,
                self.prev_dir,
                style=self.dir_style
                )
        if self.value:
            self.prev_dir = self.value
            if not os.path.isdir(self.prev_dir):
                self.prev_dir = os.path.dirname(self.prev_dir)
            return wx.ID_OK
        return wx.ID_CANCEL

    def get_value(self):
        """\
        Return the dialog value returned during the last L{ShowModal()} call.
        
        @see: L{value}
        """
        return self.value

# end of class FileDirDialog


class Application(object):
    """\
    Properties of the application being created
    
    @ivar __filename:  Name of the output XML file
    @ivar __saved:     If True, there are no changes to save
    @ivar codegen_opt: If != 0, generates a separate file for each class
    @ivar for_version: Version string of major dot minor version number
    @type for_version: String
    @ivar klass:       Name of the automatically generated class derived from
                       wxApp
    @ivar name:        Name of the wxApp instance to generate
    @ivar notebook:    Notebook to show different property panels
    
    @cvar all_supported_versions: Supported wx versions
    @type all_supported_versions: List of strings
    """
    
    all_supported_versions = ['2.6', '2.8']

    def __init__(self, property_window):
        self.property_window = property_window
        self.notebook = wx.Notebook(self.property_window, -1)
        self.notebook.sizer = None
        self.notebook.SetAutoLayout(True)
        self.notebook.Hide()
        panel = wx.ScrolledWindow(
            self.notebook, 
            wx.ID_ANY,
            style=wx.TAB_TRAVERSAL | wx.FULL_REPAINT_ON_RESIZE,
            )
        panel_settings = wx.ScrolledWindow(
            self.notebook, 
            wx.ID_ANY,
            style=wx.TAB_TRAVERSAL | wx.FULL_REPAINT_ON_RESIZE
            )
        self.name = "app" 
        self.__saved = True
        self.__filename = None
        self.klass = "MyApp"
        self.codegen_opt = config.default_multiple_files
        def set_codegen_opt(value):
            try:
                opt = int(value)
            except ValueError:
                pass
            else:
                self.codegen_opt = opt
        self.indent_mode = 1
        self.indent_amount = config.default_indent_amount
        def set_indent_mode(value):
            try: opt = int(value)
            except ValueError: pass
            else: self.indent_mode = opt
        def set_indent_amount(value):
            try: opt = int(value)
            except ValueError: pass
            else: self.indent_amount = opt
        self.source_ext = 'cpp'
        self.header_ext = 'h'
        def set_source_ext(value): self.source_ext = value
        def set_header_ext(value): self.header_ext = value
        self.output_path = ""
        self.language = 'python' # output language
        def get_output_path(): return os.path.expanduser(self.output_path)
        def set_output_path(value): self.output_path = value
        self.is_template = False
        self.use_gettext = config.default_use_gettext
        def set_use_gettext(value): self.use_gettext = bool(int(value))
        self.for_version = wx.VERSION_STRING[:3]
        def set_for_version(value):
            self.for_version = self.for_version_prop.get_str_value()
        self.access_functions = {
            'name': (lambda : self.name, self.set_name),
            'class': (lambda : self.klass, self.set_klass), 
            'code_generation': (lambda : self.codegen_opt, set_codegen_opt),
            'indent_mode': (lambda : self.indent_mode, set_indent_mode),
            'indent_amount': (lambda : self.indent_amount, set_indent_amount),
            'source_ext' : (lambda : self.source_ext, set_source_ext),
            'header_ext' : (lambda : self.header_ext, set_header_ext),
            'output_path': (get_output_path, set_output_path),
            'language': (self.get_language, self.set_language),
            'encoding': (self.get_encoding, self.set_encoding),
            'use_gettext': (lambda : self.use_gettext, set_use_gettext),
            'for_version': (lambda : self.for_version, set_for_version),
            }
        self.name_prop = TextProperty(self, "name", panel, True)
        self.name_prop.set_tooltip(
            _('Name of the instance created from "Class"')
            )
        self.klass_prop = TextProperty(self, "class", panel, True)
        self.klass_prop.set_tooltip(
            _("Name of the automatically generated class derived from wxApp")
            )

        self.encoding = self._get_default_encoding()
        self.encoding_prop = TextProperty(self, 'encoding', panel)
        self.encoding_prop.set_tooltip(
            _("Encoding of the generated source files")
            )

        self.use_gettext_prop = CheckBoxProperty(self, "use_gettext", panel,
                                                 _("Enable gettext support"))
        self.use_gettext_prop.set_tooltip(
            _("Enable internationalisation and localisation for the generated source files")
            )
        TOP_WIN_ID = wx.NewId()
        self.top_win_prop = wx.Choice(panel, TOP_WIN_ID, choices=[],
                                     size=(1, -1))
        self.top_win_prop.SetToolTip(wx.ToolTip(
            _("This widget is used as top window in the wxApp start code")
            ))
        self.top_window = '' # name of the top window of the generated app

        codegen_tooltips = [
            _("Write all source code in one file"),
            _("Split source code in one file per class / widget"),
            ]
        self.codegen_prop = RadioProperty(self, "code_generation", panel,
                                          [_("Single file"),
                                           _("Separate file for" \
                                           " each class")],
                                          label=_("Code Generation"),
                                          tooltips=codegen_tooltips)
        self.indent_mode_prop = RadioProperty(self, "indent_mode",
                                              panel_settings,
                                              [_("Tabs"), _("Spaces")],
                                              columns=2,
                                              label=_("Indentation mode"))
        self.indent_amount_prop = SpinProperty(self, 'indent_amount',
                                               panel_settings, r=(1, 100))
        self.indent_amount_prop.set_tooltip(
            _('Number of spaces or tabs used for one indentation level.')
            )
        self.source_ext_prop = TextProperty(self, 'source_ext',
                                            panel_settings)
        self.source_ext_prop.set_tooltip(_('Extension of the source file'))
        self.header_ext_prop = TextProperty(self, 'header_ext',
                                            panel_settings)
        self.header_ext_prop.set_tooltip(_('Extension of the header file'))

        _writers = common.code_writers.keys()
        columns = 3

        self.codewriters_prop = RadioProperty(self, "language", panel,
                                              _writers, columns=columns, 
                                              sort=True, capitalize=True)
        self.codewriters_prop.set_str_value('python')

        for_version_tooltips = [
            _("Generate source files for wxWidgets version %s") % version \
            for version in self.all_supported_versions
            ]
        self.for_version_prop = RadioProperty(
            self,
            "for_version",
            panel,
            self.all_supported_versions,
            columns=2,
            label=_("wxWidgets compatibility"),
            tooltips=for_version_tooltips,
            )
        self.for_version_prop.set_str_value(self.for_version)
        
        self.access_functions['use_new_namespace'] = (
            self.get_use_old_namespace, self.set_use_old_namespace)
        self.use_old_namespace_prop = CheckBoxProperty(
            self, 'use_new_namespace', panel_settings,
            _('Use old import\n"from wxPython.wx import *"'))
        self.use_old_namespace_prop.set_tooltip(
            _('It is generally recommended to use the new namespace. '
              'The old one ("from wxPython.wx import *") has some '
              'significant drawbacks like potential namespace conflicts.'
              ))

        self.overwrite = config.default_overwrite
        self.access_functions['overwrite'] = \
            (self.get_overwrite, self.set_overwrite)
        self.overwrite_prop = CheckBoxProperty(
            self,
            'overwrite',
            panel,
            _('Overwrite existing sources'),
            )
        self.overwrite_prop.set_tooltip(
            _("Overwrite existing source files or modify the code sequences "
              "generated by wxGlade in place")
            )
            
        dialog = FileDirDialog(
            self,
            _('All files|*'),
            _("Select output file"),
            _("Select output directory"),
            wx.SAVE|wx.OVERWRITE_PROMPT
            )
        self.outpath_prop = DialogProperty(self, "output_path", panel,
                                           dialog, label=_('Output path'))
        # update wildcards and default extention in the dialog
        self._update_dialog(self.outpath_prop.dialog, 'python')
        self.outpath_prop.set_tooltip(
            _("Output file or directory")
            )

        BTN_ID = wx.NewId()
        btn = wx.Button(
            panel,
            BTN_ID,
            _("Generate code"),
            name="BtnGenerateCode",
            )
        btn.SetToolTip(wx.ToolTip(_("Start generating source files")))

        # layout of self.notebook - page "Application"
        #=============================================

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(self.name_prop.panel, 0, wx.EXPAND)
        sizer.Add(self.klass_prop.panel, 0, wx.EXPAND)
        sizer.Add(self.encoding_prop.panel, 0, wx.EXPAND)
        sizer.Add(self.use_gettext_prop.panel, 0, wx.EXPAND)
        szr = wx.BoxSizer(wx.HORIZONTAL)
        from widget_properties import _label_initial_width as _w
        label = wx.StaticText(panel, -1, _("Top window"), size=(_w, -1))
        label.SetToolTip(wx.ToolTip(
            _("This widget is used as top window in the wxApp start code")
            ))
        szr.Add(label, 2, wx.ALL|wx.ALIGN_CENTER, 3)
        szr.Add(self.top_win_prop, 5, wx.ALL|wx.ALIGN_CENTER, 3)
        sizer.Add(szr, 0, wx.EXPAND)
        sizer.Add(self.codegen_prop.panel, 0, wx.ALL|wx.EXPAND, 4)
        sizer.Add(self.codewriters_prop.panel, 0, wx.ALL|wx.EXPAND, 4)
        sizer.Add(self.for_version_prop.panel, 0, wx.ALL|wx.EXPAND, 4)
        sizer.Add(self.overwrite_prop.panel, 0, wx.EXPAND)
        sizer.Add(self.outpath_prop.panel, 0, wx.EXPAND)
                
        sizer.Add(btn, 0, wx.ALL|wx.EXPAND, 5)

        self._add_page(_('Application'), panel, sizer)
        
        # layout self.notebook - page "Settings"
        #=======================================

        # general settings
        staticbox_general = wx.StaticBox(
            panel_settings,
            wx.ID_ANY,
            _("General Settings"),
            )
        sizer_general = wx.StaticBoxSizer(staticbox_general, wx.VERTICAL)
        sizer_general.Add(self.indent_mode_prop.panel, 0, wx.ALL|wx.EXPAND, 4)
        sizer_general.Add(self.indent_amount_prop.panel, 0, wx.EXPAND)

        # python specific settings
        staticbox_python = wx.StaticBox(
            panel_settings,
            wx.ID_ANY,
            _("Python Settings"),
            )
        sizer_python = wx.StaticBoxSizer(staticbox_python, wx.VERTICAL)
        sizer_python.Add(self.use_old_namespace_prop.panel, 0, wx.EXPAND)

        # C++ specific settings
        staticbox_cpp = wx.StaticBox(
        panel_settings,
            wx.ID_ANY,
            _("C++ Settings"),
            )
        sizer_cpp = wx.StaticBoxSizer(staticbox_cpp, wx.VERTICAL)
        sizer_cpp.Add(self.source_ext_prop.panel, 0, wx.EXPAND)
        sizer_cpp.Add(self.header_ext_prop.panel, 0, wx.EXPAND)

        # add all to one sizer
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(sizer_general, 0, wx.EXPAND|wx.ALL, 3)
        sizer.Add(sizer_python,  0, wx.EXPAND|wx.ALL, 3)
        sizer.Add(sizer_cpp,     0, wx.EXPAND|wx.ALL, 3)
        self._add_page(_('Settings'), panel_settings, sizer)
        
        wx.EVT_BUTTON(btn, BTN_ID, self.generate_code)
        wx.EVT_CHOICE(self.top_win_prop, TOP_WIN_ID, self.set_top_window)

        # this is here to keep the interface similar to the various widgets
        # (to simplify Tree)
        self.widget = None  # this is always None

    def set_name(self, value):
        value = "%s" % value
        if not re.match(self.set_name_pattern, value):
            self.name_prop.set_value(self.name)
        else:
            self.name = value
    set_name_pattern = re.compile('^[a-zA-Z]+[\w0-9-]*$')

    def set_klass(self, value):
        value = "%s" % value
        if not re.match(self.set_klass_pattern, value):
            self.klass_prop.set_value(self.klass)
        else:
            self.klass = value
    set_klass_pattern = re.compile('^[a-zA-Z]+[\w:.0-9-]*$')

    def _add_page(self, label, page, sizer):
        """\
        Add a page to properties notebook (L{self.notebook})
        """
        page.SetAutoLayout(True)
        page.SetSizer(sizer)
        sizer.Layout()
        sizer.Fit(page)
        self.notebook.AddPage(page, label)
        h = page.GetSize()[1]
        page.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))

    def _get_default_encoding(self):
        """\
        Return determinated machine default character encoding.
       
        The default application L{config.default_encoding} is the fallback
        only.
        """
        # try to set locale
        try:
            locale.setlocale(locale.LC_ALL)
        except locale.Error:
            # ignore problems by fallback to ascii
            print 'WARNING: Setting locale failed. Use "ascii" instead'
            return 'ascii'

        # try to query character encoding used in the selected locale
        try:
            encoding = locale.nl_langinfo(locale.CODESET)
        except AttributeError, e:
            print 'WARNING: locale.nl_langinfo(locale.CODESET) failed: %s' % str(e)
            # try getdefaultlocale, it used environment variables
            try:
                encoding = locale.getdefaultlocale()[1]
            except ValueError:
                encoding = config.default_encoding

        # On Mac OS X encoding may None or '' somehow
        if not encoding:
            encoding = config.default_encoding
            print 'WARNING: Empty encoding. Use "%s" instead' % encoding

        # check if a codec for the encoding exists
        try:
            codecs.lookup(encoding)
        except LookupError:
            print 'WARNING: No codec for encoding "%s" found. Use "ascii" instead' % encoding
            encoding = 'ascii'
            
        return encoding.upper()

    def get_encoding(self):
        return self.encoding

    def set_encoding(self, value):
        try: unicode('a', value)
        except LookupError, e:
            wx.MessageBox(str(e), _("Error"), wx.OK|wx.CENTRE|wx.ICON_ERROR)
            self.encoding_prop.set_value(self.encoding)
        else:
            self.encoding = value

    def set_language(self, value):
        language = self.codewriters_prop.get_str_value()
        
        # update wildcards and default extention in the dialog
        self._update_dialog(self.outpath_prop.dialog, language)
        
        # check that the new language supports all the widgets in the tree
        if self.language != language:
            self.language = language
            self.check_codegen()

    def get_language(self):
        return self.language #codewriters_prop.get_str_value()

    def _get_saved(self): return self.__saved
    def _set_saved(self, value):
        if self.__saved != value:
            self.__saved = value
            t = common.app_tree.get_title().strip()
            if not value: common.app_tree.set_title('* ' + t)
            else:
                if t[0] == '*': common.app_tree.set_title(t[1:].strip())
    saved = property(_get_saved, _set_saved)

    def _get_filename(self):
        return self.__filename

    def _set_filename(self, value):
        if not misc.streq(self.__filename, value):
            self.__filename = value
            if self.__saved: flag = ' '
            else: flag = '* '
            if self.__filename is not None:
                common.app_tree.set_title('%s(%s)' % (flag, self.__filename))
            else:
                common.app_tree.set_title(flag)
    filename = property(_get_filename, _set_filename)

    def get_overwrite(self):
        return self.overwrite

    def set_overwrite(self, val):
        self.overwrite = bool(int(val))

    def get_top_window(self): return self.top_window

    def set_top_window(self, *args):
        self.top_window = self.top_win_prop.GetStringSelection()

    def add_top_window(self, name):
        self.top_win_prop.Append("%s" % name)
        if not self.top_window:
            self.top_win_prop.SetSelection(self.top_win_prop.GetCount()-1)
            self.set_top_window()
            
    def remove_top_window(self, name):
        index = self.top_win_prop.FindString("%s" % name)
        if index != -1:
            if wx.Platform == '__WXGTK__':
                choices = [ self.top_win_prop.GetString(i) for i in \
                            range(self.top_win_prop.GetCount()) if i != index ]
                self.top_win_prop.Clear()
                for c in choices:
                    self.top_win_prop.Append(c)
            else:
                self.top_win_prop.Delete(index)

    def update_top_window_name(self, oldname, newname):
        index = self.top_win_prop.FindString(oldname)
        if index != -1:
            if self.top_window == oldname:
                self.top_window = newname
            if wx.Platform == '__WXGTK__':
                sel_index = self.top_win_prop.GetSelection()
                choices = [ self.top_win_prop.GetString(i) for i in \
                            range(self.top_win_prop.GetCount()) ]
                choices[index] = newname
                self.top_win_prop.Clear()
                for c in choices:
                    self.top_win_prop.Append(c)
                self.top_win_prop.SetSelection(sel_index)
            else:
                self.top_win_prop.SetString(index, newname)
        
    def reset(self):
        """\
        resets the default values of the attributes of the app
        """
        self.klass = "MyApp"; self.klass_prop.set_value("MyApp")
        self.klass_prop.toggle_active(False)
        self.name = "app"
        self.name_prop.set_value("app")
        self.name_prop.toggle_active(False)
        self.codegen_opt = config.default_multiple_files
        self.codegen_prop.set_value(config.default_multiple_files)
        self.indent_mode = 1
        self.indent_amount = config.default_indent_amount
        self.cpp_source_ext = 'cpp'
        self.cpp_header_ext = 'h'
        self.output_path = ""
        self.outpath_prop.set_value("")
        # do not reset language, but call set_language anyway to update the
        # wildcard of the file dialog
        self.set_language(self.get_language())
        self.top_window = ''
        self.top_win_prop.Clear()
        # ALB 2004-01-18
        #self.set_use_new_namespace(True)
        #self.use_new_namespace_prop.set_value(True)
        self.set_use_old_namespace(False)
        self.use_old_namespace_prop.set_value(False)
        
    def show_properties(self, *args):
        sizer_tmp = self.property_window.GetSizer()
        child = sizer_tmp.GetChildren()[0]
        w = child.GetWindow()
        if w is self.notebook: return
        w.Hide()

        self.notebook.Reparent(self.property_window)
        child.SetWindow(self.notebook)
        w.Reparent(misc.hidden_property_panel)
        
        self.notebook.Show(True)
        self.property_window.Layout()
        self.property_window.SetTitle(_('Properties - <%s>') % self.name)
        try: common.app_tree.select_item(self.node)
        except AttributeError: pass

    def __getitem__(self, name):
        return self.access_functions[name]

    def generate_code(self, *args, **kwds):
        preview = kwds.get('preview', False)
        if not self.output_path:
            return wx.MessageBox(_("You must specify an output file\n"
                                "before generating any code"), _("Error"),
                                wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION,
                                self.notebook)
        if not preview and \
               ((self.name_prop.is_active() or self.klass_prop.is_active()) \
                and self.top_win_prop.GetSelection() < 0):
            return wx.MessageBox(_("Please select a top window "
                                "for the application"), _("Error"), wx.OK |
                                wx.CENTRE | wx.ICON_EXCLAMATION, self.notebook)

        from cStringIO import StringIO
        class EncStringIO(object):
            def __init__(self, encoding=None):
                self.out = StringIO()
                self.encoding = encoding

            def write(self, data):
                if self.encoding is not None and type(data) == type(u''):
                    data = data.encode(self.encoding)
                self.out.write(data)

            def getvalue(self):
                return self.out.getvalue()

        # end of class EncStringIO
        
        out = EncStringIO(self.encoding)
        #common.app_tree.write(out) # write the xml onto a temporary buffer
        from xml_parse import CodeWriter
        try:
            # generate the code from the xml buffer
            cw = self.get_language() #self.codewriters_prop.get_str_value()
            if preview and cw == 'python': # of course cw == 'python', but...
                old = common.code_writers[cw].use_new_namespace
                common.code_writers[cw].use_new_namespace = True #False
                overwrite = self.overwrite
                self.overwrite = True
            class_names = common.app_tree.write(out) # write the xml onto a
                                                     # temporary buffer
            if not os.path.isabs(self.output_path) and self.filename:
                out_path = os.path.join(os.path.dirname(self.filename),
                                        self.output_path)
            else:
                out_path = None
            CodeWriter(
                common.code_writers[cw],
                out.getvalue(), True,
                preview=preview,
                out_path=out_path,
                class_names=class_names,
                )
            if preview and cw == 'python':
                common.code_writers[cw].use_new_namespace = old
                self.overwrite = overwrite
        except (errors.WxgOutputDirectoryNotExist,
                errors.WxgOutputDirectoryNotWritable,
                errors.WxgOutputPathIsDirectory,
                ), inst:
            wx.MessageBox(
                _("Error generating code:\n%s") % inst,
                _("Error"),
                wx.OK|wx.CENTRE|wx.ICON_ERROR,
                )
        except (IOError, OSError), msg:
            wx.MessageBox(
                _("Error generating code:\n%s") % msg,
                _("Error"),
                wx.OK|wx.CENTRE|wx.ICON_ERROR,
                )
        except Exception, msg:
            common.message.exception(_('Internal Error'))
            wx.MessageBox(
                _("An exception occurred while generating the code "
                  "for the application.\n"
                  "This is the error message associated with it:\n"
                  "        %s\n"
                  "For more details, look at the full traceback "
                  "on the console.\nIf you think this is a wxGlade bug,"
                  " please report it.") % msg,
                  _("Error"),
                  wx.OK|wx.CENTRE|wx.ICON_ERROR,
                  )
        else:
            if not preview:
                if config.preferences.show_completion:
                    # Show informational dialog
                    wx.MessageBox(
                        _("Code generation completed successfully"),
                        _("Information"),
                        wx.OK | wx.CENTRE | wx.ICON_INFORMATION,
                        )
                else:
                    # Show message in application status bar
                    app = wx.GetApp()
                    frame = app.GetTopWindow()
                    frame.user_message(_('Code generated'))

    def get_name(self):
        if self.name_prop.is_active():
            return self.name
        return ''

    def get_class(self):
        if self.klass_prop.is_active(): return self.klass
        return ''

    def update_view(self, *args): pass

    def is_visible(self): return True

    def preview(self, widget, out_name=None):
        if out_name is None:
            import warnings
            warnings.filterwarnings("ignore", "tempnam", RuntimeWarning,
                                    "application")
            out_name = os.tempnam(None, 'wxg') + '.py'
            #print 'Temporary name:', out_name
        widget_class_name = widget.klass

        # make a valid name for the class (this can be invalid for
        # some sensible reasons...)
        widget.klass = widget.klass[widget.klass.rfind('.')+1:]
        widget.klass = widget.klass[widget.klass.rfind(':')+1:]
        #if widget.klass == widget.base:
        # ALB 2003-11-08: always randomize the class name: this is to make
        # preview work even when there are multiple classes with the same name
        # (which makes sense for XRC output...)
        import random
        widget.klass = '_%d_%s' % \
                       (random.randrange(10**8, 10**9), widget.klass)
            
        self.real_output_path = self.output_path
        self.output_path = out_name
        real_codegen_opt = self.codegen_opt
        real_language = self.language
        real_use_gettext = self.use_gettext
        self.use_gettext = False
        self.language = 'python'
        self.codegen_opt = 0
        overwrite = self.overwrite
        self.overwrite = 0
        
        frame = None
        try:
            self.generate_code(preview=True)
            # dynamically import the generated module
            FrameClass = misc.import_name(self.output_path, widget.klass)
            if issubclass(FrameClass, wx.MDIChildFrame):
                frame = wx.MDIParentFrame(None, -1, '')
                child = FrameClass(frame, -1, '')
                child.SetTitle('<Preview> - ' + child.GetTitle())
                w, h = child.GetSize()
                frame.SetClientSize((w+20, h+20))
            elif not (issubclass(FrameClass, wx.Frame) or
                      issubclass(FrameClass, wx.Dialog)):
                # the toplevel class isn't really toplevel, add a frame...
                frame = wx.Frame(None, -1, widget_class_name)
                if issubclass(FrameClass, wx.MenuBar):
                    menubar = FrameClass()
                    frame.SetMenuBar(menubar)
                elif issubclass(FrameClass, wx.ToolBar):
                    toolbar = FrameClass(frame, -1)
                    frame.SetToolBar(toolbar)
                else:
                    panel = FrameClass(frame, -1)
                frame.Fit()
            else:
                frame = FrameClass(None, -1, '')
                # make sure we don't get a modal dialog...
                s = frame.GetWindowStyleFlag()
                frame.SetWindowStyleFlag(s & ~wx.DIALOG_MODAL)
            def on_close(event):
                frame.Destroy()
                widget.preview_widget = None
                widget.preview_button.SetLabel(_('Preview'))
            wx.EVT_CLOSE(frame, on_close)
            frame.SetTitle(_('<Preview> - %s') % frame.GetTitle())
            # raise the frame
            frame.CenterOnScreen()
            frame.Show()
            # remove the temporary file (and the .pyc/.pyo ones too)
            for ext in '', 'c', 'o', '~':
                name = self.output_path + ext
                if os.path.isfile(name):
                    os.unlink(name)
        except Exception, e:
            #common.message.exception(_('Internal Error'))
            widget.preview_widget = None
            widget.preview_button.SetLabel(_('Preview'))
            wx.MessageBox(_("Problem previewing gui: %s") % str(e), _("Error"),
                         wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION)#, self.notebook)
        # restore app state
        widget.klass = widget_class_name
        self.output_path = self.real_output_path
        del self.real_output_path
        self.codegen_opt = real_codegen_opt
        self.language = real_language
        self.use_gettext = real_use_gettext
        self.overwrite = overwrite
        return frame

    def get_use_old_namespace(self):
        try:
            return not common.code_writers['python'].use_new_namespace
        except:
            return False

    def set_use_old_namespace(self, val):
        try:
            common.code_writers['python'].use_new_namespace = not bool(int(val))
        except:
            pass

    def check_codegen(self, widget=None, language=None):
        """\
        Checks whether widget has a suitable code generator for the given
        language (default: the current active language). If not, the user is
        informed with a message.
        """
        if language is None: language = self.language
        if widget is not None:
            cname = common.class_names[widget.__class__.__name__]
            if language != 'XRC':
                ok = cname in common.code_writers[language].obj_builders
            else:
                # xrc is special...
                xrcgen = common.code_writers['XRC']
                ok = xrcgen.obj_builders.get(cname, None) is not \
                     xrcgen.NotImplementedXrcObject
            if not ok:
                common.message.warn(
                               _('No %s code generator for %s (of type %s)'
                               ' available'),
                               misc.capitalize(language), widget.name, cname)
        else:
            # in this case, we check all the widgets in the tree
            def check_rec(node):
                if node.widget is not None:
                    self.check_codegen(node.widget)
                if node.children:
                    for c in node.children:
                        check_rec(c)
            if common.app_tree.root.children:
                for c in common.app_tree.root.children:
                    check_rec(c)

    def _update_dialog(self, dialog, language):
        """\
        Update wildcards and default extension in the generic file and
        directory dialog (L{FileDirDialog}).
        """
        ext = getattr(common.code_writers[language], 'default_extensions', [])
        wildcards = []
        for e in ext:
            wildcards.append(
                _('%s files (*.%s)|*.%s') % (misc.capitalize(language), e, e)
                )
        wildcards.append(_('All files|*'))
        dialog.wildcard = '|'.join(wildcards)
        
        if ext:
            dialog.default_extension = '.%s' % ext[0]
        else:
            dialog.default_extension = None

# end of class Application
