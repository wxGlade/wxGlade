"""
Application class to store properties of the application being created

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import os
import random
import re
import types
import wx
import plugins

from widget_properties import *
import bugdialog
import common
import compat
import config
import errors
import math
import misc


class FileDirDialog(object):
    """\
    Custom class which displays a FileDialog or a DirDialog, according to the
    value of the L{Application.multiple_files} of its parent (instance of
    L{Application}).

    @ivar default_extension: The default extension will be added to all
                             file names without extension.
    @type default_extension: str

    @ivar file_message: Message to show on the file dialog
    @type file_message: str

    @ivar dir_message: Message to show on the directory dialog
    @type dir_message: str

    @ivar file_style: Style for the file dialog
    @ivar dir_style:  Style for the directory dialog
    @ivar value:      Value returned by file or directory dialog on success
    @ivar parent:     Parent instance of L{Application}
    @ivar prev_dir:   Previous directory
    """

    def __init__(self, parent, wildcard=_("All files|*"),
                 file_message=_("Choose a file"), dir_message=None,
                 file_style=0):
        self.prev_dir = config.preferences.codegen_path or ""
        self.wildcard = wildcard
        self.file_message = file_message
        self.dir_message = dir_message
        self.file_style = file_style
        self.dir_style = wx.DD_DEFAULT_STYLE | wx.DD_NEW_DIR_BUTTON
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
        if self.parent.multiple_files == 0:
            self.value = wx.FileSelector(
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
            self.value = wx.DirSelector(
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
    @ivar multiple_files: If != 0, generates a separate file for each class
    @ivar for_version: Version string of major dot minor version number
    @type for_version: str
    @ivar klass:       Name of the automatically generated class derived from
                       wxApp
    @ivar name:        Name of the wxApp instance to generate
    @ivar notebook:    Notebook to show different property panels
    @ivar _logger: Instance specific logger

    @cvar all_supported_versions: Supported wx versions
    @type all_supported_versions: list[str]
    """

    all_supported_versions = ['2.8', '3.0']

    def __init__(self, property_window):
        self._logger = logging.getLogger(self.__class__.__name__)
        self.property_window = property_window
        self.notebook = wx.Notebook(self.property_window, -1)
        self.notebook.sizer = None
        self.notebook.SetAutoLayout(True)
        self.notebook.Hide()
        panel_application = wx.ScrolledWindow(
            self.notebook,
            wx.ID_ANY,
            style=wx.TAB_TRAVERSAL | wx.FULL_REPAINT_ON_RESIZE,
            name='ApplicationPanel',
            )
        panel_settings = wx.ScrolledWindow(
            self.notebook,
            wx.ID_ANY,
            style=wx.TAB_TRAVERSAL | wx.FULL_REPAINT_ON_RESIZE,
            name='SettingsPanel'
            )
        self.name = "app"
        self.__saved = True
        self.__filename = None
        self.klass = "MyApp"
        self.multiple_files = config.default_multiple_files
        self.indent_mode = 1
        self.indent_amount = config.default_indent_amount

        self.source_ext = 'cpp'
        self.header_ext = 'h'
        if self.multiple_files:
            self.output_path = config.default_output_path
        else:
            self.output_path = config.default_output_file
        self.language = 'python'  # output language
        self.is_template = False
        self.use_gettext = config.default_use_gettext
        self.for_version = wx.VERSION_STRING[:3]

        self.access_functions = {
            'name': (lambda: self.name, self.set_name),
            'class': (lambda: self.klass, self.set_klass),
            'multiple_files': (
                lambda: self.multiple_files,
                self.set_multiple_files
                ),
            'indent_mode': (lambda: self.indent_mode, self.set_indent_mode),
            'indent_amount': (
                lambda: self.indent_amount,
                self.set_indent_amount
                ),
            'source_ext': (lambda: self.source_ext, self.set_source_ext),
            'header_ext': (lambda: self.header_ext, self.set_header_ext),
            'output_path': (self.get_output_path, self.set_output_path),
            'language': (self.get_language, self.set_language),
            'encoding': (self.get_encoding, self.set_encoding),
            'use_gettext': (lambda: self.use_gettext, self.set_use_gettext),
            'for_version': (lambda: self.for_version, self.set_for_version),
            }
        self.name_prop = TextProperty(self, "name", panel_application, True)
        self.name_prop.set_tooltip(
            _('Name of the instance created from "Class"')
            )
        self.klass_prop = TextProperty(self, "class", panel_application, True)
        self.klass_prop.set_tooltip(
            _("Name of the automatically generated class derived from wxApp")
            )

        self.encoding = config.encoding
        self.encoding_prop = TextProperty(self, 'encoding', panel_application)
        self.encoding_prop.set_tooltip(
            _("Encoding of the generated source files")
            )

        self.use_gettext_prop = CheckBoxProperty(
            self,
            'use_gettext',
            panel_application,
            _("Enable gettext support"),
            )
        self.use_gettext_prop.set_tooltip(
            _("Enable internationalisation and localisation for the "
                "generated source files")
            )
        TOP_WIN_ID = wx.NewId()
        self.top_win_prop = wx.Choice(
            panel_application,
            TOP_WIN_ID,
            choices=[],
            size=(1, -1),
            )
        self.top_win_prop.SetToolTip(wx.ToolTip(
            _("This widget is used as top window in the wxApp start code")
            ))
        self.top_window = ''  # name of the top window of the generated app

        codegen_tooltips = [
            _("Write all source code in one file"),
            _("Split source code in one file per class / widget"),
            ]
        self.multiple_files_prop = RadioProperty(
            self, "multiple_files", panel_application,
            [_("Single file"), _("Separate file for each class")],
            label=_("Code Generation"), tooltips=codegen_tooltips)
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

        self.codewriters_prop = RadioProperty(self, "language", panel_application,
                                              _writers, columns=columns,
                                              sort=True, capitalize=True)
        self.codewriters_prop.set_str_value('python')

        for_version_tooltips = []
        for version in self.all_supported_versions:
            if version == '3.0':
                for_version_tooltips.append(
                    _('Generate source files for wxWidgets version %s\n'
                    'Starting with wxPython 3.0 old style import are not '
                    'supported anymore.'
                    ) % version
                    )
            else:
                for_version_tooltips.append(
                    _("Generate source files for wxWidgets version %s") % version
                    )
        self.for_version_prop = RadioProperty(
            self,
            "for_version",
            panel_application,
            self.all_supported_versions,
            columns=3,
            label=_("wxWidgets compatibility"),
            tooltips=for_version_tooltips,
            )
        self.for_version_prop.set_str_value(self.for_version)

        self.overwrite = config.default_overwrite
        self.access_functions['overwrite'] = \
            (self.get_overwrite, self.set_overwrite)
        self.overwrite_prop = CheckBoxProperty(
            self,
            'overwrite',
            panel_application,
            _('Overwrite existing sources'),
            )
        self.overwrite_prop.set_tooltip(
            _("Overwrite existing source files or modify the code sequences "
              "generated by wxGlade in place.\n"
              "Modifying code in place is deprecated. "
              "Please adapt your application.")
            )

        dialog = FileDirDialog(
            self,
            _('All files|*'),
            _("Select output file"),
            _("Select output directory"),
            wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT
            )
        self.outpath_prop = DialogProperty(self, "output_path", panel_application,
                                           dialog, label=_('Output path'))
        # update wildcards and default extension in the dialog
        self._update_wildcards(self.outpath_prop.dialog, 'python')
        self.outpath_prop.set_tooltip(
            _("Output file or directory")
            )

        BTN_ID = wx.NewId()
        btn = wx.Button(
            panel_application,
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
        label = wx.StaticText(
            panel_application,
            -1,
            _("Top window"),
            size=(config.label_initial_width, -1),
            )
        label.SetToolTip(wx.ToolTip(
            _("This widget is used as top window in the wxApp start code")
            ))
        szr.Add(label, 2, wx.ALL | wx.ALIGN_CENTER, 3)
        szr.Add(self.top_win_prop, 5, wx.ALL | wx.ALIGN_CENTER, 3)
        sizer.Add(szr, 0, wx.EXPAND)
        sizer.Add(self.multiple_files_prop.panel, 0, wx.ALL | wx.EXPAND, 4)
        sizer.Add(self.codewriters_prop.panel, 0, wx.ALL | wx.EXPAND, 4)
        sizer.Add(self.for_version_prop.panel, 0, wx.ALL | wx.EXPAND, 4)
        sizer.Add(self.overwrite_prop.panel, 0, wx.EXPAND)
        sizer.Add(self.outpath_prop.panel, 0, wx.EXPAND)

        sizer.Add(btn, 0, wx.ALL | wx.EXPAND, 5)

        self._add_page(_('Application'), panel_application, sizer)

        # layout self.notebook - page "Settings"
        #=======================================

        # general settings
        staticbox_general = wx.StaticBox(
            panel_settings,
            wx.ID_ANY,
            _("General Settings"),
            )
        sizer_general = wx.StaticBoxSizer(staticbox_general, wx.VERTICAL)
        sizer_general.Add(
            self.indent_mode_prop.panel,
            0,
            wx.ALL | wx.EXPAND,
            4,
            )
        sizer_general.Add(self.indent_amount_prop.panel, 0, wx.EXPAND)

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
        sizer.Add(sizer_general, 0, wx.EXPAND | wx.ALL, 3)
        sizer.Add(sizer_cpp, 0, wx.EXPAND | wx.ALL, 3)
        self._add_page(_('Settings'), panel_settings, sizer)

        wx.EVT_BUTTON(btn, BTN_ID, self.generate_code)
        wx.EVT_CHOICE(self.top_win_prop, TOP_WIN_ID, self.set_top_window)

        # this is here to keep the interface similar to the various widgets
        # (to simplify Tree)
        self.widget = None  # this is always None

    def set_source_ext(self, value):
        self.source_ext = value

    def set_header_ext(self, value):
        self.header_ext = value

    def set_multiple_files(self, value):
        try:
            opt = int(value)
        except ValueError:
            pass
        else:
            self.multiple_files = opt

    def set_for_version(self, value):
        self.for_version = self.for_version_prop.get_str_value()

        if self.for_version.startswith('3.'):
            ## disable lisp for wx > 2.8
            if self.codewriters_prop.get_str_value() == 'lisp':
                wx.MessageBox(
                    _('Generating Lisp code for wxWidgets version %s is not '
                      'supported.\n'
                      'Set version to "2.8" instead.') % self.for_version,
                    _("Warning"),
                     wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION
                    )
                self.for_version_prop.set_str_value('2.8')
                self.set_for_version('2.8')
                return
            self.codewriters_prop.enable_item('lisp', False)
        else:
            # enable lisp again
            self.codewriters_prop.enable_item('lisp', True)

    def set_indent_mode(self, value):
        try:
            opt = int(value)
        except ValueError:
            pass
        else:
            self.indent_mode = opt

    def set_indent_amount(self, value):
        try:
            opt = int(value)
        except ValueError:
            pass
        else:
            self.indent_amount = opt

    def set_name(self, value):
        value = "%s" % value
        if not re.match(self.set_name_pattern, value):
            self.name_prop.set_value(self.name)
        else:
            self.name = value
    set_name_pattern = re.compile(r'^[a-zA-Z]+[\w0-9-]*$')

    def set_klass(self, value):
        value = "%s" % value
        if not re.match(self.set_klass_pattern, value):
            self.klass_prop.set_value(self.klass)
        else:
            self.klass = value
    set_klass_pattern = re.compile(r'^[a-zA-Z]+[\w:.0-9-]*$')

    def get_output_path(self):
        return os.path.normpath(os.path.expanduser(self.output_path))

    def set_output_path(self, value):
        self.output_path = value

    def set_use_gettext(self, value):
        self.use_gettext = bool(int(value))

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
        page.SetScrollbars(1, 5, 1, int(math.ceil(h / 5.0)))

    def get_encoding(self):
        return self.encoding

    def set_encoding(self, value):
        try:
            unicode('a', value)
        except LookupError, inst:
            bugdialog.Show(_('Set Encoding'), inst)
            self.encoding_prop.set_value(self.encoding)
        else:
            self.encoding = value

    def set_language(self, value):
        """\
        Set code generator language and adapt corresponding settings like
        file dialog wild cards.

        @type value: str | int
        """
        assert isinstance(value, types.StringTypes + (types.IntType, ))

        if isinstance(value, types.IntType):
            self.codewriters_prop.set_value(value)
            language = self.codewriters_prop.get_str_value()
        else:
            language = value
            self.codewriters_prop.set_value(value)

        # update wildcards and default extension in the dialog
        self._update_wildcards(self.outpath_prop.dialog, language)

        # check that the new language supports all the widgets in the tree
        if self.language != language:
            self.language = language
            self.check_codegen()

        # disable lisp for wx > 2.8
        if language == 'lisp':
            if self.for_version_prop.get_str_value() == '3.0':
                self.for_version_prop.set_str_value('2.8')
                self.set_for_version('2.8')
                wx.MessageBox(
                    _('Generating Lisp code for wxWidgets version %s is not '
                      'supported.\n'
                      'Set version to "2.8" instead.') % self.for_version,
                    _("Warning"),
                    wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION
                    )
            # RadioProperty
            self.for_version_prop.enable_item('3.0', False)
        else:
            self.for_version_prop.enable_item('3.0', True)

        # don't change the extension in multiple files mode

        if self.multiple_files_prop.get_value() == 1:
            return

        # update file extensions
        current_name = self.outpath_prop.get_value()
        if not current_name:
            return
        base, ext = os.path.splitext(current_name)

        # is already a valid extension?
        # ext has a leading . but default_extensions hasn't
        if ext and \
           ext[1:] in common.code_writers[language].default_extensions:
                return
        new_name = "%s.%s" % (
            base, common.code_writers[language].default_extensions[0])
        self.outpath_prop.set_value(new_name)
        self.output_path = new_name

    def get_language(self):
        """\
        Return the selected code writer language

        @rtype: str
        """
        return self.language

    def _get_saved(self):
        return self.__saved

    def _set_saved(self, value):
        if self.__saved != value:
            self.__saved = value
            t = common.app_tree.get_title().strip()
            if not value:
                common.app_tree.set_title('* ' + t)
            else:
                if t[0] == '*':
                    common.app_tree.set_title(t[1:].strip())
    saved = property(_get_saved, _set_saved)

    def _get_filename(self):
        return self.__filename

    def _set_filename(self, value):
        if not misc.streq(self.__filename, value):
            self.__filename = value
            if self.__saved:
                flag = ' '
            else:
                flag = '* '
            if self.__filename is not None:
                common.app_tree.set_title('%s(%s)' % (flag, self.__filename))
            else:
                common.app_tree.set_title(flag)
    filename = property(_get_filename, _set_filename)

    def get_overwrite(self):
        return self.overwrite

    def set_overwrite(self, val):
        self.overwrite = bool(int(val))

    def get_top_window(self):
        return self.top_window

    def set_top_window(self, *args):
        self.top_window = self.top_win_prop.GetStringSelection()

    def add_top_window(self, name):
        self.top_win_prop.Append("%s" % name)
        if not self.top_window:
            self.top_win_prop.SetSelection(self.top_win_prop.GetCount() - 1)
            self.set_top_window()

    def remove_top_window(self, name):
        index = self.top_win_prop.FindString("%s" % name)
        if index != -1:
            if wx.Platform == '__WXGTK__':
                choices = [
                    self.top_win_prop.GetString(i) for i in
                    range(self.top_win_prop.GetCount()) if i != index
                    ]
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
                choices = [
                    self.top_win_prop.GetString(i) for i in
                    range(self.top_win_prop.GetCount())
                    ]
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
        self.klass = "MyApp"
        self.klass_prop.set_value("MyApp")
        self.klass_prop.toggle_active(False)
        self.name = "app"
        self.name_prop.set_value("app")
        self.name_prop.toggle_active(False)
        self.multiple_files = config.default_multiple_files
        self.multiple_files_prop.set_value(config.default_multiple_files)
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

    def show_properties(self, *args):
        sizer_tmp = self.property_window.GetSizer()
        child = sizer_tmp.GetChildren()[0]
        w = child.GetWindow()
        if w is self.notebook:
            return
        w.Hide()

        self.notebook.Reparent(self.property_window)
        compat.SizerItem_SetWindow(child, self.notebook)

        self.notebook.Show(True)
        self.property_window.Layout()
        self.property_window.SetTitle(_('Properties - <%s>') % self.name)
        try:
            common.app_tree.select_item(self.node)
        except AttributeError:
            pass

    def __getitem__(self, name):
        return self.access_functions[name]

    def generate_code(self, *args, **kwds):
        preview = kwds.get('preview', False)
        if not self.output_path:
            return wx.MessageBox(
                _("You must specify an output file\n"
                    "before generating any code"),
                _("Error"),
                wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION,
                self.notebook,
                )
        if not preview and (
                (self.name_prop.is_active() or self.klass_prop.is_active())
                and self.top_win_prop.GetSelection() < 0):
            return wx.MessageBox(
                _("Please select a top window for the application"),
                _("Error"),
                wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION,
                self.notebook,
                )

        # temporary buffer for XML
        tmp_xml = misc.UnicodeStringIO('utf-8')

        from xml_parse import CodeWriter
        try:
            # generate the code from the xml buffer
            codewriter = self.get_language()

            # save and overwrite some code generation settings
            if preview and codewriter == 'python':
                overwrite_save = self.overwrite
                self.overwrite = True

            class_names = common.app_tree.write(tmp_xml)

            out_path = os.path.expanduser(self.output_path.strip())
            if not os.path.isabs(out_path) and self.filename:
                out_path = os.path.join(
                    os.path.dirname(self.filename), out_path)
                out_path = os.path.normpath(out_path)
            else:
                out_path = None

            CodeWriter(common.code_writers[codewriter], tmp_xml.getvalue(),
                       True, preview=preview, out_path=out_path,
                       class_names=class_names)

            # restore saved settings
            if preview and codewriter == 'python':
                self.overwrite = overwrite_save

        except (errors.WxgBaseException, IOError, OSError), inst:
            wx.MessageBox(
                _("Error generating code:\n%s") % inst,
                _("Error"),
                wx.OK | wx.CENTRE | wx.ICON_ERROR,
                )
        except Exception, inst:
            bugdialog.Show(_('Generate Code'), inst)
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
        if self.klass_prop.is_active():
            return self.klass
        return ''

    def update_view(self, *args):
        pass

    def is_visible(self):
        return True

    def preview(self, widget, out_name=None):
        """\
        Generate and instantiate preview widget.

        None will be returned in case of errors. The error details are
        written to the application log file.
        """
        if out_name is None:
            import warnings
            warnings.filterwarnings("ignore", "tempnam", RuntimeWarning,
                                    "application")
            out_name = os.tempnam(None, 'wxg') + '.py'
        widget_class_name = widget.klass

        # make a valid name for the class (this can be invalid for
        # some sensible reasons...)
        widget.klass = widget.klass[widget.klass.rfind('.') + 1:]
        widget.klass = widget.klass[widget.klass.rfind(':') + 1:]

        # ALB 2003-11-08: always randomize the class name: this is to make
        # preview work even when there are multiple classes with the same name
        # (which makes sense for XRC output...)
        widget.klass = '_%d_%s' % \
                       (random.randrange(10 ** 8, 10 ** 9), widget.klass)

        self.real_output_path = self.output_path
        self.output_path = out_name
        real_multiple_files = self.multiple_files
        real_language = self.language
        real_use_gettext = self.use_gettext
        self.use_gettext = False
        self.language = 'python'
        self.multiple_files = 0
        overwrite = self.overwrite
        self.overwrite = 0

        frame = None
        try:
            self.generate_code(preview=True)
            # import generated preview module dynamically
            preview_path = os.path.dirname(self.output_path)
            preview_module_name = os.path.basename(self.output_path)
            preview_module_name = os.path.splitext(preview_module_name)[0]
            preview_module = plugins.import_module(preview_path,
                                                   preview_module_name)
            if not preview_module:
                wx.MessageBox(
                    _('Can not import the preview module from file \n'
                      '"%s".\n'
                      'The details are written to the log file.\n'
                      'If you think this is a wxGlade bug, please '
                      'report it.') % self.output_path,
                    _('Error'), wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION)
                return None

            preview_class = getattr(preview_module, widget.klass)

            if not preview_class:
                wx.MessageBox(
                    _('No preview class "%s" found.\n'
                      'The details are written to the log file.\n'
                      'If you think this is a wxGlade bug, please '
                      'report it.') % widget.klass,
                    _('Error'), wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION)
                return None

            if issubclass(preview_class, wx.MDIChildFrame):
                frame = wx.MDIParentFrame(None, -1, '')
                child = preview_class(frame, -1, '')
                child.SetTitle('<Preview> - ' + child.GetTitle())
                w, h = child.GetSize()
                frame.SetClientSize((w + 20, h + 20))
            elif not (issubclass(preview_class, wx.Frame) or
                      issubclass(preview_class, wx.Dialog)):
                # the toplevel class isn't really toplevel, add a frame...
                frame = wx.Frame(None, -1, widget_class_name)
                if issubclass(preview_class, wx.MenuBar):
                    menubar = preview_class()
                    frame.SetMenuBar(menubar)
                elif issubclass(preview_class, wx.ToolBar):
                    toolbar = preview_class(frame, -1)
                    frame.SetToolBar(toolbar)
                else:
                    panel = preview_class(frame, -1)
                frame.Fit()
            else:
                frame = preview_class(None, -1, '')

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
        except Exception, inst:
            widget.preview_widget = None
            widget.preview_button.SetLabel(_('Preview'))
            bugdialog.Show(_("Generate Preview"), inst)

        # restore app state
        widget.klass = widget_class_name
        self.output_path = self.real_output_path
        del self.real_output_path
        self.multiple_files = real_multiple_files
        self.language = real_language
        self.use_gettext = real_use_gettext
        self.overwrite = overwrite
        return frame

    def check_codegen(self, widget=None, language=None):
        """\
        Checks whether widget has a suitable code generator for the given
        language (default: the current active language). If not, the user is
        informed with a message.
        """
        if language is None:
            language = self.language
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
                self._logger.warn(
                    _('No %s code generator for %s (of type %s) available'),
                    misc.capitalize(language),
                    widget.name,
                    cname,
                    )
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

    def _update_wildcards(self, dialog, language):
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
