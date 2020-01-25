"""
Application class to store properties of the application being created

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import os, sys, random, re, logging, math
import wx

import common, config, misc, plugins, errors, compat
import bugdialog
import new_properties as np



class FileDirDialog(object):
    """Custom class which displays a FileDialog or a DirDialog, according to the value of the
    L{Application.multiple_files} of its parent (instance of L{Application}).

    @ivar default_extension: The default extension will be added to all file names without extension.
    @ivar file_message: Message to show on the file dialog
    @ivar dir_message: Message to show on the directory dialog

    @ivar file_style: Style for the file dialog
    @ivar dir_style:  Style for the directory dialog
    @ivar value:      Value returned by file or directory dialog on success
    @ivar parent:     Parent instance of L{Application}
    @ivar prev_dir:   Previous directory"""

    def __init__(self, parent, wildcard=_("All files|*"), file_message=_("Choose a file"),dir_message=None,file_style=0):
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
        """Show a FileDialog or a DirDialog as a modal dialog.
        The selected file or directory is stored in self.value on success. Returns ID_OK or ID_CANCEL."""
        if self.parent.multiple_files == 0:
            self.value = wx.FileSelector( self.file_message, self.prev_dir, wildcard=self.wildcard,
                                          flags=self.file_style )
            # check for file extension and add default extension if missing
            if self.value and self.default_extension:
                ext = os.path.splitext(self.value)[1].lower()
                if not ext:
                    self.value = "%s%s" % (self.value, self.default_extension)
        else:
            self.value = wx.DirSelector( self.dir_message, self.prev_dir, style=self.dir_style )
        if self.value:
            self.prev_dir = self.value
            if not os.path.isdir(self.prev_dir):
                self.prev_dir = os.path.dirname(self.prev_dir)
            return wx.ID_OK
        return wx.ID_CANCEL

    def get_value(self):
        "Return the dialog value returned during the last L{ShowModal()} call;  @see: L{value}"
        return self.value



class Application(np.PropertyOwner):
    "Properties of the application being created"

    all_supported_versions = ['2.8', '3.0']  # Supported wx versions
    _VERSION_TOOLTIPS = ("Generate source files for wxWidgets version 2.8",
                         "Generate source files for wxWidgets version 3.0\nOld style import are not supported anymore.")

    PROPERTIES = ["Application", "name", "class", "encoding", "use_gettext", "top_window", "multiple_files",
                                 "language", "for_version", "overwrite", "mark_blocks",
                                 "output_path", "generate_code",
                  "Settings",    "indent_mode", "indent_amount", "source_extension", "header_extension"]
    _PROPERTY_LABELS = {"source_extension":     'C++ source file ext',
                        "header_extension":     'C++ header file ext',
                        "use_gettext":          "Enable gettext support",
                        "indent_mode":          "Indentation mode",
                        "multiple_files":       "Code Generation",
                        "overwrite":            "Keep user code",
                        "mark_blocks":          "Mark code blocks",
                        "generate_code":        "Generate Source"}
    _PROPERTY_HELP = {"name":            'Name of the instance created from "Class";\n'
                                         ' also used as (main) file name in case of "Separate file for each class"',
                      "class":           "Name of the automatically generated class derived from wxApp",
                      "use_gettext":     "Enable internationalisation and localisation for the generated source files",
                      "header_extension":'for C++ only: extension of the header file',
                      "source_extension":'for C++ only: extension of the source file',
                      'indent_amount':   'Number of spaces or tabs used for one indentation level.',
                      "top_window":      "This widget is used as top window in the wxApp start code",
                      "overwrite":"Keep user code in source files when generating code.\n"
                                  "wxGlade will just write the blocks that are marked 'begin wxGlade'/'end wxGlade'.\n"
                                  "Be aware that this feature is not too robust against e.g. renaming of frames.\n"
                                  "Always keep backups and don't touch the wxGlade begin/end markers!",
                      "output_path": "Output file or directory: absolute or relative path",
                      "mark_blocks":"Mark auto-generated code blocks with BEGIN/END wxGlade comments.\n"
                                    "This allows to identify user code in source files.\n"
                                    "Therefore it can not be disabled if 'Keep user code' is selected."
                      }
    if sys.platform=="win32":
        _PROPERTY_HELP["output_path"] = "Output file or directory; double click label to show in Explorer"
    elif sys.platform=="darwin":
        _PROPERTY_HELP["output_path"] = "Output file or directory; double click label to show in Finder"
    is_sizer = False
    _is_toplevel = False
    base = None

    def __init__(self):
        np.PropertyOwner.__init__(self)
        self._logger = logging.getLogger(self.__class__.__name__)

        self.__saved    = True  # raw value for self.saved property; if True, there are no changes to save
        self.__filename = None  # raw value for the self.filename property; Name of the output XML file

        # initialise instance properties
        self.is_template = np.Property(False)  # hidden property
        # name and derived class name, including validation
        self.name  = np.TextPropertyA("app",   default_value="")
        self.klass = np.TextPropertyA("MyApp", default_value="", name="class")
        self.properties["name"].validation_re = re.compile(r'^[a-zA-Z]+[\w0-9-]*$')
        self.properties["class"].validation_re = re.compile(r'^[a-zA-Z]+[\w:.0-9-]*$')

        # generate separate file for each class?
        labels   = [_("Single file"),                       _("Separate file for each class") ]
        tooltips = [_("Write all source code in one file"), _("Split source code in one file per class / widget") ]
        self.multiple_files = np.RadioProperty( config.default_multiple_files,
                                                values=[0,1], labels=labels, tooltips=tooltips )

        # code indentation: mode and count
        self.indent_mode   = np.RadioProperty( 1, [0,1], ["Tabs","Spaces"], aliases=["tab","space"], columns=2 )
        self.indent_amount = np.SpinProperty( config.default_indent_amount, val_range=(1, 100) )
        # C++ file extension
        self.source_extension = np.TextProperty('cpp')
        self.header_extension = np.TextProperty('h')
        # output path
        output_path = config.default_output_path  if self.multiple_files else  config.default_output_file
        self.output_path = np.FileNameProperty(output_path, style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)
        self._update_output_path('python')

        self.overwrite = np.InvCheckBoxProperty(config.default_overwrite)
        # YYY 
        self.mark_blocks = np.CheckBoxProperty(True)

        # output language
        languages = sorted( common.code_writers.keys() )
        labels = [misc.capitalize(s) for s in languages]
        self.language = np.RadioProperty('python', languages, labels, columns=3)

        # gettext?
        self.use_gettext = np.CheckBoxProperty(config.default_use_gettext)
        # wx Version: string of major dot minor version number
        version = "%d.%d"%compat.version
        if not version in self.all_supported_versions:
            version = "2.8"  if version[0]=="2" else  "3.0"
        self.for_version = np.RadioProperty( version, self.all_supported_versions, tooltips=self._VERSION_TOOLTIPS)

        # encoding
        encodings = ["UTF-8", "ISO-8859-1", "ISO-8859-15", "CP1252"]  # just some common values
        self.encoding = np.ComboBoxProperty(config.default_encoding, encodings)

        # top window name for the generated app
        self.top_window = prop = np.ListBoxProperty("", choices=[])
        prop.auto_activated = True
        self.generate_code = np.ActionButtonProperty(self.generate_code)

        self.widget = None  # always None, just to keep interface to Tree similar to other editors
        self.node = None

    def set_for_version(self, value):
        self.for_version = self.for_version_prop.get_string_value()

        if self.for_version.startswith('3.'):
            ## disable lisp for wx > 2.8
            if self.codewriters_prop.get_string_value() == 'lisp':
                misc.warning_message( _('Generating Lisp code for wxWidgets version %s is not supported.\n'
                                        'Set version to "2.8" instead.') % self.for_version )
                self.for_version_prop.set_str_value('2.8')
                self.set_for_version('2.8')
                return
            self.codewriters_prop.enable_item('lisp', False)
        else:
            # enable lisp again
            self.codewriters_prop.enable_item('lisp', True)

    def get_output_path(self):
        return os.path.normpath(os.path.expanduser(self.output_path))

    def _add_page(self, label, page, sizer):
        "Add a page to properties notebook (L{self.notebook})"
        page.SetAutoLayout(True)
        page.SetSizer(sizer)
        sizer.Layout()
        sizer.Fit(page)
        self.notebook.AddPage(page, label)
        h = page.GetSize()[1]
        page.SetScrollbars(1, 5, 1, int(math.ceil(h / 5.0)))

    def set_encoding(self, value):
        try:
            unicode('a', value)
        except LookupError as inst:
            bugdialog.Show(_('Set Encoding'), inst)
            self.encoding_prop.set_value(self.encoding)
        else:
            self.encoding = value

    def _set_language(self):
        "Set code generator language and adapt corresponding settings like file dialog wild cards (value: str or int)"
        language = self.language
        # update wildcards and default extension in the dialog
        self._update_output_path(language)

        # check that the new language supports all the widgets in the tree
        self.check_codegen()

        # disable lisp for wx > 2.8
        if language == 'lisp':
            for_version = self.for_version
            if for_version == '3.0':
                misc.warning_message( _('Generating Lisp code for wxWidgets version %s is not supported.\n'
                                        'Set version to "2.8" instead.') % self.for_version )
                self.properties["for_version"].set('2.8')
            self.properties["for_version"].set_blocked(True)
        else:
            self.properties["for_version"].set_blocked(False)

        # don't change the extension in multiple files mode
        if self.multiple_files == 1:
            return

        # update file extensions
        current_name = self.output_path
        if not current_name:
            return
        base, ext = os.path.splitext(current_name)

        # is already a valid extension? ext has a leading . but default_extensions hasn't
        if ext and ext[1:] in common.code_writers[language].default_extensions:
            return
        new_name = "%s.%s" % (base, common.code_writers[language].default_extensions[0])
        self.properties["output_path"].set(new_name)

        blocked = self.language!="C++"
        self.properties["source_extension"].set_blocked(blocked)
        self.properties["header_extension"].set_blocked(blocked)


    # properties: saved and filename
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
        if self.__filename != value:
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

    # interface from tree ##############################################################################################
    def add_top_window(self, name):
        p = self.properties["top_window"]
        p.add_choice(name)
        if not p.get() and not p.deactivated:
            p.set(name)
        if p.deactivated:
            p.set_active()

    def remove_top_window(self, name):
        p = self.properties["top_window"]
        p.remove_choice(name)

    def update_top_window_name(self, oldname, newname):
        p = self.properties["top_window"]
        if not oldname in p.choices: return
        if p.get() == oldname:
            p.value = newname
        p.choices[p.choices.index(oldname)] = newname
        p.set_choices()

    ####################################################################################################################
    def properties_changed(self, modified):
        # ['encoding', 'output_path', 'class', 'name', 'multiple_files', 'language', 'top_window', 'use_gettext',
        # 'use_gettext', 'is_template', 'overwrite', 'indent_mode', 'indent_amount', 'for_version', 'source_extension',
        # 'header_extension']
        # XXX any other to be handled?
        if not modified or "language" in modified:
            self._set_language() # update language-dependent choices
        if not modified or "name" in modified or "class" in modified:
            # enable/disable top_window
            self.properties["top_window"].set_active(self.name or self.klass)
        if not modified or "overwrite" in modified:
            block = not self.overwrite
            self.properties["mark_blocks"].set_blocked(block)
            if block:
                self.properties["mark_blocks"].set(True)
            self.properties["mark_blocks"].set_blocked(block)

    def _init(self):
        # common part for init and new
        p = self.properties
        p["multiple_files"].set( config.default_multiple_files )
        p["indent_mode"].set(1)
        p["indent_amount"].set(config.default_indent_amount)
        p["source_extension"].set('cpp')
        p["header_extension"].set('h')
        if config.default_multiple_files:
            p["output_path"].set("wxglade_out")
        else:
            p["output_path"].set("wxglade_out.dummy")  # will be changed by _set_language()
        p["top_window"].set('')
        p["top_window"].set_choices([])
        # do not reset language, but call set_language anyway to update the wildcard of the file dialog
        self._set_language()

    def init(self):
        "initializes the attributes of the app before loading a file"
        p = self.properties
        p["class"].set("MyApp", deactivate=True)
        p["name" ].set("app",   deactivate=True)
        self._init()
        self.properties_changed(None)

    def new(self):
        "resets the default values of the attributes of the app"
        p = self.properties
        p["class"].set("MyApp", activate=True)
        p["name" ].set("app",   activate=True)
        self._init()
        self.properties_changed(None)

    def generate_code(self, preview=False, out_path=None, widget=None):
        if config.use_gui:
            common.property_panel.flush()
        else:
            np.flush_current_property()
        if out_path is None:
            out_path = os.path.expanduser(self.output_path.strip())
            if not os.path.isabs(out_path) and (out_path and self.filename):
                out_path = os.path.join(os.path.dirname(self.filename), out_path)
                out_path = os.path.normpath(out_path)

        if not out_path:
            msg = "You must specify an output file before generating any code."
            if not self.filename:
                msg += "\nFor relative file names, the project needs to be saved first."
            return misc.error_message( msg )

        name_p = self.properties["name"]
        class_p = self.properties["class"]
        if self.language != "XRC":
            if not preview and ( name_p.is_active() or class_p.is_active() ) and not self.top_window:
                return misc.error_message( "Please select a top window for the application or deactivate "
                                           "the Name and Class properties for Application.\n"
                                           "In that case, only code for the windows will be generated, not for the "
                                           "application." )

        if preview:
            writer = common.code_writers["python"].copy()
        else:
            writer = common.code_writers[self.language]#.copy()

        try:
            writer.new_project(self, out_path, preview)
            writer.generate_code(self.node, widget)
            writer.finalize()
        except errors.WxgBaseException as inst:
            misc.error_message( _("Error generating code:\n%s")%inst )
            return
        except EnvironmentError as inst:
            bugdialog.ShowEnvironmentError(_('An IO/OS related error is occurred:'), inst)
            bugdialog.Show(_('Generate Code'), inst)
            return
        finally:
            writer.clean_up(self.node)

        if preview or not config.use_gui: return
        if config.preferences.show_completion:
            # Show informational dialog
            misc.info_message("Code generation completed successfully")
        else:
            # Show message in application status bar
            app = wx.GetApp()
            frame = app.GetTopWindow()
            frame.user_message(_('Code generated'))


    def is_visible(self):
        return True

    def _get_preview_filename(self):
        import warnings
        warnings.filterwarnings("ignore", "tempnam", RuntimeWarning, "application")
        if compat.PYTHON2:
            out_name = os.tempnam(None, 'wxg') + '.py'
        else:
            # create a temporary file at either the output path or the project path
            error = None
            if not self.filename:
                error = "Save project first; a temporary file will be created in the same directory."
            else:
                dirname, basename = os.path.split(self.filename)
                basename, extension = os.path.splitext(basename)
                if not os.path.exists(dirname):
                    error = "Directory '%s' not found"%dirname
                elif not os.path.isdir(dirname):
                    error = "'%s' is not a directory"%dirname
            if error:
                misc.error_message(error)
                return None
            while True:
                out_name = os.path.join(dirname, "_%s_%d.py"%(basename,random.randrange(10**8, 10**9)))
                if not os.path.exists(out_name): break
        return out_name

    def preview(self, widget, position=None):
        """Generate and instantiate preview widget.
        None will be returned in case of errors. The error details are written to the application log file."""
        # some checks
        #if compat.IS_PHOENIX:
            #found = common.app_tree.find_widgets_by_classnames(widget.node, "EditPropertyGridManager")
            #if found:
                #error = ("Preview with PropertyGridManager controls is currently deactivated as it causes crashes "
                         #"with wxPython Phoenix")
                #wx.MessageBox( error, _('Error'), wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION )
                #return
        # XXX check other things as well, e.g. different bitmap sizes for BitmapButton

        preview_filename = self._get_preview_filename()
        if preview_filename is None: return
        widget_class_name = widget.klass

        # make a valid name for the class (this can be invalid for some sensible reasons...)
        widget_class = widget.klass[widget.klass.rfind('.') + 1:]
        widget_class = widget.klass[widget.klass.rfind(':') + 1:]
        # ALB 2003-11-08: always randomize the class name: this is to make preview work even when there are multiple
        # classes with the same name (which makes sense for XRC output...)
        widget_class = '_%d_%s' % (random.randrange(10 ** 8, 10 ** 9), widget_class)
        widget.properties["class"].set(widget_class)

        if wx.Platform == "__WXMAC__" and not compat.PYTHON2:
            # workaround for Mac OS testing: sometimes the caches need to be invalidated
            import importlib
            importlib.invalidate_caches()

        frame = None
        try:
            self.generate_code(True, preview_filename, widget)
            # import generated preview module dynamically
            preview_path = os.path.dirname(preview_filename)
            preview_module_name = os.path.basename(preview_filename)
            preview_module_name = os.path.splitext(preview_module_name)[0]
            preview_module = plugins.import_module(preview_path, preview_module_name)
            if not preview_module:
                misc.error_message( _('Can not import the preview module from file \n"%s".\n'
                                      'The details are written to the log file.\n'
                                      'If you think this is a wxGlade bug, please report it.') % self.output_path )
                return None

            try:
                preview_class = getattr(preview_module, widget.klass)
            except AttributeError:
                # module loade previously -> do a re-load XXX this is required for Python 3; check alternatives
                import importlib
                preview_module = importlib.reload(preview_module)
                preview_class = getattr(preview_module, widget.klass)

            if not preview_class:
                misc.error_message( _('No preview class "%s" found.\nThe details are written to the log file.\n'
                                      'If you think this is a wxGlade bug, please report it.') % widget.klass )
                return None

            if issubclass(preview_class, wx.MDIChildFrame):
                frame = wx.MDIParentFrame(None, -1, '')
                frame.SetMenuBar( wx.MenuBar() )  # avoid assertion error
                child = preview_class(frame, -1, '')
                child.SetTitle('<Preview> - ' + child.GetTitle())
                w, h = child.GetSize()
                frame.SetClientSize((w + 20, h + 20))
            elif not issubclass(preview_class, (wx.Frame,wx.Dialog)):
                # the toplevel class isn't really toplevel, add a frame...
                frame = wx.Frame(None, -1, widget_class_name)
                if issubclass(preview_class, wx.MenuBar):
                    menubar = preview_class()
                    frame.SetMenuBar(menubar)
                    panel = wx.Panel(frame)
                elif issubclass(preview_class, wx.ToolBar):
                    toolbar = preview_class(frame, -1)
                    frame.SetToolBar(toolbar)
                    panel = wx.Panel(frame)
                    frame.SetMinSize( toolbar.GetBestSize() )
                    frame.Fit()
                else:
                    panel = preview_class(frame, -1)
                    frame.Fit()
            else:
                frame = preview_class(None, -1, '')

            def on_close(event):
                frame.Unbind(wx.EVT_CHAR_HOOK)
                compat.DestroyLater(frame)
                widget.preview_widget = None
                widget.properties["preview"].set_label(_('Show Preview'))

            frame.Bind(wx.EVT_CLOSE, on_close)
            frame.SetTitle(_('<Preview> - %s') % frame.GetTitle())
            # raise the frame
            if position:
                frame.SetPosition(position)
            else:
                frame.CenterOnScreen()
                if widget.widget:
                    # avoid Design and Preview window at the same position
                    pos = widget.widget.GetPosition()
                    if frame.GetPosition()==pos:
                        frame.SetPosition( (pos[0]+200, pos[1]+100))
            frame.Show()
            # install handler for key down events
            frame.Bind(wx.EVT_CHAR_HOOK, self.on_char_hook)

            # remove the temporary file
            if not config.debugging:
                name = os.path.join(preview_path, preview_module_name+".py")
                if os.path.isfile(name): os.unlink(name)
        except Exception as inst:
            if config.debugging or config.testing: raise
            widget.preview_widget = None
            widget.properties["preview"].set_label(_('Show Preview'))
            bugdialog.Show(_("Generate Preview"), inst)
        # XXX restore app state
        widget.properties["class"].set(widget_class_name)
        return frame
    
    def on_char_hook(self, event):
        # handler for EVT_CHAR_HOOK events on preview windows
        if event.GetKeyCode()==wx.WXK_ESCAPE:
            wx.FindWindowById(event.GetId()).GetTopLevelParent().Close()
            return
        misc.handle_key_event(event, "preview")

    def update_view(self, selected=False):
        pass

    def check_codegen(self, widget=None, language=None):
        """Checks whether widget has a suitable code generator for the given language
        (default: the current active language). If not, the user is informed with a message."""
        if language is None:
            language = self.language
        if widget is not None:
            if widget.__class__.__name__=="SizerSlot":
                return
            cname = common.class_names[widget.__class__.__name__]
            if language != 'XRC':
                ok = cname in common.code_writers[language].obj_builders
            else:
                # xrc is special...
                xrcgen = common.code_writers['XRC']
                ok = xrcgen.obj_builders.get(cname, None) is not xrcgen.NotImplementedXrcObject
            if not ok:
                self._logger.warn( _('No %s code generator for %s (of type %s) available'),
                                   misc.capitalize(language), widget.name, cname )
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

    def _update_output_path(self, language):
        "Update wildcards and default extension in the generic file and directory dialog (L{FileDirDialog})."

        prop = self.properties["output_path"]
        prop.message = _("Select output file")  if self.multiple_files else  _("Select output directory")

        ext = getattr(common.code_writers[language], 'default_extensions', [])
        wildcards = []
        for e in ext:
            wildcards.append( _('%s files (*.%s)|*.%s') % (misc.capitalize(language), e, e) )
        wildcards.append(_('All files|*'))
        prop.wildcards = '|'.join(wildcards)
        prop.default_extension = '.%s' % ext[0]  if ext else  None

    def popup_menu(self, event, pos=None):
        # right click event -> expand all or show context menu
        expanded = True
        for child_node in common.app_tree.root.children or []:
            if not common.app_tree.IsExpanded(child_node.item) and child_node.children:
                expanded = False
                break
        if not expanded:
            #  -> expand all
            common.app_tree.ExpandAll()
            return
        # already expanded -> show context menu
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        if pos is None:
            # convert relative event position to relative widget position
            event_pos  = event.GetPosition()
            screen_pos = event_widget.ClientToScreen(event_pos)
            pos        = event_widget.ScreenToClient(screen_pos)
        event_widget.PopupMenu(menu, pos=pos)
        menu.Destroy()

    def _create_popup_menu(self, widget):
        menu = misc.wxGladePopupMenu("Application")
        i = misc.append_menu_item( menu, -1, _('Generate Code') )
        misc.bind_menu_item_after(widget, i, self.generate_code)  # a property, but it can be called
        return menu

    def check_drop_compatibility(self):
        return (False, "Only toplevel widgets can be added here; click Frame or Dialog icon to do so")

    def check_compatibility(self, widget, typename=None):
        if getattr(widget, "_is_toplevel", False) or typename=="window":
            return (True, None)
        return (False, "Only toplevel widgets can be pasted here (e.g. Frame or Dialog)")

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        import clipboard
        return clipboard._paste(None, None, 0, clipboard_data)
