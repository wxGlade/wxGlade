"""\
Main wxGlade module: defines wxGladeFrame which contains the buttons to add
widgets and initializes all the stuff (tree, frame_property, etc.)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2011-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import logging, os, os.path, sys, math, time
import wx
from xml.sax import SAXParseException

# import project modules
import application
import common, config, compat, misc, clipboard
import preferencesdialog, msgdialog, bugdialog, about
import log
import template
from tree import WidgetTree
from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder, XmlParsingError



class wxGladePropertyPanel(wx.Frame):
    "Panel used to display the Properties of the various widgets"
    def __init__(self, parent, frame_style):
        wx.Frame.__init__( self, parent, -1, _('Properties - <%s>' % _('app')), style=frame_style, name='PropertyFrame' )

        self.current_widget = None        # instance currently being edited
        self.next_widget = None           # the next one, will only be edited after a small delay

        self.notebook = wx.Notebook(self, -1)
        self.Bind(wx.EVT_CLOSE, self.hide_frame)

    def hide_frame(self, event):
        #menu_bar.Check(PROPS_ID, False)
        self.Hide()

    def is_visible(self):
        return self.IsShown()

    ####################################################################################################################
    # new editor interface
    def set_widget(self, widget):
        if widget is self.current_widget:
            # just update
            return
        self.next_widget = widget
        if self.current_widget:
            for editor in self.current_widget.properties.values():
                editor.destroy_editor()
            self.current_widget = None   # delete the reference
        wx.CallLater( 150, self.edit_properties, widget )

    def edit_properties(self, edit_widget):
        # this will be called with a delay
        if edit_widget is not self.next_widget:
            # wait for another call...
            return

        self.current_widget = None
        self.create_editor(edit_widget)
        self.current_widget = edit_widget
        self.SetTitle(_('Properties - %s - <%s>') % (edit_widget.klass, edit_widget.name) )

    def create_editor(self, edit_widget):
        # fill the frame with a notebook of property editors
        # not called yet
        self.current_widget_class = edit_widget.__class__

        self.notebook.Hide()

        # remember the notebook page to be selected
        selection = self.notebook.GetSelection()
        select_page = self.pagenames[selection]  if selection!=-1  else None

        # clear notebook pages
        if self.notebook.PageCount:
            #self.notebook.DeleteAllPages()  # deletes also the windows on the pages
            while self.notebook.PageCount:
                self.notebook.DeletePage(self.notebook.PageCount-1)

        self.editors = editors = {}

        self.pagenames = pagenames = []
        self.sizers = []
        current_page = current_sizer = None
        property_instance = None
        for prop in edit_widget.PROPERTIES:
            if prop[0].isupper():
                # end previous page
                if current_page is not None:
                    if property_instance and not property_instance.GROW:
                        current_sizer.AddStretchSpacer(prop=5)
                    self.end_page(current_page, current_sizer, current_pagename)
                    current_page = None

                # start new page
                current_pagename = prop
                if prop=="Layout" and not edit_widget._has_layout:continue

                current_page = self.start_page(prop)
                current_sizer = wx.BoxSizer(wx.VERTICAL)
                self.sizers.append(current_sizer)

                self.pagenames.append(prop)

                continue

            if current_pagename=="Layout" and not edit_widget._has_layout: continue

            # a property or None
            property_instance = edit_widget.properties.get(prop)
            if property_instance is not None:
                property_instance.create_editor(current_page, current_sizer)

        self.end_page(current_page, current_sizer, current_pagename)

        if select_page and select_page in pagenames:
            index = pagenames.index(select_page)
            self.notebook.SetSelection(index)
        else:
            self.notebook.SetSelection(0)

        self.notebook.Show()

    def start_page(self, name):
        panel = wx.ScrolledWindow( self.notebook, wx.ID_ANY, style=wx.TAB_TRAVERSAL | wx.FULL_REPAINT_ON_RESIZE,
                                   name=name )
        return panel
    def end_page(self, panel, sizer, header, select=False):
        panel.SetAutoLayout(1)
        #compat.SizerItem_SetSizer(panel, sizer)
        panel.SetSizer(sizer)
        sizer.Layout()
        sizer.Fit(panel)

        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, _(header),select=select)
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))



class wxGladeArtProvider(wx.ArtProvider):
    def CreateBitmap(self, artid, client, size):
        if wx.Platform == '__WXGTK__' and artid == wx.ART_FOLDER:
            return wx.Bitmap(os.path.join(config.icons_path, 'closed_folder.xpm'), wx.BITMAP_TYPE_XPM)
        return wx.NullBitmap



class wxGladeFrame(wx.Frame):
    "Main frame of wxGlade (palette)"

    def __init__(self, parent=None):
        self._logger = logging.getLogger(self.__class__.__name__)
        style = wx.SYSTEM_MENU | wx.CAPTION | wx.MINIMIZE_BOX
        style |= wx.RESIZE_BORDER | wx.CLOSE_BOX
        wx.Frame.__init__(self, parent, -1, "wxGlade v%s" % config.version, style=style, name='MainFrame')

        if parent is None:
            parent = self
        common.palette = self  # to provide a reference accessible by the various widget classes
        icon = wx.EmptyIcon()
        bmp = wx.Bitmap( os.path.join(config.icons_path, "icon.xpm"), wx.BITMAP_TYPE_XPM )
        icon.CopyFromBitmap(bmp)
        self.SetIcon(icon)
        self.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE) )

        self.create_menu(parent)

        # load the available code generators
        all_widgets = common.init_codegen()
        # build the palette for all_widgets
        sizer = wx.FlexGridSizer(0, 2, 0, 0)
        sizer.AddGrowableCol(0)
        self.SetAutoLayout(True)
        maxlen = max([len(all_widgets[sect]) for sect in all_widgets])  # the maximum number of buttons in a section
        for section in all_widgets:
            if section:
                sizer.Add(wx.StaticText(self, -1, "%s:" % section.replace('&', '&&')), 1, wx.ALIGN_CENTER_VERTICAL)

            bsizer = wx.GridSizer(cols=maxlen, hgap=2, vgap=2)
            for button in all_widgets[section]:
                bsizer.Add(button, flag=wx.ALL, border=1)
            sizer.Add(bsizer)
        self.SetSizer(sizer)
        sizer.Fit(self)

        wx.EVT_CLOSE(self, self.cleanup)

        # the style for the property and tree frames
        frame_style = wx.DEFAULT_FRAME_STYLE
        frame_tool_win = config.preferences.frame_tool_win
        if frame_tool_win:
            frame_style |= wx.FRAME_NO_TASKBAR | wx.FRAME_FLOAT_ON_PARENT
            frame_style &= ~wx.MINIMIZE_BOX
            if wx.Platform != '__WXGTK__':
                frame_style |= wx.FRAME_TOOL_WINDOW

        # set window geometry
        main_geometry = None
        if config.preferences.remember_geometry:
            main_geometry = config.preferences.get_geometry('main')
            if isinstance(main_geometry, tuple):
                main_geometry = wx.Rect(*main_geometry)
        if not main_geometry:
            main_geometry = wx.Rect()
            main_geometry.TopLeft = wx.Display().GetClientArea().GetTopLeft()
            main_geometry.Size = (-1, -1)
        self._set_geometry(self, main_geometry)
        self.Show()
        main_geometry.Size = self.GetSize()

        # create the property and the tree frame
        self.create_property_panel(frame_style, icon, main_geometry)
        self.create_tree_frame(frame_style, icon, main_geometry)
        common.property_panel = self.property_frame

        # last visited directory, used on GTK for wxFileDialog
        self.cur_dir = config.preferences.open_save_path

        # set a drop target for us...
        self._droptarget = clipboard.FileDropTarget(self)
        self.SetDropTarget(self._droptarget)
        #self.tree_frame.SetDropTarget(self._droptarget)
        #self.property_frame.SetDropTarget(self._droptarget)

        # ALB 2004-10-15, autosave support...
        self.autosave_timer = None
        if config.preferences.autosave:
            self.autosave_timer = wx.Timer(self, -1)
            self.Bind(wx.EVT_TIMER, self.on_autosave_timer, self.autosave_timer)
            self.autosave_timer.Start( int(config.preferences.autosave_delay) * 1000 )

        self.create_statusbar()  # create statusbar for display of messages

        self.property_frame.SetAcceleratorTable(self.accel_table)
        self.tree_frame.SetAcceleratorTable(self.accel_table)

        self.Raise()

        # disable autosave checks during unittests
        if not getattr(sys, '_called_from_test', False):
            if common.check_autosaved(None):
                res = wx.MessageBox(
                    _('There seems to be auto saved data from last wxGlade session: do you want to restore it?'),
                    _('Auto save detected'), style=wx.ICON_QUESTION | wx.YES_NO)
                if res == wx.YES:
                    filename = common.get_name_for_autosave()
                    if self._open_app(filename, add_to_history=False):
                        self.cur_dir = os.path.dirname(filename)
                        common.app_tree.app.saved = False
                        common.app_tree.app.filename = None
                        self.user_message(_('Auto save loaded'))
                common.remove_autosaved()

    # menu and actions #################################################################################################
    def create_menu(self, parent):
        menu_bar = wx.MenuBar()
        file_menu = wx.Menu(style=wx.MENU_TEAROFF)
        view_menu = wx.Menu(style=wx.MENU_TEAROFF)
        help_menu = wx.Menu(style=wx.MENU_TEAROFF)
        compat.wx_ToolTip_SetDelay(1000)
        compat.wx_ToolTip_SetAutoPop(30000)

        append_menu_item = misc.append_menu_item
        self.TREE_ID = TREE_ID = wx.NewId()
        append_menu_item(view_menu, TREE_ID, _("Show &Tree\tF2"))
        self.PROPS_ID = PROPS_ID = wx.NewId()
        self.RAISE_ID = RAISE_ID = wx.NewId()
        append_menu_item(view_menu, PROPS_ID, _("Show &Properties\tF3"))
        append_menu_item(view_menu, RAISE_ID, _("&Raise All\tF4"))
        NEW_ID = wx.NewId()
        append_menu_item(file_menu, NEW_ID, _("&New\tCtrl+N"), wx.ART_NEW)
        NEW_FROM_TEMPLATE_ID = wx.NewId()
        append_menu_item(file_menu, NEW_FROM_TEMPLATE_ID, _("New from &Template...\tShift+Ctrl+N"))
        OPEN_ID = wx.NewId()
        append_menu_item(file_menu, OPEN_ID, _("&Open...\tCtrl+O"), wx.ART_FILE_OPEN)
        SAVE_ID = wx.NewId()
        append_menu_item(file_menu, SAVE_ID, _("&Save\tCtrl+S"), wx.ART_FILE_SAVE)
        SAVE_AS_ID = wx.NewId()
        append_menu_item(file_menu, SAVE_AS_ID, _("Save As...\tShift+Ctrl+S"), wx.ART_FILE_SAVE_AS)
        SAVE_TEMPLATE_ID = wx.NewId()
        append_menu_item(file_menu, SAVE_TEMPLATE_ID, _("Save As Template..."))
        file_menu.AppendSeparator()

        append_menu_item(file_menu, wx.ID_REFRESH, _("&Refresh Preview\tF5"))

        GENERATE_CODE_ID = wx.NewId()
        append_menu_item(file_menu, GENERATE_CODE_ID, _("&Generate Code\tCtrl+G"), wx.ART_EXECUTABLE_FILE)

        file_menu.AppendSeparator()
        IMPORT_ID = wx.NewId()
        append_menu_item(file_menu, IMPORT_ID, _("&Import from XRC..."))

        EXIT_ID = wx.NewId()
        file_menu.AppendSeparator()
        append_menu_item(file_menu, EXIT_ID, _('E&xit\tCtrl+Q'), wx.ART_QUIT)
        PREFS_ID = wx.ID_PREFERENCES
        view_menu.AppendSeparator()
        MANAGE_TEMPLATES_ID = wx.NewId()
        append_menu_item(view_menu, MANAGE_TEMPLATES_ID, _('Template Manager...'))
        view_menu.AppendSeparator()
        append_menu_item(view_menu, PREFS_ID, _('Preferences...'))
        menu_bar.Append(file_menu, _("&File"))
        menu_bar.Append(view_menu, _("&View"))

        MANUAL_ID = wx.NewId()
        append_menu_item(help_menu, MANUAL_ID, _('Manual\tF1'), wx.ART_HELP)
        TUTORIAL_ID = wx.NewId()
        append_menu_item(help_menu, TUTORIAL_ID, _('Tutorial'))
        help_menu.AppendSeparator()
        ABOUT_ID = wx.ID_ABOUT
        append_menu_item(help_menu, ABOUT_ID, _('About'))
        menu_bar.Append(help_menu, _('&Help'))

        parent.SetMenuBar(menu_bar)
        # Mac tweaks...
        if wx.Platform == "__WXMAC__":
            wx.App_SetMacAboutMenuItemId(ABOUT_ID)
            wx.App_SetMacPreferencesMenuItemId(PREFS_ID)
            wx.App_SetMacExitMenuItemId(EXIT_ID)
            wx.App_SetMacHelpMenuTitleName(_('&Help'))

        # file history support
        self.file_history = wx.FileHistory(config.preferences.number_history)
        self.file_history.UseMenu(file_menu)
        files = common.load_history()
        files.reverse()
        for path in files:
            self.file_history.AddFileToHistory(path.strip())

        wx.EVT_MENU_RANGE(self, wx.ID_FILE1, wx.ID_FILE9, self.open_from_history)

        wx.EVT_MENU(self, TREE_ID, self.show_tree)
        wx.EVT_MENU(self, PROPS_ID, self.show_props_window)
        wx.EVT_MENU(self, RAISE_ID, self.raise_all)
        wx.EVT_MENU(self, NEW_ID, self.new_app)
        wx.EVT_MENU(self, NEW_FROM_TEMPLATE_ID, self.new_app_from_template)
        wx.EVT_MENU(self, OPEN_ID, self.open_app)
        wx.EVT_MENU(self, SAVE_ID, self.save_app)
        wx.EVT_MENU(self, SAVE_AS_ID, self.save_app_as)
        wx.EVT_MENU(self, SAVE_TEMPLATE_ID, self.save_app_as_template)

        def generate_code(event):
            common.app_tree.app.generate_code()
        wx.EVT_MENU(self, GENERATE_CODE_ID, generate_code)
        wx.EVT_MENU(self, EXIT_ID, lambda e: self.Close())
        wx.EVT_MENU(self, MANUAL_ID, self.show_manual)
        wx.EVT_MENU(self, TUTORIAL_ID, self.show_tutorial)
        wx.EVT_MENU(self, ABOUT_ID, self.show_about_box)
        wx.EVT_MENU(self, PREFS_ID, self.edit_preferences)
        wx.EVT_MENU(self, MANAGE_TEMPLATES_ID, self.manage_templates)
        wx.EVT_MENU(self, IMPORT_ID, self.import_xrc)
        wx.EVT_MENU(self, wx.ID_REFRESH, self.preview) # self.reload_app)

        PREVIEW_ID = wx.NewId()
        wx.EVT_MENU(self, PREVIEW_ID, self.preview)

        self.accel_table = wx.AcceleratorTable([
            (wx.ACCEL_CTRL, ord('N'), NEW_ID),
            (wx.ACCEL_CTRL, ord('O'), OPEN_ID),
            (wx.ACCEL_CTRL, ord('S'), SAVE_ID),
            (wx.ACCEL_CTRL|wx.ACCEL_SHIFT, ord('S'), SAVE_AS_ID),
            (wx.ACCEL_CTRL, ord('G'), GENERATE_CODE_ID),
            #(wx.ACCEL_CTRL, ord('I'), IMPORT_ID),
            (0, wx.WXK_F1, MANUAL_ID),
            (wx.ACCEL_CTRL, ord('Q'), EXIT_ID),
            (0, wx.WXK_F5, wx.ID_REFRESH),
            (0, wx.WXK_F2, TREE_ID),
            (0, wx.WXK_F3, PROPS_ID),
            (0, wx.WXK_F4, RAISE_ID),
            (wx.ACCEL_CTRL, ord('P'), PREVIEW_ID),
            ])

    def open_from_history(self, event):
        if not self.ask_save():
            return
        pos = event.GetId() - wx.ID_FILE1
        filename = self.file_history.GetHistoryFile(pos)
        if not os.path.exists(filename):
            wx.MessageBox( _("The file %s doesn't exist.") % filename,
                           _('Information'), style=wx.CENTER | wx.ICON_INFORMATION | wx.OK )
            self.file_history.RemoveFileFromHistory(pos)
            common.remove_autosaved(filename)
            return
        if common.check_autosaved(filename):
            res = wx.MessageBox( _('There seems to be auto saved data for this file: do you want to restore it?'),
                                 _('Auto save detected'), style=wx.ICON_QUESTION | wx.YES_NO )
            if res == wx.YES:
                common.restore_from_autosaved(filename)
            else:
                common.remove_autosaved(filename)
        else:
            common.remove_autosaved(filename)
        self._open_app(filename)
        self.cur_dir = os.path.dirname(filename)

    # GUI elements: property frame, tree frame #########################################################################
    def create_property_panel(self, frame_style, icon, main_geometry):
        # create property editor frame
        self.property_frame = wxGladePropertyPanel(self, frame_style)
        self.property_frame.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE) )
        self.property_frame.SetIcon(icon)

        # set geometry
        property_geometry = None
        if config.preferences.remember_geometry:
            property_geometry = config.preferences.get_geometry('properties')
            if isinstance(property_geometry, tuple):
                property_geometry = wx.Rect(*property_geometry)
        if not property_geometry:
            property_geometry = wx.Rect()
            property_geometry.Position = main_geometry.BottomLeft
            property_geometry.Size = (345, 350)
            # sometimes especially on GTK GetSize seems to ignore window decorations (bug still exists on wx3)
            if wx.Platform != '__WXMSW__': property_geometry.Y += 40
            # set size on Mac manually
            if wx.Platform == '__WXMAC__': property_geometry.Size = (345, 384)

        self._set_geometry(self.property_frame, property_geometry)
        self.property_frame.Show()

    def create_tree_frame(self, frame_style, icon, main_geometry):
        self.tree_frame = wx.Frame(self, -1, _('wxGlade: Tree'), style=frame_style, name='TreeFrame')
        self.tree_frame.SetIcon(icon)

        app = application.Application()
        common.app_tree = WidgetTree(self.tree_frame, app)
        self.tree_frame.SetSize((300, 300))

        def on_tree_frame_close(event):
            #menu_bar.Check(TREE_ID, False)
            self.tree_frame.Hide()
        wx.EVT_CLOSE(self.tree_frame, on_tree_frame_close)

        # set geometry
        tree_geometry = None
        if config.preferences.remember_geometry:
            tree_geometry = config.preferences.get_geometry('tree')
            if isinstance(tree_geometry, tuple):
                tree_geometry = wx.Rect(*tree_geometry)
        if not tree_geometry:
            tree_geometry = wx.Rect()
            tree_geometry.Position = main_geometry.TopRight
            tree_geometry.Size = (250, 350)
            # sometimes especially on GTK GetSize seems to ignore window decorations (bug still exists on wx3)
            if wx.Platform != '__WXMSW__':
                tree_geometry.X += 10
        self._set_geometry(self.tree_frame, tree_geometry)
        self.tree_frame.Show()

    def on_autosave_timer(self, event):
        res = common.autosave_current()
        if res == 2:
            self.user_message(_("Auto saving... done"))
        elif not res:
            self.autosave_timer.Stop()
            config.preferences.autosave = False
            self._logger.info(_('Disable autosave function permanently'))
            wx.MessageBox(
                _('The autosave function failed. It has been disabled\n'
                  'permanently due to this error. Use the preferences\n'
                  'dialog to re-enable this functionality.\n'
                  'The details have been written to the wxGlade log file\n'
                  '\n'
                  'The log file is: %s' % config.log_file
                  ),
                _('Autosave Failed'), wx.OK | wx.CENTRE | wx.ICON_ERROR )

    def edit_preferences(self, event):
        dialog = preferencesdialog.wxGladePreferences(config.preferences)
        if dialog.ShowModal() == wx.ID_OK:
            wx.MessageBox( _('Changes will take effect after wxGlade is restarted'),
                           _('Preferences saved'), wx.OK|wx.CENTRE|wx.ICON_INFORMATION )
            dialog.set_preferences()
        dialog.Destroy()

    def preview(self, event):
        """Generate preview of the current loaded project.
        
        A preview can be triggered by keyboard shortcut or by pressing the preview button.
        The preview can be triggered for all selected widgets.
        This doesn't mean that the widget is opened for editing."""

        if not common.app_tree.cur_widget or isinstance(common.app_tree.cur_widget, application.Application):
            preview_widget = common.app_tree.root.children[0].widget
        else:
            preview_widget = misc.get_toplevel_widget(common.app_tree.cur_widget)

        if preview_widget is None: return

        if preview_widget.preview_widget:
            # preview is already active: close and re-generate
            preview_widget.preview_widget.Close()
            wx.CallAfter(preview_widget.preview, None)
        else:
            preview_widget.preview(None)

    def show_tree(self, event):
        self.tree_frame.Show()
        self.tree_frame.Raise()
        common.app_tree.SetFocus()

    def show_props_window(self, event):
        self.property_frame.Show()
        self.property_frame.Raise()
        try:
            c = self.property_frame.GetSizer().GetChildren()
            if c: c[0].GetWindow().SetFocus()
        except (AttributeError, TypeError):
            self.property_frame.SetFocus()

    def raise_all(self, event):
        # when one window is raised, raise all
        children = self.GetChildren()
        for child in children:
            child = misc.get_toplevel_parent(child)
            if child.IsShown() and child.GetTitle(): child.Raise()
        self.Raise()

    # status bar for message display ###################################################################################
    def create_statusbar(self):
        self.CreateStatusBar(1)
        # ALB 2004-10-15  statusbar timer: delete user message after some time
        self.clear_sb_timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.on_clear_sb_timer, self.clear_sb_timer)

    def user_message(self, msg):
        # display a message, but clear it after a few seconds again
        sb = self.GetStatusBar()
        if sb:
            sb.SetStatusText(msg)
            self.clear_sb_timer.Start(5000, True)

    def on_clear_sb_timer(self, event):
        sb = self.GetStatusBar()
        if sb: sb.SetStatusText("")
    ####################################################################################################################

    def ask_save(self):
        """checks whether the current app has changed and needs to be saved:
        if so, prompts the user;
        returns False if the operation has been cancelled"""
        if not common.app_tree.app.saved:
            ok = wx.MessageBox(_("Save changes to the current app?"),
                               _("Confirm"), wx.YES_NO|wx.CANCEL|wx.CENTRE|wx.ICON_QUESTION)
            if ok == wx.YES:
                self.save_app(None)
            return ok != wx.CANCEL
        return True

    def new_app(self, event):
        "creates a new wxGlade project"
        if self.ask_save():
            common.app_tree.clear()
            common.app_tree.app.filename = None
            common.app_tree.app.saved = True
            self.user_message("")
            common.remove_autosaved()
            if config.preferences.autosave and self.autosave_timer is not None:
                self.autosave_timer.Start()

    def new_app_from_template(self, event):
        "creates a new wxGlade project from an existing template file"
        if not self.ask_save(): return
        infile = template.select_template()
        if infile:
            self._open_app(infile, add_to_history=False)
            common.app_tree.app.template_data = None

    def open_app(self, event_unused):
        """loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :) """
        if not self.ask_save():
            return
        default_path = os.path.dirname(common.app_tree.app.filename or "") or self.cur_dir
        infile = wx.FileSelector(_("Open file"),
                                   wildcard="wxGlade files (*.wxg)|*.wxg|"
                                   "wxGlade Template files (*.wgt)|*.wgt|"
                                   "XML files (*.xml)|*.xml|All files|*",
                                   flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
                                   default_path=default_path)
        if not infile: return
        if common.check_autosaved(infile):
            if wx.MessageBox( _("There seems to be auto saved data for this file: do you want to restore it?"),
                              _("Auto save detected"), style=wx.ICON_QUESTION|wx.YES_NO ) == wx.YES:
                common.restore_from_autosaved(infile)
            else:
                common.remove_autosaved(infile)
        if infile == common.app_tree.app.filename:
            # if we are re-loading the file, go the the previous position
            path = common.app_tree.get_selected_path()
        else:
            path = None
        self._open_app(infile)
        self.cur_dir = os.path.dirname(infile)

        if path is not None:
            common.app_tree.select_path(path)  # re-loaded file -> go to previous position

    def _open_app(self, filename_or_filelike, use_progress_dialog=True, add_to_history=True):
        """\
        Load a new wxGlade project

        @param filename_or_filelike: Source filename or file-like object
        @type filename_or_filelike: file | StringIO

        @param use_progress_dialog: Show progress bar during loading WXG file
        @type use_progress_dialog: bool

        @param add_to_history: Add file to open to file history
        @type add_to_history: bool

        @return: True on Success
        @rtype: bool
        """
        if isinstance(filename_or_filelike, compat.StringIO):
            assert isinstance(filename_or_filelike.getvalue(), unicode)
        else:
            assert isinstance(filename_or_filelike, compat.basestring )

        error_msg = None
        filename = None
        infile = None
        old_dir = os.getcwd()

        if isinstance(filename_or_filelike, compat.basestring):
            common.app_tree.app.filename = filename_or_filelike
            filename = filename_or_filelike
        else:
            common.app_tree.filename = None

        start = time.clock()
        common.app_tree.clear()

        # disable auto-expansion of nodes
        common.app_tree.auto_expand = False

        try:
            try:
                if isinstance(filename_or_filelike, compat.StringIO):
                    # convert filename_or_filelike to UTF-8 and write back as lines, because
                    # ProgressXmlWidgetBuilder uses lines to calculate and show the position
                    tmp = filename_or_filelike.getvalue()
                    tmp = tmp.encode('UTF-8')
                    infile = compat.StringIO()
                    for line in tmp.split('\n'):
                        infile.write('%s\n' % line)
                    infile.seek(0)
                    self._logger.info( _('Read wxGlade project from file-like object') )

                else:
                    self._logger.info( _('Read wxGlade project from file "%s"'), filename )
                    os.chdir(os.path.dirname(filename))
                    # decoding will done automatically by SAX XML library
                    if compat.PYTHON2:
                        infile = open(filename)
                    else:
                        infile = open(filename, "r", encoding="UTF8")

                if use_progress_dialog and config.preferences.show_progress:
                    p = ProgressXmlWidgetBuilder(input_file=infile)
                else:
                    p = XmlWidgetBuilder()

                p.parse(infile)
            except (EnvironmentError, SAXParseException, XmlParsingError) as msg:
                if 'WINGDB_ACTIVE' in os.environ: raise
                if filename:
                    error_msg = _("Error loading file %s: %s") % (misc.wxstr(filename), misc.wxstr(msg))
                else:
                    error_msg = _("Error loading from a file-like object: %s") % misc.wxstr(msg)
            except Exception as inst:
                if 'WINGDB_ACTIVE' in os.environ: raise
                if filename:
                    fn = os.path.basename(filename).encode('ascii','replace')
                    msg = _('loading file "%s"') % fn
                else:
                    msg = _('loading from a file-like object')
                bugdialog.Show(msg, inst)
        finally:
            if infile and filename:
                infile.close()

            if error_msg:
                common.app_tree.clear()
                common.app_tree.app.saved = True
                common.app_tree.auto_expand = True  # re-enable auto-expansion of nodes

                os.chdir(old_dir)

                wx.MessageBox(error_msg, _('Error'), wx.OK | wx.CENTRE | wx.ICON_ERROR)

                return False

        misc.set_focused_widget(common.app_tree.root.widget)

        common.app_tree.auto_expand = True  # re-enable auto-expansion of nodes

        common.app_tree.expand()
        if common.app_tree.app.is_template:
            self._logger.info(_("Template loaded"))
            common.app_tree.app.template_data = template.Template(filename)
            common.app_tree.app.filename = None

        end = time.clock()
        self._logger.info(_('Loading time: %.5f'), end - start)

        common.app_tree.app.saved = True
        common.property_panel.Raise()

        if hasattr(self, 'file_history') and filename is not None and add_to_history and \
           (not common.app_tree.app.is_template):
            self.file_history.AddFileToHistory(misc.wxstr(filename))

        if config.preferences.autosave and self.autosave_timer is not None:
            self.autosave_timer.Start()

        duration = end - start
        if filename:
            self.user_message( _("Loaded %s in %.2f seconds") % (misc.wxstr(os.path.basename(filename)), duration) )
        else:
            self.user_message( _("Loaded in %.2f seconds") % duration )

        return True

    def save_app(self, event):
        "saves a wxGlade project onto an xml file"
        if not common.app_tree.app.filename or common.app_tree.app.is_template:
            self.save_app_as(event)
        else:
            # check whether we are saving a template
            ext = os.path.splitext(common.app_tree.app.filename)[1].lower()
            if ext == ".wgt":
                common.app_tree.app.is_template = True
            self._save_app(common.app_tree.app.filename)

    def _save_app(self, filename):
        try:
            obuffer = compat.StringIO()
            common.app_tree.write(obuffer)
            common.save_file(filename, obuffer.getvalue(), 'wxg')
        except EnvironmentError as inst:
            common.app_tree.app.saved = False
            bugdialog.ShowEnvironmentError(_('Saving this project failed'), inst)
        except Exception as inst:
            common.app_tree.app.saved = False
            fn = os.path.basename(filename).encode('ascii', 'replace')
            bugdialog.Show(_('Save File "%s"') % fn, inst)
        else:
            common.app_tree.app.saved = True
            common.remove_autosaved()
            if config.preferences.autosave and self.autosave_timer is not None:
                self.autosave_timer.Start()
            self.user_message( _("Saved %s") % os.path.basename(filename) )

    def save_app_as(self, event):
        "saves a wxGlade project onto an xml file chosen by the user"
        # both flags occurs several times
        fn = wx.FileSelector( _("Save project as..."),
                              wildcard="wxGlade files (*.wxg)|*.wxg|wxGlade Template files (*.wgt) |*.wgt|"
                              "XML files (*.xml)|*.xml|All files|*",
                              flags=wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT,
                              default_filename=common.app_tree.app.filename or self.cur_dir)
        if not fn: return

        # check for file extension and add default extension if missing
        ext = os.path.splitext(fn)[1].lower()
        if not ext:
            fn = "%s.wxg" % fn

        common.app_tree.app.filename = fn
        #remove the template flag so we can save the file.
        common.app_tree.app.properties["is_template"].set(False)

        self.save_app(event)
        self.cur_dir = os.path.dirname(fn)
        self.file_history.AddFileToHistory(fn)

    def save_app_as_template(self, event):
        "save a wxGlade project as a template"
        data = getattr(common.app_tree.app, 'template_data', None)
        outfile, data = template.save_template(data)
        if outfile:
            common.app_tree.app.properties["is_template"].set(True)
            common.app_tree.app.template_data = data
            self._save_app(outfile)

    def cleanup(self, event):
        if self.ask_save():
            # first, let's see if we have to save the geometry...
            prefs = config.preferences
            if prefs.remember_geometry:
                prefs.set_geometry('main', self._get_geometry(self))
                prefs.set_geometry('tree', self._get_geometry(self.tree_frame))
                prefs.set_geometry('properties', self._get_geometry(self.property_frame))
                prefs.changed = True
            common.app_tree.clear()
            try:
                common.save_preferences()
            except Exception as e:
                wx.MessageBox( _('Error saving preferences:\n%s') % e,
                               _('Error'), wx.OK|wx.CENTRE|wx.ICON_ERROR )
            #self._skip_activate = True
            self.property_frame.Destroy()
            self.property_frame = None
            self.tree_frame.Destroy()
            self.tree_frame = None
            self.Destroy()
            common.remove_autosaved()  # ALB 2004-10-15
            wx.CallAfter(wx.GetApp().ExitMainLoop)

    def show_about_box(self, event):
        "show the about dialog;  @see: L{about.wxGladeAboutBox}"
        about_box = about.wxGladeAboutBox()
        about_box.ShowModal()
        about_box.Destroy()

    def show_manual(self, event):
        "Show the wxGlade user manual"
        self._show_html(config.manual_file)

    def show_tutorial(self, event):
        "Show the wxGlade tutorial"
        self._show_html(config.tutorial_file)

    def _show_html(self, html_file):
        "Open browser and show an HTML documentation"

        if wx.Platform == "__WXMAC__":
            os.system(r'open -a Help\ Viewer.app %s' % html_file)
        else:
            import webbrowser
            import threading
            # ALB 2004-08-15: why did this block the program????? (at least on linux - GTK)

            def go():
                webbrowser.open_new(html_file)
            t = threading.Thread(target=go)
            t.setDaemon(True)
            t.start()

    def show_and_raise(self):
        self.property_frame.Show()  # self.GetMenuBar().IsChecked(self.PROPS_ID))
        self.tree_frame.Show()      # self.GetMenuBar().IsChecked(self.TREE_ID))
        self.property_frame.Raise()
        self.tree_frame.Raise()
        self.Raise()

    def hide_all(self):
        self.tree_frame.Hide()
        self.property_frame.Hide()

    def import_xrc(self, event):
        import xrc2wxg

        if not self.ask_save():
            return

        infilename = wx.FileSelector( _("Import file"), wildcard="XRC files (*.xrc)" "|*.xrc|All files|*",
                                      flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST, default_path=self.cur_dir)
        if infilename:
            ibuffer = compat.StringIO()
            try:
                xrc2wxg.convert(infilename, ibuffer)

                # Convert UTF-8 returned by xrc2wxg.convert() to Unicode
                tmp = ibuffer.getvalue().decode('UTF-8')
                ibuffer = compat.StringIO()
                [ibuffer.write('%s\n' % line) for line in tmp.split('\n')]
                ibuffer.seek(0)

                self._open_app(ibuffer)
                common.app_tree.app.saved = False
            except Exception as inst:
                fn = os.path.basename(infilename).encode('ascii', 'replace')
                bugdialog.Show(_('Import File "%s"') % fn, inst)

    def manage_templates(self, event):
        to_edit = template.manage_templates()
        if to_edit is not None and self.ask_save():
            # edit the template
            # TODO, you still need to save it manually...
            self._open_app(to_edit, add_to_history=False)
            wx.MessageBox( _("To save the changes to the template, edit the GUI as usual,\n"
                             "and then click File->Save As Template..."),
                           _("Information"), style=wx.OK|wx.ICON_INFORMATION )

    def _set_geometry(self, win, geometry):
        "Set position and/or size of widget; geometry must be wx.Point or wx.Rect"
        assert isinstance(geometry, (wx.Point, wx.Rect))
        if not geometry: return

        if isinstance(geometry, wx.Point):
            win.SetPosition(geometry)
        else:
            win.SetDimensions(*geometry.Get())

    def _get_geometry(self, widget):
        "Return widget position and size as wx.Rect"
        pos_size = widget.Rect
        client_area = wx.Display().ClientArea
        if client_area.Contains(pos_size.TopLeft):
            return pos_size.Get()
        return None



class wxGlade(wx.App):
    """wxGlade application class
    
    @ivar _exception_orig: Reference to original implementation of logging.exception()"""

    def OnInit(self):
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

        if not 'WINGDB_ACTIVE' in os.environ:
            # replace text based exception handler by a graphical exception dialog
            sys.excepthook = self.graphical_exception_handler

        # use graphical implementation to show caught exceptions
        self._exception_orig = logging.exception
        logging.exception = self.exception

        # needed for wx >= 2.3.4 to disable wxPyAssertionError exceptions
        self.SetAssertMode(0)

        common.init_preferences()
        if config.preferences.log_debug_info:
            log.setDebugLevel()

            # enable Python faulthandler to dump a traceback on SIGSEGV, SIGFPE, SIGABRT, SIGBUS, and SIGILL signals.
            try:
                import faulthandler
                faulthandler.enable()
                logging.info(_('Python faulthandler found and activated'))
            except ImportError:
                logging.debug(_('Python faulthandler not found'))
            except RuntimeError as details:
                logging.info(_('Python faulthandler found, but enabling failed: %s'), details)
            except Exception as details:
                logging.info(_('Generic error during faulthandler initialisation: %s'), details)

        compat.wx_ArtProviderPush(wxGladeArtProvider())

        frame = wxGladeFrame()
        self.SetTopWindow(frame)
        self.SetExitOnFrameDelete(True)

        wx.EVT_IDLE(self, self.OnIdle)

        return True

    def OnExit(self):
        "Restore original exception handler and logging.exception() on exit"
        sys.excepthook = sys.__excepthook__
        logging.exception = self._exception_orig

    def OnIdle(self, event):
        "Idle tasks - currently show error messages only;  @see: L{show_msgdialog()}"
        self.show_msgdialog()
        event.Skip()

    def show_msgdialog(self):
        """\
        Check for log messages and show them

        @see: L{main.wxGlade.OnIdle()}
        @see: L{log.getBufferAsList()}
        @see: L{msgdialog.MessageDialog}
        """
        log_msg = log.getBufferAsString()
        if not log_msg:
            return

        # initialise message dialog
        msg_dialog = msgdialog.MessageDialog(None, -1, "")
        msg_dialog.msg_list.InsertColumn(0, "")

        # clear dialog and show new messages
        msg_dialog.msg_list.Freeze()
        msg_dialog.msg_list.DeleteAllItems()
        for line in log_msg.split('\n'):
            msg_dialog.msg_list.Append([line, ])
        msg_dialog.msg_list.SetColumnWidth(0, -1)
        msg_dialog.msg_list.Thaw()
        msg_dialog.ShowModal()
        msg_dialog.Destroy()

    def graphical_exception_handler(self, exc_type, exc_value, exc_tb):
        """\
        Show detailed information about uncaught exceptions in
        L{bugdialog.BugReport}.

        The shown exception will be logged to the log file in parallel.

        The exception information will be cleared after the bug dialog has
        closed.

        @param exc_type:  Type of the exception (normally a class object)
        @param exc_value: The "value" of the exception
        @param exc_tb:    Call stack of the exception

        @see: L{bugdialog.BugReport()}
        @see: L{bugdialog.Show()}
        """
        bugdialog.ShowEI(exc_type, exc_value, exc_tb)
        sys.exc_clear()

    def exception(self, msg, *args, **kwargs):
        """\
        Graphical replacement of C{logging.exception()}.

        All exception details logged with C{logging.exception()} will be shown
        in L{bugdialog.BugReport}.

        The shown exception will be logged to the log file ding.

        The exception information will be cleared after the bug dialog has
        closed.

        @param msg: Short description of the exception
        @type msg:  str

        @see: L{bugdialog.BugReport}
        @see: L{bugdialog.ShowEI()}
        """
        if args:
            try:
                msg = msg % args
            except TypeError:
                log.exception_orig(_('Wrong format of a log message'))

        (exc_type, exc_value, exc_tb) = sys.exc_info()
        bugdialog.ShowEI(exc_type, exc_value, exc_tb, msg)
        sys.exc_clear()



def main(filename=None):
    "if filename is not None, loads it"
    logging.info(_("Using wxPython %s"), config.wx_version)
    app = wxGlade()
    if filename is not None:
        win = app.GetTopWindow()
        win._open_app(filename, False)
        win.cur_dir = os.path.dirname(filename)
    app.MainLoop()
