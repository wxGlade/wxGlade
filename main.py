"""\
Main wxGlade module: defines wxGladeFrame which contains the buttons to add
widgets and initializes all the stuff (tree, frame_property, etc.)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2011-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import StringIO
import logging
import os
import os.path
import sys
import time
import types
import wx
from xml.sax import SAXParseException

# import project modules
import about
import application
import bugdialog
import clipboard
import common
import config
import preferencesdialog
import log
import misc
import msgdialog
import template
from tree import WidgetTree
from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder, \
    XmlParsingError


class wxGladePropertyPanel(wx.Panel):
    """\
    Panel used to display the Properties of the various widgets
    """

    def SetTitle(self, title):
        try: self.GetParent().SetTitle(title)
        except AttributeError: pass

    def Layout(self):
        if self.is_visible():
            wx.Panel.Layout(self)
            self.GetParent().Layout()

    def is_visible(self):
        return self.GetParent().IsShown()

# end of class wxGladePropertyPanel


TOGGLE_BOX_EVENT = wx.NewEventType()


def EVT_TOGGLE_BOX(win, id, func):
    win.Connect(id, -1, TOGGLE_BOX_EVENT, func)


class ToggleBoxEvent(wx.PyCommandEvent):
    def __init__(self, id, value, strval):
        wx.PyCommandEvent.__init__(self)
        self.SetId(id)
        self.SetEventType(TOGGLE_BOX_EVENT)
        self.value = value
        self.strval = strval

    def GetValue(self):
        return self.value

    def GetStringValue(self):
        return self.strval

# end of class ToggleBoxEvent


class ToggleButtonBox(wx.Panel):
    def __init__(self, parent, id, choices=None, value=0):
        wx.Panel.__init__(self, parent, id)
        if choices is None:
            choices = []
        self.buttons = [wx.ToggleButton(self, -1, c) for c in choices]
        self.selected = None
        self.SetValue(value)
        for b in self.buttons:
            def handler(event, b=b):
                self.on_toggle(b, event)
            wx.EVT_TOGGLEBUTTON(self, b.GetId(), handler)
        sizer = wx.BoxSizer(wx.VERTICAL)
        for b in self.buttons:
            sizer.Add(b, 0, wx.ALL|wx.EXPAND, 1)
        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)
        sizer.SetSizeHints(self)

    def on_toggle(self, button, event):
        if self.selected is button:
            self.selected.SetValue(True)
            return
        if self.selected is not None:
            self.selected.SetValue(False)
        self.selected = button
        wx.PostEvent(self, ToggleBoxEvent(self.GetId(), self.GetValue(),
                                         self.GetStringValue()))

    def GetValue(self):
        if self.selected is not None:
            return self.buttons.index(self.selected)
        return -1

    def GetStringValue(self):
        if self.selected is None: return None
        return self.selected.GetLabel()

    def SetValue(self, index):
        if self.selected is not None:
            self.selected.SetValue(False)
        if -1 < index < len(self.buttons):
            self.selected = self.buttons[index]
            self.selected.SetValue(True)

    def SetStringValue(self, strval):
        index = -1
        for i in range(len(self.buttons)):
            if self.buttons[i].GetLabel() == strval:
                index = i
                break
        self.SetValue(index)

# end of class ToggleButtonBox


class wxGladeArtProvider(wx.ArtProvider):
    def CreateBitmap(self, artid, client, size):
        if wx.Platform == '__WXGTK__' and artid == wx.ART_FOLDER:
            return wx.Bitmap(os.path.join(config.icons_path, 'closed_folder.xpm'),
                            wx.BITMAP_TYPE_XPM)
        return wx.NullBitmap

# end of class wxGladeArtProvider


class wxGladeFrame(wx.Frame):
    """\
    Main frame of wxGlade (palette)
    
    @ivar cur_dir: Last visited directory, used for wxFileDialog and not
                   for KDE dialogs
    @type cur_dir: str

    @ivar _logger: Instance specific logger
    """

    def __init__(self, parent=None):
        self._logger = logging.getLogger(self.__class__.__name__)
        style = wx.SYSTEM_MENU | wx.CAPTION | wx.MINIMIZE_BOX
        style |= wx.RESIZE_BORDER | wx.CLOSE_BOX
        wx.Frame.__init__(self, parent, -1, "wxGlade v%s" % config.version,
                          style=style, name='MainFrame')
        self.CreateStatusBar(1)

        if parent is None:
            parent = self
        common.palette = self  # to provide a reference accessible
                               # by the various widget classes
        icon = wx.EmptyIcon()
        bmp = wx.Bitmap(
            os.path.join(config.icons_path, "icon.xpm"),
            wx.BITMAP_TYPE_XPM
            )
        icon.CopyFromBitmap(bmp)
        self.SetIcon(icon)
        self.SetBackgroundColour(wx.SystemSettings_GetColour(
            wx.SYS_COLOUR_BTNFACE))
        menu_bar = wx.MenuBar()
        file_menu = wx.Menu(style=wx.MENU_TEAROFF)
        view_menu = wx.Menu(style=wx.MENU_TEAROFF)
        help_menu = wx.Menu(style=wx.MENU_TEAROFF)
        wx.ToolTip_SetDelay(1000)

        # load the available code generators
        core_buttons, local_buttons, sizer_buttons = common.init_codegen()

        append_item = misc.append_item
        self.TREE_ID = TREE_ID = wx.NewId()
        append_item(view_menu, TREE_ID, _("Show &Tree\tF2"))
        self.PROPS_ID = PROPS_ID = wx.NewId()
        self.RAISE_ID = RAISE_ID = wx.NewId()
        append_item(view_menu, PROPS_ID, _("Show &Properties\tF3"))
        append_item(view_menu, RAISE_ID, _("&Raise All\tF4"))
        NEW_ID = wx.NewId()
        append_item(file_menu, NEW_ID, _("&New\tCtrl+N"), wx.ART_NEW)
        NEW_FROM_TEMPLATE_ID = wx.NewId()
        append_item(file_menu, NEW_FROM_TEMPLATE_ID,
                    _("New from &Template...\tShift+Ctrl+N"))
        OPEN_ID = wx.NewId()
        append_item(file_menu, OPEN_ID, _("&Open...\tCtrl+O"), wx.ART_FILE_OPEN)
        SAVE_ID = wx.NewId()
        append_item(file_menu, SAVE_ID, _("&Save\tCtrl+S"), wx.ART_FILE_SAVE)
        SAVE_AS_ID = wx.NewId()
        append_item(file_menu, SAVE_AS_ID, _("Save As...\tShift+Ctrl+S"),
                    wx.ART_FILE_SAVE_AS)
        SAVE_TEMPLATE_ID = wx.NewId()
        append_item(file_menu, SAVE_TEMPLATE_ID, _("Save As Template..."))
        file_menu.AppendSeparator()
        RELOAD_ID = wx.ID_REFRESH
        append_item(file_menu, RELOAD_ID, _("&Refresh\tf5"))
        GENERATE_CODE_ID = wx.NewId()
        append_item(file_menu, GENERATE_CODE_ID, _("&Generate Code\tCtrl+G"),
                    wx.ART_EXECUTABLE_FILE)

        file_menu.AppendSeparator()
        IMPORT_ID = wx.NewId()
        append_item(file_menu, IMPORT_ID, _("&Import from XRC...\tCtrl+I"))

        EXIT_ID = wx.NewId()
        file_menu.AppendSeparator()
        append_item(file_menu, EXIT_ID, _('E&xit\tCtrl+Q'), wx.ART_QUIT)
        PREFS_ID = wx.ID_PREFERENCES
        view_menu.AppendSeparator()
        MANAGE_TEMPLATES_ID = wx.NewId()
        append_item(view_menu, MANAGE_TEMPLATES_ID, _('Template Manager...'))
        view_menu.AppendSeparator()
        append_item(view_menu, PREFS_ID, _('Preferences...'))
        menu_bar.Append(file_menu, _("&File"))
        menu_bar.Append(view_menu, _("&View"))

        MANUAL_ID = wx.NewId()
        append_item(help_menu, MANUAL_ID, _('Manual\tF1'), wx.ART_HELP)
        TUTORIAL_ID = wx.NewId()
        append_item(help_menu, TUTORIAL_ID, _('Tutorial'))
        help_menu.AppendSeparator()
        ABOUT_ID = wx.ID_ABOUT
        append_item(help_menu, ABOUT_ID, _('About'))
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

        def open_from_history(event):
            if not self.ask_save():
                return
            pos = event.GetId() - wx.ID_FILE1
            filename = self.file_history.GetHistoryFile(pos)
            if not os.path.exists(filename):
                wx.MessageBox(
                    _("The file %s doesn't exist.") % filename,
                    _('Information'),
                    style=wx.CENTER | wx.ICON_INFORMATION | wx.OK)
                self.file_history.RemoveFileFromHistory(pos)
                common.remove_autosaved(filename)
                return
            if common.check_autosaved(filename):
                res = wx.MessageBox(
                    _('There seems to be auto saved data for this file: '
                      'do you want to restore it?'),
                    _('Auto save detected'),
                    style=wx.ICON_QUESTION | wx.YES_NO)
                if res == wx.YES:
                    common.restore_from_autosaved(filename)
                else:
                    common.remove_autosaved(filename)
            else:
                common.remove_autosaved(filename)
            self._open_app(filename)

        wx.EVT_MENU_RANGE(self, wx.ID_FILE1, wx.ID_FILE9, open_from_history)

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
        wx.EVT_MENU(self, RELOAD_ID, self.reload_app)

        PREVIEW_ID = wx.NewId()
        wx.EVT_MENU(self, PREVIEW_ID, self.preview)

        self.accel_table = wx.AcceleratorTable([
            (wx.ACCEL_CTRL, ord('N'), NEW_ID),
            (wx.ACCEL_CTRL, ord('O'), OPEN_ID),
            (wx.ACCEL_CTRL, ord('S'), SAVE_ID),
            (wx.ACCEL_CTRL|wx.ACCEL_SHIFT, ord('S'), SAVE_AS_ID),
            (wx.ACCEL_CTRL, ord('G'), GENERATE_CODE_ID),
            (wx.ACCEL_CTRL, ord('I'), IMPORT_ID),
            (0, wx.WXK_F1, MANUAL_ID),
            (wx.ACCEL_CTRL, ord('Q'), EXIT_ID),
            (0, wx.WXK_F5, RELOAD_ID),
            (0, wx.WXK_F2, TREE_ID),
            (0, wx.WXK_F3, PROPS_ID),
            (0, wx.WXK_F4, RAISE_ID),
            (wx.ACCEL_CTRL, ord('P'), PREVIEW_ID),
            ])

        # layout
        # if there are custom components, add the toggle box...
        if local_buttons:
            main_sizer = wx.BoxSizer(wx.VERTICAL)
            show_core_custom = ToggleButtonBox(
                self, -1, [_("Core components"), _("Custom components")], 0)

            core_sizer = wx.FlexGridSizer(
                0,
                config.preferences.buttons_per_row
                )
            custom_sizer = wx.FlexGridSizer(
                0,
                config.preferences.buttons_per_row
                )
            self.SetAutoLayout(True)
            # core components
            for b in core_buttons:
                core_sizer.Add(b)
            for sb in sizer_buttons:
                core_sizer.Add(sb)
            # custom components
            for b in local_buttons:
                custom_sizer.Add(b)
                custom_sizer.Show(b, False)
            custom_sizer.Layout()
            main_sizer.Add(show_core_custom, 0, wx.EXPAND)
            main_sizer.Add(core_sizer, 0, wx.EXPAND)
            main_sizer.Add(custom_sizer, 0, wx.EXPAND)
            self.SetSizer(main_sizer)
            main_sizer.Fit(self)
            # events to display core/custom components

            def on_show_core_custom(event):
                show_core = True
                show_custom = False
                if event.GetValue() == 1:
                    show_core = False
                    show_custom = True
                for b in local_buttons:
                    custom_sizer.Show(b, show_custom)
                for b in core_buttons:
                    core_sizer.Show(b, show_core)
                for b in sizer_buttons:
                    core_sizer.Show(b, show_core)
                core_sizer.Layout()
                custom_sizer.Layout()
                main_sizer.Layout()
            EVT_TOGGLE_BOX(self, show_core_custom.GetId(), on_show_core_custom)
        # ... otherwise (the common case), just add the palette of core buttons
        else:
            sizer = wx.GridSizer(0, config.preferences.buttons_per_row)
            self.SetAutoLayout(True)
            # core components
            for b in core_buttons:
                sizer.Add(b)
            for sb in sizer_buttons:
                sizer.Add(sb)
            self.SetSizer(sizer)
            sizer.Fit(self)

        # Properties window
        frame_style = wx.DEFAULT_FRAME_STYLE
        frame_tool_win = config.preferences.frame_tool_win
        if frame_tool_win:
            frame_style |= wx.FRAME_NO_TASKBAR | wx.FRAME_FLOAT_ON_PARENT
            frame_style &= ~wx.MINIMIZE_BOX
            if wx.Platform != '__WXGTK__': frame_style |= wx.FRAME_TOOL_WINDOW

        self.frame_property = wx.Frame(self, -1, _('Properties - <%s>' % _('app')),
                               style=frame_style, name='PropertyFrame')
        self.frame_property.SetBackgroundColour(wx.SystemSettings_GetColour(
            wx.SYS_COLOUR_BTNFACE))
        self.frame_property.SetIcon(icon)

        property_panel = wxGladePropertyPanel(self.frame_property, -1)
        sz = wx.BoxSizer(wx.VERTICAL)
        sz.Add(property_panel, 1, wx.EXPAND)
        self.frame_property.SetSizer(sz)
        property_panel.SetAutoLayout(True)
        self.frame_property.SetAutoLayout(True)

        def hide_frame2(event):
            #menu_bar.Check(PROPS_ID, False)
            self.frame_property.Hide()
        wx.EVT_CLOSE(self.frame_property, hide_frame2)
        wx.EVT_CLOSE(self, self.cleanup)
        common.property_panel = property_panel

        # Tree of widgets
        self.tree_frame = wx.Frame(self, -1, _('wxGlade: Tree'),
                                  style=frame_style, name='TreeFrame')
        self.tree_frame.SetIcon(icon)

        app = application.Application(common.property_panel)
        common.app_tree = WidgetTree(self.tree_frame, app)
        self.tree_frame.SetSize((300, 300))
        app.notebook.Show()

        sizer_tmp = wx.BoxSizer(wx.VERTICAL)
        sizer_tmp.Add(app.notebook, 1, wx.EXPAND)
        property_panel.SetSizer(sizer_tmp)
        sizer_tmp.Fit(property_panel)

        def on_tree_frame_close(event):
            #menu_bar.Check(TREE_ID, False)
            self.tree_frame.Hide()
        wx.EVT_CLOSE(self.tree_frame, on_tree_frame_close)
        # check to see if there are some remembered values
        prefs = config.preferences

        self_geometry = None
        property_geomentry = None
        tree_geometry = None

        if prefs.remember_geometry:
            self_geometry = prefs.get_geometry('main')
            if isinstance(self_geometry, types.TupleType):
                self_geometry = wx.Rect(*self_geometry)

            property_geomentry = prefs.get_geometry('properties')
            if isinstance(property_geomentry, types.TupleType):
                property_geomentry = wx.Rect(*property_geomentry)

            tree_geometry = prefs.get_geometry('tree')
            if isinstance(tree_geometry, types.TupleType):
                tree_geometry = wx.Rect(*tree_geometry)

        if not self_geometry:
            self_geometry = wx.Rect()
            self_geometry.TopLeft = wx.Display().GetClientArea().GetTopLeft()
            self_geometry.Size = (-1, -1)

        self._set_geometry(self, self_geometry)
        self.Show()
        self_geometry.Size = self.GetSize()

        if not property_geomentry:
            property_geomentry = wx.Rect()
            property_geomentry.Position = self_geometry.BottomLeft
            property_geomentry.Size = (345, 350)
            # sometimes especially on GTK GetSize seems to ignore window
            # decorations (bug still exists on wx3)
            if wx.Platform != '__WXMSW__':
                property_geomentry.Y += 40

            # set size on Mac manually
            if wx.Platform == '__WXMAC__':
                property_geomentry.Size = (345, 384)

        self._set_geometry(self.frame_property, property_geomentry)
        self.frame_property.Show()

        if not tree_geometry:
            tree_geometry = wx.Rect()
            tree_geometry.Position = self_geometry.TopRight
            tree_geometry.Size = (250, 350)
            # sometimes especially on GTK GetSize seems to ignore window
            # decorations (bug still exists on wx3)
            if wx.Platform != '__WXMSW__':
                tree_geometry.X += 10

        self._set_geometry(self.tree_frame, tree_geometry)
        self.tree_frame.Show()

        # last visited directory, used on GTK for wxFileDialog
        self.cur_dir = config.preferences.open_save_path

        # set a drop target for us...
        self._droptarget = clipboard.FileDropTarget(self)
        self.SetDropTarget(self._droptarget)
        #self.tree_frame.SetDropTarget(self._droptarget)
        #self.frame_property.SetDropTarget(self._droptarget)

        # ALB 2004-10-15, autosave support...
        self.autosave_timer = None
        if config.preferences.autosave:
            TIMER_ID = wx.NewId()
            self.autosave_timer = wx.Timer(self, TIMER_ID)
            wx.EVT_TIMER(self, TIMER_ID, self.on_autosave_timer)
            self.autosave_timer.Start(
                int(config.preferences.autosave_delay) * 1000)
        # ALB 2004-10-15
        CLEAR_SB_TIMER_ID = wx.NewId()
        self.clear_sb_timer = wx.Timer(self, CLEAR_SB_TIMER_ID)
        wx.EVT_TIMER(self, CLEAR_SB_TIMER_ID, self.on_clear_sb_timer)

        self.frame_property.SetAcceleratorTable(self.accel_table)
        self.tree_frame.SetAcceleratorTable(self.accel_table)

        self.Raise()

        # disable autosave checks during unittests
        if not getattr(sys, '_called_from_test', False):
            if common.check_autosaved(None):
                res = wx.MessageBox(
                    _('There seems to be auto saved data from last wxGlade '
                      'session: do you want to restore it?'),
                    _('Auto save detected'),
                    style=wx.ICON_QUESTION | wx.YES_NO)
                if res == wx.YES:
                    if self._open_app(common.get_name_for_autosave(),
                                      add_to_history=False):
                        common.app_tree.app.saved = False
                        common.app_tree.app.filename = None
                        self.user_message(_('Auto save loaded'))
                common.remove_autosaved()

    def on_autosave_timer(self, event):
        if common.autosave_current():
            self.user_message(_("Auto saving... done"))

    def edit_preferences(self, event):
        dialog = preferencesdialog.wxGladePreferences(config.preferences)
        if dialog.ShowModal() == wx.ID_OK:
            wx.MessageBox(_('Changes will take effect after wxGlade is restarted'),
                          _('Preferences saved'),
                          wx.OK|wx.CENTRE|wx.ICON_INFORMATION)
            dialog.set_preferences()
        dialog.Destroy()

    def preview(self, event):
        """\
        Generate preview of the current loaded project.
        
        A preview can be triggered by keyboard shortcut or by pressing the
        preview button. The preview can be triggered for all selected widgets.
        This doesn't mean that the widget is opened for editing.
        
        A good indicator for editing is the availability of the preview
        button.
       
        @see: L{edit_windows.PreviewMixin.preview_button}
        """
        # Show preview only, if preview_button is available
        #
        if not getattr(common.app_tree.cur_widget, 'preview_button', None):
            return
        preview_widget = misc.get_toplevel_widget(common.app_tree.cur_widget)
        if preview_widget is not None:
            preview_widget.preview(None)

    def show_tree(self, event):
        self.tree_frame.Show()
        self.tree_frame.Raise()
        common.app_tree.SetFocus()

    def show_props_window(self, event):
        self.frame_property.Show()
        self.frame_property.Raise()
        try:
            c = self.frame_property.GetSizer().GetChildren()
            if c: c[0].GetWindow().SetFocus()
        except (AttributeError, TypeError):
            self.frame_property.SetFocus()

    def raise_all(self, event):
        children = self.GetChildren()
        for child in children:
            child = misc.get_toplevel_parent(child)
            if child.IsShown() and child.GetTitle(): child.Raise()
        self.Raise()

    def user_message(self, msg):
        sb = self.GetStatusBar()
        if sb:
            sb.SetStatusText(msg)
            self.clear_sb_timer.Start(5000, True)

    def on_clear_sb_timer(self, event):
        sb = self.GetStatusBar()
        if sb:
            sb.SetStatusText("")

    def ask_save(self):
        """\
        checks whether the current app has changed and needs to be saved:
        if so, prompts the user;
        returns False if the operation has been cancelled
        """
        if not common.app_tree.app.saved:
            ok = wx.MessageBox(_("Save changes to the current app?"),
                               _("Confirm"),
                               wx.YES_NO|wx.CANCEL|wx.CENTRE|wx.ICON_QUESTION)
            if ok == wx.YES:
                self.save_app(None)
            return ok != wx.CANCEL
        return True

    def new_app(self, event):
        """\
        creates a new wxGlade project
        """
        if self.ask_save():
            common.app_tree.clear()
            common.app_tree.app.filename = None
            common.app_tree.app.saved = True
            self.user_message("")
            common.remove_autosaved()
            if config.preferences.autosave and self.autosave_timer is not None:
                self.autosave_timer.Start()

    def new_app_from_template(self, event):
        """\
        creates a new wxGlade project from an existing template file
        """
        if not self.ask_save(): return
        infile = template.select_template()
        if infile:
            self._open_app(infile, add_to_history=False)
            common.app_tree.app.template_data = None

    def reload_app(self, event):
        self.ask_save()
        if not common.app_tree.app.filename:
            wx.MessageBox(_("Impossible to reload an unsaved application"),
                          _("Alert"), style=wx.OK|wx.ICON_INFORMATION)
            return
        path = common.app_tree.get_selected_path()
        self._open_app(common.app_tree.app.filename, add_to_history=False)
        common.app_tree.select_path(path)

    def open_app(self, event_unused):
        """\
        loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :)
        """
        if not self.ask_save():
            return
        infile = wx.FileSelector(_("Open file"),
                                   wildcard="wxGlade files (*.wxg)|*.wxg|"
                                   "wxGlade Template files (*.wgt)|*.wgt|"
                                   "XML files (*.xml)|*.xml|All files|*",
                                   flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
                                   default_path=self.cur_dir)
        if infile:
            if common.check_autosaved(infile) and \
                   wx.MessageBox(_("There seems to be auto saved data for "
                                "this file: do you want to restore it?"),
                                _("Auto save detected"),
                                style=wx.ICON_QUESTION|wx.YES_NO) == wx.YES:
                common.restore_from_autosaved(infile)
            else:
                common.remove_autosaved(infile)
            self._open_app(infile)
            self.cur_dir = os.path.dirname(infile)

    def _open_app(self, filename_or_filelike, use_progress_dialog=True,
                  add_to_history=True):
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
        assert isinstance(filename_or_filelike,
                          types.StringTypes + (StringIO.StringIO, ))
        if isinstance(filename_or_filelike, StringIO.StringIO):
            assert isinstance(filename_or_filelike.getvalue(), types.UnicodeType)

        error_msg = None
        filename = None
        infile = None
        old_dir = os.getcwd()

        if isinstance(filename_or_filelike, types.StringTypes):
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
                if isinstance(filename_or_filelike, StringIO.StringIO):
                    # convert filename_or_filelike to UTF-8 and write back
                    # as lines, because ProgressXmlWidgetBuilder uses lines
                    # to calculate and show the position
                    tmp = filename_or_filelike.getvalue()
                    tmp = tmp.encode('UTF-8')
                    infile = StringIO.StringIO()
                    for line in tmp.split('\n'):
                        infile.write('%s\n' % line)
                    infile.seek(0)
                    self._logger.info(
                        _('Read wxGlade project from file-like object'))

                else:
                    self._logger.info(
                        _('Read wxGlade project from file "%s"'), filename
                    )
                    os.chdir(os.path.dirname(filename))
                    # decoding will done automatically by SAX XML library
                    infile = open(filename)

                if use_progress_dialog and config.preferences.show_progress:
                    p = ProgressXmlWidgetBuilder(input_file=infile)
                else:
                    p = XmlWidgetBuilder()

                p.parse(infile)
            except (IOError, OSError, SAXParseException,
                    XmlParsingError), msg:
                if filename:
                    error_msg = _("Error loading file %s: %s") % \
                                (misc.wxstr(filename), misc.wxstr(msg))
                else:
                    error_msg = _("Error loading from a file-like "
                                  "object: %s") % misc.wxstr(msg)
            except Exception, inst:
                if filename:
                    fn = os.path.basename(filename).encode('ascii',
                                                             'replace')
                    msg = _('loading file "%s"') % fn
                else:
                    msg = _('loading from a file-like object')
                bugdialog.Show(msg, inst)
        finally:
            if infile and filename:
                infile.close()

            if error_msg:
                common.app_tree.clear()
                common.property_panel.Reparent(self.frame_property)
                common.app_tree.app.saved = True

                # re-enable auto-expansion of nodes
                common.app_tree.auto_expand = True

                os.chdir(old_dir)

                wx.MessageBox(error_msg, _('Error'),
                              wx.OK | wx.CENTRE | wx.ICON_ERROR)

                return False

        common.app_tree.select_item(common.app_tree.root)
        common.app_tree.root.widget.show_properties()
        common.property_panel.Reparent(self.frame_property)

        # re-enable auto-expansion of nodes
        common.app_tree.auto_expand = True

        common.app_tree.expand()
        if common.app_tree.app.is_template:
            self._logger.info(_("Template loaded"))
            common.app_tree.app.template_data = template.Template(filename)
            common.app_tree.app.filename = None

        end = time.clock()
        self._logger.info(_('Loading time: %.5f'), end - start)

        common.app_tree.app.saved = True

        if hasattr(self, 'file_history') and filename is not None and \
                add_to_history and (not common.app_tree.app.is_template):
            self.file_history.AddFileToHistory(misc.wxstr(filename))

        if config.preferences.autosave and self.autosave_timer is not None:
            self.autosave_timer.Start()

        duration = end - start
        if filename:
            self.user_message(
                _("Loaded %s in %.2f seconds") % (
                    misc.wxstr(os.path.basename(filename)), duration))
        else:
            self.user_message(_("Loaded in %.2f seconds") % duration)

        return True

    def save_app(self, event):
        """\
        saves a wxGlade project onto an xml file
        """
        if not common.app_tree.app.filename or \
           common.app_tree.app.is_template:
            self.save_app_as(event)
        else:
            # check whether we are saving a template
            ext = os.path.splitext(common.app_tree.app.filename)[1].lower()
            if ext == ".wgt":
                common.app_tree.app.is_template = True
            self._save_app(common.app_tree.app.filename)

    def _save_app(self, filename):
        try:
            obuffer = StringIO.StringIO()
            common.app_tree.write(obuffer)
            common.save_file(filename, obuffer.getvalue(), 'wxg')
        except (IOError, OSError), inst:
            common.app_tree.app.saved = False
            wx.MessageBox(_("Error saving app:\n%s") % inst, _("Error"),
                         wx.OK | wx.CENTRE | wx.ICON_ERROR)
        except Exception, inst:
            common.app_tree.app.saved = False
            fn = os.path.basename(filename).encode('ascii', 'replace')
            bugdialog.Show(_('Save File "%s"') % fn, inst)
        else:
            common.app_tree.app.saved = True
            common.remove_autosaved()
            if config.preferences.autosave and \
                   self.autosave_timer is not None:
                self.autosave_timer.Start()
            self.user_message(_("Saved %s") % os.path.basename(filename))

    def save_app_as(self, event):
        """\
        saves a wxGlade project onto an xml file chosen by the user
        """
        # both flags occurs several times
        fn = wx.FileSelector(_("Save project as..."),
                             wildcard="wxGlade files (*.wxg)|*.wxg|"
                             "wxGlade Template files (*.wgt) |*.wgt|"
                             "XML files (*.xml)|*.xml|All files|*",
                             flags=wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT,
                             default_path=self.cur_dir)
        if fn:
            # check for file extension and add default extension if missing
            ext = os.path.splitext(fn)[1].lower()
            if not ext:
                fn = "%s.wxg" % fn

            common.app_tree.app.filename = fn
            #remove the template flag so we can save the file.
            common.app_tree.app.is_template = False

            self.save_app(event)
            self.cur_dir = os.path.dirname(fn)
            self.file_history.AddFileToHistory(fn)

    def save_app_as_template(self, event):
        """\
        save a wxGlade project as a template
        """
        data = getattr(common.app_tree.app, 'template_data', None)
        outfile, data = template.save_template(data)
        if outfile:
            common.app_tree.app.is_template = True
            common.app_tree.app.template_data = data
            self._save_app(outfile)

    def cleanup(self, event):
        if self.ask_save():
            # first, let's see if we have to save the geometry...
            prefs = config.preferences
            if prefs.remember_geometry:
                prefs.set_geometry('main', self._get_geometry(self))
                prefs.set_geometry('tree',
                                   self._get_geometry(self.tree_frame))
                prefs.set_geometry('properties',
                                   self._get_geometry(self.frame_property))
                prefs.changed = True
            common.app_tree.clear()
            try:
                common.save_preferences()
            except Exception, e:
                wx.MessageBox(_('Error saving preferences:\n%s') % e,
                              _('Error'),
                              wx.OK|wx.CENTRE|wx.ICON_ERROR)
            #self._skip_activate = True
            self.frame_property.Destroy()
            self.tree_frame.Destroy()
            self.Destroy()
            common.remove_autosaved()  # ALB 2004-10-15
            wx.CallAfter(wx.GetApp().ExitMainLoop)

    def show_about_box(self, event):
        """\
        show the about dialog
        
        @see: L{about.wxGladeAboutBox}
        """
        about_box = about.wxGladeAboutBox()
        about_box.ShowModal()
        about_box.Destroy()

    def show_manual(self, event):
        """\
        Show the wxGlade user manual
        """
        self._show_html(config.manual_file)

    def show_tutorial(self, event):
        """\
        Show the wxGlade tutorial
        """
        self._show_html(config.tutorial_file)

    def _show_html(self, html_file):
        """\
        Open browser and show an HTML documentation

        @param html_file: HTML file to show
        @type html_file: str | Unicode
        """
        if wx.Platform == "__WXMAC__":
            os.system(r'open -a Help\ Viewer.app %s' % html_file)
        else:
            import webbrowser
            import threading
            # ALB 2004-08-15: why did this block the program?????
            # (at least on linux - GTK)

            def go():
                webbrowser.open_new(html_file)
            t = threading.Thread(target=go)
            t.setDaemon(True)
            t.start()

    def show_and_raise(self):
        self.frame_property.Show()  # self.GetMenuBar().IsChecked(self.PROPS_ID))
        self.tree_frame.Show()  # self.GetMenuBar().IsChecked(self.TREE_ID))
        self.frame_property.Raise()
        self.tree_frame.Raise()
        self.Raise()

    def hide_all(self):
        self.tree_frame.Hide()
        self.frame_property.Hide()

    def import_xrc(self, event):
        import xrc2wxg

        if not self.ask_save():
            return

        infilename = wx.FileSelector(
            _("Import file"),
            wildcard="XRC files (*.xrc)" "|*.xrc|All files|*",
            flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            default_path=self.cur_dir)
        if infilename:
            ibuffer = StringIO.StringIO()
            try:
                xrc2wxg.convert(infilename, ibuffer)

                # Convert UTF-8 returned by xrc2wxg.convert() to Unicode
                tmp = ibuffer.getvalue().decode('UTF-8')
                ibuffer = StringIO.StringIO()
                [ibuffer.write('%s\n' % line) for line in tmp.split('\n')]
                ibuffer.seek(0)

                self._open_app(ibuffer)
                common.app_tree.app.saved = False
            except Exception, inst:
                fn = os.path.basename(infilename).encode('ascii', 'replace')
                bugdialog.Show(_('Import File "%s"') % fn, inst)

    def manage_templates(self, event):
        to_edit = template.manage_templates()
        if to_edit is not None and self.ask_save():
            # edit the template
            # TODO, you still need to save it manually...
            self._open_app(to_edit, add_to_history=False)
            wx.MessageBox(_("To save the changes to the template, edit the "
                            "GUI as usual,\nand then click "
                            "File->Save As Template..."), _("Information"),
                          style=wx.OK|wx.ICON_INFORMATION)

    def _set_geometry(self, win, geometry):
        """\
        Set position and/or size of widget.

        @param win: Frame to set position and/or size
        @type win: wx.Frame

        @param geometry: Position and Size
        @type geometry: (int, int, int, int)
        """
        assert isinstance(geometry, (wx.Point, wx.Rect))

        if not geometry:
            return

        if isinstance(geometry, wx.Point):
            win.SetPosition(geometry)
        else:
            win.SetDimensions(*geometry.Get())

    def _get_geometry(self, widget):
        """\
        Return widgets position and size.

        @param widget: Frame to return position and/or size
        @type widget: wx.Frame

        @rtype: wx.Rect | None
        """
        pos_size = widget.Rect
        client_area = wx.Display().ClientArea
        if client_area.Contains(pos_size.TopLeft):
            return pos_size.Get()
        return None

# end of class wxGladeFrame


class wxGlade(wx.App):
    """\
    wxGlade application class
    
    @ivar _exception_orig: Reference to original implementation of
                           logging.exception()
    """

    def OnInit(self):
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

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

        wx.ArtProvider.PushProvider(wxGladeArtProvider())

        frame = wxGladeFrame()
        self.SetTopWindow(frame)
        self.SetExitOnFrameDelete(True)

        wx.EVT_IDLE(self, self.OnIdle)

        return True

    def OnExit(self):
        """\
        Restore original exception handler and logging.exception() on exit
        """
        sys.excepthook = sys.__excepthook__
        logging.exception = self._exception_orig

    def OnIdle(self, event):
        """\
        Idle tasks - currently show error messages only

        @see: L{show_msgdialog()}
        """
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

# end of class wxGlade


def main(filename=None):
    """\
    if filename is not None, loads it
    """
    logging.info(_("Using wxPython %s"), config.wx_version)

#    # now, silence a deprecation warning for py2.3
#    import warnings
#    warnings.filterwarnings(
#        "ignore",
#        "integer",
#        DeprecationWarning,
#        "wxPython.gdi",
#        )

    app = wxGlade()
    if filename is not None:
        app.GetTopWindow()._open_app(filename, False)
    app.MainLoop()
