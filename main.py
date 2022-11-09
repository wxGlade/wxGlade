"""\
Main wxGlade module: defines wxGladeFrame which contains the buttons to add
widgets and initializes all the stuff (tree, frame_property, etc.)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2011-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import logging, os, os.path, sys, math, time, functools
import wx
from xml.sax import SAXParseException

# import project modules
import application
import common, config, compat, misc, history
import new_properties as np
import preferencesdialog, msgdialog, bugdialog, about
import log
import template
from tree import WidgetTree
from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder, XmlParsingError



class FileDropTarget(wx.FileDropTarget):
    # file drop target that checks first whether the property panel is the target
    def __init__(self, parent):
        wx.FileDropTarget.__init__(self)
        self.parent = parent

    if config.debugging:
        def OnDragOver(self, x, y, defResult):
            x0,y0 = self.parent.GetClientAreaOrigin()
            screen_xy = self.parent.ClientToScreen( (x-x0,y-y0) )
            ctrl = wx.FindWindowAtPoint( screen_xy )
            print("DragOver", x0,y0, x-x0,y-y0, ctrl)
            return wx.FileDropTarget.OnDragOver(self, x,y, defResult)

    def OnDropFiles(self, x, y, filenames):
        if len(filenames) > 1:
            wx.MessageBox( _("Please only drop one file at a time"), "wxGlade", wx.ICON_ERROR )
            return False
        if not filenames or not os.path.exists(filenames[0]): return False
        # find control under drop point; this does also work if a dialog is open
        x0,y0 = self.parent.GetClientAreaOrigin()
        screen_xy = self.parent.ClientToScreen( (x-x0,y-y0) )
        ctrl = c = wx.FindWindowAtPoint( screen_xy )
        # go up the hierarchy and find a widget with an 'on_drop_files' method
        while c:
            if hasattr(c, "on_drop_files"):
                handled = c.on_drop_files(screen_xy, ctrl, filenames)
                if handled: return True
            if c is self.parent.property_panel: break
            if isinstance(c, wx.Dialog): return False  # when a dialog is open, don't open wxg or xrc files
            c = c.GetParent()

        # not handled by a control; try to open .wxg or .XRC file
        if not self.parent.ask_save(): return False

        path = filenames[0]
        if os.path.splitext(path)[1].upper() == ".XRC":
            self.parent.import_xrc(path, ask_save=False)
        else:
            self.parent._open_app(path)
        self.parent.cur_dir = os.path.dirname(path)
        return True


class wxGladePropertyPanel(wx.Panel):
    "Panel used to display the Properties of the various widgets"
    def __init__(self, parent):
        wx.Panel.__init__( self, parent, -1, name='PropertyPanel' )
        self.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE) )

        self.current_widget = None        # instance currently being edited
        self.next_widget = None           # the next one, will only be edited after a small delay

        self.pagenames = None

        sizer = wx.BoxSizer(wx.VERTICAL)
        self.heading = wx.TextCtrl(self, style=wx.TE_READONLY)
        sizer.Add(self.heading, 0, wx.EXPAND, 0)
        self.notebook = wx.Notebook(self)
        self.notebook.Bind(wx.EVT_SIZE, self.on_notebook_size)

        sizer.Add(self.notebook, 1, wx.EXPAND, 0)

        # for GTK3: add a panel to determine page size
        p = wx.Panel(self.notebook)
        self.notebook.AddPage(p, "panel")
        self._notebook_decoration_size = None
        p.Bind(wx.EVT_SIZE, self.on_panel_size)

        self.SetSizer(sizer)
        self.Layout()

    def on_drop_files(self, screen_xy, ctrl, filenames):
        if not self.current_widget: return False
        for p_name in self.current_widget.PROPERTIES:
            if p_name[0].isupper(): continue
            prop = self.current_widget.properties.get(p_name)
            if not prop or not hasattr(prop, "on_drop_file"): continue
            if ( hasattr(prop, "label_ctrl") and prop.label_ctrl.ScreenRect.Contains( screen_xy ) and
                 prop.label_ctrl.IsShownOnScreen() ) or prop.has_control(ctrl):
                return prop.on_drop_file(filenames[0])
        return False

    ####################################################################################################################
    # new editor interface
    def set_widget(self, widget, force=False):
        if widget is self.current_widget and not force:
            # just update
            return
        self.next_widget = widget
        if self.current_widget:
            # this might not be executed if there was an error during creation of the property editors
            for editor in self.current_widget.properties.values():
                editor.destroy_editor()
            self.current_widget = None   # delete the reference
        wx.CallLater( 150, self.edit_properties, widget )

    def edit_properties(self, edit_widget):
        # this will be called with a delay
        if edit_widget is not self.next_widget:
            # wait for another call...
            return
        if self._notebook_decoration_size is None:
            # try again later
            wx.CallLater( 150, self.edit_properties, edit_widget )
            return

        self.current_widget = None
        self.create_editor(edit_widget)
        # this code might not be reached in case of an error
        self.current_widget = edit_widget
        if edit_widget:
            # XXX set status bar
            klass = edit_widget.get_prop_value("class", edit_widget.WX_CLASS)
            self.heading.SetValue( _('Properties - %s - <%s>:') % (klass, edit_widget.name) )
        else:
            self.heading.SetValue( _('Properties') )

    def create_editor(self, edit_widget):
        # fill the frame with a notebook of property editors

        if not self.notebook: return  # already deleted
        self.current_widget_class = edit_widget.__class__
        if wx.Platform != "__WXMSW__" :
            focus_before = self.FindFocus()
        self.notebook.Hide()

        # remember the notebook page to be selected
        selection = self.notebook.GetSelection()
        select_page = self.pagenames[selection]  if selection!=-1  else None

        # clear notebook pages
        #self.notebook.DeleteAllPages()  # deletes also the windows on the pages
        while self.notebook.PageCount:
            print("DELETE PAGE; new widget:", edit_widget)
            self.notebook.DeletePage(self.notebook.PageCount-1)

        self.pagenames = pagenames = []
        self.sizers = []
        if not edit_widget: return
        current_page = current_sizer = current_pagename = None
        property_instance = None
        for prop in edit_widget.PROPERTIES:
            if prop[0].isupper():
                # end previous page
                if current_page is not None:
                    self.end_page(current_page, current_sizer, current_pagename)
                    current_page = None

                # start new page
                current_pagename = prop
                if prop=="Layout" and not edit_widget._has_layout:continue
                if prop=="Events" and edit_widget.events is None: continue

                current_page = self.start_page(prop)
                current_sizer = wx.BoxSizer(wx.VERTICAL)
                self.sizers.append(current_sizer)

                self.pagenames.append(prop)

                continue

            if current_pagename=="Layout" and not edit_widget._has_layout: continue

            # a property or None
            property_instance_ = edit_widget.properties.get(prop)
            if property_instance_ is not None:
                property_instance = property_instance_
                property_instance.create_editor(current_page, current_sizer)

        if current_page is not None:
            self.end_page(current_page, current_sizer, current_pagename)

        if select_page and select_page in pagenames:
            index = pagenames.index(select_page)
            self.notebook.SetSelection(index)
        else:
            self.notebook.SetSelection(0)

        self.notebook.Show()

        if wx.Platform != "__WXMSW__" and focus_before is common.app_tree:
            focus_before.SetFocus()

    def start_page(self, name):
        # create a ScrolledWindow and a Panel; with only ScrolledWindow, scrolling on gtk 3 does not work
        scrolled = wx.ScrolledWindow( self.notebook, name=name)
        panel = wx.Panel(scrolled, name="%s properties"%name)
        if wx.VERSION[0]<3:
            panel.SetBackgroundColour(scrolled.GetBackgroundColour())
        return panel

    def end_page(self, panel, sizer, header, select=False):
        sizer.AddSpacer(30)
        panel.SetAutoLayout(1)
        panel.SetSizer(sizer)
        sizer.Layout()
        sizer.Fit(panel)

        scrolled = panel.GetParent()
        self.notebook.AddPage(scrolled, _(header),select=select)
        self._set_page_size(scrolled)

    def _set_page_size(self, scrolled):
        # set ScrolledWindow and Panel to available size; enable scrolling, if required
        # gets available size for notebook pages
        ws, hs = self.notebook.GetSize()
        ws -= self._notebook_decoration_size[0]
        hs -= self._notebook_decoration_size[1]
        w_scrollbar = wx.SystemSettings.GetMetric(wx.SYS_VSCROLL_X)  # width a of a scrollbar

        panel = [w for w in scrolled.GetChildren() if isinstance(w, wx.Panel)][0]
        szr = panel.GetSizer()
        if not szr: return
        wm, hm = szr.GetMinSize()
        if hs<hm:
            # best size is smaller than the available height -> enable scrolling
            scrolled.SetScrollbars(1, 5, 1, int(math.ceil(hm/5.0)))
            panel.SetSize( (ws-w_scrollbar, hm) )
        else:
            panel.SetSize( (ws, hs) )

    def on_notebook_size(self, event):
        # calculate available size for pages
        if self._notebook_decoration_size:
            for p in range( self.notebook.GetPageCount() ):
                self._set_page_size( self.notebook.GetPage(p) )
        if event: event.Skip()

    def on_panel_size(self, event):
        # when the dummy panel receives a size event, we know that things are ready to calculate the notebook pages size
        # calculate decoration size from the dummy panel that was added initially
        if event.GetSize() != (0,0):
            dummy_panel = self.notebook.GetPage(0)
            wp, hp = dummy_panel.GetSize()    # page/panel size
            wn, hn = self.notebook.GetSize()  # notebook size
            self._notebook_decoration_size = (wn-wp, hn-hp)
            self.notebook.RemovePage(0)
            compat.DestroyLater(dummy_panel)
        else:
            # Mac OS: inital event on creation
            event.Skip()


# don't use any more for application; causes crashes on Cent OS 7; still used when testing
class wxGladeArtProvider(wx.ArtProvider):
    def CreateBitmap(self, artid, client, size):
        if wx.Platform == '__WXGTK__' and artid == wx.ART_FOLDER:
            return wx.Bitmap(os.path.join(config.icons_path, 'closed_folder.png'))
        return wx.NullBitmap


class wxGladePalettePanel(wx.Panel):

    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        common.palette = self # for building the buttons
        self.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE) )

        # load the available code generators
        all_widgets = common.init_codegen()
        if not config.use_gui: return
        self.all_togglebuttons = []  # used by reset_togglebuttons

        # for keyboard navigation:
        self._id_to_coordinate = {}
        self._ids_by_row = []
        self._section_to_row = {}

        # build the palette for all_widgets
        sizer = wx.FlexGridSizer(0, 2, 0, 0)
        maxlen = max([len(all_widgets[sect]) for sect in all_widgets])  # the maximum number of buttons in a section
        for row, section in enumerate(all_widgets):
            self._section_to_row[section] = row
            self._ids_by_row.append([])
            if section:
                label = wx.StaticText(self, -1, "%s:" % section.replace('&', '&&'))
                sizer.Add( label, 1, wx.ALIGN_CENTER_VERTICAL | wx.LEFT, 2 )
            bsizer = wx.BoxSizer()
            for col, button in enumerate(all_widgets[section]):
                self._ids_by_row[-1].append(button.Id)
                self._id_to_coordinate[button.Id] = (row,col)
                bsizer.Add(button, flag=wx.ALL, border=1)
                if isinstance(button, wx.ToggleButton):
                    self.all_togglebuttons.append(button)
            sizer.Add(bsizer)
        self.SetSizer(sizer)
        # on platforms other than Windows, we'll set the ToggleButton background colour to indicate the selection
        if wx.Platform == "__WXMSW__":
            self._highlight_colour = None
        else:
            self._highlight_colour = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHT)
        self.Bind(wx.EVT_CHAR_HOOK, self.on_char)

    def reset_togglebuttons(self, keep=None):
        # un-toggle all buttons except keep
        for button in self.all_togglebuttons:
            if keep is not None and button is keep:
                if self._highlight_colour:
                    button.SetBackgroundColour(self._highlight_colour)
                continue
            if self._highlight_colour and button.GetBackgroundColour()==self._highlight_colour:
                button.SetBackgroundColour(wx.NullColour)
            if button.GetValue(): button.SetValue(False)

    def on_char(self, event):
        key = (event.GetKeyCode(), event.GetModifiers())    # modifiers: 1,2,4 for Alt, Ctrl, Shift
        if key[1]: return event.Skip()

        focused = self.FindFocus()
        if not focused or not focused.Id in self._id_to_coordinate:
            return event.Skip()

        row, col = self._id_to_coordinate[focused.Id]
        new_row = new_col = None
        if key[0]==wx.WXK_UP:
            if row>0: new_row = row-1
        elif key[0]==wx.WXK_DOWN:
            if row < len(self._ids_by_row)-1: new_row = row+1
        elif key[0]==wx.WXK_LEFT:
            if col>0: new_col = col-1
        elif key[0]==wx.WXK_RIGHT:
            if col < len(self._ids_by_row[row])-1: new_col = col+1
        elif key[0]==wx.WXK_HOME:
            new_col = 0
        elif key[0]==wx.WXK_END:
            new_col = len(self._ids_by_row[row])-1
        elif key[0]==wx.WXK_PAGEUP:
            new_row = 0
        elif key[0]==wx.WXK_PAGEDOWN:
            new_row = len(self._ids_by_row)-1
        elif (ord("A") <= key[0] <= ord("Z")) and chr(key[0]) in misc.palette_hotkeys:
            section = misc.palette_hotkeys[chr(key[0])]
            new_row = self._section_to_row[section]
            new_col = 0
        else:
            return event.Skip()

        if new_row is None and new_col is None:
            # limits hit
            wx.Bell()
        else:
            if new_col is None: new_col = min(col, len(self._ids_by_row[new_row])-1)
            if new_row is None: new_row = row
            focus = self.FindWindowById(self._ids_by_row[new_row][new_col])
            if focus: focus.SetFocus()

import shell_frame
class ShellFrame(shell_frame.ShellFrame):
    def on_btn_assign(self, event):
        # insert a variable assignment
        widget = misc.focused_widget
        if not widget:
            event.Skip()
            return
        path = widget.get_path()
        command = 'widget = common.root.find_widget_from_path("%s")\r\n'%path
        #self.shell.push(command) # or .write ?
        self.shell.write(command)


class wxGladeFrame(wx.Frame):
    "Main frame of wxGlade"
    def __init__(self):
        version = config.version
        pos, size, layout = self.init_layout_settings()
        wx.Frame.__init__(self, None, -1, "wxGlade v%s" % version, pos=pos, size=size,
                          style=wx.DEFAULT_FRAME_STYLE, name='MainFrame')

        common.main = self
        self._set_icon()
        self.create_menu()
        self.create_toolbar()

        style = wx.SP_3D | wx.SP_LIVE_UPDATE
        self.splitter1 = wx.SplitterWindow(self, style=style)
        self.splitter2 = wx.SplitterWindow(self.splitter1, style=style)
        self.palette = wxGladePalettePanel(self.splitter2)

        # create the property and the tree frame
        common.property_panel = self.property_panel = wxGladePropertyPanel(self.splitter2)
        common.root = app = application.Application()
        common.app_tree = self.tree = WidgetTree(self.splitter1, app)

        self.splitter1.SplitVertically(self.splitter2, self.tree)
        self.splitter2.SplitHorizontally(self.palette, self.property_panel)

        self.switch_layout(layout, initial=True)

        # last visited directory, used on GTK for wxFileDialog
        self.cur_dir = config.preferences.open_save_path

        # set a drop target for us...
        self._droptarget = FileDropTarget(self)
        self.SetDropTarget(self._droptarget)

        self.create_statusbar()  # create statusbar for display of messages

        self.Show()

        w, h = self.GetSize()
        if w>size[0] or h>size[1]:
            # can happen on e.g. Linux due to menu/tool/status bar
            new_size = ( 2*size[0] - w, 2*size[1] - h )
            self.SetSize(new_size)

        #misc.set_focused_widget(common.root)
        self.Bind(wx.EVT_CLOSE, self.on_close)

        # disable autosave checks during unittests
        if config.testing: return
        self.init_autosave()
        self.check_autosaved()

        self.Bind(wx.EVT_CHAR_HOOK, self.on_char_hook)
        if config.debugging:
            self.splitter1.Bind(wx.EVT_SPLITTER_SASH_POS_CHANGED, self.on_sash)
            self.splitter2.Bind(wx.EVT_SPLITTER_SASH_POS_CHANGED, self.on_sash)

    def on_sash(self, event):
        # XXX not yet used, but it could be used to re-format the palette panel
        layout = self.layout_settings["layout"]
        if layout==0:
            size = (self.splitter1.GetSashPosition(), self.splitter2.GetSashPosition())
        elif layout==1:
            size = (self.splitter2.GetSashPosition(), self.splitter1.GetSashPosition())
        elif layout==2:
            size = (self.GetClientSize()[0], self.splitter2.GetSashPosition())

    def on_char_hook(self, event):
        # bound to EVT_CHAR_HOOK
        focus = parent = self.FindFocus()
        grid = None  # will be set if a grid or a grid's child is focused
        window_type = None
        while parent:
            # go up and identify parent: Palette, Property or Tree
            if isinstance(parent, wx.grid.Grid):
                grid = parent
            if parent is self.palette:
                window_type = "palette"
            elif parent is self.tree:
                window_type = "tree"
            elif parent is self.property_panel:
                window_type = "properties"
            if window_type: break
            parent = parent.GetParent()

        # forward to specific controls / properties? (on wx 2.8 installing EVT_CHAR_HOOK on controls does not work)
        if window_type=="properties" and grid and grid.Name!="grid":
            # forward event to grid property?
            if misc.focused_widget.properties[grid.Name].on_char(event):
                return
        if window_type=="tree":
            if common.app_tree.on_char(event):
                return

        # global handler
        misc.handle_key_event(event, window_type)

    def set_widget(self, widget):
        # update redo/repeat tools and menus
        if not common.history: return
        redo_state = (common.history.can_undo, common.history.can_redo, common.history.can_repeat)
        if self._previous_redo_state == redo_state:
            return
        self._menu_undo.Enable(common.history.can_undo)
        self._menu_redo.Enable(common.history.can_redo)
        self._menu_repeat.Enable(common.history.can_repeat)
        if self._tool_redo:
            self._tool_undo.Enable(common.history.can_undo)
            self._tool_redo.Enable(common.history.can_redo)
            self._tool_repeat.Enable(common.history.can_repeat)
            self.toolbar.Realize()
        self._previous_redo_state = redo_state

    # menu and actions #################################################################################################
    def create_menu(self):
        self._previous_redo_state = None
        menu_bar = wx.MenuBar()

        compat.wx_ToolTip_SetDelay(1000)
        compat.wx_ToolTip_SetAutoPop(30000)

        append_menu_item = misc.append_menu_item

        # File menu
        file_menu = wx.Menu(style=wx.MENU_TEAROFF)

        NEW = append_menu_item(file_menu, -1, _("&New\tCtrl+N"), wx.ART_NEW)
        misc.bind_menu_item(self, NEW, self.new_app)

        item = append_menu_item(file_menu, -1, _("New from &Template...\tShift+Ctrl+N"))
        misc.bind_menu_item(self, item, self.new_app_from_template)

        OPEN = append_menu_item(file_menu, -1, _("&Open...\tCtrl+O"), wx.ART_FILE_OPEN)
        misc.bind_menu_item(self, OPEN, self.open_app)

        SAVE = append_menu_item(file_menu, -1, _("&Save\tCtrl+S"), wx.ART_FILE_SAVE)
        misc.bind_menu_item(self, SAVE, self.save_app)

        SAVE_AS = append_menu_item(file_menu, -1, _("Save As..."), wx.ART_FILE_SAVE_AS)
        misc.bind_menu_item(self, SAVE_AS, self.save_app_as)

        item = append_menu_item(file_menu, -1, _("Save As Template..."))
        misc.bind_menu_item(self, item, self.save_app_as_template)

        file_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        GENERATE_CODE = append_menu_item(file_menu, -1, _("&Generate Code\tCtrl+G"), wx.ART_EXECUTABLE_FILE)
        misc.bind_menu_item(self, GENERATE_CODE, lambda: common.root.generate_code())

        file_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        item = append_menu_item(file_menu, -1, _("&Import from XRC..."))
        misc.bind_menu_item(self, item, self.import_xrc)

        file_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        EXIT = append_menu_item(file_menu, -1, _('E&xit\tCtrl+Q'), wx.ART_QUIT)
        misc.bind_menu_item(self, EXIT, self.Close)

        menu_bar.Append(file_menu, _("&File"))


        # Edit menu ====================================================================================================
        edit_menu = wx.Menu(style=wx.MENU_TEAROFF)

        # these menu items will be updated
        self._menu_undo = item = append_menu_item(edit_menu, -1, _('Un-do\tCtrl+Z'),
                                                  helpString="Un-do the last property modification on another widget")
        misc.bind_menu_item(self, item, lambda: common.history.undo(misc.focused_widget))

        self._menu_redo = item = append_menu_item(edit_menu, -1, _('Re-do\tCtrl+Y'),
                                                  helpString="Re-do the last property modification on another widget")
        misc.bind_menu_item(self, item, lambda: common.history.repeat(misc.focused_widget))

        self._menu_repeat = item = append_menu_item(edit_menu, -1, _('Repeat\tCtrl-R'),
          helpString="Repeat the last property modifications on another widget (multiple modifications, if applicable)")
        misc.bind_menu_item(self, item, lambda: common.history.repeat(misc.focused_widget))

        edit_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        item = append_menu_item(edit_menu, -1, _('Template Manager...'))
        misc.bind_menu_item(self, item, self.manage_templates)

        item = append_menu_item(edit_menu, wx.ID_PREFERENCES, _('Preferences...'), "prefs.png")
        misc.bind_menu_item(self, item, self.edit_preferences)

        menu_bar.Append(edit_menu, _("&Edit"))

        # Windows menu: layout and focus ===============================================================================
        view_menu = wx.Menu(style=wx.MENU_TEAROFF)

        i = append_menu_item(view_menu, -1, _("Layout &1: Tree\tAlt-1"), "../layout1.png")
        misc.bind_menu_item(self, i, self.switch_layout, 0)
        
        i = append_menu_item(view_menu, -1, _("Layout &2: Properties\tAlt-2"), "../layout2.png")
        misc.bind_menu_item(self, i, self.switch_layout, 1)

        i = append_menu_item(view_menu, -1, _("Layout &3: Narrow\tAlt-3"), "../layout3.png")
        misc.bind_menu_item(self, i, self.switch_layout, 2)
        view_menu.AppendSeparator()

        i = append_menu_item(view_menu, -1, _("Focus &Tree\tF2"))
        misc.bind_menu_item(self, i, self.show_tree)

        i = append_menu_item(view_menu, -1, _("Focus &Properties\tF3"))
        misc.bind_menu_item(self, i, self.show_props_window )

        i = append_menu_item(view_menu, -1, _("Focus Pa&lette\tF4"))
        misc.bind_menu_item(self, i, self.show_palette )

        # submenu focus sections >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        view_props_menu = wx.Menu()
        # randomly select set of shortcuts to be displayed:
        if int(math.ceil(time.time())) % 2:
            shortcuts = ["F8", "F9", "F10", "F11", "F12"]
        else:
            shortcuts = ["Ctrl-M", "Ctrl-L", "Ctrl-W", "Ctrl-E", "Ctrl-D"]
        for sc, section in zip(shortcuts, ("Common", "Layout", "Widget", "Events", "Code")):
            i = append_menu_item(view_props_menu, -1, _("Focus &%s\t%s"%(section, sc)))
            misc.bind_menu_item(self, i, self.show_props_window, section)
        view_menu.AppendSubMenu(view_props_menu, _("Focus Properties &Section"))
        view_menu.AppendSeparator() # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        i = append_menu_item(view_menu, -1, _("Show/Hide &Design\tF6"))
        misc.bind_menu_item(self, i, self.show_design_window)
        self._m_pin_design_window = i = append_menu_item(view_menu, -1, _("&Pin &Design\tCtrl-P"),  kind=wx.ITEM_CHECK)
        misc.bind_menu_item(self, i, self.pin_design_window)
        view_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        item = append_menu_item(view_menu, wx.ID_REFRESH, _("&Refresh Preview\tF5"), "refresh.png")
        misc.bind_menu_item(self, item, self.preview)

        menu_bar.Append(view_menu, _("&Windows"))

        # Help menu ====================================================================================================
        help_menu = wx.Menu(style=wx.MENU_TEAROFF)

        MANUAL = append_menu_item(help_menu, -1, _('Manual\tF1'), wx.ART_HELP_BOOK)
        misc.bind_menu_item(self, MANUAL, self.show_manual)
        #item = append_menu_item(help_menu, -1, _('Tutorial'))
        #misc.bind_menu_item(self, item, self.show_tutorial)
        help_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        i = append_menu_item(help_menu, -1, _('Mailing list'))
        misc.bind_menu_item(self, i, self.show_mailing_list)
        i = append_menu_item(help_menu, -1, _('Bug tracker'))
        misc.bind_menu_item(self, i, self.show_bug_tracker)
        i = append_menu_item(help_menu, -1, _('Releases'))
        misc.bind_menu_item(self, i, self.show_releases)
        help_menu.AppendSeparator() # ----------------------------------------------------------------------------------

        if config.debugging:
            i = append_menu_item(help_menu, -1, 'Shell\tF7')
            misc.bind_menu_item(self, i, self.create_shell_window)
            help_menu.AppendSeparator()

        item = append_menu_item(help_menu, wx.ID_ABOUT, _('About'), wx.ART_INFORMATION)
        misc.bind_menu_item(self, item, self.show_about_box)

        menu_bar.Append(help_menu, _('&Help'))

        self.SetMenuBar(menu_bar)
        # Mac tweaks...
        if wx.Platform == "__WXMAC__":
            if compat.IS_PHOENIX:
                wx.PyApp.SetMacAboutMenuItemId(wx.ID_ABOUT)
                wx.PyApp.SetMacPreferencesMenuItemId(wx.ID_PREFERENCES)
                wx.PyApp.SetMacExitMenuItemId(wx.ID_EXIT)
                wx.PyApp.SetMacHelpMenuTitleName(_('&Help'))
            else:
                wx.App_SetMacAboutMenuItemId(wx.ID_ABOUT)
                wx.App_SetMacPreferencesMenuItemId(wx.ID_PREFERENCES)
                wx.App_SetMacExitMenuItemId(wx.ID_EXIT)
                wx.App_SetMacHelpMenuTitleName(_('&Help'))

        # file history support
        num_entries = config.preferences.number_history
        self.file_history = wx.FileHistory(num_entries)
        self.file_history.UseMenu(file_menu)
        files = common.load_file_history()
        files.reverse()
        for path in files:
            self.file_history.AddFileToHistory(path.strip())

        self.Bind(wx.EVT_MENU_RANGE, self.open_from_history, id=wx.ID_FILE1, id2=wx.ID_FILE1+num_entries-1)

    def _add_label_tool(self, tb, size, id, label, bmp, itemtype, msg, msg_long=None):
        ADD = tb.AddLabelTool  if compat.IS_CLASSIC else  tb.AddTool
        if compat.IS_PHOENIX:
            method = getattr(tb, "AddTool")
        else:
            method = getattr(tb, "AddLabelTool")

        if isinstance(bmp, str) and not bmp.startswith("wxART_"):
            bmp = wx.Bitmap( os.path.join(config.icons_path, bmp) )
        else:
            # a wx.ART_... constant
            bmp = wx.ArtProvider.GetBitmap(bmp, wx.ART_TOOLBAR, size)
        return ADD(-1, _(label), bmp, wx.NullBitmap, itemtype, _(msg), _(msg_long or msg))

    def create_toolbar(self):
        # new, open, save, generate, add, delete, re-do,  Layout 1, 2, 3,  pin,    help
        #   insert slot/page?
        #   Layout: Alt + 1,2,3
        
        self.toolbar = tb = wx.ToolBar(self, -1)
        self.SetToolBar(tb)
        size = (21,21)
        add = functools.partial(self._add_label_tool, tb, size)
        t = add( wx.ID_NEW, "New", wx.ART_NEW, wx.ITEM_NORMAL, "Start a new file (Ctrl+N)")
        self.Bind(wx.EVT_TOOL, self.new_app, t)
        
        t = add( wx.ID_OPEN, "Open", wx.ART_FILE_OPEN, wx.ITEM_NORMAL, "Open an existing file (Ctrl+O)")
        self.Bind(wx.EVT_TOOL, self.open_app, t)
        
        t = add( wx.ID_SAVE, "Save", wx.ART_FILE_SAVE, wx.ITEM_NORMAL, "Save file (Ctrl+S)")
        self.Bind(wx.EVT_TOOL, self.save_app, t)

        if config.debugging and hasattr(wx, "ART_PLUS"):
            t = add( -1, "Add", wx.ART_PLUS, wx.ITEM_NORMAL, "Add widget (Ctrl+A)")
            t.Enable(False)

            # XXX switch between wx.ART_DELETE for filled slots and wx.ART_MINUS for empty slots
            t = add( -1, "Remove", wx.ART_MINUS, wx.ITEM_NORMAL, "Add widget (Ctrl+A)")
            t.Enable(False)

            tb.AddSeparator()

        self._tool_undo = t = add( wx.ID_UNDO, "Un-do", wx.ART_UNDO, wx.ITEM_NORMAL, "Un-do (Ctrl+Z)" )
        t.Enable(False)
        self.Bind(wx.EVT_TOOL, lambda evt: common.history.undo(misc.focused_widget), t)

        self._tool_redo = t = add( wx.ID_REDO, "Re-do", wx.ART_REDO, wx.ITEM_NORMAL, "Re-do (Ctrl+Y)" )
        t.Enable(False)
        self.Bind(wx.EVT_TOOL, lambda evt: common.history.redo(misc.focused_widget), t)

        self._tool_repeat = t = add( -1, "Repeat", wx.ART_REDO, wx.ITEM_NORMAL, "Repeat (Ctrl+R)" )
        t.Enable(False)
        self.Bind(wx.EVT_TOOL, lambda evt: common.history.repeat(misc.focused_widget), t)

        tb.AddSeparator()
        t = add(-1, "Generate Code", wx.ART_EXECUTABLE_FILE, wx.ITEM_NORMAL, "Generate Code (Ctrl+G)" )
        self.Bind(wx.EVT_TOOL, lambda event: common.root.generate_code(), t)
        tb.AddSeparator()
        
        t1 = add(-1, "Layout 1", "layout1.png", wx.ITEM_RADIO, "Switch layout: Tree", 
                                                               "Switch layout: Palette and Properties left, Tree right")
        self.Bind(wx.EVT_TOOL, lambda event: self.switch_layout(0), t1)
        t2 = add(-1, "Layout 2", "layout2.png", wx.ITEM_RADIO,"Switch layout: Properties",
                                                              "Switch layout: Palette and Tree top,  Properties bottom") 
        self.Bind(wx.EVT_TOOL, lambda event: self.switch_layout(1), t2)
        t3 = add(-1, "Layout 3", "layout3.png", wx.ITEM_RADIO, "Switch layout: narrow",
                                                     "Switch layout: Palette, Tree and Properties on top of each other")
        self.Bind(wx.EVT_TOOL, lambda event: self.switch_layout(2), t3)
        self._layout_tools = [t1,t2,t3]

        tb.AddSeparator()
        t = add(-1, "Pin Design Window", "pin_design.png", wx.ITEM_CHECK, "Pin Design Window",
                                                                          "Pin Design Window to stay on top")
        self.Bind(wx.EVT_TOOL, lambda event: self.pin_design_window(), t)
        self._t_pin_design_window = t

        tb.AddSeparator()

        t = add(wx.ID_HELP, "Help", wx.ART_HELP_BOOK, wx.ITEM_NORMAL, "Show manual (F1)")
        self.Bind(wx.EVT_TOOL, self.show_manual, t)

        self.toolbar.Realize()

    def init_autosave(self):
        # ALB 2004-10-15, autosave support...
        self.autosave_timer = None
        if not config.preferences.autosave: return
        self.autosave_timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.on_autosave_timer, self.autosave_timer)
        self.autosave_timer.Start( int(config.preferences.autosave_delay) * 1000 )

    def on_autosave_timer(self, event):
        res = common.autosave_current()
        if res == 2:
            self.user_message(_("Auto saving... done"))
        elif not res:
            self.autosave_timer.Stop()
            config.preferences.autosave = False
            logging.info(_('Disable autosave function permanently'))
            wx.MessageBox(
                _('The autosave function failed. It has been disabled\n'
                  'permanently due to this error. Use the preferences\n'
                  'dialog to re-enable this functionality.\n'
                  'The details have been written to the wxGlade log file\n\n'
                  'The log file is: %s' % config.log_file ),
                _('Autosave Failed'), wx.OK | wx.CENTRE | wx.ICON_ERROR )

    def check_autosaved(self):
        if not common.check_autosaved(None): return
        res = wx.MessageBox(
            _('There seems to be auto saved data from last wxGlade session: do you want to restore it?'),
            _('Auto save detected'), style=wx.ICON_QUESTION | wx.YES_NO)
        if res == wx.YES:
            filename = common.get_name_for_autosave()
            if self._open_app(filename, add_to_history=False):
                self.cur_dir = os.path.dirname(filename)
                common.root.saved = False
                common.root.filename = None
                self.user_message(_('Auto save loaded'))
        common.remove_autosaved()


    def edit_preferences(self):
        dialog = preferencesdialog.wxGladePreferences(config.preferences)
        if dialog.ShowModal() == wx.ID_OK:
            wx.MessageBox( _('Changes will take effect after wxGlade is restarted'),
                           _('Preferences saved'), wx.OK|wx.CENTRE|wx.ICON_INFORMATION )
            dialog.set_preferences()
        else:
            dialog.canceled()
        dialog.Destroy()

    def _get_toplevel(self):
        # return the toplevel for a preview or design window
        if misc.focused_widget and not isinstance(misc.focused_widget, application.Application):
            # a widget is selected, find the toplevel window for it
            return misc.focused_widget.toplevel_parent
        # find main toplevel window
        return common.root._get_top_window()

    def preview(self):
        """Generate preview of the current loaded project.
        
        A preview can be triggered by keyboard shortcut or by pressing the preview button.
        The preview can be triggered for all selected widgets.
        This doesn't mean that the widget is opened for editing."""
        toplevel = self._get_toplevel()
        if toplevel is not None:
            toplevel.preview(refresh=True)

    def show_palette(self):
        if self.IsIconized(): self.Iconize(False)
        self.palette.SetFocus()

    def show_tree(self):
        if self.IsIconized(): self.Iconize(False)
        common.app_tree.SetFocus()

    def show_props_window(self, section=None):
        # XXX implement: if a section is active already, then go to first property of the page
        if not self.property_panel.notebook: return
        if self.IsIconized(): self.Iconize(False)
        self.property_panel.pagenames
        if not section:
            self.property_panel.notebook.SetFocus()
        else:
            if not section in self.property_panel.pagenames:
                return
            i = self.property_panel.pagenames.index(section)
            if self.property_panel.notebook.GetSelection() != i:
                self.property_panel.notebook.ChangeSelection(i)
            else:
                self.property_panel.notebook.SetFocus()
                # try to set the focus if the widget has changed; this is not yet implemented for many property types
                widget = self.property_panel.current_widget
                if ( widget and (widget is not np.current_property.owner) and
                     section in widget.PROPERTIES ):
                    i_p = widget.PROPERTIES.index(section) + 1
                    if i_p<len(widget.PROPERTIES):
                        prop = widget.properties.get(widget.PROPERTIES[i_p])
                        if prop: prop.set_focus()
        self.Raise()

    def show_design_window(self):
        toplevel = self._get_toplevel()
        if not toplevel: return

        if toplevel.widget:
            focus = toplevel.widget.FindFocus()
            focused = focus and focus.GetTopLevelParent() is toplevel.widget.GetTopLevelParent()

        if toplevel.widget and toplevel.widget.IsShownOnScreen() and not focused:
            # just raise it
            if toplevel.widget.GetTopLevelParent().IsIconized():
                toplevel.widget.GetTopLevelParent().Iconize(False)
            toplevel.widget.GetTopLevelParent().Raise()
            return
        # open or close
        common.app_tree.show_toplevel(None, editor=toplevel)

    def pin_design_window(self):
        common.pin_design_window = not common.pin_design_window
        if common.pin_design_window != self._t_pin_design_window.IsToggled():
            self._t_pin_design_window.Toggle()
            self.toolbar.Realize()
        self._m_pin_design_window.Check(common.pin_design_window)

        toplevel = self._get_toplevel()
        if not toplevel or not toplevel.widget: return
        frame = toplevel.widget.GetTopLevelParent()
        if not isinstance(frame, wx.Frame): return
        style = frame.GetWindowStyle()
        if common.pin_design_window:
            frame.SetWindowStyle( style | wx.STAY_ON_TOP)
        elif style & wx.STAY_ON_TOP:
            frame.ToggleWindowStyle(wx.STAY_ON_TOP)
            if wx.Platform=='__WXMSW__':
                frame.Iconize(True)
                frame.Iconize(False)
            else:
                toplevel.widget.Raise()

    def create_shell_window(self):
        common.shell = ShellFrame(None)
        if misc.focused_widget: common.shell.txt_path.SetValue( misc.focused_widget.get_path() )
        common.shell.Show()

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
            if msg:
                self.clear_sb_timer.Start(5000, True)

    def on_clear_sb_timer(self, event):
        sb = self.GetStatusBar()
        if sb: sb.SetStatusText("")
    ####################################################################################################################

    def ask_save(self):
        """checks whether the current app has changed and needs to be saved:
        if so, prompts the user;
        returns False if the operation has been cancelled"""
        if not common.root.saved:
            ok = wx.MessageBox(_("Save changes to the current app?"),
                               _("Confirm"), wx.YES_NO|wx.CANCEL|wx.CENTRE|wx.ICON_QUESTION)
            if ok == wx.YES:
                self.save_app()
            return ok != wx.CANCEL
        return True

    def new_app(self, event=None):
        "creates a new wxGlade project"
        if not self.ask_save(): return
        common.root.clear()
        common.root.new()
        common.root.filename = None
        self.user_message("")
        misc.rebuild_tree(common.root)
        common.root.saved = True
        common.remove_autosaved()
        if common.history: common.history.reset()
        if config.preferences.autosave and self.autosave_timer is not None:
            self.autosave_timer.Start()

    def new_app_from_template(self):
        "creates a new wxGlade project from an existing template file"
        if not self.ask_save(): return
        infile = template.select_template()
        if infile:
            self._open_app(infile, add_to_history=False)
            common.root.template_data = None

    def open_app(self, event=None):
        """loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :) """
        if not self.ask_save():
            return
        default_path = os.path.dirname(common.root.filename or "") or self.cur_dir
        infile = wx.FileSelector(_("Open file"),
                                   wildcard="wxGlade files (*.wxg)|*.wxg|wxGlade Template files (*.wgt)|*.wgt|"
                                            "XML files (*.xml)|*.xml|All files|*",
                                   flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST, default_path=default_path)
        if not infile: return
        self._open(infile)

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
        self._open(filename)

    def _open(self, filename):
        # called by open_app and open_from_history
        if common.check_autosaved(filename):
            res = wx.MessageBox( _('There seems to be auto saved data for this file: do you want to restore it?'),
                                 _('Auto save detected'), style=wx.ICON_QUESTION | wx.YES_NO )
            if res == wx.YES:
                common.restore_from_autosaved(filename)
            else:
                common.remove_autosaved(filename)
        else:
            common.remove_autosaved(filename)

        path = position = None
        if filename == common.root.filename:
            # if we are re-loading the file, store path and position
            if misc.focused_widget:
                path = misc.focused_widget.get_path()
                if misc.focused_widget.widget is not None and not misc.focused_widget.IS_ROOT:
                    toplevel = misc.get_toplevel_parent(misc.focused_widget.widget)
                    if toplevel: position = toplevel.GetPosition()

        self._open_app(filename)
        self.cur_dir = os.path.dirname(filename)
        if not path: return
        editor = common.root.find_widget_from_path(path)
        if not editor: return
        misc.set_focused_widget(editor)
        if editor is common.root: return
        editor.toplevel_parent.create()

        common.app_tree.ExpandAllChildren(editor.item)

        if not position or not editor.widget: return
        misc.get_toplevel_parent(editor.widget).SetPosition(position)

    def _open_app(self, filename, use_progress_dialog=True, add_to_history=True):
        "Load a new wxGlade project"
        error_msg = None
        infile = None

        start = time.time()

        common.root.clear()
        common.root.init()
        common.app_tree.DeleteChildren(common.root.item)
        common.app_tree.auto_expand = False  # disable auto-expansion of nodes

        try:
            try:
                logging.info( _('Read wxGlade project from file "%s"'), filename )
                input_file_version = None

                if not isinstance(filename, list):
                    common.root.filename = filename
                    # decoding will done automatically by SAX XML library
                    if compat.PYTHON2:
                        infile = open(filename)
                    else:
                        infile = open(filename, "r", encoding="UTF8")
                    if hasattr(infile, "seek"):
                        # try to read file version number from the first few lines
                        import re
                        version_re = re.compile(r"<!-- generated by wxGlade (\d+)\.(\d+)\.(\d+)(\S*)\s*")
                        for n in range(3):
                            match = version_re.match( infile.readline() )
                            if match:
                                major, minor, sub, extension = match.groups()
                                input_file_version = (int(major), int(minor), int(sub), extension)
                                break
                        infile.seek(0)

                else:
                    common.root.filename = None

                if use_progress_dialog and config.preferences.show_progress:
                    p = ProgressXmlWidgetBuilder(filename, input_file_version, input_file=infile)
                else:
                    p = XmlWidgetBuilder(filename, input_file_version)

                if infile is not None:
                    p.parse(infile)
                else:
                    p.parse_string(filename)
                    filename = None
            except (EnvironmentError, SAXParseException, XmlParsingError) as msg:
                if config.debugging: raise
                if infile is not None:
                    error_msg = _("Error loading file %s:\n%s") % (misc.wxstr(filename), misc.wxstr(msg))
                else:
                    error_msg = _("Error loading from a file-like object:\n%s") % misc.wxstr(msg)
            except Exception as inst:
                if config.debugging: raise
                if filename and not isinstance(filename, list):
                    fn = os.path.basename(filename).encode('ascii','replace')
                    msg = _('loading file "%s"') % fn
                else:
                    msg = _('loading from a file-like object')
                bugdialog.Show(msg, inst)
        finally:
            if infile and filename:
                infile.close()

            if error_msg:
                common.root.clear()
                common.root.new()
                common.root.saved = True
                if common.history: common.history.reset()
                common.app_tree.auto_expand = True  # re-enable auto-expansion of nodes

                wx.MessageBox(error_msg, _('Error'), wx.OK | wx.CENTRE | wx.ICON_ERROR)

                return False

        misc.rebuild_tree(common.root, freeze=True)

        common.app_tree.auto_expand = True  # re-enable auto-expansion of nodes

        common.app_tree.Expand(common.root.item)
        if common.root.is_template:
            logging.info(_("Template loaded"))
            common.root.template_data = template.Template(filename)
            common.root.filename = None

        end = time.time()
        logging.info(_('Loading time: %.5f'), end - start)

        common.root.saved = True
        if common.history: common.history.reset()
        #common.property_panel.Raise()

        if hasattr(self, 'file_history') and filename is not None and add_to_history and \
           (not common.root.is_template):
            self.file_history.AddFileToHistory(misc.wxstr(filename))

        if config.preferences.autosave and self.autosave_timer is not None:
            self.autosave_timer.Start()

        duration = end - start
        if filename:
            self.user_message( _("Loaded %s in %.2f seconds") % (misc.wxstr(os.path.basename(filename)), duration) )
        else:
            self.user_message( _("Loaded in %.2f seconds") % duration )

        return True

    def save_app(self, event=None):
        "saves a wxGlade project onto an xml file"
        np.flush_current_property()
        if not common.root.filename or common.root.is_template:
            self.save_app_as()
        else:
            # check whether we are saving a template
            ext = os.path.splitext(common.root.filename)[1].lower()
            if ext == ".wgt":
                common.root.is_template = True
            self._save_app(common.root.filename)

    def _save_app(self, filename):
        try:
            obuffer = []
            common.root.write(obuffer)
            common.save_file(filename, obuffer, 'wxg')
        except EnvironmentError as inst:
            if config.debugging: raise
            common.root.saved = False
            bugdialog.ShowEnvironmentError(_('Saving this project failed'), inst)
        except Exception as inst:
            if config.debugging: raise
            common.root.saved = False
            fn = os.path.basename(filename).encode('ascii', 'replace')
            bugdialog.Show(_('Save File "%s"') % fn, inst)
        else:
            common.root.saved = True
            common.remove_autosaved()
            if config.preferences.autosave and self.autosave_timer is not None:
                self.autosave_timer.Start()
            self.user_message( _("Saved %s") % os.path.basename(filename) )

    def save_app_as(self):
        "saves a wxGlade project onto an xml file chosen by the user"
        if common.root.filename:
            default_path, default_filename = os.path.split(common.root.filename)
        else:
            default_path, default_filename = self.cur_dir, "wxglade.wxg"
        fn = wx.FileSelector( _("Save project as..."),
                              wildcard="wxGlade files (*.wxg)|*.wxg|wxGlade Template files (*.wgt) |*.wgt|"
                              "XML files (*.xml)|*.xml|All files|*",
                              flags=wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT,
                              default_path=default_path,
                              default_filename=default_filename)
        if not fn: return

        # check for file extension and add default extension if missing
        ext = os.path.splitext(fn)[1].lower()
        if not ext:
            fn = "%s.wxg" % fn

        common.root.filename = fn
        #remove the template flag so we can save the file.
        common.root.properties["is_template"].set(False)

        self.save_app()
        self.cur_dir = os.path.dirname(fn)
        self.file_history.AddFileToHistory(fn)

    def save_app_as_template(self):
        "save a wxGlade project as a template"
        data = getattr(common.root, 'template_data', None)
        outfile, data = template.save_template(data)
        if outfile:
            common.root.properties["is_template"].set(True)
            common.root.template_data = data
            self._save_app(outfile)

    def on_close(self, event):
        if not event.CanVeto():
            event.Skip
            return
        if self.ask_save():
            # close application
            # first, let's see if we have to save the geometry...
            prefs = config.preferences
            if prefs.remember_geometry:
                self._store_layout()
                prefs.set_dict("layout", self.layout_settings)
                prefs.changed = True
            common.root.clear()
            common.root.new()
            try:
                common.save_preferences()
            except Exception as e:
                wx.MessageBox( _('Error saving preferences:\n%s') % e,
                               _('Error'), wx.OK|wx.CENTRE|wx.ICON_ERROR )
            self.Destroy()
            common.remove_autosaved()
            wx.CallAfter(wx.GetApp().ExitMainLoop)
        elif event.CanVeto():
            event.Veto()

    def show_about_box(self):
        "show the about dialog;  see: about.wxGladeAboutBox"
        about_box = about.wxGladeAboutBox()
        about_box.ShowModal()
        about_box.Destroy()

    def show_manual(self, event=None):
        "Show the wxGlade user manual"
        self._show_html(config.manual_file)

    def show_bug_tracker(self):
        self._show_html("https://github.com/wxGlade/wxGlade/issues")
    def show_mailing_list(self):
        self._show_html("https://sourceforge.net/p/wxglade/mailman/wxglade-general/")
    def show_releases(self):
        self._show_html("https://github.com/wxGlade/wxGlade/releases")

    def _show_html(self, html_file):
        "Open browser and show an HTML documentation"

        if wx.Platform == "__WXMAC__":
            os.system(r'open -a Safari.app "%s"' % html_file)
        else:
            import webbrowser, threading
            # ALB 2004-08-15: why did this block the program????? (at least on linux - GTK)
            def go():
                webbrowser.open_new(html_file)
            t = threading.Thread(target=go)
            t.setDaemon(True)
            t.start()

    def show_and_raise(self):
        self.property_panel.Show()  # self.GetMenuBar().IsChecked(self.PROPS_ID))
        self.tree_panel.Show()      # self.GetMenuBar().IsChecked(self.TREE_ID))
        self.property_panel.Raise()
        self.tree_panel.Raise()
        self.Raise()

    def hide_all(self):
        self.tree_panel.Hide()
        self.property_panel.Hide()

    def import_xrc(self, infilename=None, ask_save=True):
        import xrc2wxg

        if ask_save and not self.ask_save():
            return

        if not infilename:
            infilename = wx.FileSelector( _("Import file"), wildcard="XRC files (*.xrc)" "|*.xrc|All files|*",
                                          flags=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST, default_path=self.cur_dir)
        if infilename:
            ibuffer = []
            try:
                xrc2wxg.convert(infilename, ibuffer)

                # Convert UTF-8 returned by xrc2wxg.convert() to Unicode
                tmp = b"".join(ibuffer).decode('UTF-8')
                ibuffer = ['%s\n'%line for line in tmp.split('\n')]

                self._open_app(ibuffer)
                common.root.saved = False
            except Exception as inst:
                fn = os.path.basename(infilename).encode('ascii', 'replace')
                bugdialog.Show(_('Import File "%s"') % fn, inst)

    def manage_templates(self):
        to_edit = template.manage_templates()
        if to_edit is not None and self.ask_save():
            # edit the template
            # TODO, you still need to save it manually...
            self._open_app(to_edit, add_to_history=False)
            wx.MessageBox( _("To save the changes to the template, edit the GUI as usual,\n"
                             "and then click File->Save As Template..."),
                           _("Information"), style=wx.OK|wx.ICON_INFORMATION )
    ####################################################################################################################
    # user interface helpers
    def _set_icon(self):
        icon = compat.wx_EmptyIcon()
        bmp = wx.Bitmap( os.path.join(config.icons_path, "icon.png") )
        icon.CopyFromBitmap(bmp)
        self.SetIcon(icon)
    def init_layout_settings(self):
        # either load from file or init with defaults
        display_area = wx.Display(0).ClientArea
        default_pos = display_area.TopLeft
        height = display_area.height
        width = 800
        default_size = (width,height)

        self.layout_settings = {}
        self.layout_settings["layout"] = 0
        self.layout_settings["sash_positions"] = [[400,     380       ],   # 0: palette and properties left; tree right
                                                  [height//2,400       ],   # 1: palette and tree top; properties bottom
                                                  [2*height//3,height//3] ]  # 2: all on top of each other
        self.layout_settings["widths"] = [width,500]  # for layouts 0/1 and 2
        self.layout_settings["height"] = height
        self.layout_settings["x"], self.layout_settings["y"] = default_pos

        if not config.preferences.remember_geometry:
            return default_pos, default_size, 0

        # read from preferences
        try:
            layout = config.preferences.get_int("layout", "layout")
            x = config.preferences.get_int("layout", "x")
            y = config.preferences.get_int("layout", "y")
            widths = [config.preferences.get_int("layout", "widths_l0"),
                      config.preferences.get_int("layout", "widths_l1")]
            width = widths[0]  if layout<2  else widths[1]
            height = config.preferences.get_int("layout", "height")

            sash_positions = [[config.preferences.get_int("layout", "sash_positions_l0_l0"),
                               config.preferences.get_int("layout", "sash_positions_l0_l1")],
                              [config.preferences.get_int("layout", "sash_positions_l1_l0"),
                               config.preferences.get_int("layout", "sash_positions_l1_l1")],
                              [config.preferences.get_int("layout", "sash_positions_l2_l0"),
                               config.preferences.get_int("layout", "sash_positions_l2_l1")]]
        except:
            return default_pos, default_size, 0
        if layout<0 or layout>2 or not self._check_geometry(x, y, width, height):
            return default_pos, default_size, 0
        self.layout_settings["height"] = height
        self.layout_settings["sash_positions"] = sash_positions
        self.layout_settings["widths"] = widths
        return (x,y), (widths[0],height), layout  # return widths[0] as 0 is the initial setting

    def switch_layout(self, new_layout, initial=False):
        if new_layout != self.layout_settings["layout"]:
            # set the splitters
            if not initial: self._store_layout()
            self.splitter2.Unsplit()
            self.splitter1.Unsplit()
            if new_layout==0:
                self.property_panel.Reparent(self.splitter2)
                self.palette.Reparent(self.splitter2)
                self.tree.Reparent(self.splitter1)
                self.splitter1.SplitVertically(self.splitter2, self.tree)
                self.splitter2.SplitHorizontally(self.palette, self.property_panel)
            elif new_layout==1:
                self.property_panel.Reparent(self.splitter1)
                self.palette.Reparent(self.splitter2)
                self.tree.Reparent(self.splitter2)
                self.splitter1.SplitHorizontally(self.splitter2, self.property_panel)
                self.splitter2.SplitVertically(self.palette, self.tree)
            elif new_layout==2:
                self.property_panel.Reparent(self.splitter1)
                self.palette.Reparent(self.splitter2)
                self.tree.Reparent(self.splitter2)
                self.splitter1.SplitHorizontally(self.splitter2, self.property_panel)
                self.splitter2.SplitHorizontally(self.palette, self.tree)
            if self.layout_settings["layout"] in (0,1) and new_layout==2:
                self.SetSize( (self.layout_settings["widths"][1], self.GetSize()[1]) )
            elif self.layout_settings["layout"]==2 and new_layout in (0,1):
                self.SetSize( (self.layout_settings["widths"][0], self.GetSize()[1]) )
            self.layout_settings["layout"] = new_layout

        # display in toolbar
        t = self._layout_tools[new_layout]
        if not t.IsToggled(): t.Toggle()
        self.toolbar.Realize()

        # set splitter sash positions
        if new_layout==0:
            self.splitter2.SetMinimumPaneSize(1)
            self.splitter2.SetSashGravity(0)
            self.splitter1.SetMinimumPaneSize(2)
            self.splitter1.SetSashGravity(0)
        elif new_layout==1:
            self.splitter2.SetMinimumPaneSize(1)
            self.splitter2.SetSashGravity(0)
            self.splitter1.SetMinimumPaneSize(2)
            self.splitter1.SetSashGravity(0.5)
        elif new_layout==2:
            self.splitter2.SetMinimumPaneSize(1)
            self.splitter2.SetSashGravity(0)
            self.splitter1.SetMinimumPaneSize(2)
            self.splitter1.SetSashGravity(0.5)
        positions = self.layout_settings["sash_positions"][new_layout]
        self.splitter1.SetSashPosition( positions[0] )
        self.splitter2.SetSashPosition( positions[1] )

    def _store_layout(self):
        # store position, size and splitter sash positions
        self.layout_settings["x"], self.layout_settings["y"] = self.GetPosition()

        layout = self.layout_settings["layout"]
        self.layout_settings["sash_positions"][layout] = [self.splitter1.GetSashPosition(),
                                                          self.splitter2.GetSashPosition()]
        width, height = self.GetSize()
        if layout in (0,1):
            self.layout_settings["widths"][0] = width
        else:
            self.layout_settings["widths"][1] = width
        self.layout_settings["height"] = height

    def _check_geometry(self, x,y,width,height):
        # check whether a significant part would be visible
        # also, at least a part of the top edge needs to be visible
        if width<150 or height<150: return False

        # check top line
        top_line = wx.Rect(x,y,width,1)
        min_visible = int(round(width/3))
        top_line_OK = False
        for d in range(wx.Display.GetCount()):
            display = wx.Display(d)
            client_area = display.ClientArea
            if not client_area.width or not client_area.height:
                # the display info is broken on some installations
                continue
            intersection = client_area.Intersect(top_line)
            if intersection.width>=min_visible:
                top_line_OK = True
                break

        if not top_line_OK: return False

        # check rectangle
        geometry = wx.Rect(x,y,width,height)

        for d in range(wx.Display.GetCount()):
            display = wx.Display(d)
            client_area = display.ClientArea
            if not client_area.width or not client_area.height:
                # the display info is broken on some installations
                continue
            intersection = client_area.Intersect(geometry)
            if intersection.width>150 and intersection.height>150 or geometry.width==-1 or geometry.height==-1:
                return True
        return False


class wxGlade(wx.App):
    """wxGlade application class
    
    _exception_orig: Reference to original implementation of logging.exception()"""

    def OnInit(self):
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

        # replace text based exception handler by a graphical exception dialog
        sys.excepthook = self.graphical_exception_handler

        # use graphical implementation to show caught exceptions
        self._exception_orig = logging.exception  # Reference to original implementation of logging.exception()
        logging.exception = self.exception

        # needed for wx >= 2.3.4 to disable wxPyAssertionError exceptions
        if not config.debugging:
            self.SetAssertMode(0)

        common.init_preferences()

        self.locale = wx.Locale(wx.LANGUAGE_DEFAULT)  # avoid PyAssertionErrors
        #compat.wx_ArtProviderPush(wxGladeArtProvider())

        frame = wxGladeFrame()
        self.SetTopWindow(frame)
        self.SetExitOnFrameDelete(True)

        self.init_idle()

        if config.inform_screen_reader:
            message = ("It seems you have a screen reader software installed.\n"
                       "Please be aware that there are some options to improve wxGlade accessibility\n"
                       "with screen readers.\n"
                       "See menu Edit -> Preferences -> Accessibility.")
            wx.CallLater(1000, wx.MessageBox, message, "Accessibility Info")

        return True

    def OnExit(self):
        "Restore original exception handler and logging.exception() on exit"
        sys.excepthook = sys.__excepthook__
        logging.exception = self._exception_orig
        return 0

    def init_idle(self):
        if wx.Platform == "__WXMAC__" and compat.IS_CLASSIC:
            # it seems that EVT_IDLE makes wx.CallAfter stall from time to time, so we use a timer
            wx.CallLater(200, self.OnIdle)
        else:
            self.Bind(wx.EVT_IDLE, self.OnIdle)

    def OnIdle(self, event=None):
        "Idle tasks - currently show error messages only;  see: show_msgdialog()"
        try:
            self.show_msgdialog()
        finally:
            if wx.Platform == "__WXMAC__" and compat.IS_CLASSIC:
                wx.CallLater(200, self.OnIdle)

    def show_msgdialog(self):
        """
        Check for log messages and show them

        see: main.wxGlade.OnIdle(), log.getBufferAsList(), msgdialog.MessageDialog"""
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
        """Show detailed information about uncaught exceptions in bugdialog.BugReport.

        The shown exception will be logged to the log file in parallel.
        The exception information will be cleared after the bug dialog has closed.

        exc_type:  Type of the exception (normally a class object)
        exc_value: The "value" of the exception
        exc_tb:    Call stack of the exception

        see: bugdialog.BugReport(), bugdialog.Show()"""
        bugdialog.ShowEI(exc_type, exc_value, exc_tb)
        if compat.PYTHON2: sys.exc_clear()

    def exception(self, msg, *args, **kwargs):
        """Graphical replacement of logging.exception().

        All exception details logged with logging.exception() will be shown in bugdialog.BugReport.
        The shown exception will be logged to the log file ding.
        The exception information will be cleared after the bug dialog has closed.

        msg: Short description of the exception

        see: bugdialog.BugReport, bugdialog.ShowEI()"""
        if args:
            try:
                msg = msg % args
            except TypeError:
                log.exception_orig(_('Wrong format of a log message'))

        (exc_type, exc_value, exc_tb) = sys.exc_info()
        bugdialog.ShowEI(exc_type, exc_value, exc_tb, msg)
        if compat.PYTHON2: sys.exc_clear()



def main(filename=None):
    "if filename is not None, loads it"
    logging.info(_("Using wxPython %s"), config.wx_version)
    common.history = history.History()
    app = wxGlade()
    if filename is not None:
        win = app.GetTopWindow()
        if os.path.splitext(filename)[1].upper() == ".XRC":
            win.import_xrc(filename)
        else:
            win._open_app(filename, False)

            # mainly for debugging we want the first window to be opened already
            if filename and config.open_design_window and common.root.children:
                editor = common.root.children[0]
                misc.set_focused_widget(editor)
                editor.create()
                common.app_tree.ExpandAllChildren(editor.item)

        win.cur_dir = os.path.dirname(filename)
    #win = app.GetTopWindow()
    ##win.import_xrc(r"D:\Python\Sources35\wxglade\wxglade_dev\tests\casefiles\CalendarCtrl.xrc")
    #win.import_xrc(r"D:\Python\Sources35\wxglade\wxglade_dev\tests\casefiles\AllWidgets_30.xrc")

    app.MainLoop()
