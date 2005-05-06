# main.py: Main wxGlade module: defines wxGladeFrame which contains the buttons
# to add widgets and initializes all the stuff (tree, property_frame, etc.)
# $Id: main.py,v 1.69 2005/05/06 21:48:25 agriggio Exp $
# 
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from widget_properties import *
from tree import Tree, WidgetTree
import edit_sizers
import common, os, os.path, misc, config
import clipboard

import xml_parse


class wxGladePropertyPanel(wxPanel):
    """\
    Panel used to display the Properties of the various widgets
    """
    def SetTitle(self, title):
        try: self.GetParent().SetTitle(title)
        except AttributeError: pass

    def Layout(self):
        if self.is_visible():
            wxPanel.Layout(self)
            self.GetParent().Layout()

    def is_visible(self):
        return self.GetParent().IsShown()

# end of class wxGladePropertyPanel


TOGGLE_BOX_EVENT = wxNewEventType()

def EVT_TOGGLE_BOX(win, id, func):
    win.Connect(id, -1, TOGGLE_BOX_EVENT, func)

class ToggleBoxEvent(wxPyCommandEvent):
    def __init__(self, id, value, strval):
        wxPyCommandEvent.__init__(self)
        self.SetId(id)
        self.SetEventType(TOGGLE_BOX_EVENT)
        self.value = value
        self.strval = strval

    def GetValue(self):
        return self.value

    def GetStringValue(self):
        return self.strval

# end of class ToggleBoxEvent


class ToggleButtonBox(wxPanel):
    def __init__(self, parent, id, choices=[], value=0):
        wxPanel.__init__(self, parent, id)
        self.buttons = [wxToggleButton(self, -1, c) for c in choices]
        self.selected = None
        self.SetValue(value)
        for b in self.buttons:
            def handler(event, b=b):
                self.on_toggle(b, event)
            EVT_TOGGLEBUTTON(self, b.GetId(), handler)
        sizer = wxBoxSizer(wxVERTICAL)
        for b in self.buttons:
            sizer.Add(b, 0, wxALL|wxEXPAND, 1)
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
        wxPostEvent(self, ToggleBoxEvent(self.GetId(), self.GetValue(),
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


class wxGladeArtProvider(wxArtProvider):
    def CreateBitmap(self, artid, client, size):
        if wxPlatform == '__WXGTK__' and artid == wxART_FOLDER:
            return wxBitmap(os.path.join(common.wxglade_path, 'icons',
                                         'closed_folder.xpm'),
                            wxBITMAP_TYPE_XPM)
        return wxNullBitmap

# end of class wxGladeArtProvider


class wxGladeFrame(wxFrame):
    """\
    Main frame of wxGlade (palette)
    """
    def __init__(self, parent=None):
        style = wxSYSTEM_MENU|wxCAPTION|wxMINIMIZE_BOX|wxRESIZE_BORDER
        if misc.check_wx_version(2, 5):
            style |= wxCLOSE_BOX
        wxFrame.__init__(self, parent, -1, "wxGlade v%s" % common.version,
                         style=style)
        self.CreateStatusBar(1)

        if parent is None: parent = self
        common.palette = self # to provide a reference accessible
                              # by the various widget classes
        icon = wxEmptyIcon()
        bmp = wxBitmap(os.path.join(common.wxglade_path, "icons/icon.xpm"),
                       wxBITMAP_TYPE_XPM)
        icon.CopyFromBitmap(bmp)
        self.SetIcon(icon)
        self.SetBackgroundColour(wxSystemSettings_GetColour(
            wxSYS_COLOUR_BTNFACE))
        menu_bar = wxMenuBar()
        file_menu = wxMenu(style=wxMENU_TEAROFF)
        view_menu = wxMenu(style=wxMENU_TEAROFF)
        help_menu = wxMenu(style=wxMENU_TEAROFF)
        wxToolTip_SetDelay(1000)

        # load the available code generators
        common.load_code_writers()
        # load the available widgets and sizers
        core_btns, custom_btns = common.load_widgets()
        sizer_btns = common.load_sizers()
        
        self.TREE_ID = TREE_ID = wxNewId()
        view_menu.Append(TREE_ID, "Show &Tree\tCtrl+T", "", True)
        view_menu.Check(TREE_ID, True)
        self.PROPS_ID = PROPS_ID = wxNewId()
        view_menu.Append(PROPS_ID, "Show &Properties\tCtrl+P", "", True)
        view_menu.Check(PROPS_ID, True)
        append_item = misc.append_item
        NEW_ID = wxNewId()
        append_item(file_menu, NEW_ID, "&New\tCtrl+N", 'new.xpm')
        OPEN_ID = wxNewId()
        append_item(file_menu, OPEN_ID, "&Open...\tCtrl+O", 'open.xpm') 
        SAVE_ID = wxNewId()
        append_item(file_menu, SAVE_ID, "&Save\tCtrl+S", 'save.xpm') 
        SAVE_AS_ID = wxNewId()
        append_item(file_menu, SAVE_AS_ID, "Save As...\tShift+Ctrl+S",
                    'save_as.xpm')
        file_menu.AppendSeparator()
        RELOAD_ID = wxNewId()
        append_item(file_menu, RELOAD_ID, "&Refresh\tf5", 'refresh.xpm')
        GENERATE_CODE_ID = wxNewId()
        append_item(file_menu, GENERATE_CODE_ID, "&Generate Code\tCtrl+G",
                    'generate.xpm')
        
        file_menu.AppendSeparator()
        IMPORT_ID = wxNewId()
        append_item(file_menu, IMPORT_ID, "&Import from XRC...\tCtrl+I")
        
        EXIT_ID = wxNewId()
        file_menu.AppendSeparator()
        append_item(file_menu, EXIT_ID, 'E&xit\tCtrl+Q', 'exit.xpm')
        PREFS_ID = wxNewId()
        view_menu.AppendSeparator()
        append_item(view_menu, PREFS_ID, 'Preferences...', 'prefs.xpm')
        menu_bar.Append(file_menu, "&File")
        menu_bar.Append(view_menu, "&View")
        TUT_ID = wxNewId()
        append_item(help_menu, TUT_ID, 'Contents\tF1', 'tutorial.xpm')
        ABOUT_ID = wxNewId()
        append_item(help_menu, ABOUT_ID, 'About...', 'about.xpm')
        menu_bar.Append(help_menu, '&Help')
        parent.SetMenuBar(menu_bar)
        # Mac tweaks...
        if wxPlatform == "__WXMAC__":
            wxApp_SetMacAboutMenuItemId(ABOUT_ID)
            wxApp_SetMacPreferencesMenuItemId(PREFS_ID)
            wxApp_SetMacExitMenuItemId(EXIT_ID)
            wxApp_SetMacHelpMenuTitleName('&Help')

        # file history support
        if misc.check_wx_version(2, 3, 3):
            self.file_history = wxFileHistory(
                config.preferences.number_history)
            self.file_history.UseMenu(file_menu)
            files = config.load_history()
            files.reverse()
            for path in files:
                self.file_history.AddFileToHistory(path.strip())
                
            def open_from_history(event):
                infile = self.file_history.GetHistoryFile(
                    event.GetId() - wxID_FILE1)
                # ALB 2004-10-15 try to restore possible autosave content...
                if common.check_autosaved(infile) and \
                       wxMessageBox("There seems to be auto saved data for "
                                    "this file: do you want to restore it?",
                                    "Auto save detected",
                                    style=wxICON_QUESTION|wxYES_NO) == wxYES:
                    common.restore_from_autosaved(infile)
                else:
                    common.remove_autosaved(infile)
                self._open_app(infile)
                
            EVT_MENU_RANGE(self, wxID_FILE1, wxID_FILE9, open_from_history)
        
        EVT_MENU(self, TREE_ID, self.show_tree)
        EVT_MENU(self, PROPS_ID, self.show_props_window)
        EVT_MENU(self, NEW_ID, self.new_app)
        EVT_MENU(self, OPEN_ID, self.open_app)
        EVT_MENU(self, SAVE_ID, self.save_app)
        EVT_MENU(self, SAVE_AS_ID, self.save_app_as)
        def generate_code(event):
            common.app_tree.app.generate_code()
        EVT_MENU(self, GENERATE_CODE_ID, generate_code)
        EVT_MENU(self, EXIT_ID, lambda e: self.Close())
        EVT_MENU(self, TUT_ID, self.show_tutorial)
        EVT_MENU(self, ABOUT_ID, self.show_about_box)
        EVT_MENU(self, PREFS_ID, self.edit_preferences)
        EVT_MENU(self, IMPORT_ID, self.import_xrc)
        EVT_MENU(self, RELOAD_ID, self.reload_app)

        self.accel_table = wxAcceleratorTable([
            (wxACCEL_CTRL, ord('N'), NEW_ID),
            (wxACCEL_CTRL, ord('O'), OPEN_ID),
            (wxACCEL_CTRL, ord('S'), SAVE_ID),
            (wxACCEL_CTRL|wxACCEL_SHIFT, ord('S'), SAVE_AS_ID),
            (wxACCEL_CTRL, ord('G'), GENERATE_CODE_ID),
            (wxACCEL_CTRL, ord('I'), IMPORT_ID),
            (0, WXK_F1, TUT_ID),
            (wxACCEL_CTRL, ord('Q'), EXIT_ID),
            (0, WXK_F5, RELOAD_ID),
            ])

        # Tutorial window
##         self.tut_frame = None
        # layout
        # if there are custom components, add the toggle box...
        if custom_btns:
            main_sizer = wxBoxSizer(wxVERTICAL)
            show_core_custom = ToggleButtonBox(
                self, -1, ["Core components", "Custom components"], 0)

            if misc.check_wx_version(2, 5):
                core_sizer = wxFlexGridSizer(
                    0, config.preferences.buttons_per_row)
                custom_sizer = wxFlexGridSizer(
                    0, config.preferences.buttons_per_row)
            else:
                core_sizer = wxGridSizer(
                    0, config.preferences.buttons_per_row)
                custom_sizer = wxGridSizer(
                    0, config.preferences.buttons_per_row)                
            self.SetAutoLayout(True)
            # core components
            for b in core_btns: core_sizer.Add(b)
            for sb in sizer_btns: core_sizer.Add(sb)
            # custom components
            for b in custom_btns:
                custom_sizer.Add(b)
                if misc.check_wx_version(2, 5):
                    custom_sizer.Show(b, False)
            custom_sizer.Layout()
            main_sizer.Add(show_core_custom, 0, wxEXPAND)
            main_sizer.Add(core_sizer, 0, wxEXPAND)
            main_sizer.Add(custom_sizer, 0, wxEXPAND)
            self.SetSizer(main_sizer)
            if not misc.check_wx_version(2, 5):
                main_sizer.Show(custom_sizer, False)
            #main_sizer.Show(1, False)
            main_sizer.Fit(self)
            # events to display core/custom components
            if misc.check_wx_version(2, 5):
                def on_show_core_custom(event):
                    show_core = True
                    show_custom = False
                    if event.GetValue() == 1:
                        show_core = False
                        show_custom = True
                    for b in custom_btns:
                        custom_sizer.Show(b, show_custom)
                    for b in core_btns:
                        core_sizer.Show(b, show_core)
                    for b in sizer_btns:
                        core_sizer.Show(b, show_core)
                    core_sizer.Layout()
                    custom_sizer.Layout()
                    main_sizer.Layout()
            else:
                def on_show_core_custom(event):
                    to_show = core_sizer
                    to_hide = custom_sizer
                    if event.GetValue() == 1:
                        to_show, to_hide = to_hide, to_show
                    main_sizer.Show(to_show, True)
                    main_sizer.Show(to_hide, False)
                    main_sizer.Layout()           
            EVT_TOGGLE_BOX(self, show_core_custom.GetId(), on_show_core_custom)
        # ... otherwise (the common case), just add the palette of core buttons
        else:
            sizer = wxGridSizer(0, config.preferences.buttons_per_row)
            self.SetAutoLayout(True)
            # core components
            for b in core_btns: sizer.Add(b)
            for sb in sizer_btns: sizer.Add(sb)
            self.SetSizer(sizer)
            sizer.Fit(self)
        
        # Properties window
        frame_style = wxDEFAULT_FRAME_STYLE
        frame_tool_win = config.preferences.frame_tool_win
        if wxPlatform == '__WXMSW__' and frame_tool_win:
            frame_style |= wxFRAME_TOOL_WINDOW|wxFRAME_NO_TASKBAR
        
        self.frame2 = wxFrame(self, -1, 'Properties - <app>',
                              style=frame_style)
        self.frame2.SetBackgroundColour(wxSystemSettings_GetColour(
            wxSYS_COLOUR_BTNFACE))
        self.frame2.SetIcon(icon)
        
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        property_panel = wxGladePropertyPanel(self.frame2, -1)

        #---- 2003-06-22 Fix for what seems to be a GTK2 bug (notebooks)
        misc.hidden_property_panel = wxPanel(self.frame2, -1)
        sz = wxBoxSizer(wxVERTICAL)
        sz.Add(property_panel, 1, wxEXPAND)
        sz.Add(misc.hidden_property_panel, 1, wxEXPAND)
        self.frame2.SetSizer(sz)
        sz.Show(misc.hidden_property_panel, False)
        self.property_frame = self.frame2
        #--------------------------------------------------------
        
        property_panel.SetAutoLayout(True)
        self.hidden_frame = wxFrame(self, -1, "")
        self.hidden_frame.Hide()
        sizer_tmp.Add(property_panel, 1, wxEXPAND)
        self.frame2.SetAutoLayout(True)
        self.frame2.SetSizer(sizer_tmp)
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        def hide_frame2(event):
            menu_bar.Check(PROPS_ID, False)
            self.frame2.Hide()
        EVT_CLOSE(self.frame2, hide_frame2)
        EVT_CLOSE(self, self.cleanup)
        common.property_panel = property_panel
        # Tree of widgets
        self.tree_frame = wxFrame(self, -1, 'wxGlade: Tree',
                                  style=frame_style)
        self.tree_frame.SetIcon(icon)
        
        import application
        app = application.Application(common.property_panel)
        common.app_tree = WidgetTree(self.tree_frame, app)
        self.tree_frame.SetSize((300, 300))

        app.notebook.Show()
        sizer_tmp.Add(app.notebook, 1, wxEXPAND)
        property_panel.SetSizer(sizer_tmp)
        sizer_tmp.Fit(property_panel)
        
        def on_tree_frame_close(event):
            menu_bar.Check(TREE_ID, False)
            self.tree_frame.Hide()
        EVT_CLOSE(self.tree_frame, on_tree_frame_close)
        # check to see if there are some remembered values
        prefs = config.preferences
        if prefs.remember_geometry:
            #print 'initializing geometry'
            try:
                x, y, w, h = prefs.get_geometry('main')
                misc.set_geometry(self, (x, y))
            except Exception, e:
                pass
            misc.set_geometry(self.frame2, prefs.get_geometry('properties'))
            misc.set_geometry(self.tree_frame, prefs.get_geometry('tree'))
        else:
            if wxPlatform == '__WXMAC__':
                self.frame2.SetSize((345, 384)) # I've been told this is OK...
                self.SetPosition((0, 45)) # to avoid the OS X menubar
            else:
                self.frame2.SetSize((max(self.GetSize()[0], 250), 350))
                self.SetPosition((0, 0))
            x, y = self.GetPosition()
            h = self.GetSize()[1]
            w = self.frame2.GetSize()[0]
            if wxPlatform != '__WXMSW__':
                # under X, IceWM (and Sawfish, too), GetSize seems to ignore
                # window decorations
                h += 60
                w += 10
            self.frame2.SetPosition((x, y+h))
            self.tree_frame.SetPosition((x+w, y))
        self.tree_frame.Show()
        self.frame2.Show()    
        self.Show()

        self._skip_activate = False
        if wxPlatform == '__WXMSW__':
            import about
            # I'll pay a beer to anyone who can explain to me why this prevents
            # a segfault on Win32 when you exit without doing anything!!
            self.about_box = about.wxGladeAboutBox(self.GetParent())
            def on_iconize(event):
                if event.Iconized():
                    self.hide_all()
                else:
                    self.show_and_raise()
                event.Skip()
            EVT_ICONIZE(self, on_iconize)
        else:
            self.about_box = None

        # last visited directory, used on GTK for wxFileDialog
        self.cur_dir = config.preferences.open_save_path

        # set a drop target for us...
        self._droptarget = clipboard.FileDropTarget(self)
        self.SetDropTarget(self._droptarget)
        #self.tree_frame.SetDropTarget(self._droptarget)
        #self.frame2.SetDropTarget(self._droptarget)

        # ALB 2004-10-15, autosave support...
        self.autosave_timer = None
        if config.preferences.autosave:
            TIMER_ID = wxNewId()
            self.autosave_timer = wxTimer(self, TIMER_ID)
            EVT_TIMER(self, TIMER_ID, self.on_autosave_timer)
            self.autosave_timer.Start(
                int(config.preferences.autosave_delay) * 1000)
        # ALB 2004-10-15
        CLEAR_SB_TIMER_ID = wxNewId()
        self.clear_sb_timer = wxTimer(self, CLEAR_SB_TIMER_ID)
        EVT_TIMER(self, CLEAR_SB_TIMER_ID, self.on_clear_sb_timer)

        self.frame2.SetAcceleratorTable(self.accel_table)
        self.tree_frame.SetAcceleratorTable(self.accel_table)

        self.Raise()

        # ALB 2004-10-16
        if common.check_autosaved(None) and \
               wxMessageBox("There seems to be auto saved data "
                            "from last wxGlade session: "
                            "do you want to restore it?",
                            "Auto save detected",
                            style=wxICON_QUESTION|wxYES_NO) == wxYES:
            if self._open_app(common.get_name_for_autosave(),
                              add_to_history=False):
                common.app_tree.app.saved = False
                common.app_tree.app.filename = None
                self.user_message("Recovery from auto save complete")
                common.remove_autosaved()
        else:
            common.remove_autosaved()

    def on_autosave_timer(self, event):
        if common.autosave_current():
            self.user_message("Auto saving... done")
        
    def edit_preferences(self, event):
        config.edit_preferences()

    def show_tree(self, event):
        self.tree_frame.Show(event.IsChecked())

    def show_props_window(self, event):
        show = event.IsChecked()
        self.frame2.Show(show)
        if show:
            common.app_tree.app.show_properties()
            if common.app_tree.cur_widget:
                common.app_tree.cur_widget.show_properties()

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
            ok = wxMessageBox("Save changes to the current app?", "Confirm",
                              wxYES_NO|wxCANCEL|wxCENTRE|wxICON_QUESTION)
            if ok == wxYES:
                self.save_app(None)
            return ok != wxCANCEL
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
            # ALB 2004-10-15
            common.remove_autosaved()
            if config.preferences.autosave and self.autosave_timer is not None:
                self.autosave_timer.Start()

    def reload_app(self, event):
        self.ask_save()
        if not common.app_tree.app.filename:
            wxMessageBox("Impossible to reload an unsaved application",
                         "Alert", style=wxOK|wxICON_INFORMATION)
            return
        path = common.app_tree.get_selected_path()
        print 'path:', path
        self._open_app(common.app_tree.app.filename, add_to_history=False)
        common.app_tree.select_path(path)
        
    def open_app(self, event_unused):
        """\
        loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :)
        """
        if not self.ask_save(): return
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder
        infile = misc.FileSelector("Open file", wildcard="wxGlade files (*.wxg)"
                                   "|*.wxg|XML files (*.xml)|*.xml|All files|*",
                                   flags=wxOPEN|wxFILE_MUST_EXIST,
                                   default_path=self.cur_dir)
        if infile:
            # ALB 2004-10-15 try to restore possible autosave content...
            if common.check_autosaved(infile) and \
                   wxMessageBox("There seems to be auto saved data for "
                                "this file: do you want to restore it?",
                                "Auto save detected",
                                style=wxICON_QUESTION|wxYES_NO) == wxYES:
                common.restore_from_autosaved(infile)
            else:
                common.remove_autosaved(infile)
            self._open_app(infile)
            self.cur_dir = os.path.dirname(infile)

    def _open_app(self, infilename, use_progress_dialog=True,
                  is_filelike=False, add_to_history=True):
        import time
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder, \
             XmlParsingError
        from xml.sax import SAXParseException

        start = time.clock()

        common.app_tree.clear()
        if not is_filelike:
            common.app_tree.app.filename = infilename
        else:
            common.app_tree.filename = getattr(infilename, 'name', None)
        common.property_panel.Reparent(self.hidden_frame)
        # prevent the auto-expansion of nodes
        common.app_tree.auto_expand = False

        old_dir = os.getcwd()
        try:
            if not is_filelike:
                os.chdir(os.path.dirname(infilename))
                infile = open(infilename)
            else:
                infile = infilename
                infilename = getattr(infile, 'name', None)
            if use_progress_dialog and config.preferences.show_progress:
                p = ProgressXmlWidgetBuilder(input_file=infile)
            else:
                p = XmlWidgetBuilder()
            p.parse(infile)
        except (IOError, OSError, SAXParseException, XmlParsingError), msg:
            if locals().has_key('infile') and not is_filelike: infile.close()
            common.app_tree.clear()
            common.property_panel.Reparent(self.frame2)
            common.app_tree.app.saved = True
            wxMessageBox("Error loading file %s: %s" % (infilename, msg),
                         "Error", wxOK|wxCENTRE|wxICON_ERROR)
            # reset the auto-expansion of nodes
            common.app_tree.auto_expand = True
            os.chdir(old_dir)
            return False            
        except Exception, msg:
            import traceback; traceback.print_exc()

            if locals().has_key('infile') and not is_filelike: infile.close()
            common.app_tree.clear()
            common.property_panel.Reparent(self.frame2)
            common.app_tree.app.saved = True
            wxMessageBox("An exception occurred while loading file \"%s\".\n"
                         "This is the error message associated with it:\n"
                         "        %s\n"
                         "For more details, look at the full traceback "
                         "on the console.\nIf you think this is a wxGlade bug,"
                         " please report it." % (infilename, msg), "Error",
                         wxOK|wxCENTRE|wxICON_ERROR)
            # reset the auto-expansion of nodes
            common.app_tree.auto_expand = True
            os.chdir(old_dir)
            return False

        if not is_filelike:
            infile.close()
        common.app_tree.select_item(common.app_tree.root)
        common.app_tree.root.widget.show_properties()
        common.property_panel.Reparent(self.frame2)
        # reset the auto-expansion of nodes
        common.app_tree.auto_expand = True
        common.app_tree.expand()

        end = time.clock()
        print 'Loading time: %.5f' % (end-start)

        common.app_tree.app.saved = True
        
        if hasattr(self, 'file_history') and infilename is not None and \
               add_to_history:
            self.file_history.AddFileToHistory(infilename)

        # ALB 2004-10-15
        if config.preferences.autosave and self.autosave_timer is not None:
            self.autosave_timer.Start()

        self.user_message("Loaded %s (%.2f seconds)" % \
                          (common.app_tree.app.filename, end-start))

        return True

    def save_app(self, event):
        """\
        saves a wxGlade project onto an xml file
        """
        if not common.app_tree.app.filename:
            self.save_app_as(event)
        else:
            try:
                from cStringIO import StringIO
                buffer = StringIO()
                common.app_tree.write(buffer)
                common.save_file(common.app_tree.app.filename,
                                 buffer.getvalue(), 'wxg')
            except (IOError, OSError), msg:
                common.app_tree.app.saved = False
                fn = common.app_tree.app.filename
                wxMessageBox("Error saving app:\n%s" % msg, "Error",
                             wxOK|wxCENTRE|wxICON_ERROR)
            except Exception, msg:
                import traceback; traceback.print_exc()
                common.app_tree.app.saved = False
                fn = common.app_tree.app.filename
                wxMessageBox("An exception occurred while saving file \"%s\"."
                             "\n"
                             "This is the error message associated with it:\n"
                             "        %s\n"
                             "For more details, look at the full traceback "
                             "on the console.\nIf you think this is a "
                             "wxGlade bug,"
                             " please report it." % (fn, msg), "Error",
                         wxOK|wxCENTRE|wxICON_ERROR)
            else:
                common.app_tree.app.saved = True
                common.remove_autosaved() # ALB 2004-10-15
                # ALB 2004-10-15
                if config.preferences.autosave and \
                       self.autosave_timer is not None:
                    self.autosave_timer.Start()
                self.user_message("Saved %s" % common.app_tree.app.filename)

    def save_app_as(self, event):
        """\
        saves a wxGlade project onto an xml file chosen by the user
        """
        fn = misc.FileSelector("Save project as...",
                               wildcard="wxGlade files (*.wxg)|*.wxg|"
                               "XML files (*.xml)|*.xml|All files|*",
                               flags=wxSAVE|wxOVERWRITE_PROMPT,
                               default_path=self.cur_dir)
        if fn:
            common.app_tree.app.filename = fn
            self.save_app(event)
            self.cur_dir = os.path.dirname(fn)
            if misc.check_wx_version(2, 3, 3):
                self.file_history.AddFileToHistory(fn)

    def cleanup(self, event):
        if self.ask_save():
            # first, let's see if we have to save the geometry...
            prefs = config.preferences
            if prefs.remember_geometry:
                prefs.set_geometry('main', misc.get_geometry(self))
                prefs.set_geometry('tree',
                                   misc.get_geometry(self.tree_frame))
                prefs.set_geometry('properties',
                                   misc.get_geometry(self.frame2))
                prefs.changed = True
            common.app_tree.clear()
            if self.about_box: self.about_box.Destroy()
            try: config.save_preferences()
            except Exception, e:
                wxMessageBox('Error saving preferences:\n%s' % e, 'Error',
                             wxOK|wxCENTRE|wxICON_ERROR)
            self._skip_activate = True
            self.Destroy()
            common.remove_autosaved() # ALB 2004-10-15
            misc.wxCallAfter(wxGetApp().ExitMainLoop)

    def show_about_box(self, event):
        if self.about_box is None:
            import about
            self.about_box = about.wxGladeAboutBox(None)
        self.about_box.ShowModal()

    def show_tutorial(self, event):
        docs_path = os.path.join(common.wxglade_path, 'docs', 'index.html')
        if wxPlatform == "__WXMAC__":
            os.system('open -a Help\ Viewer.app %s' % docs_path)
        else:
            import webbrowser, threading
            # ALB 2004-08-15: why did this block the program?????
            # (at least on linux - GTK)
            def go():
                webbrowser.open_new(docs_path)
                t = threading.Thread(target=go)
                t.setDaemon(True)
                t.start()

    def show_and_raise(self):
        self.frame2.Show(self.GetMenuBar().IsChecked(self.PROPS_ID))
        self.tree_frame.Show(self.GetMenuBar().IsChecked(self.TREE_ID))
        self.Raise()
        self.frame2.Raise()
        self.tree_frame.Raise()

    def hide_all(self):
        self.tree_frame.Hide()
        self.frame2.Hide()

    def import_xrc(self, event):
        import xrc2wxg, cStringIO

        if not self.ask_save():
            return
        
        infile = misc.FileSelector("Import file", wildcard="XRC files (*.xrc)"
                                   "|*.xrc|All files|*",
                                   flags=wxOPEN|wxFILE_MUST_EXIST,
                                   default_path=self.cur_dir)
        if infile:
            buf = cStringIO.StringIO()
            try:
                xrc2wxg.convert(infile, buf)
                buf.seek(0)
                self._open_app(buf, is_filelike=True)
                common.app_tree.app.saved = False
            except Exception, msg:
                import traceback; traceback.print_exc()
                wxMessageBox("An exception occurred while importing file "
                             "\"%s\".\nThis is the error message associated "
                             "with it:\n        %s\n"
                             "For more details, look at the full traceback "
                             "on the console.\nIf you think this is a wxGlade "
                             "bug, please report it." % (infile, msg), "Error",
                             wxOK|wxCENTRE|wxICON_ERROR)

# end of class wxGladeFrame


class wxGlade(wxApp):
    def OnInit(self):
        import sys
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        # needed for wx >= 2.3.4 to disable wxPyAssertionError exceptions
        if misc.check_wx_version(2, 3, 4):
            self.SetAssertMode(0)
        wxInitAllImageHandlers()
        config.init_preferences()

        # ALB 2004-10-27
        if wxPlatform == '__WXGTK__' and config.preferences.use_kde_dialogs:
            import kdefiledialog
            if kdefiledialog.test_kde():
                misc.FileSelector = kdefiledialog.kde_file_selector
                misc.DirSelector = kdefiledialog.kde_dir_selector

        wxArtProvider_PushProvider(wxGladeArtProvider())

        frame = wxGladeFrame()
        if wxPlatform == '__WXMSW__':
            def on_activate(event):
                if event.GetActive() and not frame.IsIconized():
                    frame.show_and_raise()
                event.Skip()
            EVT_ACTIVATE_APP(self, on_activate)

        self.SetTopWindow(frame)
        self.SetExitOnFrameDelete(True)
        return True

# end of class wxGlade


def main(filename=None):
    """\
    if filename is not None, loads it
    """
    # first thing to do, patch wxSizerPtr's Insert if needed...
    from wxPython import wx
    if wx.__version__ == '2.4.0.2':
        wxSizerPtr.Insert = misc.sizer_fixed_Insert

    # now, silence a deprecation warining for py2.3
    import warnings
    warnings.filterwarnings("ignore", "integer", DeprecationWarning,
                            "wxPython.gdi")
    
    app = wxGlade()
    if filename is not None:
        app.GetTopWindow()._open_app(filename, False)
    app.MainLoop()
