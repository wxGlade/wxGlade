# main.py: Main wxGlade module: defines wxGladeFrame which contains the buttons
# to add widgets and initializes all the stuff (tree, property_frame, etc.)
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from widget_properties import *
from tree import Tree, WidgetTree
import edit_sizers
import common, os.path, misc, config

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

class wxGladeFrame(wxFrame):
    """\
    Main frame of wxGlade (palette)
    """
    def __init__(self, parent=None):
        wxFrame.__init__(self, parent, -1, "wxGlade v%s" % common.version,
                         style=wxSYSTEM_MENU|wxCAPTION|wxMINIMIZE_BOX)
        if parent is None: parent = self
        common.palette = self # to provide a reference accessible
                              # by the various widget classes
        icon = wxEmptyIcon()
        bmp = wxBitmap("icons/icon.xpm", wxBITMAP_TYPE_XPM)
        icon.CopyFromBitmap(bmp)
        self.SetIcon(icon)
        self.SetBackgroundColour(wxSystemSettings_GetSystemColour(
            wxSYS_COLOUR_BTNFACE))
        menu_bar = wxMenuBar()
        file_menu = wxMenu(style=wxMENU_TEAROFF)
        view_menu = wxMenu(style=wxMENU_TEAROFF)
        help_menu = wxMenu(style=wxMENU_TEAROFF)
        sizer = wxGridSizer(0, 5)
        wxToolTip_SetDelay(1000)
        # load the available code generators
        common.load_code_writers()
        # load the available widgets and sizers
        buttons = common.load_widgets()
        sizer_btns = common.load_sizers()
        
        TREE_ID = wxNewId()
        view_menu.Append(TREE_ID, "Show &Tree\tCtrl+T", "", True)
        view_menu.Check(TREE_ID, True)
        PROPS_ID = wxNewId()
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
        GENERATE_CODE_ID = wxNewId()
        append_item(file_menu, GENERATE_CODE_ID, "&Generate Code\tCtrl+G",
                    'generate.xpm')
        EXIT_ID = wxNewId()
        file_menu.AppendSeparator()
        append_item(file_menu, EXIT_ID, 'E&xit\tCtrl+Q', 'exit.xpm')
        PREFS_ID = wxNewId()
        view_menu.AppendSeparator()
        append_item(view_menu, PREFS_ID, 'Preferences...', 'prefs.xpm')
        menu_bar.Append(file_menu, "&File")
        menu_bar.Append(view_menu, "&View")
        TUT_ID = wxNewId()
        append_item(help_menu, TUT_ID, 'Tutorial\tF1', 'tutorial.xpm')
        ABOUT_ID = wxNewId()
        append_item(help_menu, ABOUT_ID, 'About...', 'about.xpm')
        menu_bar.Append(help_menu, '&Help')
        parent.SetMenuBar(menu_bar)

        # file history support
        if misc.check_wx_version(2, 3, 3):
            self.file_history = wxFileHistory(
                config.preferences.number_history)
            self.file_history.UseMenu(file_menu)
            for path in config.load_history():
                self.file_history.AddFileToHistory(path.strip())
                
            def open_from_history(event):
                self._open_app(self.file_history.GetHistoryFile(
                    event.GetId() - wxID_FILE1))
                
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
        # Tutorial window
        self.tut_frame = None
        # layout
        self.SetAutoLayout(True)
        for b in buttons: sizer.Add(b)
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
        self.frame2.SetBackgroundColour(wxSystemSettings_GetSystemColour(
            wxSYS_COLOUR_BTNFACE))
        self.frame2.SetIcon(icon)
        
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        property_panel = wxGladePropertyPanel(self.frame2, -1)
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
        self.frame2.SetSize((250, 350))
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
        self.Show()
        self.tree_frame.Show()
        self.frame2.Show()

        # I'll pay a beer to anyone who can explain to me why this prevents
        # a segfault on Win32 when you exit without doing anything!!
        if wxPlatform == '__WXMSW__':
            import about
            self.about_box = about.wxGladeAboutBox(self.GetParent())
            def on_activate(event):
                hide = self.IsIconized()
                if not hide:
                    self.frame2.Show(menu_bar.IsChecked(PROPS_ID))
                    self.tree_frame.Show(menu_bar.IsChecked(TREE_ID))
                else:
                    self.frame2.Hide()
                    self.tree_frame.Hide()
                event.Skip()
            if frame_tool_win:
                EVT_ACTIVATE(self, on_activate)
        else:
            self.about_box = None

        # last visited directory, used on GTK for wxFileDialog
        self.cur_dir = config.preferences.open_save_path
##         self.cur_dir = os.path.expanduser('~')
##         if self.cur_dir == '~': self.cur_dir = os.getcwd() 

        self.Raise()

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
        
    def open_app(self, event_unused):
        """\
        loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :)
        """
        if not self.ask_save(): return
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder
        infile = wxFileSelector("Open file", wildcard="wxGlade files|*.wxg|"
                                "XML files|*.xml|All files|*",
                                flags=wxOPEN|wxFILE_MUST_EXIST,
                                default_path=self.cur_dir)
        if infile:
            self._open_app(infile)
            self.cur_dir = os.path.dirname(infile)

    def _open_app(self, infilename, use_progress_dialog=True):
        import time
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder
        start = time.clock()

        common.app_tree.clear()
        common.app_tree.app.filename = infilename
        common.property_panel.Reparent(self.hidden_frame)
        # prevent the auto-expansion of nodes
        common.app_tree.auto_expand = False

        try:
            infile = open(infilename)
            if use_progress_dialog:
                p = ProgressXmlWidgetBuilder(input_file=infile)
            else: p = XmlWidgetBuilder()
            p.parse(infile)
        except Exception, msg:
            import traceback; traceback.print_exc()

            if locals().has_key('infile'): infile.close()
            common.app_tree.clear()
            common.property_panel.Reparent(self.frame2)
            common.app_tree.app.saved = True
            wxMessageBox("Error loading file:\n%s" % msg, "Error",
                         wxOK|wxCENTRE|wxICON_ERROR)
            # reset the auto-expansion of nodes
            common.app_tree.auto_expand = True
            return 

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
        
        if hasattr(self, 'file_history'):
            self.file_history.AddFileToHistory(infilename)

    def save_app(self, event):
        """\
        saves a wxGlade project onto an xml file
        """
        if not common.app_tree.app.filename:
            self.save_app_as(event)
        else:
            try:
                f = open(common.app_tree.app.filename, 'w')
                common.app_tree.write(f)
                f.close()
            except Exception, msg:
                import traceback; traceback.print_exc()
                if locals().has_key('f'): f.close()
                common.app_tree.app.saved = False
                wxMessageBox("Error saving app:\n%s" % msg, "Error",
                             wxOK|wxCENTRE|wxICON_ERROR)
            else:
                common.app_tree.app.saved = True

    def save_app_as(self, event):
        """\
        saves a wxGlade project onto an xml file chosen by the user
        """
        fn = wxFileSelector("Save project as...",
                            wildcard="wxGlade files|*.wxg|XML files|*.xml|"
                            "All files|*",
                            flags=wxSAVE|wxOVERWRITE_PROMPT,
                            default_path=self.cur_dir)
        common.app_tree.app.filename = fn
        if fn:
            self.save_app(event)
            self.cur_dir = os.path.dirname(fn)

    def cleanup(self, event):
        if self.ask_save():
            common.app_tree.clear()
            if self.about_box: self.about_box.Destroy()
            #self.tree_frame.Destroy()
            #self.frame2.Destroy()
            #self.Destroy()
            try: config.save_preferences()
            except Exception, e:
                wxMessageBox('Error saving preferences:\n%s' % e, 'Error',
                             wxOK|wxCENTRE|wxICON_ERROR)
            raise SystemExit

    def add_object(self, event):
        """\
        Adds a widget or a sizer to the current app.
        """
        common.adding_widget = True
        tmp = event.GetId()
        common.widget_to_add = common.refs[tmp]
        # TODO: find a better way
        if common.widget_to_add.find('Sizer') != -1:
            common.adding_sizer = True
        
    def add_toplevel_object(self, event):
        """\
        Adds a toplevel widget (Frame or Dialog) to the current app.
        """
        common.widgets[common.refs[event.GetId()]](None, None, 0)
        common.app_tree.app.saved = False

    def show_about_box(self, event):
        if self.about_box is None:
            import about
            self.about_box = about.wxGladeAboutBox(None)
        self.about_box.ShowModal()

    def show_tutorial(self, event):
        if not self.tut_frame:
            from wxPython.html import wxHtmlWindow
            self.tut_frame = wxFrame(self, -1, "wxGlade Tutorial")
            self.tut_frame.SetIcon(self.GetIcon())
            panel = wxPanel(self.tut_frame, -1)
            sizer = wxBoxSizer(wxVERTICAL)
            html = wxHtmlWindow(panel, -1, (0, 0), (-1, -1), wxSUNKEN_BORDER)
            html.LoadPage('docs/tutorial.html')
            sizer.Add(html, 1, wxEXPAND)
            btn = wxButton(panel, -1, 'OK')
            btn.SetDefault()
            sizer.Add(btn, 0, wxALL|wxALIGN_RIGHT, 10)
            panel.SetAutoLayout(True)
            panel.SetSizer(sizer)
            self.tut_frame.SetSize((610, 550))
            EVT_CLOSE(self.tut_frame, lambda e: self.tut_frame.Hide())
            EVT_BUTTON(btn, btn.GetId(), lambda e: self.tut_frame.Hide())
            if wxPlatform == '__WXMSW__':
                self.tut_frame.CenterOnScreen() # on Unix, WM are smart enough
                                                # to place the frame at a
                                                # reasonable position
        self.tut_frame.Show()

# end of class wxGladeFrame


class wxGlade(wxApp):
    def OnInit(self):
        import sys
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        wxInitAllImageHandlers()
        config.init_preferences()
        frame = wxGladeFrame()
        self.SetTopWindow(frame)
        return True

# end of class wxGlade


def main(filename=None):
    """\
    if filename is not None, loads it
    """
    app = wxGlade()
    if filename is not None:
        app.GetTopWindow()._open_app(filename, False)
    app.MainLoop()
