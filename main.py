# main.py: Main wxGlade module: defines wxGladeFrame which contains the buttons
# to add widgets and initializes all the stuff (tree, property_frame, etc.)
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
from widget_properties import *
from tree import Tree, WidgetTree
import edit_sizers
import common, os.path 

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
        wxFrame.__init__(self, parent, -1, "wxGlade - Palette",
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
        def append_item(menu, id, text, xpm_file=None):
            item = wxMenuItem(menu, id, text)
            if xpm_file is not None:
                try: item.SetBitmap(wxBitmap(xpm_file, wxBITMAP_TYPE_XPM))
                except AttributeError: pass
            menu.AppendItem(item)
        NEW_ID = wxNewId()
        append_item(file_menu, NEW_ID, "&New\tCtrl+N") #, 'icons/new.xpm')
        OPEN_ID = wxNewId()
        append_item(file_menu, OPEN_ID, "&Open...\tCtrl+O") #,'icons/open.xpm')
        SAVE_ID = wxNewId()
        append_item(file_menu, SAVE_ID, "&Save\tCtrl+S") #, 'icons/save.xpm')
        SAVE_AS_ID = wxNewId()
        append_item(file_menu, SAVE_AS_ID, "Save As...\tShift+Ctrl+S") #,
                    #'icons/save_as.xpm')
        file_menu.AppendSeparator()
        GENERATE_CODE_ID = wxNewId()
        append_item(file_menu, GENERATE_CODE_ID, "&Generate Code...\tCtrl+G")
        EXIT_ID = wxNewId()
        file_menu.AppendSeparator()
        append_item(file_menu, EXIT_ID, 'E&xit\tCtrl+X') #, 'icons/exit.xpm')
        menu_bar.Append(file_menu, "&File")
        menu_bar.Append(view_menu, "&View")
        TUT_ID = wxNewId()
        help_menu.Append(TUT_ID, 'Tutorial\tF1')
        ABOUT_ID = wxNewId()
        help_menu.Append(ABOUT_ID, 'About...')
        menu_bar.Append(help_menu, '&Help')
        parent.SetMenuBar(menu_bar)
        self.SetAcceleratorTable(wxAcceleratorTable([
            (wxACCEL_CTRL, ord('t'), TREE_ID),
            (wxACCEL_CTRL, ord('n'), NEW_ID),
            (wxACCEL_CTRL, ord('o'), OPEN_ID),
            (wxACCEL_CTRL, ord('s'), SAVE_ID),
            (wxACCEL_CTRL, ord('p'), PROPS_ID),
            (wxACCEL_CTRL|wxACCEL_SHIFT, ord('s'), SAVE_AS_ID),
            (wxACCEL_CTRL, ord('g'), GENERATE_CODE_ID),
            (wxACCEL_NORMAL, WXK_F1, TUT_ID)
            ]))
        EVT_MENU(parent, TREE_ID, self.show_tree)
        EVT_MENU(parent, PROPS_ID, self.show_props_window)
        EVT_MENU(parent, NEW_ID, self.new_app)
        EVT_MENU(parent, OPEN_ID, self.open_app)
        EVT_MENU(parent, SAVE_ID, self.save_app)
        EVT_MENU(parent, SAVE_AS_ID, self.save_app_as)
        def generate_code(event):
            common.app_tree.app.generate_code()
        EVT_MENU(parent, GENERATE_CODE_ID, generate_code)
        EVT_MENU(parent, EXIT_ID, lambda e: self.Close())
        EVT_MENU(parent, TUT_ID, self.show_tutorial)
        EVT_MENU(parent, ABOUT_ID, self.show_about_box)
        # Tutorial window
        self.tut_frame = None
        # layout
        self.SetAutoLayout(True)
        for b in buttons: sizer.Add(b)
        for sb in sizer_btns: sizer.Add(sb)
        self.SetSizer(sizer)
        sizer.Fit(self)
        # Properties window
        self.frame2 = wxFrame(parent, -1, 'Properties - <app>')
        self.frame2.SetBackgroundColour(wxSystemSettings_GetSystemColour(
            wxSYS_COLOUR_BTNFACE))
        self.frame2.SetIcon(icon)
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        property_panel = wxGladePropertyPanel(self.frame2, -1)
        property_panel.SetAutoLayout(True)
        self.hidden_frame = wxFrame(parent, -1, "")
        self.hidden_frame.Hide()
        sizer_tmp.Add(property_panel, 1, wxEXPAND)
        self.frame2.SetAutoLayout(True)
        self.frame2.SetSizer(sizer_tmp)
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        def hide_frame2(event):
            self.frame2.Hide()
            menu_bar.Check(PROPS_ID, False)
        EVT_CLOSE(self.frame2, hide_frame2)
        EVT_CLOSE(self, self.cleanup)
        common.property_panel = property_panel
        # Tree of widgets
        self.tree_frame = wxFrame(parent, -1, 'wxGlade: Tree', size=(350, 200))
        self.tree_frame.SetIcon(icon)
        import application
        app = application.Application(common.property_panel)
        common.app_tree = WidgetTree(self.tree_frame, app)

        app.notebook.Show()
        sizer_tmp.Add(app.notebook, 1, wxEXPAND)
        property_panel.SetSizer(sizer_tmp)
        sizer_tmp.Fit(property_panel)
        
        def on_tree_frame_close(event):
            self.tree_frame.Hide()
            menu_bar.Check(TREE_ID, True)
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
        self.Raise()

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
                                flags=wxOPEN|wxFILE_MUST_EXIST)
        if infile: self._open_app(infile)

    def _open_app(self, infile, use_progress_dialog=True):
        import time
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder
        start = time.clock()

        common.app_tree.clear()
        common.app_tree.app.filename = infile
        common.property_panel.Reparent(self.hidden_frame)
        # prevent the auto-expansion of nodes
        common.app_tree.auto_expand = False

        try:
            infile = open(infile)
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
                            flags=wxSAVE|wxOVERWRITE_PROMPT)
        common.app_tree.app.filename = fn
        if fn: self.save_app(event)

    def cleanup(self, event):
        if self.ask_save():
            common.app_tree.clear()
            self.Destroy()
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
        import about
        about.wxGladeAboutBox(self.GetParent()).ShowModal()

    def show_tutorial(self, event):
        if not self.tut_frame:
            from wxPython.html import wxHtmlWindow
            self.tut_frame = wxFrame(self, -1, "wxGlade Tutorial")
            html = wxHtmlWindow(self.tut_frame, -1)
            html.LoadPage('docs/tutorial.html')
            self.tut_frame.SetSize((640, 480))
            EVT_CLOSE(self.tut_frame, lambda e: self.tut_frame.Hide())
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
