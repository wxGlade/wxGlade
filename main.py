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
        self.load_code_writers()
        # load the available widgets and sizers
        buttons = self.load_widgets()
        sizer_btns = self.load_sizers()
        
        TREE_ID = wxNewId()
        view_menu.Append(TREE_ID, "Show &Tree\tCtrl+T", checkable=True)
        view_menu.Check(TREE_ID, True)
        NEW_ID = wxNewId()
        file_menu.Append(NEW_ID, "&New\tCtrl+N")
        OPEN_ID = wxNewId()
        file_menu.Append(OPEN_ID, "&Open...\tCtrl+O")
        SAVE_ID = wxNewId()
        file_menu.Append(SAVE_ID, "&Save\tCtrl+S")
        SAVE_AS_ID = wxNewId()
        file_menu.Append(SAVE_AS_ID, "Save As...")
        EXIT_ID = wxNewId()
        file_menu.AppendSeparator()
        file_menu.Append(EXIT_ID, 'E&xit\tCtrl+X')
        menu_bar.Append(file_menu, "&File")
        menu_bar.Append(view_menu, "&View")
        ABOUT_ID = wxNewId()
        help_menu.Append(ABOUT_ID, 'About...')
        menu_bar.Append(help_menu, '&Help')
        parent.SetMenuBar(menu_bar)
        self.SetAcceleratorTable(wxAcceleratorTable([
            (wxACCEL_CTRL, ord('t'), TREE_ID),
            (wxACCEL_CTRL, ord('n'), NEW_ID),
            (wxACCEL_CTRL, ord('o'), OPEN_ID),
            (wxACCEL_CTRL, ord('s'), SAVE_ID)
            ]))
        EVT_MENU(parent, TREE_ID, self.show_tree)
        EVT_MENU(parent, NEW_ID, self.new_app)
        EVT_MENU(parent, OPEN_ID, self.open_app)
        EVT_MENU(parent, SAVE_ID, self.save_app)
        EVT_MENU(parent, SAVE_AS_ID, self.save_app_as)
        EVT_MENU(parent, EXIT_ID, lambda e: self.Close())
        EVT_MENU(parent, ABOUT_ID, self.show_about_box)
        # layout
        self.SetAutoLayout(True)
        for b in buttons: sizer.Add(b)
        for sb in sizer_btns: sizer.Add(sb)
        self.SetSizer(sizer)
        sizer.Fit(self)
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
        EVT_CLOSE(self.frame2, self.hide_frame2)
        EVT_CLOSE(self, self.cleanup)
        common.property_panel = property_panel
        # setup of tree_frame
        self.tree_frame = wxFrame(parent, -1, 'wxGlade: Tree', size=(350, 200))
        self.tree_frame.SetIcon(icon)
        app = common.Application(common.property_panel)
        common.app_tree = WidgetTree(self.tree_frame, app)

        app.notebook.Show()
        sizer_tmp.Add(app.notebook, 1, wxEXPAND)
        property_panel.SetSizer(sizer_tmp)
        sizer_tmp.Fit(property_panel)
        
        def on_tree_frame_close(event):
            self.tree_frame.Hide()
            menu_bar.Check(TREE_ID, 0)
        EVT_CLOSE(self.tree_frame, on_tree_frame_close)
        self.frame2.SetSize((250, 340))
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

    def load_widgets(self):
        """\
        Scans the 'widgets/' directory to find the installed widgets,
        and returns a list of buttons to handle them
        """
        buttons = []
        modules = open('widgets/widgets.txt')
        print 'loading widget modules:'
        for line in modules:
            module = line.strip()
            if not module or module.startswith('#'): continue
            module = module.split('#')[0].strip()
            try:
                b = __import__(module).initialize()
            except (ImportError, AttributeError):
                print 'ERROR loading "%s"' % module
                import traceback; traceback.print_exc()
            else:
                print '\t' + module
                buttons.append(b)
        modules.close()
        return buttons

    def load_sizers(self):
        import edit_sizers
        return edit_sizers.init_all()

    def load_code_writers(self):
        """\
        Fills the common.code_writers dictionary: to do so, loads the modules
        found in the 'codegen/' subdir
        """
        import imp
        sys.path.append('codegen')
        for module in os.listdir('codegen'):
            name, ext = os.path.splitext(module)
            if name not in sys.modules and \
                   os.path.isfile(os.path.join('codegen', module)):
                try: writer = __import__(name).writer
                except (ImportError, AttributeError):
                    print '"%s" is not a valid code generator module' % module
                else:
                    common.code_writers[writer.language] = writer
                    print 'loaded code generator for %s' % writer.language

    def hide_frame2(self, event):
        self.frame2.Hide()

    def show_tree(self, event):
        self.tree_frame.Show(event.IsChecked())

    def ask_save(self):
        """\
        checks wether the current app has changed and needs to be saved: if so,
        prompts the user; returns False if the operation has been cancelled
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
        
    def open_app(self, event):
        """\
        loads a wxGlade project from an xml file
        NOTE: this is very slow and needs optimisation efforts
        NOTE2: the note above should not be True anymore :)
        """
        if not self.ask_save(): return
        from xml_parse import XmlWidgetBuilder, ProgressXmlWidgetBuilder
        infile = wxFileSelector("Open file", wildcard="XML files|*.xml|" \
                                "All files|*", flags=wxOPEN|wxFILE_MUST_EXIST)
        if infile:
            import time
            start = time.clock()
            
            common.app_tree.clear()
            common.app_tree.app.filename = infile
            common.property_panel.Reparent(self.hidden_frame)

            try:
                infile = open(infile)
                p = ProgressXmlWidgetBuilder(input_file=infile)
                p.parse(infile)
            except Exception, msg:
                import traceback; traceback.print_exc()
                
                if locals().has_key('infile'): infile.close()
                common.app_tree.clear()
                common.property_panel.Reparent(self.frame2)
                common.app_tree.app.saved = True
                wxMessageBox("Error loading file:\n%s" % msg, "Error",
                             wxOK|wxCENTRE|wxICON_ERROR)
                return 

            infile.close()
            for node in common.app_tree.root.children:
                common.app_tree.expand(node, False)
##                 ww = w.widget
##                 ww.Show()
##                 if ww.has_sizer:
##                     ww.GetSizer().Layout()
##                     if not ww.properties['size'].is_active():
##                         ww.GetSizer().Fit(ww)
            common.app_tree.select_item(common.app_tree.root)
            common.app_tree.root.widget.show_properties()
            common.property_panel.Reparent(self.frame2)

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
                            wildcard="XML files|*.xml|All files|*",
                            flags=wxSAVE|wxOVERWRITE_PROMPT)
        common.app_tree.app.filename = fn
        if fn: self.save_app(event)

    def cleanup(self, event):
        if self.ask_save():
            common.app_tree.clear()
            common.app_tree.remove()
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


def main():    
    app = wxGlade()
    app.MainLoop()
