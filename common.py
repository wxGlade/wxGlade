# common.py: global variables and Application class
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
from widget_properties import *
from tree import Tree, WidgetTree
import math, misc

# version identification string
version = '0.1.2'

# widgets dictionary: each key is the name of some EditWidget class; the mapped
# value is a 'factory' function which actually builds the object. Each of these
# functions accept 3 parameters: the parent of the widget, the sizer by which
# such widget is controlled, and the position inside this sizer.
widgets = {}

# widgets_from_xml dictionary: table of factory functions to build objects from
# an xml file
widgets_from_xml = {}

# property_panel wxPanel: container inside which Properties of the current
# focused widget are displayed
property_panel = None

# app_tree Tree: represents the widget hierarchy of the application; the root 
# is the application itself 
app_tree = None

# if True, the user is adding a widget to some sizer
adding_widget = False
# needed to add toplevel sizers
adding_sizer = False

# reference to the widget that is being added: this is a key in the
# 'widgets' dictionary
widget_to_add = None

# reference to the main window (the one which contains the various buttons to
# add the different widgets)
palette = None

# dictionary which maps the ids used in the event handlers to the
# corresponding widgets: used to call the appropriate builder function
# when a dropping of a widget occurs, knowing only the id of the event
refs = {}

# dictionary which maps the name of the classes used by wxGlade to the
# correspondent classes of wxWindows
class_names = {}

"""
dictionary of objects used to generate the code in a given language.
NOTE: a code writer object must implement this interface:
  - initialize(out_path, multi_files)
  - language
  - add_widget_handler(widget_name, handler[, properties_handler])
  - add_property_handler(property_name, handler[, widget_name])
  - add_object(top_obj, sub_obj)
  - add_class(obj)
  - add_sizeritem(toplevel, sizer, obj_name, option, flag, border)
  - add_app(app_attrs, top_win_class)
  - ...
"""
code_writers = {}


# function used by the various widget modules to add a button to the widgets
# toolbar
def make_object_button(widget, icon_path, toplevel=False, tip=None):
    """\
    creates a button for the widgets toolbar.
    Params:
      - widget: (name of) the widget the button will add to the app
      - icon_path: path to the icon used for the button
      - toplevel: true if the widget is a toplevel object (frame, dialog)
      - tip: tool tip to display
    Returns:
      the newly created wxBitmapButton
    """
    id = wxNewId()
    tmp = wxBitmapButton(palette, id, wxBitmap(icon_path, wxBITMAP_TYPE_XPM),
                         size=(31, 31))
    if not toplevel:
        EVT_BUTTON(palette, id, palette.add_object)
    else:
        EVT_BUTTON(palette, id, palette.add_toplevel_object)
    refs[id] = widget
    if not tip:
        tip = 'Add a %s' % widget.replace('Edit', '')
    tmp.SetToolTip(wxToolTip(tip))

    WidgetTree.images[widget] = icon_path
    return tmp


class FileDirDialog:
    """\
    Custom class which displays a FileDialog or a DirDialog, according to the
    value of the codegen_opt of its parent (instance of Application)
    """
    def __init__(self, owner, parent, wildcard="All Files|*",
                 file_message="Choose a file", dir_message=None, style=0):
        self.owner = owner
        self.file_dialog = wxFileDialog(parent, file_message,
                                        wildcard=wildcard, style=style)
        if dir_message is None: dir_message = file_message
        log_null = wxLogNull() # to prevent popup messages about lack of
                               # permissions to view the contents of
                               # some directories
        self.dir_dialog = wxDirDialog(parent, dir_message)
        del log_null

    def ShowModal(self):
        if self.owner.codegen_opt == 0: return self.file_dialog.ShowModal()
        else: return self.dir_dialog.ShowModal()

    def get_value(self):
        if self.owner.codegen_opt == 0: return self.file_dialog.GetPath()
        else: return self.dir_dialog.GetPath()

# end of class FileDirDialog


class Application(object):
    """\
    properties of the application being created
    """
    def __init__(self, property_window):
        self.property_window = property_window
        self.notebook = wxNotebook(self.property_window, -1)
        nb_sizer = wxNotebookSizer(self.notebook)
        self.notebook.SetAutoLayout(True)
        self.notebook.sizer = nb_sizer
        self.notebook.Hide()
        panel = wxPanel(self.notebook, -1)
        self.name = "app" # name of the wxApp instance to generate
        self.__saved = True # if True, there are no changes to save
        self.__filename = None # name of the output xml file
        def set_name(value): self.name = str(value)
        self.klass = "MyApp"
        def set_klass(value): self.klass = str(value)
        self.codegen_opt = 0 # if != 0, generates a separate file
                             # for each class 
        def set_codegen_opt(value):
            try: opt = int(value)
            except ValueError: pass
            else: self.codegen_opt = opt
        self.output_path = ""
        def set_output_path(value): self.output_path = value
        self.access_functions = {
            'name': (lambda : self.name, set_name),
            'class': (lambda : self.klass, set_klass), 
            'code_generation': (lambda : self.codegen_opt, set_codegen_opt),
            'output_path': (lambda : self.output_path, set_output_path),
            'language': (lambda : 0, lambda v: None)
            }
        TOP_WIN_ID = wxNewId()
        self.top_win_prop = wxChoice(panel, TOP_WIN_ID, choices=[])
        self.top_window = '' # name of the top window of the generated app
        
        self.name_prop = TextProperty(self, "name", panel, True)
        self.klass_prop = TextProperty(self, "class", panel, True)

        self.codegen_prop = RadioProperty(self, "code_generation", panel,
                                          ["Single file", "Separate file for" \
                                           " each class"])
        dialog = FileDirDialog(self, panel, "Python Files|*.py|All Files|*",
                               "Select output file", "Select output directory",
                               wxSAVE|wxOVERWRITE_PROMPT)
        self.outpath_prop = DialogProperty(self, "output_path", panel,
                                           dialog)
        
        self.codewriters_prop = RadioProperty(self, "language", panel,
                                              code_writers.keys())
        
        BTN_ID = wxNewId()
        btn = wxButton(panel, BTN_ID, "Generate code")

        # layout of self.notebook
        sizer = misc.Sizer(wxVERTICAL)
        sizer.Add(self.name_prop.panel, 0, wxEXPAND)
        sizer.Add(self.klass_prop.panel, 0, wxEXPAND)
        szr = wxBoxSizer(wxHORIZONTAL)
        szr.Add(wxStaticText(panel, -1, "Top window"), 2,
                wxALL|wxALIGN_CENTER, 3)
        szr.Add(self.top_win_prop, 5, wxALL|wxALIGN_CENTER, 3)
        sizer.Add(szr, 0, wxEXPAND)
        sizer.Add(self.codegen_prop.panel, 0, wxALL|wxEXPAND, 4)
        sizer.Add(self.codewriters_prop.panel, 0, wxALL|wxEXPAND, 4)
        sizer.Add(self.outpath_prop.panel, 0, wxEXPAND)
        sizer.Add(btn, 0, wxALL|wxEXPAND, 5)
        
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer)
        sizer.Layout()
        sizer.Fit(panel)
        self.notebook.AddPage(panel, "Application")

        EVT_BUTTON(btn, BTN_ID, self.generate_code)
        EVT_CHOICE(self.top_win_prop, TOP_WIN_ID, self.set_top_window)

        # this is here to keep the interface similar to the various widgets
        # (to simplify Tree)
        widget = None # this is always None

    def _get_saved(self): return self.__saved
    def _set_saved(self, value):
        if self.__saved != value:
            self.__saved = value
            t = app_tree.get_title()
            if not value: app_tree.set_title('* ' + t)
            else:
                if t[0] == '*': app_tree.set_title(t[1:])
    saved = property(_get_saved, _set_saved)

    def _get_filename(self): return self.__filename
    def _set_filename(self, value):
        if self.__filename != value:
            self.__filename = value
            if self.__saved: flag = ' '
            else: flag = '* '
            app_tree.set_title('%s(%s)' % (flag, self.__filename))
    filename = property(_get_filename, _set_filename)
       
    def get_top_window(self): return self.top_window

    def set_top_window(self, *args):
        self.top_window = self.top_win_prop.GetStringSelection()

    def add_top_window(self, name):
        self.top_win_prop.Append(str(name))

    def remove_top_window(self, name):
        index = self.top_win_prop.FindString(str(name))
        if index != -1:
            if wxPlatform == '__WXGTK__':
                choices = [ self.top_win_prop.GetString(i) for i in \
                            range(self.top_win_prop.Number()) if i != index ]
                self.top_win_prop.Clear()
                for c in choices:
                    self.top_win_prop.Append(c)
            else:
                self.top_win_prop.Delete(index)
        
    def reset(self):
        """\
        resets the default values of the attributes of the app
        """
        self.klass = "MyApp"; self.klass_prop.set_value("MyApp")
        self.klass_prop.toggle_active(False)
        self.name = "app"; self.name_prop.set_value("app")
        self.codegen_opt = 0; self.codegen_prop.set_value(0)
        self.output_path = ""; self.outpath_prop.set_value("")
        self.top_window = ''
        self.top_win_prop.Clear()
        
    def show_properties(self, *args):
        sizer_tmp = self.property_window.GetSizer()
        sizer_tmp = wxPyTypeCast(sizer_tmp, "wxBoxSizer")
        child = wxPyTypeCast(sizer_tmp.GetChildren()[0], "wxSizerItem")
        w = wxPyTypeCast(child.GetWindow(), "wxWindow")
        if w is self.notebook: return
        w.Hide()
        child.SetWindow(self.notebook)
        self.notebook.Show(True)
        self.property_window.Layout()
        self.property_window.SetTitle('Properties - <%s>' % self.name)
        try: app_tree.select_item(self.node)
        except AttributeError: pass

    def __getitem__(self, name):
        return self.access_functions[name]

    def generate_code(self, event):
        if not self.output_path:
            return wxMessageDialog(self.notebook, "You must specify an output "
                                   "file\nbefore generating any code", "Error",
                                   wxOK | wxCENTRE |
                                   wxICON_EXCLAMATION).ShowModal()
        if (self.name_prop.is_active() or self.klass_prop.is_active()) and \
               self.top_win_prop.GetSelection() < 0:
            return wxMessageDialog(self.notebook, "Please select a top window "
                                   "for the application", "Error", wxOK |
                                   wxCENTRE | wxICON_EXCLAMATION).ShowModal()
                
        from cStringIO import StringIO
        out = StringIO()
        app_tree.write(out) # write the xml onto a temporary buffer
        from xml_parse import CodeWriter
        try:
            # generate the code from the xml buffer
            CodeWriter(code_writers[self.codewriters_prop.get_str_value()],
                       out.getvalue(), True) 
        except Exception, msg:
            import traceback; traceback.print_exc()
            wxMessageBox("Error generating code:\n%s" % msg, "Error",
                         wxOK|wxCENTRE|wxICON_ERROR)
        else:
            wxMessageBox("Code generation completed successfully",
                         "Information", wxOK|wxCENTRE|wxICON_INFORMATION)

    def get_name(self):
        if self.name_prop.is_active(): return self.name
        return ''

    def get_class(self):
        if self.klass_prop.is_active(): return self.klass
        return ''

    def update_view(self, *args): pass

    def is_visible(self): return True

# end of class Application
