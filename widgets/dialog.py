# dialog.py: wxDialog objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, math, misc
from tree import Tree, MenuTree
from widget_properties import *
from edit_windows import TopLevelBase

class EditDialog(TopLevelBase):
    def __init__(self, name, parent, id, title, property_window,
                 style=wxDEFAULT_DIALOG_STYLE, show=True, klass='wxDialog'):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show)
        prop = self.properties
        # style property
        self.access_functions['style'] = (self.get_style, self.set_style)
        style_labels = ('#section#Style', 'wxDIALOG_MODAL', 'wxCAPTION',
                        'wxRESIZE_BORDER', 'wxSYSTEM_MENU', 'wxTHICK_FRAME',
                        'wxSTAY_ON_TOP', 'wxNO_3D', 'wxDIALOG_NO_PARENT')
        self.style_pos = (wxDIALOG_MODAL, wxCAPTION, wxRESIZE_BORDER,
                          wxSYSTEM_MENU, wxTHICK_FRAME, wxSTAY_ON_TOP, wxNO_3D,
                          wxDIALOG_NO_PARENT)
        # remove the unused _rmenu items Copy and Cut
        for label in ("Copy", "Cut"):
            item_id = self._rmenu.FindItem(label)
            self._rmenu.Delete(item_id)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        self.has_sizer = False # if True, the dialog already has a sizer

    def create_properties(self):
        TopLevelBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['style'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, math.ceil(w/5.0), math.ceil(h/5.0))

    def on_enter(self, event):
        if not self.has_sizer and common.adding_sizer:
            self.SetCursor(wxCROSS_CURSOR)
        else:
            self.SetCursor(wxNullCursor)

    def drop_sizer(self, event):
        if self.has_sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        common.adding_widget = common.adding_sizer = False
        self.widget.SetCursor(wxNullCursor)
        common.widgets[common.widget_to_add](self, None, None)
        common.widget_to_add = None
        self.has_sizer = True

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget:
            self.SetWindowStyleFlag(self.style)

    def create_widget(self):
        if self.parent:
            parent = self.parent.widget
        else:
            parent = None
        self.widget = wxDialog(parent, self.id, self.title, style=self.style)
        # event handlers
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        EVT_CLOSE(self.widget, self.ask_remove)
        self.widget.SetAutoLayout(True)
        self.widget.SetSize((400, 300))
        # !!! Show is always False (sse WindowBase). However EditBase wants
        # to show the widget, without checking if it's created, so this
        # must be modified.
        # self.widget.Show(self.show)

    def ask_remove(self, event):
        if wxPlatform == '__WXMSW__':
            # this msgbox causes a segfault on GTK... and I don't know why :-(
            if wxMessageBox("Do you want to remove this dialog\n"
                            "from the current app?", "Are you sure?",
                            wxYES_NO|wxCENTRE|wxICON_QUESTION) == wxYES:
                self.remove()
        else:
            wxMessageBox("To remove the dialog, right-click on it on the "
                         "tree\nand select the 'remove' option", "Information",
                         wxOK|wxCENTRE|wxICON_INFORMATION, self)

    def on_size(self, event):
        TopLevelBase.on_size(self, event)
        if self.has_sizer: self.widget.GetSizer().Refresh()

# end of class EditFrame

        
def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditDialog objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select dialog class')
            if not number[0]: self.klass = 'MyDialog'
            else: self.klass = 'MyDialog%s' % number[0]
            number[0] += 1
            klass_prop = TextProperty(self, 'class', self)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(klass_prop, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
        def __getitem__(self, value):
            def set_klass(c): self.klass = c
            return (lambda : self.klass, set_klass)
    # end of inner class

    class_dialog = Dialog()
    class_dialog.ShowModal()
    label = 'dialog_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'dialog_%d' % number[0]
    dialog = EditDialog(label, parent, wxNewId(), label, common.property_panel,
                        klass=class_dialog.klass)
    node = Tree.Node(dialog)
    dialog.node = node
    common.app_tree.add(node)
    class_dialog.Destroy()

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditDialog objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    dialog = EditDialog(label, parent, wxNewId(), label, common.property_panel,
                        show=False)
    node = Tree.Node(dialog)
    dialog.node = node
    common.app_tree.add(node)
    return dialog


def generate_dialog_properties(dialog):
    """\
    generates the code for the various wxDialog specific properties.
    Returns a list of strings containing the generated code
    """
    prop = dialog.properties
    pygen = common.code_writers['python']
    out = []
    title = prop.get('title')
    if title: out.append('self.SetTitle("%s")\n' % title)
    out.extend(pygen.generate_common_properties(dialog))
    return out


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditDialog'] = xml_builder

    common.widgets['EditDialog'] = builder
    
    cn = common.class_names
    cn['EditDialog'] = 'wxDialog'
    
    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxDialog', lambda o: None, generate_dialog_properties)
        
    return common.make_object_button('EditDialog', 'icons/dialog.xpm', 1)
