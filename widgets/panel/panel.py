# panel.py: wxPanel objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, TopLevelBase

class PanelBase(object):
    def __init__(self):
        """\
        Class to handle wxPanel objects
        """
        super(PanelBase, self).__init__()
        self.top_sizer = None # sizer to handle the layout of children
        self.style = wxTAB_TRAVERSAL
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.style_pos  = (wxSIMPLE_BORDER, wxDOUBLE_BORDER, wxSUNKEN_BORDER,
                           wxRAISED_BORDER, wxSTATIC_BORDER, wxNO_3D,
                           wxTAB_TRAVERSAL, wxWANTS_CHARS,
                           wxNO_FULL_REPAINT_ON_RESIZE, wxCLIP_CHILDREN)

        style_labels = ('#section#Style', 'wxSIMPLE_BORDER', 'wxDOUBLE_BORDER',
                        'wxSUNKEN_BORDER', 'wxRAISED_BORDER',
                        'wxSTATIC_BORDER', 'wxNO_3D', 'wxTAB_TRAVERSAL',
                        'wxWANTS_CHARS', 'wxNO_FULL_REPAINT_ON_RESIZE',
                        'wxCLIP_CHILDREN')
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)  

    def finish_widget_creation(self):
        super(PanelBase, self).finish_widget_creation()
        # this must be done here since ManagedBase.finish_widget_creation
        # normally sets EVT_LEFT_DOWN to update_wiew
        if not self.widget.Disconnect(-1, -1, wxEVT_LEFT_DOWN):
            print "EditPanel: Unable to disconnect the event hanlder"
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)

    def create_properties(self):
        super(PanelBase, self).create_properties()
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['style'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        
    def on_enter(self, event):
        if not self.top_sizer and common.adding_sizer:
            self.widget.SetCursor(wxCROSS_CURSOR)
        else:
            self.widget.SetCursor(wxNullCursor)

    def set_sizer(self, sizer):
        self.top_sizer = sizer
        if self.top_sizer and self.top_sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.top_sizer.widget)
            self.widget.Layout()
            #self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

    def drop_sizer(self, event):
        if self.top_sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        self.widget.SetCursor(wxNullCursor)
        common.widgets[common.widget_to_add](self, None, None)
        common.adding_widget = common.adding_sizer = False
        common.widget_to_add = None
        common.app_tree.app.saved = False

    def get_widget_best_size(self):
        if self.top_sizer and self.widget.GetSizer():
            #return self.widget.GetSizer().CalcMin()
            self.top_sizer.fit_parent()
            return self.widget.GetSize()
        return wxPanel.GetBestSize(self.widget)

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
        #if self.widget: self.widget.SetWindowStyleFlag(self.style)    

# end of class PanelBase
    

class EditPanel(PanelBase, ManagedBase):
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxPanel objects
        """
        ManagedBase.__init__(self, name, 'wxPanel', parent, id, sizer,
                             pos, property_window, show=show)
        PanelBase.__init__(self)

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, self.id)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size

    def set_sizer(self, sizer):
        super(EditPanel, self).set_sizer(sizer)
        if self.top_sizer and self.top_sizer.widget and self.widget:
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

# end of class EditPanel


class EditTopLevelPanel(PanelBase, TopLevelBase):
    def __init__(self, name, parent, id, property_window, klass='wxPanel',
                 show=True):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show, has_title=False)
        PanelBase.__init__(self)
        self.skip_on_size = False

    def create_widget(self):
        win = wxFrame(None, -1, self.name, size=(400, 300)) 
        self.widget = wxPanel(win, self.id)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        self.widget.SetSize = win.SetSize
        EVT_CLOSE(win, self.hide_widget)
        if wxPlatform == '__WXMSW__': win.CentreOnScreen()

    def show_widget(self, yes):
        super(EditTopLevelPanel, self).show_widget(yes)
        if self.widget:
            if yes and not self.properties['size'].is_active() \
                   and self.top_sizer:
                self.top_sizer.fit_parent()
            self.widget.GetParent().Show(yes)

    def hide_widget(self, *args):
        super(EditTopLevelPanel, self).hide_widget(*args)
        self.widget.GetParent().Hide()

    def set_name(self, name):
        super(EditTopLevelPanel, self).set_name()
        if self.widget: self.widget.GetParent().SetTitle(self.name)

    def delete(self):
        win = None
        if self.widget: win = self.widget.GetParent()
        super(EditTopLevelPanel, self).delete()
        if win is not None: win.Destroy()

    def on_size(self, event):
        if wxPlatform == '__WXMSW__' and self.skip_on_size:
            self.skip_on_size = False
            return
        super(EditTopLevelPanel, self).on_size(event)
        w, h = self.widget.GetSize()
        self.skip_on_size = True
        self.widget.GetParent().SetClientSize((w+2, h+2))

# end of class EditTopLevelPanel
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditPanel objects.
    """
    name = 'panel_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'panel_%d' % number[0]
    panel = EditPanel(name, parent, wxNewId(), sizer, pos,
                      common.property_panel)
    node = Tree.Node(panel)
    panel.node = node

    panel.set_option(1)
    panel.set_flag("wxEXPAND")
    panel.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(panel.pos, 1, wxEXPAND)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditPanel objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    panel = EditPanel(name, parent, wxNewId(), sizer, pos,
                      common.property_panel, True)
    sizer.set_item(panel.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(panel)
    panel.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return panel

def xml_toplevel_builder(attrs, parent, sizer, sizeritem, pos=None):
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    panel = EditTopLevelPanel(label, parent, wxNewId(), common.property_panel,
                              show=False)
    node = Tree.Node(panel)
    panel.node = node
    common.app_tree.add(node)
    return panel


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditPanel'] = builder
    common.widgets_from_xml['EditPanel'] = xml_builder

    common.widgets_from_xml['EditTopLevelPanel'] = xml_toplevel_builder
    from tree import WidgetTree
    WidgetTree.images['EditTopLevelPanel'] = 'icons/panel.xpm'
        
    return common.make_object_button('EditPanel', 'icons/panel.xpm')
    
