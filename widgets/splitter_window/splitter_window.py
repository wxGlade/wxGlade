# splitter_window.py: wxSplitterWindow objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, WindowBase, EditBase

class SplitterPane(WindowBase):
    def __init__(self, name, parent, id, pos, property_window, show=True):
        WindowBase.__init__(self, name, 'wxPanel', parent,
                            id, property_window, show)
        self.sizer = None
        self.sel_marker = None 

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, self.id)
        self.sel_marker = misc.SelectionMarker(self.widget, self.parent.widget)
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        EVT_MOVE(self.widget, self.on_move)
        def GetBestSize():
            if self.sizer and self.widget:
                return self.widget.GetSizer().GetMinSize()
            return wxPanel.GetBestSize(self.widget)
        self.widget.GetBestSize = GetBestSize
        
    def set_sizer(self, sizer):
        self.sizer = sizer
        if self.sizer and self.sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.sizer.widget)    
            self.widget.Layout()
        
    def popup_menu(self, *args): pass

    def on_enter(self, event):
        if not self.sizer and common.adding_sizer:
            self.widget.SetCursor(wxCROSS_CURSOR)
        else:
            self.widget.SetCursor(wxNullCursor)

    def drop_sizer(self, event):
        if self.sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        self.widget.SetCursor(wxNullCursor)
        common.widgets[common.widget_to_add](self, None, None)
        common.adding_widget = common.adding_sizer = False
        common.widget_to_add = None
        common.app_tree.app.saved = False
    
    def update_view(self, selected):
        if self.sel_marker: self.sel_marker.Show(selected)

    def on_move(self, event): self.sel_marker.update()

    def on_size(self, event):
        WindowBase.on_size(self, event)
        self.sel_marker.update()

    def delete(self):
        if not self.widget: return
        if self.sel_marker: self.sel_marker.Destroy()
        WindowBase.delete(self)
        
# end of class SplitterPane


class EditSplitterWindow(ManagedBase):
    def __init__(self, name, parent, id, style, win_1, win_2, orientation,
                 sizer, pos, property_window, show=True):
        """\
        Class to handle wxSplitterWindow objects
        """
        ManagedBase.__init__(self, name, 'wxSplitterWindow', parent, id, sizer,
                             pos, property_window, show=show)
        if style is None: style = wxSP_3D
        self.style = style
        self.window_1 = win_1 
        self.window_2 = win_2 
        self.orientation = orientation
        self.sash_pos = 0

        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['sash_pos'] = (self.get_sash_pos,
                                             self.set_sash_pos)

        self.style_pos  = (wxSP_3D, wxSP_3DSASH, wxSP_3DBORDER,
                           wxSP_FULLSASH, wxSP_BORDER, wxSP_NOBORDER,
                           wxSP_PERMIT_UNSPLIT, wxSP_LIVE_UPDATE)
        style_labels = ('#section#Style', 'wxSP_3D', 'wxSP_3DSASH',
                        'wxSP_3DBORDER', 'wxSP_FULLSASH', 'wxSP_BORDER',
                        'wxSP_NOBORDER', 'wxSP_PERMIT_UNSPLIT',
                        'wxSP_LIVE_UPDATE')

        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)
        if self.orientation == wxSPLIT_HORIZONTAL:
            od = 'wxSPLIT_HORIZONTAL'
        else: od = 'wxSPLIT_VERTICAL'
        self.access_functions['orientation'] = (self.get_orientation,
                                                self.set_orientation)
        self.properties['orientation'] = HiddenProperty(self, 'orientation')

        self.access_functions['window_1'] = (self.get_win_1, lambda v: None)
        self.access_functions['window_2'] = (self.get_win_2, lambda v: None)
        self.properties['window_1'] = HiddenProperty(self, 'window_1')
        self.properties['window_2'] = HiddenProperty(self, 'window_2')

        self.properties['sash_pos'] = SpinProperty(self, 'sash_pos', None,
                                                   r=(0, 20),
                                                   can_disable=True) 

    def create_widget(self):
        self.widget = wxSplitterWindow(self.parent.widget, self.id,
                                       style=self.style)
        self.split()
        EVT_SPLITTER_SASH_POS_CHANGED(self.widget, self.widget.GetId(),
                                      self.on_sash_pos_changed)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        sp = self.properties['sash_pos']
        if not sp.is_active():
            if self.orientation == wxSPLIT_HORIZONTAL:
                max_pos = self.widget.GetClientSize()[1]
            else: max_pos = self.widget.GetClientSize()[0]
            sp.set_range(0, max_pos)
            sp.set_value(max_pos/2)
            self.set_sash_pos(max_pos/2)
        else:
            self.set_sash_pos(sp.get_value())
        
    def on_set_focus(self, event):
        self.show_properties()
        # here we must call event.Skip() also on Win32 as this we should be
        # able to move the sash
        event.Skip()

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        sizer = wxBoxSizer(wxVERTICAL)
        self.properties['style'].display(panel)
        self.properties['sash_pos'].display(panel)
        sizer.Add(self.properties['style'].panel, 0, wxEXPAND)
        sizer.Add(self.properties['sash_pos'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        
    def split(self):
        if not self.widget: return
        if self.window_1 and self.window_2:
            self.window_1.show_widget(True)
            self.window_2.show_widget(True)
            if self.orientation == wxSPLIT_VERTICAL:
                self.widget.SplitVertically(self.window_1.widget,
                                            self.window_2.widget)
            else:
                self.widget.SplitHorizontally(self.window_1.widget,
                                              self.window_2.widget)
            sp = self.properties['sash_pos'].get_value()
            if not sp:
                w, h = self.widget.GetClientSize()
                if self.orientation == wxSPLIT_VERTICAL:
                    self.widget.SetSashPosition(w/2)
                else: self.widget.SetSashPosition(h/2)
            else: self.widget.SetSashPosition(sp)

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]: retval[i] = 1
        except AttributeError: pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

    def get_sash_pos(self): return self.sash_pos

    def set_sash_pos(self, value):
        try: value = int(value)
        except ValueError: return
        self.sash_pos = value
        if self.widget:
            self.widget.SetSashPosition(value)

    def on_size(self, event):
        if not self.widget: return
        try:
            if self.orientation == wxSPLIT_VERTICAL:
                max_pos = self.widget.GetClientSize()[0]
            else: max_pos = self.widget.GetClientSize()[1]
            self.properties['sash_pos'].set_range(0, max_pos)
            if not self.properties['sash_pos'].is_active():
                self.widget.SetSashPosition(max_pos/2)
        except (AttributeError, KeyError): pass
        ManagedBase.on_size(self, event)

    def on_sash_pos_changed(self, event):
        self.sash_pos = self.widget.GetSashPosition()
        self.properties['sash_pos'].set_value(self.sash_pos)
        event.Skip()

    def get_orientation(self):
        od = { wxSPLIT_HORIZONTAL: 'wxSPLIT_HORIZONTAL',
               wxSPLIT_VERTICAL: 'wxSPLIT_VERTICAL' }
        return od.get(self.orientation, 'wxSPLIT_VERTICAL')

    def set_orientation(self, value):
        od = { 'wxSPLIT_HORIZONTAL': wxSPLIT_HORIZONTAL,
               'wxSPLIT_VERTICAL': wxSPLIT_VERTICAL }
        self.orientation = od.get(value, wxSPLIT_VERTICAL)

    def get_win_1(self):
        if self.window_1: return self.window_1.name
        return ''

    def get_win_2(self):
        if self.window_2: return self.window_2.name
        return ''
    
# end of class EditSplitterWindow
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditSplitterWindow objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select orientation')
            self.orientations = [ wxSPLIT_VERTICAL, wxSPLIT_HORIZONTAL ]
            self.orientation = wxSPLIT_VERTICAL
            prop = RadioProperty(self, 'orientation', self,
                                 ['wxSPLIT_VERTICAL', 'wxSPLIT_HORIZONTAL'])
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
        def __getitem__(self, value):
            def set_orientation(o): self.orientation = self.orientations[o]
            return (lambda: self.orientation, set_orientation)
    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    name = 'window_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'window_%d' % number[0]
    window = EditSplitterWindow(name, parent, wxNewId(), None, None, None,
                                dialog.orientation,
                                sizer, pos, common.property_panel, show=False)
    pane1 = SplitterPane(name + '_pane_1', window, wxNewId(), 1,
                         common.property_panel)
    pane2 = SplitterPane(name + '_pane_2', window, wxNewId(), 2,
                         common.property_panel)
    window.window_1 = pane1
    window.window_2 = pane2
    
    node = Tree.Node(window)
    window.node = node

    window.set_option(1)
    window.set_flag("wxEXPAND")
    window.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos-1)

    node2 = Tree.Node(window.window_1)
    window.window_1.node = node2
    common.app_tree.add(node2, window.node)

    node3 = Tree.Node(window.window_2)
    window.window_2.node = node3
    common.app_tree.add(node3, window.node)

    sizer.set_item(window.pos, 1, wxEXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditSplitterWindow objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    window = EditSplitterWindow(name, parent, wxNewId(), None, None, None,
                                wxSPLIT_VERTICAL,
                                sizer, pos, common.property_panel, True)
    sizer.set_item(window.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(window)
    window.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return window

def xml_builder_pane(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build SplitterPane objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not parent.window_1: pos = 1
    else: pos = 2
    pane = SplitterPane(name, parent, wxNewId(), 1, common.property_panel)
    if not parent.window_1:
        parent.window_1 = pane
        parent.properties['window_1'].set_value(pane.name)
    else:
        parent.window_2 = pane
        parent.properties['window_2'].set_value(pane.name)
        parent.split()
    node = Tree.Node(pane)
    pane.node = node
    common.app_tree.add(node, parent.node)
    return pane


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSplitterWindow'] = builder
    common.widgets_from_xml['EditSplitterWindow'] = xml_builder

    common.widgets_from_xml['SplitterPane'] = xml_builder_pane

    from tree import WidgetTree
    WidgetTree.images['SplitterPane'] = 'icons/panel.xpm'

    return common.make_object_button('EditSplitterWindow',
                                     'icons/splitter_window.xpm')
    
