# splitter_window.py: wxSplitterWindow objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

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
        self.properties['orientation'] = HiddenProperty(self, 'orientation',od)

        def get_window(win):
            try: return win.name
            except: return ''
        def get_win_1(): return get_window(self.window_1)
        def get_win_2(): return get_window(self.window_2)
        self.properties['window_1'] = HiddenProperty(self, 'window_1',
                                                     get_win_1)
        self.properties['window_2'] = HiddenProperty(self, 'window_2',
                                                     get_win_2)

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
        sizer = misc.Sizer(wxVERTICAL)
        self.properties['style'].display(panel)
        self.properties['sash_pos'].display(panel)
        sizer.Add(self.properties['style'].panel, 0, wxEXPAND)
        sizer.Add(self.properties['sash_pos'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        
    def __getitem__(self, key):
        if key == 'window_1' or key == 'window_2':
            return (lambda : '', lambda v: None)
        return ManagedBase.__getitem__(self, key)

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
                self.widget.SetSashPosition(self.widget.GetClientSize()[0]/2)
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
##             w, h = self.widget.GetClientSize()
##             if self.orientation == wxSPLIT_VERTICAL:
##                 if w < value: self.widget.SetClientSize((value, -1))
##             elif h < value: self.widget.SetClientSize((-1, value))
            self.widget.SetSashPosition(value)

    def on_size(self, event):
        if not self.widget: return
        try:
            if self.orientation == wxSPLIT_VERTICAL:
                max_pos = self.widget.GetClientSize()[0]
            else: max_pos = self.widget.GetClientSize()[1]
            self.properties['sash_pos'].set_range(0, max_pos)
        except (AttributeError, KeyError): pass
        ManagedBase.on_size(self, event)

    def on_sash_pos_changed(self, event):
        self.sash_pos = self.widget.GetSashPosition()
        self.properties['sash_pos'].set_value(self.sash_pos)
        event.Skip()

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
##     window.sizer_properties['option'].set_value(1)
##     window.sizer_properties['flag'].set_value("wxEXPAND")

##     window.split()
##     window.Show()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None, complete=False,
                tmp_win=[None]):
    """\
    factory to build EditSplitterWindow objects from an xml file
    """
    class FakeSplitter:
        orientations = { 'wxSPLIT_HORIZONTAL': wxSPLIT_HORIZONTAL,
                         'wxSPLIT_VERTICAL': wxSPLIT_VERTICAL }
        def __init__(self, attrs, parent, sizer, sizeritem, pos):
            self.attrs = attrs
            self.parent = parent
            self.sizer = sizer
            self.sizeritem = sizeritem
            self.pos = pos
            self.functions = { 'style': self.set_style,
                               'sash_pos': self.set_sash_pos,
                               'orientation': self.set_orient,
                               'window_1': lambda v: None,
                               'window_2': lambda v: None }
        def __getitem__(self, value):
            return (None, self.functions[value])
                
        def set_style(self, val):
            try: self.style = eval(val)
            except: self.style = 0

        def set_sash_pos(self, val):
            try: self.sash_pos = int(val)
            except ValueError: self.sash_pos = 0

        def set_orient(self, val):
            self.orientation = FakeSplitter.orientations[val]
            return xml_builder(self.attrs, self.parent, self.sizer,
                               self.sizeritem, self.pos, True)
        
    # end of class FakeSplitter
    
    if not complete:
        tmp_win[0] = FakeSplitter(attrs, parent, sizer, sizeritem, pos)
        return tmp_win[0]
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    if hasattr(tmp_win[0], 'style'): style = tmp_win[0].style
    else: style=None
    window = EditSplitterWindow(name, parent, wxNewId(), style, None, None,
                                tmp_win[0].orientation,
                                sizer, pos, common.property_panel, True)
    if hasattr(tmp_win[0], 'sash_pos'):
        sp = tmp_win[0].sash_pos
        window.properties['sash_pos'].set_value(sp)
        window['sash_pos'][1](sp)
    # see if the window is an instance of a custom class, and set its klass
    # property 
    if hasattr(tmp_win[0], 'klass'):
        window.klass = tmp_win[0].klass
        window.klass_prop.set_value(window.klass)
        
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
    if not parent.window_1: parent.window_1 = pane
    else:
        parent.window_2 = pane
        parent.split()
    node = Tree.Node(pane)
    pane.node = node
    common.app_tree.add(node, parent.node)
    return pane

def code_generator(window):
    """\
    generates the python code for wxSplitterWindow
    """
    pygen = common.code_writers['python']
    prop = window.properties
    id_name, id = pygen.generate_code_id(window)
    if window.is_toplevel:
        l = ['self.%s = %s(self, %s)\n' % (window.name, window.klass, id)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(window)
    if not window.parent.is_toplevel: parent = 'self.%s' % window.parent.name
    else: parent = 'self'
    style = prop.get('style', 'wxSP_3D')
    init = ['self.%s = wxSplitterWindow(%s, %s, size=%s, style=%s)\n' %
            (window.name, parent, id, size, style) ]
    if id_name: init.append(id_name)

    props_buf = []
    win_1 = prop.get('window_1')
    win_2 = prop.get('window_2')
    orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')
    if win_1 and win_2:
        if orientation == 'wxSPLIT_VERTICAL': f_name = 'SplitVertically'
        else: f_name = 'SplitHorizontally'
        props_buf.append('self.%s.%s(self.%s, self.%s)\n' % \
                         (window.name, f_name, win_1, win_2))
    else:
        def add_sub(win):
            props_buf.append('self.%s.SetSplitMode(%s)\n' % (window.name,
                                                             orientation))
            props_buf.append('self.%s.Initialize(self.%s)\n' % \
                             (window.name, win))
        if win_1: add_sub(win_1)
        elif win_2: add_sub(win_2)

    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(window))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(window))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(window))
    sash_pos = prop.get('sash_pos')
    if sash_pos:
        props_buf.append('self.%s.SetSashPosition(%s)\n' % (window.name,
                                                            sash_pos))
    return init, props_buf, []


def generate_splitter_properties(obj):
    prop = obj.properties
    pygen = common.code_writers['python']
    win_1 = prop.get('window_1')
    win_2 = prop.get('window_2')
    orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')
    props_buf = []
    if win_1 and win_2:
        if orientation == 'wxSPLIT_VERTICAL': f_name = 'SplitVertically'
        else: f_name = 'SplitHorizontally'
        props_buf.append('self.%s(self.%s, self.%s)\n' %
                         (f_name, win_1, win_2))
    else:
        def add_sub(win):
            props_buf.append('self.SetSplitMode(%s)\n' % orientation)
            props_buf.append('self.Initialize(self.%s)\n' % win)
        if win_1: add_sub(win_1)
        elif win_2: add_sub(win_2)
    sash_pos = prop.get('sash_pos')
    if sash_pos:
        props_buf.append('self.SetSashPosition(%s)\n' % sash_pos)
    props_buf.extend(pygen.generate_common_properties(obj))
    return props_buf    


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSplitterWindow'] = builder
    common.widgets_from_xml['EditSplitterWindow'] = xml_builder
    common.class_names['EditSplitterWindow'] = 'wxSplitterWindow'

    common.widgets_from_xml['SplitterPane'] = xml_builder_pane
    common.class_names['SplitterPane'] = 'wxPanel'

    from tree import WidgetTree
    WidgetTree.images['SplitterPane'] = 'icons/panel.xpm'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSplitterWindow', code_generator,
                                 generate_splitter_properties)
        
    return common.make_object_button('EditSplitterWindow',
                                     'icons/splitter_window.xpm')
    
