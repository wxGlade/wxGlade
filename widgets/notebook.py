# notebook.py: wxNotebook objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, WindowBase

class NotebookPane(wxPanel, WindowBase):
    def __init__(self, name, parent, id, property_window, show=True):
        wxPanel.__init__(self, parent, id)
        WindowBase.__init__(self, name, 'wxPanel', parent,
                            id, property_window, show)
        self.has_sizer = False
        self.sel_marker = misc.SelectionMarker(self, self)
        self.remove_page_from_parent = True # if True, a call to Destroy will
                                            # also remove the pane from its
                                            # parent's list of pages
        EVT_LEFT_DOWN(self, self.drop_sizer)
        EVT_ENTER_WINDOW(self, self.on_enter)
        EVT_MOVE(self, self.on_move)
        
    def popup_menu(self, *args): pass

    def on_enter(self, event):
        if not self.has_sizer and common.adding_sizer:
            self.SetCursor(wxCROSS_CURSOR)
        else:
            self.SetCursor(wxNullCursor)

    def drop_sizer(self, event):
        if self.has_sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        common.widgets[common.widget_to_add](self, None, None)
        common.adding_widget = common.adding_sizer = 0
        common.widget_to_add = None
        self.has_sizer = True
        common.app_tree.app.saved = False
        self.SetCursor(wxNullCursor)

    def on_parent_size(self, event):
        if self.has_sizer:
            self.GetSizer().Layout()
    
    def GetBestSize(self):
        if self.has_sizer: return self.GetSizer().GetMinSize()
        return wxPanel.GetBestSize(self)

    def update_view(self, selected):
        self.sel_marker.Show(selected)

    def on_move(self, event): self.sel_marker.update()

    def on_size(self, event):
        WindowBase.on_size(self, event)
        self.sel_marker.update()

    def remove(self, remove_page_from_parent=True):
        self.remove_page_from_parent = remove_page_from_parent
        ManagedBase.remove(self)

    def Destroy(self):
        if self.remove_page_from_parent:
            index = self.parent.find_page(self)
            if index >= 0: self.parent.RemovePage(index)
        wxPanel.Destroy(self)

# end of class NotebookPane


class NotebookPagesProperty(GridProperty):      
    def write(self, outfile, tabs):
        from xml.sax.saxutils import escape, quoteattr
        write = outfile.write
        write('    ' * tabs + '<tabs>\n')
        tab_s = '    ' * (tabs+1)
        import widget_properties
        value = self.get_value()
        for i in range(len(value)):
            val = value[i]
            v = widget_properties._encode(val[0])
            window = None
            try:
                t = self.owner.tabs[i]
                if t[0] == val[0]: window = t[1]
            except: pass
            if window:
                write('%s<tab window=%s>%s</tab>\n' % (tab_s,
                                                       quoteattr(window.name),
                                                       v))
        write('    ' * tabs + '</tabs>\n')

# end of class NotebookPagesProperty


class TabsHandler:
    def __init__(self, parent):
        self.parent = parent
        self.tab_names = []
        self.curr_tab = []

    def start_elem(self, name, attrs):
        pass

    def end_elem(self, name):
        if name == 'tabs':
            # set a temporary attribute of the notebook, that will be used by
            # the last NotebookPane added: this is an ugly hack anyway...
            self.parent.tmp_tab_names = self.tab_names
            return True
        elif name == 'tab':
            self.tab_names.append("".join(self.curr_tab))
            self.curr_tab = []
        return False

    def char_data(self, data):
        self.curr_tab.append(data)

# end of class TabsHandler


class EditNotebook(wxNotebook, ManagedBase):
    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxNotebook objects
        """
        wxNotebook.__init__(self, parent, id, style=style)
        ManagedBase.__init__(self, name, 'wxNotebook', parent, id, sizer,
                             pos, property_window, show=show)
        self.tabs = [ ['tab1', None] ] # list of pages of this notebook
                                       # (actually a list of
                                       # 2-list label, window)

        styles = { wxNB_LEFT: 'wxNB_LEFT', wxNB_RIGHT: 'wxNB_RIGHT',
                   wxNB_BOTTOM: 'wxNB_BOTTOM' }
        self.properties['style'] = HiddenProperty(self, 'style',
                                                  styles.get(style, '0'))
        self.access_functions['tabs'] = (self.get_tabs, self.set_tabs)
        tab_cols = [('Tab label', GridProperty.STRING)]
        self.properties['tabs'] = NotebookPagesProperty(self, 'tabs', None,
                                                        tab_cols)
        del tab_cols
        self.nb_sizer = wxNotebookSizer(self)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        self.properties['tabs'].display(panel)
        self.properties['tabs'].set_col_sizes([-1])
        sizer = misc.Sizer(wxVERTICAL)
        sizer.Add(self.properties['tabs'].panel, 1, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def on_set_focus(self, event):
        self.show_properties()
        event.Skip()

    def add_tab(self, window, name):
        self.tabs.append([name, window])
        self.AddPage(window, name)
        node = Tree.Node(window)
        window.node = node
        common.app_tree.add(node, self.node)
       
    def get_tabs(self):
        return [ [n] for n, w in self.tabs ]

    def set_tabs(self, tabs):
        delta = len(self.tabs) - len(tabs)
        if delta > 0:
            # we have to remove some pages
            i = len(tabs)
            for n, window in self.tabs[i:]:
                self.RemovePage(i)
                window.remove(False)
            del self.tabs[i:]
            self.SetSelection(0)
        elif delta < 0:
            # we have to add some pages
            number = len(self.tabs)+1
            while common.app_tree.has_name(self.name + '_pane_%s' % number):
                number += 1
            for i in range(-delta):
                window = NotebookPane(self.name + '_pane_%s' % number, self,
                                      wxNewId(), self.property_window)
                number += 1
                self.add_tab(window, "_")
            self.SetSelection(self.GetPageCount()-1)
        # finally, we must update the labels of the tabs
        for i in range(len(tabs)):
            self.SetPageText(i, tabs[i][0])
            self.tabs[i][0] = tabs[i][0]

    def remove(self, *args):
        for i in range(self.GetPageCount()):
            self.RemovePage(i)
        ManagedBase.remove(self, *args)

    def Destroy(self):
        for i in range(self.GetPageCount()):
            self.RemovePage(i)
        wxNotebook.Destroy(self)

    def get_property_handler(self, name):
        if name == 'tabs': return TabsHandler(self)

    def find_page(self, page):
        """\
        returns the index of the given page in the notebook, or -1 if the page
        cannot be found
        """
        for i in range(len(self.tabs)):
            if self.tabs[i][1] is page: return i
        return -1
            
# end of class EditNotebook
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditNotebook objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select tab placement')
            self.styles = [ 0, wxNB_BOTTOM, wxNB_LEFT, wxNB_RIGHT ]
            self.style = 0
            prop = RadioProperty(self, 'tab_placement', self,
                                 ['Top', 'Bottom', 'Left', 'Right'])
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
        def __getitem__(self, value):
            def set_style(s): self.style = self.styles[s]
            return (lambda: self.style, set_style)
    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    name = 'notebook_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'notebook_%d' % number[0]
    window = EditNotebook(name, parent, wxNewId(), dialog.style,
                          sizer, pos, common.property_panel, show=False)
    pane1 = NotebookPane(name + '_pane_1', window, wxNewId(),
                         common.property_panel)

    node = Tree.Node(window)
    window.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

    window.tabs = []
    window.add_tab(pane1, 'tab1')

    sizer.set_item(window.pos, 1, wxEXPAND)
    window.sizer_properties['option'].set_value(1)
    window.sizer_properties['flag'].set_value("wxEXPAND")

    window.Show()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None, complete=False,
                tmp_win=[None]):
    """\
    factory to build EditNotebook objects from an xml file
    """
    class FakeNotebook:
        styles = { 'wxNB_BOTTOM': wxNB_BOTTOM, 'wxNB_RIGHT': wxNB_RIGHT,
                   'wxNB_LEFT': wxNB_LEFT }
        def __init__(self, attrs, parent, sizer, sizeritem, pos):
            self.attrs = attrs
            self.parent = parent
            self.sizer = sizer
            self.sizeritem = sizeritem
            self.pos = pos

        def __getitem__(self, value):
            if value != 'style': raise KeyError
            return (None, self.set_style)
                
        def set_style(self, val):
            try: self.style = FakeNotebook.styles(val)
            except: self.style = 0
            return xml_builder(self.attrs, self.parent, self.sizer,
                               self.sizeritem, self.pos, True)

        def get_property_handler(self, name):
            if name == 'tabs': return TabsHandler(self)            

    # end of class FakeNotebook
    
    if not complete:
        tmp_win[0] = FakeNotebook(attrs, parent, sizer, sizeritem, pos)
        return tmp_win[0]
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    style = tmp_win[0].style
    window = EditNotebook(name, parent, wxNewId(), style, sizer, pos,
                          common.property_panel, True)
    if hasattr(tmp_win[0], 'tmp_tab_names'):
        window.tmp_tab_names = tmp_win[0].tmp_tab_names

    # see if the notebook is an instance of a custom class, and set its klass
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
    factory to build NotebookPane objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if hasattr(parent, 'tmp_tab_names'):
        try: tab_name = parent.tmp_tab_names[parent.GetPageCount()]
        except IndexError: tab_name = name
    else: tab_name = name

    pane = NotebookPane(name, parent, wxNewId(), common.property_panel)
    if not parent.GetPageCount(): parent.tabs = []
    parent.add_tab(pane, tab_name)
    if hasattr(parent, 'tmp_tab_names') and \
       parent.GetPageCount() == len(parent.tmp_tab_names):
        # all the tabs have been added, so now we can set the value of the
        # tabs property
        parent.properties['tabs'].set_value(zip(parent.tmp_tab_names))
    return pane


class TabsCodeHandler:
    def __init__(self):
        self.tabs = []
        self.curr_tab_name = []
        self.tab_window = None

    def start_elem(self, name, attrs):
        if name == 'tab':
            window = attrs.get('window')
            if not window: return
            self.tab_window = window
            self.curr_tab_name = []

    def end_elem(self, name, code_obj):
        if name == 'tabs':
            # set a temporary attribute of the notebook, that will be used by
            # the last NotebookPane added: this is an ugly hack anyway...
            code_obj.properties['tabs'] = self.tabs
            return True
        elif name == 'tab':
            tab_name = "".join(self.curr_tab_name)
            if self.tab_window: self.tabs.append((tab_name, self.tab_window))
        return False

    def char_data(self, data):
        self.curr_tab_name.append(data)

# end of class TabsHandler


def code_generator(window):
    """\
    generates the python code for wxNotebook
    """
    pygen = common.code_writers['python']
    prop = window.properties
    id_name, id = pygen.generate_code_id(window)

    layout_props = ['%s_sizer = wxNotebookSizer(self.%s)\n' % \
                    (window.name, window.name)]
    tabs = prop.get('tabs', [])
    for label, tab_win in tabs:
        layout_props.append('self.%s.AddPage(self.%s, "%s")\n' % \
                            (window.name, tab_win, label.replace('"', '\"')))
        
    if window.is_toplevel:
        l = ['self.%s = %s(self, %s)\n' % (window.name, window.klass, id)]
        if id_name: l.append(id_name)
        return l, [], [] #layout_props #[]
    size = pygen.generate_code_size(window)
    if not window.parent.is_toplevel: parent = 'self.%s' % window.parent.name
    else: parent = 'self'
    style = prop.get('style', '0')
    init = ['self.%s = wxNotebook(%s, %s, size=%s, style=%s)\n' %
            (window.name, parent, id, size, style) ]
    if id_name: init.append(id_name)

    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(window))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(window))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(window))

    return init, props_buf, layout_props #[]


def generate_notebook_properties(obj):
    prop = obj.properties
    pygen = common.code_writers['python']
    props_buf = ['nb_sizer = wxNotebookSizer(self)\n']
    tabs = prop.get('tabs', [])
    for label, window in tabs:
        props_buf.append('self.AddPage(self.%s, "%s")\n' % \
                         (window, label.replace('"', '\"')))
    props_buf.extend(pygen.generate_common_properties(obj))
    return props_buf    


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditNotebook'] = builder
    common.widgets_from_xml['EditNotebook'] = xml_builder
    common.class_names['EditNotebook'] = 'wxNotebook'

    common.widgets_from_xml['NotebookPane'] = xml_builder_pane
    common.class_names['NotebookPane'] = 'wxPanel'

    from tree import WidgetTree
    WidgetTree.images['NotebookPane'] = 'icons/panel.xpm'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxNotebook', code_generator,
                                 generate_notebook_properties)
        pygen.add_property_handler('tabs', TabsCodeHandler, 'wxNotebook')
        
    return common.make_object_button('EditNotebook', 'icons/notebook.xpm')
    
