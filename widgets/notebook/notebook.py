# notebook.py: wxNotebook objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, WindowBase

class NotebookPane(WindowBase):
    def __init__(self, name, parent, id, property_window, show=True):
        WindowBase.__init__(self, name, 'wxPanel', parent,
                            id, property_window, show)
        self.sizer = None
        self.sel_marker = None 
        self.remove_page_from_parent = True # if True, a call to Destroy will
                                            # also remove the pane from its
                                            # parent's list of pages
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

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, self.id)
        self.sel_marker = misc.SelectionMarker(self.widget, self.widget)
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        EVT_MOVE(self.widget, self.on_move)
        def GetBestSize():
            if self.sizer and self.widget:
                return self.widget.GetSizer().GetMinSize()
            return wxPanel.GetBestSize(self.widget)
        self.widget.GetBestSize = GetBestSize

    def create_properties(self):
        WindowBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['style'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
       
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

    def remove(self, remove_page_from_parent=True):
        self.remove_page_from_parent = remove_page_from_parent
        WindowBase.remove(self)

    def delete(self):
        if not self.widget: return
        if self.sel_marker: self.sel_marker.Destroy()
        if self.remove_page_from_parent:
            index = self.parent.find_page(self)
            if index >= 0: self.parent.widget.RemovePage(index)
        WindowBase.delete(self)

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


class EditNotebook(ManagedBase):
    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxNotebook objects
        """
        ManagedBase.__init__(self, name, 'wxNotebook', parent, id, sizer,
                             pos, property_window, show=show)
        self.style = style
        self.tabs = [ ['tab1', None] ] # list of pages of this notebook
                                       # (actually a list of
                                       # 2-list label, window)

        self.access_functions['style'] = (self.get_tab_pos, self.set_tab_pos)
        self.properties['style'] = HiddenProperty(self, 'style')
                                                  #styles.get(style, '0'))
        self.access_functions['tabs'] = (self.get_tabs, self.set_tabs)
        tab_cols = [('Tab label', GridProperty.STRING)]
        self.properties['tabs'] = NotebookPagesProperty(self, 'tabs', None,
                                                        tab_cols)
        del tab_cols
        self.nb_sizer = None 

    def create_widget(self):
        self.widget = wxNotebook(self.parent.widget, self.id, style=self.style)
        self.nb_sizer = wxNotebookSizer(self.widget)
        if not self.widget.GetPageCount():
            for name, window in self.tabs:
                if not window: continue
                window.show_widget(True)
                self.widget.AddPage(window.widget, name)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        # replace 'self' with 'self.nb_sizer' in 'self.sizer'
        self.sizer._fix_notebook(self.pos, self.nb_sizer)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        self.properties['tabs'].display(panel)
        self.properties['tabs'].set_col_sizes([-1])
        sizer = wxBoxSizer(wxVERTICAL)
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
        node = Tree.Node(window)
        window.node = node
        common.app_tree.add(node, self.node)
        if self.widget:
            window.show_widget(True)
            self.widget.AddPage(window.widget, name)
       
    def get_tabs(self):
        return [ [n] for n, w in self.tabs ]

    def set_tabs(self, tabs):
        delta = len(self.tabs) - len(tabs)
        if delta > 0:
            # we have to remove some pages
            i = len(tabs)
            if self.widget:
                for n, window in self.tabs[i:]:
                    self.widget.RemovePage(i)
                    window.remove(False)
            del self.tabs[i:]
            if self.widget: self.widget.SetSelection(0)
        elif delta < 0:
            # we have to add some pages
            number = len(self.tabs)+1
            while common.app_tree.has_name(self.name + '_pane_%s' % number):
                number += 1
            for i in range(-delta):
                window = NotebookPane(self.name + '_pane_%s' % number,
                                      self, wxNewId(), self.property_window)
                number += 1
                self.add_tab(window, "_")
            if self.widget:
                self.widget.SetSelection(self.widget.GetPageCount()-1)
        # finally, we must update the labels of the tabs
        for i in range(len(tabs)):
            if self.widget:
                self.widget.SetPageText(i, tabs[i][0])
            self.tabs[i][0] = tabs[i][0]

    def delete(self):
        if self.widget:
            for i in range(self.widget.GetPageCount()):
                self.widget.RemovePage(i)
            #self.nb_sizer.Destroy()
        ManagedBase.delete(self)

    def get_property_handler(self, name):
        if name == 'tabs': return TabsHandler(self)

    def find_page(self, page):
        """\
        returns the index of the given page in the notebook, or -1 if the page
        cannot be found
        """
        if not self.widget: return -1
        for i in range(len(self.tabs)):
            if self.tabs[i][1] is page:
                if i < self.widget.GetPageCount(): return i
                else: return -1
        return -1

    def get_tab_pos(self): 
        styles = { wxNB_LEFT: 'wxNB_LEFT', wxNB_RIGHT: 'wxNB_RIGHT',
                   wxNB_BOTTOM: 'wxNB_BOTTOM' }
        return styles.get(self.style, '0')
    
    def set_tab_pos(self, value):
        styles = { 'wxNB_LEFT': wxNB_LEFT, 'wxNB_RIGHT': wxNB_RIGHT,
                   'wxNB_BOTTOM': wxNB_BOTTOM }
        self.style = styles.get(value, 0)

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
                                 ['Top', 'Bottom', 'Left', 'Right'],
                                 columns=2)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxALL|wxEXPAND, 10)
            btn = wxButton(self, wxID_OK, 'OK')
            btn.SetDefault()
            szr.Add(btn, 0, wxBOTTOM|wxALIGN_CENTER, 10)
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
    window.set_option(1)
    window.set_flag("wxEXPAND")
    window.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

    window.tabs = []
    window.add_tab(pane1, 'tab1')

    sizer.set_item(window.pos, 1, wxEXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditNotebook objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    window = EditNotebook(name, parent, wxNewId(), 0, sizer, pos,
                          common.property_panel, True)

    sizer.set_item(window.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(window)
    window.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return window


def xml_builder_pane(attrs, parent, sizer, sizeritem, pos=None, index=[-1]):
    """\
    factory to build NotebookPane objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    index[0] += 1
    if hasattr(parent, 'tmp_tab_names'):
        try: tab_name = parent.tmp_tab_names[index[0]]
        except IndexError: tab_name = name
    else: tab_name = name

    pane = NotebookPane(name, parent, wxNewId(), common.property_panel)
    if not index[0]: parent.tabs = []
    parent.add_tab(pane, tab_name)
    if hasattr(parent, 'tmp_tab_names') and \
           index[0] == len(parent.tmp_tab_names)-1:
        # all the tabs have been added, so now we can set the value of the
        # tabs property
        parent.properties['tabs'].set_value(zip(parent.tmp_tab_names))
        index[0] = -1 # reset the index for the next notebook
    return pane


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditNotebook'] = builder
    common.widgets_from_xml['EditNotebook'] = xml_builder

    common.widgets_from_xml['NotebookPane'] = xml_builder_pane

    from tree import WidgetTree
    WidgetTree.images['NotebookPane'] = 'icons/panel.xpm'

    return common.make_object_button('EditNotebook', 'icons/notebook.xpm')
    
