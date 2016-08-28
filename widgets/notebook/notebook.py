"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc
import wcodegen
from tree import Tree, Node, SlotNode
from widget_properties import *
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from xml_parse import XmlParsingError

try:
    from panel import EditPanel
    _has_panel = True
except ImportError:
    _has_panel = False # this case is not tested


class NotebookVirtualSizer(Sizer):
    '"Virtual sizer" responsible for the management of the pages of a Notebook'

    def __init__(self, *args, **kwds):
        Sizer.__init__(self, *args, **kwds)
        self._itempos = 0

    def set_item(self, pos, option=None, flag=None, border=None, size=None, force_layout=True):
        "Updates the layout of the item at the given pos; (re-)creates the notebook page if required"
        if not self.window.widget:
            return
        index = pos-1 # 0 based
        label, item = self.window.tabs[index]
        if not item or not item.widget:
            return
        if not (index < self.window.widget.GetPageCount()):
            self.window.widget.AddPage(item.widget, label) # this is e.g. for the first creation after loading a file
        elif self.window.widget.GetPage(index) is not item.widget:
            # XXX delete this part if it's not called; insert_tab and remove_tab should handle this now
            #self.window.widget.RemovePage(index) # deletes the specified page, without deleting the associated window
            self.window.widget.DeletePage(index)  # deletes the specified page, and the associated window
            self.window.widget.InsertPage(index, item.widget, label)
            self.window.widget.SetSelection(index)
            try:
                wx.CallAfter(item.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass
        if self.window.sizer is not None:
            self.window.sizer.set_item( self.window.pos, size=self.window.widget.GetBestSize() )

    ####################################################################################################################
    # new implementation
    def insert_tab(self, index):
        "inserts or adds a page"
        label, item = self.window.tabs[index]
        if not (index < self.window.widget.GetPageCount()):
            self.window.widget.AddPage(item.widget, label)
        elif self.window.widget.GetPage(index) is not item.widget:
            self.window.widget.InsertPage(index, item.widget, label)

        try:
            wx.CallAfter(item.sel_marker.update)
        except AttributeError:
            pass
        if self.window.sizer is not None:
            self.window.sizer.set_item( self.window.pos, size=self.window.widget.GetBestSize() )

    def remove_tab(self, index):
        if not self.window.widget: return
        #self.window.widget.DeletePage(index)  # deletes the specified page, and the associated window
        self.window.widget.RemovePage(index)  # deletes the specified page
    ####################################################################################################################

    def add_item(self, item, pos=None, option=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self.window"
        #self._logger.debug('pos: %s, item.name: %s', pos, item.name)
        try:
            self.window.tabs[pos-1][1] = item
        except IndexError:
            raise XmlParsingError( _('Notebook widget "%s" does not have any tabs!')%self.window.name )
        item._dont_destroy = True

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if self.window._is_removing_pages or not self.window.widget:
            return
        slot = SizerSlot(self.window, self, pos) # XXX node handling?
        #self._logger.debug('free: %s, %s, %s', slot, slot.pos, pos)
        slot.show_widget(True)
        pos -= 1
        label, item = self.window.tabs[pos]
        self.window.widget.RemovePage(pos)
        self.window.widget.InsertPage(pos, slot.widget, label)
        self.window.widget.SetSelection(pos)

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        self._itempos += 1
        return self._itempos

    def is_virtual(self):
        return True



class NotebookPagesProperty(GridProperty):
    def write(self, outfile, tabs):
        inner_xml = u''
        value = self.get_value()
        for i in range(len(value)):
            val = value[i]
            window = None
            try:
                t = self.owner.tabs[i]
                if t[0] == val[0]:
                    window = t[1]
            except:
                pass
            if window:
                inner_xml += common.format_xml_tag(u'tab', val[0], tabs+1, window=window.name)
        stmt = common.format_xml_tag(u'tabs', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)



class TabsHandler(BaseXmlBuilderTagHandler):
    def __init__(self, parent):
        super(TabsHandler, self).__init__()
        self.parent = parent
        self.tab_names = []

    def end_elem(self, name):
        if name == 'tabs':
            self.parent.tabs = [ [misc.wxstr(name), None] for name in self.tab_names ]
            self.parent.properties['tabs'].set_value( [[name] for name in self.tab_names] )
            return True
        elif name == 'tab':
            char_data = self.get_char_data()
            self.tab_names.append(char_data)
        return False



class EditNotebook(ManagedBase, EditStylesMixin):
    "Class to handle wxNotebook objects"
    _custom_base_classes = True
    _next_notebook_number = 1 # next free number for notebook names
    update_widget_style = False

    def __init__(self, name, parent, id, style, sizer, pos, property_window, show=True):
        # create new and (still) unused notebook name
        if not name:
            name = self.next_notebook_name()

        # initialise parent classes
        ManagedBase.__init__(self, name, 'wxNotebook', parent, id, sizer, pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.set_style(style)
        self.virtual_sizer = NotebookVirtualSizer(self)
        self._is_removing_pages = False

        # initialise properties remaining staff
        self.tabs = [['tab1', None]]  # list of pages of this notebook (actually a list of 2-list label, window)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(self, 'style', self.widget_writer)
        self.access_functions['tabs'] = (self.get_tabs, self.set_tabs)
        tab_cols = [('Tab label', GridProperty.STRING)]
        self.properties['tabs'] = NotebookPagesProperty( self, 'tabs', None, tab_cols, label=_("Tabs"),
                                                         can_remove_last=False, with_index=True)
        del tab_cols
        self.nb_sizer = None
        self._create_slots = False

        self.no_custom_class = False
        self.access_functions['no_custom_class'] = (self.get_no_custom_class, self.set_no_custom_class)
        self.properties['no_custom_class'] = CheckBoxProperty( self, 'no_custom_class',
                                                               label=_("Don't generate code for this class") )

        # first pane should have number 1
        self.next_pane_number = 1

    def create_widget(self):
        self.widget = wx.Notebook( self.parent.widget, self.id, style=self.get_int_style() )

    def show_widget(self, yes):
        ManagedBase.show_widget(self, yes)
        if self._create_slots:
            self._create_slots = False
            for i in range(len(self.tabs)):
                if self.tabs[i][1] is None:
                    self.tabs = self.tabs[:i]
                    self.properties['tabs'].set_value(self.get_tabs())

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        # replace 'self' with 'self.nb_sizer' in 'self.sizer'

    def post_load(self):
        wx.CallAfter(self._toggle_pages)

    def _toggle_pages(self):
        "workaround: toggle through all pages and toggle page size to ensure correct display"
        if not self.widget: return
        count = self.widget.PageCount
        if count<=1: return
        i0 = i = self.widget.GetSelection()
        while True:
            i += 1
            if i==count: i = 0
            #self.widget.SetSelection(i)
            wx.CallAfter( self.widget.SetSelection, i )
            if i==i0: break
        # resize top level window
        toplevel = self.widget.GetTopLevelParent()
        size = toplevel.GetSize()
        wx.CallAfter( toplevel.SetSize, (size[0]+20, size[1]+20) )
        wx.CallAfter( toplevel.SetSize, size )

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow( self.notebook, wx.ID_ANY, style=wx.TAB_TRAVERSAL )
        self.properties['no_custom_class'].display(panel)
        self.properties['style'].display(panel)
        self.properties['tabs'].display(panel)
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(self.properties['no_custom_class'].panel, 0, wx.ALL | wx.EXPAND, 3)
        sizer.Add(self.properties['style'].panel, 0, wx.EXPAND)
        sizer.Add(self.properties['tabs'].panel, 1, wx.ALL | wx.EXPAND, 3)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))
        self.properties['tabs'].set_col_sizes([-1])

    def on_set_focus(self, event):
        self.show_properties()
        event.Skip()

    def _add_tab(self, window, pos):
        # XXX remove method if not used
        if window is None:
            window = SizerSlot(self, self.virtual_sizer, pos)
            node = SlotNode(window) # XXX not tested
            window.node = node
            common.app_tree.add(node, self.node)
            self.tabs[pos - 1][1] = window # if window is not None, it's an EditPanel which sets itself to tabs during it's __init__
        else:
            window._dont_destroy = True
            node = Node(window)
            window.node = node
            common.app_tree.add(node, self.node)
        if self.widget:
            window.show_widget(True)
            self.virtual_sizer.set_item(pos)
            try:
                wx.CallAfter(window.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass

    def _remove_tab(self, index):
        # XXX remove method if not used
        self._is_removing_pages = True
        window = self.tabs[index][1]
        if self.widget:
            self.widget.RemovePage(index) # remove the page
        window.remove(False)          # delete the page content
        del self.tabs[index:]             # delete from our list of tabs
        self._is_removing_pages = False

    def _set_tab_name(self, index, name):
        # XXX remove method if not used
        tt = misc.wxstr(name)
        if self.widget:
            self.widget.SetPageText(index, tt)
        self.tabs[index][0] = tt

    def get_tabs(self):
        return [[n] for n, w in self.tabs]

    ####################################################################################################################
    # new implementation:
    # together with NotebookVirtualSizer insert_tab, remove_tab, free_tab
    def insert_tab(self, index, label):
        self.tabs.insert(index, [label, None]) # this needs to be done before EditPanel calls self.virtual_sizer.add_item

        pos = index+1
        if _has_panel:
            window = EditPanel( self.next_pane_name(), self, -1, self.virtual_sizer, pos, self.property_window )
            window._dont_destroy = True
            node = Node(window)
        else: # a SizerSlot/SlotNode; not tested
            window = SizerSlot(self, self.virtual_sizer, pos)
            node = SlotNode(window) # XXX not tested
            self.tabs[index][1] = window # the EditPanel above sets itself to tabs during it's __init__

        window.node = node
        common.app_tree.add(node, self.node)

        if self.widget:
            window.show_widget(True)
            self.virtual_sizer.insert_tab(index)

            try:
                wx.CallAfter(window.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass

    def remove_tab(self, index):
        self._is_removing_pages = True
        window = self.tabs[index][1]
        self.virtual_sizer.remove_tab(index) # 
        window.remove(False)   # delete the page content
        del self.tabs[index:]  # delete from our list of tabs
        self._is_removing_pages = False

    def set_tabs(self, tabs, indices):
        """tabs: list of strings
        indices: the current indices of the tabs or None for a new tab; re-ordering is currently not supported"""
        keep_indices = [i for i in indices if i is not None]
        if keep_indices != sorted(keep_indices):
            raise ValueError("Re-ordering is not yet implemented")

        # set tab labels if modified
        for (name,), index in zip(tabs, indices):
            if index is not None and self.tabs[index][0] != name:
                self._set_tab_name(index, name)

        # remove tabs
        for i in range(len(self.tabs)-1, -1, -1):
            if not i in keep_indices:
                self.remove_tab(i)

        # insert/add tabs
        added = None
        for i, (name,) in enumerate(tabs):
            index = indices[i]
            if index is not None:
                # old tab to be kept
                continue
            # actually add/insert
            self.insert_tab(i, name)
            added = i

        # select the last added tab
        if added is not None and self.widget:
            self.widget.SetSelection(added)

    ####################################################################################################################

    def delete(self):
        if self.widget: self.widget.DeleteAllPages()
        ManagedBase.delete(self)

    def get_property_handler(self, name):
        if name == 'tabs':
            return TabsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def find_page(self, page):
        "returns the index of the given page in the notebook, or -1 if the page cannot be found"
        if not self.widget:
            return -1
        for i in range(len(self.tabs)):
            if self.tabs[i][1] is page:
                if i < self.widget.GetPageCount():
                    return i
                else:
                    return -1
        return -1

    def get_no_custom_class(self):
        return self.no_custom_class

    def set_no_custom_class(self, value):
        self.no_custom_class = bool(int(value))

    def next_notebook_name(self):
        # return new and (still) unused notebook name
        while True:
            name = 'notebook_%d' % EditNotebook._next_notebook_number
            if not common.app_tree.has_name(name):
                break
            EditNotebook._next_notebook_number += 1
        return name

    def next_pane_name(self):
        # return new and (still) unused pane name
        while True:
            pane_name = "%s_pane_%d" % (self.name, self.next_pane_number)
            self.next_pane_number += 1
            if not common.app_tree.has_name(pane_name):
                break
        return pane_name

# end of class EditNotebook


editor_class = EditNotebook
editor_icon = 'notebook.xpm'
editor_name = 'EditNotebook'
editor_style = ''

dlg_title = _('wxNotebook')
box_title = _('Orientation')
choices = 'wx.NB_TOP|wx.NB_BOTTOM|wx.NB_LEFT|wx.NB_RIGHT'
tmpl_label = 'notebook'


def builder(parent, sizer, pos, number=[1]):
    "Factory function for editor objects from GUI"
    dialog = wcodegen.WidgetStyleSelectionDialog(dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    widget = editor_class(None, parent, wx.ID_ANY, style, sizer, pos, common.property_panel, show=False)
    #if _has_panel:
    #    pane1 = EditPanel(widget.next_pane_name(), widget, wx.ID_ANY, widget.virtual_sizer, 1, common.property_panel)

    node = Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node

    widget.set_option(1)
    widget.set_style("wxEXPAND")
    widget.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)

    #if _has_panel:
    #    widget._add_tab(pane1, 1)
    widget.insert_tab(0, widget.next_pane_name())

    sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.ID_ANY, editor_style, sizer, pos, common.property_panel)
    sizer.set_item(widget.pos, option=sizeritem.option, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(widget)
    widget.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
