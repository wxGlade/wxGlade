"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc
import wcodegen
from tree import Node
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from xml_parse import XmlParsingError

from panel import EditPanel



class NotebookVirtualSizer(Sizer):
    '"Virtual sizer" responsible for the management of the pages of a Notebook'

    def __init__(self, *args, **kwds):
        Sizer.__init__(self, *args, **kwds)
        self._itempos = 0

    def set_item(self, pos, proportion=None, flag=None, border=None, size=None, force_layout=True):
        "Updates the layout of the item at the given pos; (re-)creates the notebook page if required"
        if not self.window.widget:
            return
        index = pos-1 # 0 based
        item = self.window.pages[index]
        if not item or not item.widget:
            return
        label = self.window.tabs[index][0]
        if not (index < self.window.widget.GetPageCount()):
            self.window.widget.AddPage(item.widget, label) # this is e.g. for the first creation after loading a file
        elif self.window.widget.GetPage(index) is not item.widget:
            # XXX delete this part if it's not called; insert_tab and remove_tab should handle this now
            if self.window.widget.GetPageCount()==len(self.window.pages):
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
        #label, item = self.window.tabs[index]
        label = self.window.tabs[index][0]
        item = self.window.pages[index]
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
        #if not self.window.pages:
            ## during XML parsing, the tab names are set before
            #page_count = len(self.window.properties["tabs"].value)
            #self.window.pages.extend( [None]*page_count )
        try:
            self.window.pages[pos-1] = item
        except IndexError:
            raise XmlParsingError( _('Notebook widget "%s" does not have any tabs!')%self.window.name )
        item._dont_destroy = True

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if self.window._is_removing_pages or not self.window.widget:
            return
        slot = SizerSlot(self.window, self, pos) # XXX node handling?
        #self._logger.debug('free: %s, %s, %s', slot, slot.pos, pos)
        slot.create()
        pos -= 1
        label = self.window.tabs[pos][0]
        self.window.widget.RemovePage(pos)
        self.window.widget.InsertPage(pos, slot.widget, label)
        self.window.widget.SetSelection(pos)

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        self._itempos += 1
        return self._itempos

    def is_virtual(self):
        return True



class NotebookPagesProperty(np.GridProperty):
    def __init__(self, value, cols):
        col_widths = [300,]
        np.GridProperty.__init__(self, value, cols, col_sizes=col_widths, can_remove_last=False, with_index=True)

    def write(self, outfile, tabs):
        inner_xml = u''
        for (name,), window in zip(self.owner.tabs, self.owner.pages):
            # XXX what happens with empty pages?
            if window:
                inner_xml += common.format_xml_tag(u'tab', name, tabs+1, window=window.name)
        stmt = common.format_xml_tag(u'tabs', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)



class TabsHandler(BaseXmlBuilderTagHandler):
    def __init__(self, parent):
        super(TabsHandler, self).__init__()
        self.parent = parent
        self.tab_names = [] # a list of one-item lists; to be compatible to GridProperty

    def end_elem(self, name):
        if name == 'tabs':
            self.parent.properties['tabs'].set(self.tab_names)
            self.parent.properties_changed(["tabs"])
            return True
        elif name == 'tab':
            char_data = self.get_char_data()
            self.tab_names.append([misc.wxstr(char_data),])
        return False



class EditNotebook(ManagedBase, EditStylesMixin):
    "Class to handle wxNotebook objects"
    _custom_base_classes = True
    _next_notebook_number = 1 # next free number for notebook names
    update_widget_style = False

    _PROPERTIES = ["Widget", "no_custom_class", "style", "tabs"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, style, sizer, pos):
        name = name or self.next_notebook_name()  # create new and (still) unused notebook name
        ManagedBase.__init__(self, name, 'wxNotebook', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        self.virtual_sizer = NotebookVirtualSizer(self)
        self._is_removing_pages = False

        # initialise instance properties
        self.pages = []  # the windows per page
        tabs = []  # list of pages of this notebook (actually a list of 2-list label, window)
        tab_cols = [('Tab label', np.GridProperty.STRING)]
        self.tabs = NotebookPagesProperty(tabs, tab_cols)
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)

        self.next_pane_number = 1  # first pane should have number 1

    def create_widget(self):
        self.widget = wx.Notebook( self.parent.widget, self.id, style=self.style )

    def on_set_focus(self, event):
        # allow switching of pages
        misc.set_focused_widget(self)
        event.Skip()
        #misc.exec_after(self.widget.Refresh())

    ####################################################################################################################
    # new implementation:
    # together with NotebookVirtualSizer insert_tab, remove_tab, free_tab
    def insert_tab(self, index, label):

        # add tab/page this needs to be done before EditPanel calls self.virtual_sizer.add_item
        tabs_p = self.properties["tabs"]
        tabs = tabs_p.get()
        tabs.insert(index, [label,])
        tabs_p.set(tabs)
        self.pages.insert(index, None)
        # adjust pos of the following pages
        for i, page in enumerate(self.pages[index+1:]):
            pos_p = page.properties["pos"]
            pos_p.set(index+2+i)

        pos = index+1
        window = EditPanel( self.next_pane_name(suggestion=label), self, -1, self.virtual_sizer, pos )
        window._dont_destroy = True
        node = Node(window)

        window.node = node
        common.app_tree.add(node, self.node)

        if self.widget:
            window.create()
            compat.SetToolTip(window.widget, _("Notebook page pane:\nAdd a sizer here") )
            self.virtual_sizer.insert_tab(index)

            try:
                wx.CallAfter(window.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                import os
                if 'WINGDB_ACTIVE' in os.environ: raise
        self.properties["tabs"].update_display()

    def set_tabs(self, old_names, indices):
        """tabs: list of strings
        indices: the current indices of the tabs or None for a new tab; re-ordering is currently not supported"""
        keep_indices = [i for i in indices if i is not None]
        if keep_indices != sorted(keep_indices):
            raise ValueError("Re-ordering is not yet implemented")
        keep_indices = set(keep_indices)
        new_names = old_names[:]

        # set tab labels of existing pages, if modified
        for (name,), index in zip(self.tabs, indices):
            if index is not None and old_names[index]!=name:
                new_names[index] = [name,]
                if self.widget:
                    self.widget.SetPageText(index, name)

        # remove tabs
        for index in range(len(old_names)-1, -1, -1):
            if not index in keep_indices:
                self._is_removing_pages = True
                self.virtual_sizer.remove_tab(index)        # remove from sizer; does not delete window
                self.pages[index].remove(False)             # delete the page content
                del self.pages[index]                       # delete from page list
                del new_names[index]                        # delete from list of names
                self._is_removing_pages = False

        # insert/add tabs
        added = None
        for i, (name,) in enumerate(self.tabs):
            index = indices[i]
            if index is not None: continue                  # old tab to be kept

            # actually add/insert
            new_names.insert(i, [name,]) # this needs to be done before EditPanel calls self.virtual_sizer.add_item
            self.pages.insert(i, None)
            # create panel and node, add to tree
            pos = i+1
            window = EditPanel( self.next_pane_name(name), self, -1, self.virtual_sizer, pos )
            window._dont_destroy = True
            node = window.node = Node(window)

            # adjust pos of the following pages
            for p, page in enumerate(self.pages[i+1:]):
                pos_p = page.properties["pos"]
                pos_p.set(i+2+p)

            self.virtual_sizer.add_item(window, pos)
            common.app_tree.insert(node, self.node, i)
            # add to widget
            if self.widget:
                window.create()
                compat.SetToolTip(window.widget, _("Notebook page pane:\nAdd a sizer here") )
                self.virtual_sizer.insert_tab(i)
    
                try:
                    wx.CallAfter(window.sel_marker.update)
                except AttributeError:
                    import os
                    if 'WINGDB_ACTIVE' in os.environ: raise

                added = i  # remember last added index for selection

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

    def properties_changed(self, modified):
        if not modified or "tabs" in modified:
            if not self.pages:
                # during XML parsing, the tab names are set before
                page_count = len(self.properties["tabs"].value)
                self.pages.extend( [None]*page_count )
        ManagedBase.properties_changed(self, modified)

    # helpers ##########################################################################################################
    def next_notebook_name(self):
        # return new and (still) unused notebook name
        while True:
            name = 'notebook_%d' % EditNotebook._next_notebook_number
            if not common.app_tree.has_name(name):
                break
            EditNotebook._next_notebook_number += 1
        return name

    def next_pane_name(self, suggestion=None):
        # return new and (still) unused pane name
        if suggestion and not common.app_tree.has_name(suggestion):
            return suggestion
        while True:
            pane_name = "%s_pane_%d" % (self.name, self.next_pane_number)
            self.next_pane_number += 1
            if not common.app_tree.has_name(pane_name):
                break
        return pane_name

    def find_page(self, page):
        "returns the index of the given page in the notebook, or -1 if the page cannot be found"
        if not self.widget: return -1
        for i,p in enumerate(self.pages):
            if p is page:
                if i < self.widget.GetPageCount():
                    return i
                else:
                    return -1
        return -1


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

    widget = editor_class(None, parent, wx.ID_ANY, style, sizer, pos)

    node = Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node
    widget.properties["proportion"].set(1)
    widget.properties["flag"].set("wxEXPAND")
    if parent.widget: widget.create()
    common.app_tree.insert(node, sizer.node, pos-1)

    widget.insert_tab(0, widget.next_pane_name()) # next_pane_name will be used as label and as pane name, if possible
    sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.ID_ANY, editor_style, sizer, pos)
    sizer.set_item(widget.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
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
