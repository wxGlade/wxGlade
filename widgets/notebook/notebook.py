"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc
import wcodegen
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin, Slot
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from panel import EditPanel


class NotebookPagesProperty(np.GridProperty):
    def __init__(self, value, cols):
        col_widths = [300,]
        np.GridProperty.__init__(self, value, cols, col_sizes=col_widths, can_remove_last=True, with_index=True)

    def _get_default_row(self, index):
        return ["Page %d"%(index+1)]

    def write(self, output, tabs):
        inner_xml = []
        for (name,), window in zip(self.owner.tabs, self.owner.children):
            # XXX what happens with empty pages?
            if window:
                inner_xml += common.format_xml_tag(u'tab', name, tabs+1, window=window.name)
        output.extend( common.format_xml_tag(u'tabs', inner_xml, tabs, is_xml=True) )

    def flush(self):
        # for this one, we don't want an automatic update on Ctrl-S / Ctrl-G
        pass


class TabsHandler(BaseXmlBuilderTagHandler):
    def __init__(self, parent):
        super(TabsHandler, self).__init__()
        self.parent = parent
        self.tab_names = [] # a list of one-item lists; to be compatible to GridProperty
        self.pagenames = []

    def start_elem(self, name, attrs):
        if name=='tab':
            window = attrs["window"]
            if window=="SLOT": window = None
            self.pagenames.append(window)

    def end_elem(self, name):
        if name == 'tabs':
            self.parent.pages = self.pagenames
            self.parent.children = [None]*len(self.tab_names)
            self.parent.properties['tabs'].set(self.tab_names)
            self.parent.properties_changed(["tabs"])
            return True
        elif name == 'tab':
            # actually, the tab labels
            char_data = self.get_char_data()
            self.tab_names.append([misc.wxstr(char_data),])
        return False


class EditNotebook(ManagedBase, EditStylesMixin):
    "Class to handle wxNotebook objects"
    _next_notebook_number = 1 # next free number for notebook names
    update_widget_style = False

    WX_CLASS = "wxNotebook"
    CAN_BE_CLASS = True
    _PROPERTIES = ["Widget", "no_custom_class", "style", "tabs"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    np.insert_after(PROPERTIES, "name", "class", "custom_base")

    def __init__(self, name, parent, pos, style, instance_class=None, class_=None):
        ManagedBase.__init__(self, name, parent, pos, instance_class, class_)
        EditStylesMixin.__init__(self)
        self.properties["style"].set(style)

        self._is_removing_pages = False

        # initialise instance properties
        self.pages = None  # on loading from XML, this will be used
        tabs = []  # list of page labels of this notebook
        tab_cols = [('Tab label', np.GridProperty.STRING)]
        self.tabs = NotebookPagesProperty(tabs, tab_cols)
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wx.Notebook( self.parent_window.widget, self.id, style=self.style )
        for c,(label,) in zip(self.children, self.tabs):
            c.create()
            if c.IS_SLOT:
                self.widget.AddPage(c.widget, label)

    def on_load(self, child=None):
        ManagedBase.on_load(self)
        self.pages = None

    def post_load(self):
        # at this time, all children should be available
        if not self.parent.IS_SIZER or not self.widget: return
        w,h = self.properties["size"].get_size(self.widget)
        sz = self.parent
        sz.widget.SetItemMinSize(self.widget, w, h)
        sz.layout(True)

    def on_set_focus(self, event):
        # allow switching of pages
        misc.set_focused_widget(self)
        event.Skip()

    ####################################################################################################################
    # new implementation, parts from previous 'virtual sizer':
    def add_item(self, child, pos=None):
        ManagedBase.add_item(self, child, pos)
        # avoid widgets being destroyed; this is done by DeletePage/InsertPage
        child._dont_destroy = True

    def vs_insert_tab(self, index):
        "inserts or adds a page"
        label = self.tabs[index][0]
        item = self.children[index]
        if not (index < self.widget.GetPageCount()):
            self.widget.AddPage(item.widget, label)
        elif self.widget.GetPage(index) is not item.widget:
            self.widget.InsertPage(index, item.widget, label)

        try:
            wx.CallAfter(item.sel_marker.update)
        except AttributeError:
            pass
        if hasattr(self.parent, "set_item_best_size"):
            self.parent.set_item_best_size( self, size=self.widget.GetBestSize() )

    def insert_tab(self, index, label):
        # add tab/page; called from GUI
        tab = self._insert_tab(index, label)
        misc.rebuild_tree(self)

    def add_slot(self):
        # actually adds a page, but it needs to be compatible to sizers
        self.insert_tab(len(self.children), "new tab")

    def _insert_tab(self, index, label):
        # add tab/page
        tabs_p = self.properties["tabs"]
        tabs = tabs_p.get()   # the value will be modified in place
        tabs.insert(index, [label,])
        tabs_p.set(tabs)
        self.children.insert(index, None)

        # adjust pos of the following pages
        for i, page in enumerate(self.children[index+1:]):
            pos_p = page.properties["pos"]
            pos_p.set(index+1+i)

        # create panel and node, add to tree
        editor = EditPanel( self.next_pane_name(), self, index )

        if self.widget:
            # add to widget
            editor.create()
            compat.SetToolTip(editor.widget, _("Notebook page pane:\nAdd a sizer here") )
            self.vs_insert_tab(index)

            try:
                wx.CallAfter(editor.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                if config.debugging: raise

            self.widget.SetSelection(index)

        self.properties["tabs"].update_display()
        return editor

    def remove_tab(self, index):
        # for context menu of an empty page / Slot instance
        indices = [i for i in range(0,len(self.tabs)) if i!=index]
        old_labels = [tab[0] for tab in self.tabs]
        del self.properties["tabs"].value[index]
        self.set_tabs(old_labels, indices)

        if self.children:
            if index>=len(self.children): index -= 1
            misc.set_focused_widget(self.children[index])
        else:
            misc.set_focused_widget(self)

    @misc.restore_focus
    def set_tabs(self, old_labels, indices):  # called from tabs proberty on Apply button
        """tabs: list of strings
        indices: the current indices of the tabs or None for a new tab; re-ordering is currently not supported"""
        keep_indices = [pos for pos in indices if pos is not None]
        if keep_indices != sorted(keep_indices):
            raise ValueError("Re-ordering is not yet implemented")
        keep_indices = set(keep_indices)
        new_labels = old_labels[:]

        # set tab labels of existing pages, if modified
        for (label,), index in zip(self.tabs, indices):
            if index is not None and old_labels[index]!=label:
                new_labels[index] = [label,]
                if self.widget:
                    self.widget.SetPageText(index, label)

        # remove tabs
        for index in range(len(old_labels)-1, -1, -1):
            if not index in keep_indices:
                self._is_removing_pages = True
                self.children[index].recursive_remove()
                if self.widget: self.widget.RemovePage(index)    # deletes the specified page
                del new_labels[index]                            # delete from list of names
                self._is_removing_pages = False

        # insert/add tabs
        added = None
        for pos, (label,) in enumerate(self.tabs):
            index = indices[pos]
            if index is not None:
                # old tab to be kept, just ensure that pos is correct
                pos_p = self.children[pos].properties["pos"]
                if pos_p.value!=pos: pos_p.set(pos)
                continue

            # actually add/insert
            new_labels.insert(pos, [label,]) # this needs to be done before EditPanel calls self.virtual_sizer.add_item
            self.children.insert(pos, None)
            # create panel and node, add to tree
            suggestion = "%s_%s" % (self.name, label)
            editor = EditPanel( self.next_pane_name(suggestion), self, pos )

            # adjust pos of the following pages
            for p, page in enumerate(self.children[pos+1:]):
                pos_p = page.properties["pos"]
                pos_p.set(pos+2+p)

            if self.widget:
                # add to widget
                editor.create()
                self.vs_insert_tab(pos)
    
                try:
                    wx.CallAfter(editor.sel_marker.update)
                except AttributeError:
                    if config.debugging: raise

                added = pos  # remember last added index for selection

        # select the last added tab
        if added is not None and self.widget:
            self.widget.SetSelection(added)

        common.app_tree.build(self, recursive=False)

    def _free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if self._is_removing_pages: return

        slot = Slot(self, pos)
        slot.create()
        label = self.tabs[pos][0]
        if self.widget: self.widget.RemovePage(pos)

        if self.widget:
            self.widget.InsertPage(pos, slot.widget, label)
            self.widget.SetSelection(pos)
        return slot

    ####################################################################################################################
    # methods moved from NotebookVirtualSizer:

    def item_properties_modified(self, widget, modified=None, force_layout=True):
        if not self.widget: return
        index = widget.pos
        item = self.children[index]
        if not item or not item.widget: return
        label = self.tabs[index][0]
        if not (index < self.widget.GetPageCount()):
            self.widget.AddPage(item.widget, label) # this is e.g. for the first creation after loading a file
        elif self.widget.GetPage(index) is not item.widget:
            # XXX delete this part if it's not called; insert_tab and remove_tab should handle this now
            if self.widget.GetPageCount()==len(self.children):
                #self.widget.RemovePage(index) # deletes the specified page, without deleting the associated window
                self.widget.DeletePage(index)  # deletes the specified page, and the associated window
            self.widget.InsertPage(index, item.widget, label)
            self.widget.SetSelection(index)
            try:
                wx.CallAfter(item.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass
        if hasattr(self.parent, "set_item_best_size"):
            #self.sizer.set_item( self.pos, size=self.widget.GetBestSize() )
            self.parent.set_item_best_size( self, size=self.widget.GetBestSize() )

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        # only used when loading from XML, while pages is defined
        name = attrs.get("original_name", None) or attrs.get_attributes('name')[0]
        try:
            return self.pages.index(name)
        except ValueError:
            from xml_parse import XmlParsingError
            raise XmlParsingError( _('Notebook widget "%s" does not have tab "%s"!')%(self.name, name) )

    ####################################################################################################################
    def destroy_widget(self):
        if self.widget: self.widget.DeleteAllPages()
        ManagedBase.destroy_widget(self)

    def get_property_handler(self, name):
        if name == 'tabs':
            return TabsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def properties_changed(self, modified):
        if modified and "tabs" in modified and self.widget:
            for i,(tab,) in enumerate(self.tabs):
                self.widget.SetPageText(i,tab)
            
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)

    ####################################################################################################################
    def _add_popup_menu_items(self, menu, widget):
        # called from managed widget items' _create_popup_menu method
            i = misc.append_menu_item(menu, -1, _('Add Notebook Tab') )
            misc.bind_menu_item_after(widget, i, self.add_slot)

    def _add_parent_popup_menu_items(self, menu, item, widget):
        # called from managed widget items' _create_popup_menu method
        if item.IS_SLOT:
            i = misc.append_menu_item(menu, -1, _('Remove Notebook Tab\tDel') )
            misc.bind_menu_item_after(widget, i, self.remove_tab, item.pos)
            
        i = misc.append_menu_item(menu, -1, _('Insert Notebook Tab before') ) # \tCtrl+I') )
        misc.bind_menu_item_after(widget, i, self.insert_tab, item.pos, "new tab")

        if item.pos==len(self.tabs)-1: # last slot -> allow to add
            i = misc.append_menu_item(menu, -1, _('Add Notebook Tab') ) # \tCtrl+A') )
            misc.bind_menu_item_after(widget, i, self.add_slot)
        menu.AppendSeparator()

    # helpers ##########################################################################################################
    def next_pane_name(self, suggestion=None):
        # return new and (still) unused pane name
        if suggestion:
            suggestion = [c for c in suggestion if "a"<=c<="z" or "A"<=c<="Z" or "0"<=c<="9" or c=="_"]
            while suggestion and "0"<=suggestion[0]<="9":
                del suggestion[0]
            suggestion = "".join(suggestion)
        if suggestion and suggestion not in self.toplevel_parent.names:
            return suggestion
        tmpl = "%s_pane_%%d" % self.name
        return self.toplevel_parent.get_next_contained_name(tmpl)

    def _get_slot_label(self, pos):
        title = self.tabs[pos][0]
        return '[%s] Page %s'%(title, pos)

    def check_compatibility(self, widget, typename=None):
        return (False,"No objects can be pasted here; paste to empty pages instead.")

    def check_drop_compatibility(self):
        # checks whether a widget can be dropped here
        return (False, "Items can only be added to empty pages/slots, not to the notebook itself.")

    def _get_parent_tooltip(self, pos):
        return "Notebook page %s:"%pos


def builder(parent, pos):
    "Factory function for editor objects from GUI"
    choices = 'wxNB_TOP|wxNB_BOTTOM|wxNB_LEFT|wxNB_RIGHT'
    dialog = wcodegen.WidgetStyleSelectionDialog(_('wxNotebook'), _('Orientation'), choices)
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK: return

    name = parent.toplevel_parent.get_next_contained_name('notebook_%d')
    with parent.frozen():
        editor = EditNotebook(name, parent, pos, style)
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
        editor._insert_tab(0, editor.next_pane_name()) # next_pane_name will be used as label and as pane name, if possible
    return editor


def xml_builder(parent, pos, attrs):
    "Factory to build editor objects from a XML file"
    attrs.set_editor_class(EditNotebook)
    name, class_, instance_class = attrs.get_attributes("name", "class", "instance_class")
    return EditNotebook(name, parent, pos, '', instance_class, class_)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditNotebook'] = EditNotebook
    common.widgets['EditNotebook'] = builder
    common.widgets_from_xml['EditNotebook'] = xml_builder
    return common.make_object_button('EditNotebook', 'notebook.xpm')
