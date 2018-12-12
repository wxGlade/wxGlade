"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc
import wcodegen
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin, Slot
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from xml_parse import XmlParsingError

from panel import EditPanel


"""
class NotebookVirtualSizer(Sizer):

    def set_item(self, index):
        "Updates the layout of the item at the given pos; (re-)creates the notebook page if required"
        if not self.window.widget:
            return
        index = pos
        item = self.window.pages[index]
        if not item or not item.widget:
            return
        label = self.window.tabs[index][0]
        if not (index < self.window.widget.GetPageCount()):
            self.window.widget.AddPage(item.widget, label) # this is e.g. for the first creation after loading a file
        elif self.window.widget.GetPage(index) is not item.widget:
            # XXX delete this part if it's not called; insert_tab and remove_tab should handle this now
            if self.window.widget.GetPageCount()==len(self.window.children):
                #self.window.widget.RemovePage(index) # deletes the specified page, without deleting the associated window
                self.window.widget.DeletePage(index)  # deletes the specified page, and the associated window
            self.window.widget.InsertPage(index, item.widget, label)
            self.window.widget.SetSelection(index)
            try:
                wx.CallAfter(item.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass
        if self.parent.IS_SIZER:
            self.parent.set_item_best_size( self.window, size=self.window.widget.GetBestSize() )

"""

class NotebookPagesProperty(np.GridProperty):
    def __init__(self, value, cols):
        col_widths = [300,]
        np.GridProperty.__init__(self, value, cols, col_sizes=col_widths, can_remove_last=False, with_index=True)

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
    _custom_base_classes = True
    _next_notebook_number = 1 # next free number for notebook names
    update_widget_style = False

    _PROPERTIES = ["Widget", "no_custom_class", "style", "tabs"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, style, pos):
        ManagedBase.__init__(self, name, 'wxNotebook', parent, pos)
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
        #self.widget.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)

    def on_load(self):
        print("on_load", self, self.name)
        ## XXX now it should be possible to add Nones as placeholder first and then create all Slots here
        ## create slot nodes for empty slots
        #if len(self.children)<len(self.pages):
            #self.children.extend( [None] * (len(self.pages) - len(self.children)) )
        ##self._add_slots()
        ManagedBase.on_load(self)
        self.pages = None

    def post_load(self):
        # at this time, all children should be available
        # no longer required when the data structure is changed
        if self.parent.IS_SIZER:
            self.parent.item_properties_modified2(self)

    def on_set_focus(self, event):
        # allow switching of pages
        misc.set_focused_widget(self)
        event.Skip()
        #misc.exec_after(self.widget.Refresh())

    ####################################################################################################################
    # new implementation:
    # together with NotebookVirtualSizer insert_tab, remove_tab, free_tab

    #def add_item(self, child, pos=None):
        ## ensure that empty pages before pos get their slots, as otherwise common.app_tree.insert will fail
        #ManagedBase.add_item(self, child, pos)
        #self._add_slots()

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
        if self.sizer is not None:
            self.sizer.set_item_best_size( self, size=self.widget.GetBestSize() )

    def insert_tab(self, index, label):
        # add tab/page; called from GUI
        tab = self._insert_tab(self, index, label)
        misc.rebuild_tree(self)

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
        editor._dont_destroy = True

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

    #@misc.restore_focus
    #def set_tabs(self, old_labels, indices):  # called from tabs proberty on Apply button
        #"""tabs: list of strings
        #indices: the current indices of the tabs or None for a new tab; re-ordering is currently not supported"""
        #keep_indices = [pos for pos in indices if pos is not None]
        #if keep_indices != sorted(keep_indices):
            #raise ValueError("Re-ordering is not yet implemented")
        #keep_indices = set(keep_indices)
        #new_labels = old_labels[:]

        ## set tab labels of existing pages, if modified
        #for (label,), index in zip(self.tabs, indices):
            #if index is not None and old_labels[index]!=label:
                #new_labels[index] = [label,]
                #if self.widget:
                    #self.widget.SetPageText(index, label)

        ## remove tabs
        #for index in range(len(old_labels)-1, -1, -1):
            #if not index in keep_indices:
                #self._is_removing_pages = True
                #self.virtual_sizer.remove_tab(index)            # remove from sizer; does not delete window
                #self.children[index]._remove()                     # delete the page content without setting the focus
                #self.children[index].tree_remove() + XXX  # this has not been tested
                #common.app_tree.remove(self.children[index])  # remove from tree
                #del self.children[index]                           # delete from page list
                #del new_labels[index]                            # delete from list of names
                #self._is_removing_pages = False

        ## insert/add tabs
        #added = None
        #for pos, (label,) in enumerate(self.tabs):
            #index = indices[pos]
            #if index is not None:
                ## old tab to be kept, just ensure that pos is correct
                #pos_p = self.children[pos].properties["pos"]
                #if pos_p.value!=pos: pos_p.set(pos)
                #continue

            ## actually add/insert
            #new_labels.insert(pos, [label,]) # this needs to be done before EditPanel calls self.virtual_sizer.add_item
            #self.children.insert(pos, None)
            ## create panel and node, add to tree
            #suggestion = "%s_%s" % (self.name, label)
            #window = EditPanel( self.next_pane_name(suggestion), self, pos )
            #window._dont_destroy = True

            ## adjust pos of the following pages
            #for p, page in enumerate(self.children[pos+1:]):
                #pos_p = page.properties["pos"]
                #pos_p.set(pos+2+p)

            #common.app_tree.insert(window, self, pos, select=False)  # XXX call this only when called from GUI

            #if self.widget:
                ## add to widget
                #window.create()
                #compat.SetToolTip(window.widget, _("Notebook page pane:\nAdd a sizer here") )
                #self.vs_insert_tab(pos)
    
                #try:
                    #wx.CallAfter(window.sel_marker.update)
                #except AttributeError:
                    #if config.debugging: raise

                #added = pos  # remember last added index for selection

        ## select the last added tab
        #if added is not None and self.widget:
            #self.widget.SetSelection(added)

        ## update tree labels
        #for child in self.children:
            #common.app_tree.refresh(child, refresh_label=True, refresh_image=False)

    def _free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        #if self._is_removing_pages or not self.window.widget:
        if self._is_removing_pages:
            return
        old_child = self.children[pos]
        slot = Slot(self, pos)
        slot.create()
        label = self.tabs[pos][0]
        if self.widget:
            self.widget.RemovePage(pos)

        old_child.tree_remove()
        common.app_tree.remove(old_child)

        if self.widget:
            self.widget.InsertPage(pos, slot.widget, label)
            self.widget.SetSelection(pos)
        return slot

    ####################################################################################################################
    # methods moved from NotebookVirtualSizer:
    def _add_item(self, item, pos=None, size=None, force_layout=True):
        pass

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
        if self.sizer is not None:
            #self.sizer.set_item( self.pos, size=self.widget.GetBestSize() )
            self.sizer.set_item_best_size( self, size=self.widget.GetBestSize() )

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        # only used when loading from XML, while pages is defined
        try:
            name = attrs.get("original_name", None) or attrs['name']
            return self.pages.index(name)
        except ValueError:
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
    def _add_popup_menu_items(self, menu, item, widget):
        # called from managed widget items' _create_popup_menu method
        i = misc.append_menu_item(menu, -1, _('Insert Notebook Tab before') ) # \tCtrl+I') )
        misc.bind_menu_item_after(widget, i, self.insert_tab, item.pos, "new tab")

        if item.pos==len(self.tabs)-1: # last slot -> allow to add
            i = misc.append_menu_item(menu, -1, _('Add Notebook Tab') ) # \tCtrl+A') )
            misc.bind_menu_item_after(widget, i, self.insert_tab, item.pos+1, "new tab")
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
        return common.root.get_next_name(tmpl, self)

    def _get_slot_label(self, pos):
        return "Notebook Page %d"%pos


def builder(parent, pos):
    "Factory function for editor objects from GUI"
    choices = 'wxNB_TOP|wxNB_BOTTOM|wxNB_LEFT|wxNB_RIGHT'
    dialog = wcodegen.WidgetStyleSelectionDialog(_('wxNotebook'), _('Orientation'), choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return
    name = common.root.get_next_name('notebook_%d', parent)
    with parent.frozen():
        editor = EditNotebook(name, parent, style, pos)
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
        editor._insert_tab(0, editor.next_pane_name()) # next_pane_name will be used as label and as pane name, if possible
    return editor


def xml_builder(attrs, parent, pos=None):
    "Factory to build editor objects from a XML file"
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditNotebook(name, parent, '', pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditNotebook'] = builder
    common.widgets_from_xml['EditNotebook'] = xml_builder
    return common.make_object_button('EditNotebook', 'notebook.xpm')
