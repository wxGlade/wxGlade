"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2023 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc, config
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import panel, edit_sizers, edit_base


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
            self.parent.properties['tabs'].load(self.tab_names)
            return True
        elif name == 'tab':
            # actually, the tab labels
            char_data = self.get_char_data()
            self.tab_names.append([misc.wxstr(char_data),])
        return False


import history

class HistoryNotebookTabsItem(history.HistoryItem):
    def __init__(self, notebook, property_item, removed_tabs, added_tabs):
        self.old_tabs = property_item.old.value
        self.new_tabs = property_item.new.value
        self.path = notebook.get_path()
        self.removed_tabs = removed_tabs
        self.added_tabs = added_tabs

    def _do(self, old, new, added, removed):
        notebook = common.root.find_widget_from_path(self.path)
        assert notebook.tabs == old
        tabs_p = notebook.properties["tabs"]
        tabs_p.reset(update_display=False)
        notebook.restore_tabs(old, new, added, removed)
        tabs_p._initialize_indices()
        tabs_p.update_display()
        return notebook

    def undo(self):
        #                                      reversed === sorted          reversed === highest first
        return self._do(self.new_tabs, self.old_tabs, reversed(self.removed_tabs), reversed(self.added_tabs))

    def redo(self):
        return self._do(self.old_tabs, self.new_tabs, self.added_tabs, self.removed_tabs)


class EditNotebook(ManagedBase, EditStylesMixin):
    "Class to handle wxNotebook objects"
    _next_notebook_number = 1 # next free number for notebook names

    WX_CLASS = "wxNotebook"
    IS_CONTAINER = True
    _PROPERTIES = ["Widget", "no_custom_class", "style", "tabs"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    np.insert_after(PROPERTIES, "name", "class", "custom_base")

    def __init__(self, name, parent, index, style):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)

        # initialise instance properties
        self.pages = None  # on loading from XML, this will be used
        tabs = []  # list of page labels of this notebook
        tab_cols = [('Tab label', np.GridProperty.STRING)]
        self.tabs = NotebookPagesProperty(tabs, tab_cols)
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wx.Notebook( self.parent_window.widget, wx.ID_ANY, style=self.style )

    def child_widgets_created(self, level):
        # at this time, all children should be available
        self.pages = None
        if not self.parent.IS_SIZER or not self.widget: return
        w,h = self.properties["size"].get_size(self.widget)
        sz = self.parent
        sz.widget.SetItemMinSize(self.widget, w, h)

    def on_set_focus(self, event):
        # allow switching of pages
        misc.set_focused_widget(self)
        event.Skip()

    ####################################################################################################################
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

    def add_slot(self):
        # actually adds a page, but it needs to be compatible to sizers
        common.history.property_changing( self.properties["tabs"] )
        index = len(self.children)
        editor = self.insert_tab(index, "new tab")
        item = common.history._finalize_item(stop=True)  # property_changing was called for "tabs"
        import clipboard
        added_tabs = [ (index, clipboard.dump_widget(editor)) ]
        common.history.add_item( HistoryNotebookTabsItem(self, item, [], added_tabs), can_repeat=False )

    def insert_tab(self, index, label, add_panel=True, add_sizer=False):
        # add tab/page; called from GUI
        self.properties["tabs"].insert( index, [label,] )

        # create panel and node, add to tree
        self.insert_item(None, index)  # placeholder
        if add_panel:
            if not isinstance(add_panel, compat.unicode): add_panel = self.next_pane_name()
            editor = panel.EditPanel( add_panel, self, index )
            if add_sizer:
                edit_sizers._builder(editor, 0)
        else:
            # just add a slot
            editor = edit_base.Slot(self, index)

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
        misc.rebuild_tree(self)
        return editor

    def remove_item(self, child, level, keep_slot=False):
        # history is handled from EditBase.remove    (when the empty page/slot is removed)
        #                      or ManagedBase.remove (when e.g. the panel is removed, but the page/slot remains)
        if not keep_slot:
            tabs_p = self.properties["tabs"]
            # if the length is different, then due to tabs_p.apply calling self.set_tabs and so it's removed already
            if len(tabs_p.value)==len(self.children):
                del tabs_p.value[child.index]
        ManagedBase.remove_item(self, child, level, keep_slot)

    def set_tabs(self, old_labels, indices, user=True):  # called from tabs property on Apply button
        """tabs: list of strings
        indices: the current indices of the tabs or None for a new tab; re-ordering is currently not supported"""
        import clipboard
        removed_tabs = []  # (index,xml_data)
        added_tabs = []
        keep_indices = [index for index in indices if index is not None]
        if keep_indices != sorted(keep_indices):
            raise ValueError("Re-ordering is not yet implemented")
        keep_indices = set(keep_indices)

        # set tab labels of existing pages, if modified
        for (label,), index in zip(self.tabs, indices):
            if index is not None and old_labels[index]!=label and self.widget:
                self.widget.SetPageText(index, label)

        # remove tabs
        for index in range(len(old_labels)-1, -1, -1):
            if not index in keep_indices:
                removed_tabs.append( (index, clipboard.dump_widget(self.children[index])) )
                self.children[index].recursive_remove(level=0)

        # insert/add tabs
        set_selected_page = None
        for index, (label,) in enumerate(self.tabs):
            if indices[index] is not None: continue  # keep tab

            # actually add/insert
            self.children.insert(index, None)
            # create panel and node, add to tree
            suggestion = "%s_%s" % (self.name, label)
            editor = panel.EditPanel( self.next_pane_name(suggestion), self, index )

            if self.widget:
                # add to widget
                editor.create()
                self.vs_insert_tab(index)

                try:
                    wx.CallAfter(editor.sel_marker.update)
                except AttributeError:
                    if config.debugging: raise

                set_selected_page = index  # remember last added index for selection
            added_tabs.append( (index, clipboard.dump_widget(editor)) )

        # select the last added tab
        if set_selected_page is not None:
            self.widget.SetSelection(set_selected_page)

        if user:
            # history: undo/redo
            item = common.history._finalize_item(stop=True)  # property_changing was called for "tabs"
            common.history.add_item( HistoryNotebookTabsItem(self, item, removed_tabs, added_tabs), can_repeat=False )

        common.app_tree.build(self, recursive=False)

    def restore_tabs(self, old_value, new_value, add_tabs, remove_tabs):  # called for undo/redo
        # add_tabs: (index, xml_data) each, where the indices refer to the new_value
        # removed_tabs are (index, xml_data) each, where the indices refer to the old_value
        old_labels = [v[0] for v in old_value]  # will be modified when adding or removing tabs, to check for changes
        new_labels = [v[0] for v in new_value]
        tabs_p = self.properties["tabs"]

        # remove tabs: index is highest first
        for index, xml_data in remove_tabs:
            self.children[index].recursive_remove(level=0)
            del old_labels[index]

        # insert/add tabs: index is lowest first
        set_selected_page = None
        rebuild = []
        for index, xml_data in add_tabs:
            # actually add/insert
            tabs_p.value.insert(index, [new_labels[index]])  # placeholder for pasting
            self.children.insert(index, None)
            old_labels.insert(index, None)
            import clipboard
            clipboard._paste(self, index, xml_data)

            if self.widget: set_selected_page = index  # remember last added index for selection

        # select the last added tab
        if set_selected_page is not None:
            self.widget.SetSelection(set_selected_page)
        tabs_p.value[:] = [[l] for l in new_labels]
        # change labels, if required
        if self.widget:
            for index, label in enumerate(old_labels):
                if label is None or label==new_labels[index]: continue
                print("setting label", index, new_labels[index])
                self.widget.SetPageText(index, new_labels[index])

        for index in rebuild:
            common.app_tree.build(self.children[index], recursive=True)
        common.app_tree.build(self, recursive=False)

    ####################################################################################################################
    def destroying_child_widget(self, child, index):
        self.widget.RemovePage(index) # deletes the specified page, without deleting the associated window

    def child_widget_created(self, child, level):
        # add, insert or replace a notebook page
        index = child.index
        label = self.tabs[index][0]
        if not (index < self.widget.GetPageCount()):
            # this is e.g. for the first creation after loading a file
            self.widget.AddPage(child.widget, label)
        else:
            # single page being replaced; e.g. panel -> slot or slot -> panel
            self.widget.InsertPage(index, child.widget, label)
        if level==0:
            self.widget.SetSelection(index)
            try:
                wx.CallAfter(child.sel_marker.update)
            except AttributeError:
                pass

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        # only used when loading from XML, while pages is defined
        name = attrs.get("original_name", None) or attrs['name']
        try:
            return self.pages.index(name)
        except ValueError:
            from xml_parse import XmlParsingError
            raise XmlParsingError( _('Notebook widget "%s" does not have tab "%s"!')%(self.name, name) )

    ####################################################################################################################
    def destroy_widget(self, level, later=True):
        if self.widget: self.widget.DeleteAllPages()
        ManagedBase.destroy_widget(self, level, later)

    def get_property_handler(self, name):
        if name == 'tabs':
            return TabsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def _get_direction(self, value_set):
        if value_set is None: return None
        if 'wxNB_LEFT' in value_set: return "L"
        if 'wxNB_RIGHT' in value_set: return "R"
        if 'wxNB_BOTTOM' in value_set: return "B"
        return "T"

    def check_property_modification(self, name, value, new_value):
        # avoid repeat if the number of pages does not match
        if name=="tabs" and len(value)!=len(new_value): return False
        return True

    def _properties_changed(self, modified, actions):
        if modified and "tabs" in modified and self.widget:
            for i,(tab,) in enumerate(self.tabs):
                self.widget.SetPageText(i,tab)

        if modified and "style" in modified:
            # wxNB_TOP to wxNB_BOTTOM can be changed on the fly; any other change requires re-creation
            p = self.properties["style"]
            new = self._get_direction(p.value_set)
            old = self._get_direction(p.previous_value)
            if wx.Platform=="__WXMSW__" and ((old=="T" and new=="B") or (old=="B" and new=="T")):
                actions.add("layout")
            elif old!=new:
                actions.add("recreate2")

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)

    ####################################################################################################################
    def _add_popup_menu_items(self, menu, widget):
        # called from managed widget items' _create_popup_menu method
            i = misc.append_menu_item(menu, -1, _('Add Notebook Tab') )
            misc.bind_menu_item_after(widget, i, self.add_slot)

    def _add_parent_popup_menu_items(self, menu, item, widget):
        # called from managed widget items' _create_popup_menu method
        if item.IS_SLOT:
            i = misc.append_menu_item(menu, -1, _('Remove Notebook Tab\tDel') )
            misc.bind_menu_item_after(widget, i, item.remove)

        i = misc.append_menu_item(menu, -1, _('Insert Notebook Tab before') ) # \tCtrl+I') )
        misc.bind_menu_item_after(widget, i, self.insert_tab, item.index, "new tab")

        if item.index==len(self.tabs)-1: # last slot -> allow to add
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
        tabs = set(tab[0] for tab in self.tabs)
        return self.toplevel_parent.get_next_contained_name(tmpl, tabs)

    def _get_slot_label(self, index):
        title = self.tabs[index][0]
        return '[%s] PAGE %s'%(title, index)

    def check_compatibility(self, widget, typename=None):
        return (False,"No objects can be pasted here; paste to empty pages instead.")

    def check_drop_compatibility(self):
        # checks whether a widget can be dropped here
        return (False, "Items can only be added to empty pages/slots, not to the notebook itself.")

    def _get_parent_tooltip(self, index):
        return "Notebook page %s:"%index


def builder(parent, index):
    "Factory function for editor objects from GUI"
    import dialogs
    choices = 'wxNB_TOP|wxNB_BOTTOM|wxNB_LEFT|wxNB_RIGHT'
    options = [("Pages", (0,20)),
                ">Add panels",
                ">Add sizers to panels"]
    defaults = [1, True, False]
    dialog = dialogs.WidgetStyleSelectionDialog(_('wxNotebook'), _('Orientation'), choices, options, defaults)
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    pages, add_panels, add_sizers = dialog.get_options()
    dialog.Destroy()
    if res != wx.ID_OK: return

    name = parent.toplevel_parent.get_next_contained_name('notebook_%d')
    with parent.frozen():
        editor = EditNotebook(name, parent, index, style)
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        for n in range(pages):
            # next_pane_name will be used as label and as pane name, if possible
            name = editor.next_pane_name()
            editor.insert_tab(n, name, add_panel=add_panels and name or False, add_sizer=add_sizers) 
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "Factory to build editor objects from a XML file"
    return EditNotebook(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditNotebook'] = EditNotebook
    common.widgets['EditNotebook'] = builder
    common.widgets_from_xml['EditNotebook'] = xml_builder
    return common.make_object_button('EditNotebook', 'notebook.png')
