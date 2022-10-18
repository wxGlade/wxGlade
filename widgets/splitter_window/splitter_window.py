"""
wxSplitterWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, compat, config, misc
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin
from edit_base import Slot
from panel import EditPanel


class ChildWidgetNameProperty(np.Property):
    def __init__(self, child_index):
        self.child_index = child_index
        np.Property.__init__(self, None, default_value=None, name=None)

    def get(self):
        child = self.owner.children[self.child_index]
        if child is None: return self.value
        if child.IS_SLOT: return None
        return child.name

    def write(self, output, tabs=0):
        value = self.get()
        if value is not None:
            output.extend( common.format_xml_tag(self.name, value, tabs) )


class EditSplitterWindow(ManagedBase, EditStylesMixin):
    "Class to handle wxSplitterWindow objects; orientation: Orientation of the widget as string e.g. 'wxSPLIT_VERTICAL'"

    WX_CLASS = 'wxSplitterWindow'
    IS_CONTAINER = True
    _PROPERTIES = ["Widget", "no_custom_class", "style", "sash_pos", "sash_gravity", "min_pane_size"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase._EXTRA_PROPERTIES
    np.insert_after(PROPERTIES, "name", "class", "custom_base")
    _PROPERTY_LABELS = {'no_custom_class':"Don't generate code for this class",
                        'sash_pos':"Sash position"}
    _PROPERTY_HELP = {'no_custom_class':"Don't generate code for this class",
                      'sash_gravity':"0.0: only the bottom/right window is automatically resized\n"
                                     "0.5: both windows grow by equal size\n"
                                     "1.0: only left/top window grows"}
    CHILDREN = 2
    def __init__(self, name, parent, index, orientation):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)
        self.sash_pos = np.SpinPropertyD(0, default_value="")
        if hasattr(wx, "SpinCtrlDouble"):
            self.sash_gravity = np.SpinDoublePropertyD(0.5, (0.0,1.0), default_value=0.0, immediate=True)
        else:
            self.sash_gravity = np.FloatPropertyD(0.5, (0.0,1.0), default_value=0.0)
        self.min_pane_size = np.SpinPropertyA(20)

        # hidden properties: orientation string, window names window_1, window_2
        self.orientation = np.Property(orientation)
        self.window_1 = ChildWidgetNameProperty(0)
        self.window_2 = ChildWidgetNameProperty(1)

    def _get_label(self, index):
        if self.orientation=="wxSPLIT_VERTICAL":
            return ("Left","Right")[index]
        return ("Top","Bottom")[index]
    
    def _get_slot_label(self, index):
        return "SLOT %s"%self._get_label(index)

    def create_widget(self):
        size = self._get_default_or_client_size()
        self.widget = wx.SplitterWindow(self.parent_window.widget, wx.ID_ANY, size=size, style=self.style)

    def finish_widget_creation(self, level):
        ManagedBase.finish_widget_creation(self, level, sel_marker_parent=self.widget)

        sash_pos_p = self.properties['sash_pos']
        if sash_pos_p.is_active():
            self.widget.SetSashPosition(sash_pos_p.get())
        else:
            sash_pos_p.set(self.widget.GetSashPosition())

        sash_gravity_p = self.properties['sash_gravity']
        if sash_gravity_p.is_active():
            self.widget.SetSashGravity(sash_gravity_p.get())

        min_pane_size_p = self.properties['min_pane_size']
        if min_pane_size_p.is_active():
            self.widget.SetMinimumPaneSize( min_pane_size_p.get() )
        else:
            min_pane_size_p.set( self.widget.GetMinimumPaneSize() )

        self.widget.Bind(wx.EVT_SPLITTER_SASH_POS_CHANGED, self.on_sash_pos_changed )

        if self.widget.GetTopLevelParent().IsShown():
            # e.g. when pasting into an existing window
            wx.CallAfter(self.widget.UpdateSize)

    def on_set_focus(self, event):
        misc.set_focused_widget(self)
        # here we must call event.Skip() also on Win32 as this we should be able to move the sash
        event.Skip()

    def split(self):
        orientation = self.orientation
        sash_pos_p = self.properties['sash_pos']
        if sash_pos_p.is_active():
            sash_pos = sash_pos_p.get()
        else:
            max_pos = self.widget.GetClientSize() [0 if orientation=='wxSPLIT_VERTICAL' else 1]
            sash_pos = max_pos // 2
        if orientation == 'wxSPLIT_VERTICAL':
            self.widget.SplitVertically  (self.children[0].widget, self.children[1].widget, sash_pos)
        else:
            self.widget.SplitHorizontally(self.children[0].widget, self.children[1].widget, sash_pos)

        if getattr(self.children[0], 'sel_marker', None): self.children[0].sel_marker.update()
        if getattr(self.children[1], 'sel_marker', None): self.children[1].sel_marker.update()

    def _properties_changed(self, modified, actions):
        if (not modified or "sash_pos" in modified) and self.widget and self.check_prop("sash_pos"):
            self.widget.SetSashPosition(self.sash_pos)
        if (not modified or "sash_gravity" in modified) and self.widget and self.check_prop("sash_gravity"):
            self.widget.SetSashGravity(self.sash_gravity)
        if (not modified or "min_pane_size" in modified) and self.widget and self.check_prop("min_pane_size"):
            self.widget.SetMinimumPaneSize(self.min_pane_size)

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)

        if modified and "orientation" in modified and common.app_tree is not None:
            # update horizontal/vertical icons
            actions.add("image")
            if self.children[0] and self.children[0].IS_SLOT:
                self.children[0].label = self._get_slot_label(0)
                common.app_tree.refresh(self.children[0])
            if self.children[1] and self.children[1].IS_SLOT:
                self.children[1].label = self._get_slot_label(1)
                common.app_tree.refresh(self.children[1])

    def on_size(self, event):
        if not self.widget:
            return
        try:
            if self.orientation == 'wxSPLIT_VERTICAL':
                max_pos = self.widget.GetClientSize()[0]
            else:
                max_pos = self.widget.GetClientSize()[1]
            self.properties['sash_pos'].set_range(-max_pos, max_pos)
            if not self.properties['sash_pos'].is_active():
                self.widget.SetSashPosition(max_pos // 2)
                self.properties['sash_pos'].set( self.widget.GetSashPosition() )
        except (AttributeError, KeyError):
            pass
        ManagedBase.on_size(self, event)

    def on_child_pasted(self):
        if not self.widget: return
        self.widget.UpdateSize()

    def on_sash_pos_changed(self, event):
        self.properties['sash_pos'].set( self.widget.GetSashPosition() )
        event.Skip()

    def on_mouse_events(self, event):
        # resize instead of drag & drop
        event.Skip()

    def check_compatibility(self, widget, typename=None):
        return (False,"No objects can be pasted here; paste to empty slots instead.")

    def check_drop_compatibility(self):
        # checks whether a widget can be dropped here
        return (False, "Items can only be added to empty slots, not to the splitter window itself.")

    def _get_parent_tooltip(self, index):
        return "%s splitter pane:"%self._get_label(index)

    ####################################################################################################################
    # methods moved from SplitterWindowSizer:
    def add_item(self, child, index=None):
        if index is not None and self.widget and self.widget.IsSplit():
            self.widget.Unsplit(self.children[index].widget)
        ManagedBase.add_item(self, child, index)

    def _free_slot(self, index, force_layout=True):
        "Replaces the element at pos with an empty slot"
        slot = Slot(self, index)
        if self.widget: slot.create()
        return slot

    def destroying_child_widget(self, child, index):
        if self.widget.IsSplit():
            self.widget.Unsplit(child.widget)

    def child_widget_created(self, widget, level):
        if level==0: self.split()  # a single child was added

    def child_widgets_created(self, level):
        self.split()

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        name = attrs.get("original_name", None) or attrs['name']
        if name==self.properties["window_1"].value: return 0
        if name==self.properties["window_2"].value: return 1
        return None


dlg_title = _('wxSplitterWindow')
box_title = _('Orientation')
choices = 'wxSPLIT_VERTICAL (left/right)|wxSPLIT_HORIZONTAL (top/bottom)'


def builder(parent, index):
    "Factory function for EditSplitterWindow objects"
    import dialogs
    dialog = dialogs.WidgetStyleSelectionDialog( dlg_title, box_title, choices, ["Create panels"], [True])
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    orientation = dialog.get_selection().split(" ")[0]
    create_panels = dialog.get_options()[0]
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = parent.toplevel_parent.get_next_contained_name('window_%d')
    with parent.frozen():
        editor = EditSplitterWindow(name, parent, index, orientation)
        editor.properties["style"].set_to_default()
        if create_panels:
            pane1 = EditPanel(name + '_pane_1', editor, 0)
            pane2 = EditPanel(name + '_pane_2', editor, 1)
        else:
            editor._add_slots()  # XXX focus should be set to first slot

        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")

        if parent.widget: editor.create()

        return editor


def xml_builder(parser, base, name, parent, index):
    "Factory to build editor objects from a XML file"
    return EditSplitterWindow(name, parent, index, 'wxSPLIT_VERTICAL')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSplitterWindow'] = EditSplitterWindow
    common.widgets['EditSplitterWindow'] = builder
    common.widgets_from_xml['EditSplitterWindow'] = xml_builder

    import os.path
    from tree import WidgetTree
    WidgetTree.images['EditSplitterSlot-Left']   = os.path.join( config.icons_path, 'splitter_slot-left.png' )
    WidgetTree.images['EditSplitterSlot-Right']  = os.path.join( config.icons_path, 'splitter_slot-right.png' )
    WidgetTree.images['EditSplitterSlot-Top']    = os.path.join( config.icons_path, 'splitter_slot-top.png' )
    WidgetTree.images['EditSplitterSlot-Bottom'] = os.path.join( config.icons_path, 'splitter_slot-bottom.png' )
    WidgetTree.images['EditSplitterWindow']      = os.path.join( config.icons_path, 'splitter_window.png' )
    WidgetTree.images['EditSplitterWindow-h']    = os.path.join( config.icons_path, 'splitter_window-h.png' )

    return common.make_object_button('EditSplitterWindow', 'splitter_window.png')
