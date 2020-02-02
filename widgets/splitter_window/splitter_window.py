"""
wxSplitterWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, compat, config, misc
import wcodegen
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

    _custom_base_classes = True

    WX_CLASS = 'wxSplitterWindow'
    CAN_BE_CLASS = True
    _PROPERTIES = ["Widget", "no_custom_class", "style", "sash_pos", "sash_gravity", "min_pane_size"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase._EXTRA_PROPERTIES
    _PROPERTY_LABELS = {'no_custom_class':"Don't generate code for this class",
                        'sash_pos':"Sash position"}
    _PROPERTY_HELP = {'no_custom_class':"Don't generate code for this class",
                      'sash_gravity':"0.0: only the bottom/right window is automatically resized\n"
                                     "0.5: both windows grow by equal size\n"
                                     "1.0: only left/top window grows"}
    CHILDREN = 2
    def __init__(self, name, parent, orientation, pos, create_slots=True):
        ManagedBase.__init__(self, name, 'wxSplitterWindow', parent, pos)
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
        self._window_old = None

    def _get_label(self, pos):
        if self.orientation=="wxSPLIT_VERTICAL":
            return ("Left","Right")[pos]
        return ("Top","Bottom")[pos]
    
    def _get_slot_label(self, pos):
        return "SLOT %s"%self._get_label(pos)

    #def create_widget(self):
        #if not self.parent.IS_SIZER:
            #size = self.parent.widget.GetClientSize()
            #self.widget = wx.SplitterWindow(self.parent_window.widget, self.id, size=size, style=self.style)
        #else:
            #self.widget = wx.SplitterWindow(self.parent_window.widget, self.id, style=self.style)
        #self.split()

    def create_widget(self):
        size = self._get_default_or_client_size()
        self.widget = wx.SplitterWindow(self.parent_window.widget, self.id, size=size, style=self.style)
        self.split()

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

        sash_pos_p = self.properties['sash_pos']
        if sash_pos_p.is_active():
            self.widget.SetSashPosition(sash_pos_p.get())
        else:
            sash_pos_p.set(self.widget.GetSashPosition())

        sash_gravity_p = self.properties['sash_gravity']
        if sash_gravity_p.is_active():
            self.widget.SetSashPosition(sash_gravity_p.get())

        min_pane_size_p = self.properties['min_pane_size']
        if min_pane_size_p.is_active():
            self.widget.SetMinimumPaneSize( min_pane_size_p.get() )
        else:
            min_pane_size_p.set( self.widget.GetMinimumPaneSize() )

        self.widget.Bind(wx.EVT_SPLITTER_SASH_POS_CHANGED, self.on_sash_pos_changed )

        if self.widget.GetTopLevelParent().IsShown():
            # e.g. when pasting into an existing window
            wx.CallAfter(self.widget.UpdateSize)

    #def _get_client_size(self, pos):
        ## returns the available size for a child

        #width, height = self.widget.GetClientSize()
        #sash_position = self.widget.GetSashPosition()
        #sash_size = self.widget.GetSashSize()
        #if self.widget.GetSplitMode()==wx.SPLIT_VERTICAL:
            ## side by side
            #if pos==0:
                #width = sash_position
            #else:
                #width = width - sash_position - sash_size
        #else:
            ## top to bottom
            #if pos==0:
                #height = sash_position
            #else:
                #width = height - sash_position - sash_size
        #print("_get_client_size", (width, height))
        #return (width, height)

    def on_set_focus(self, event):
        misc.set_focused_widget(self)
        # here we must call event.Skip() also on Win32 as this we should be able to move the sash
        event.Skip()

    def split(self):
        if not self.widget or not self.children[0] or not self.children[1]: return
        self.children[0].create()
        self.children[1].create()
        
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

    def properties_changed(self, modified):
        if (not modified or "sash_pos" in modified) and self.widget and self.check_prop("sash_pos"):
            self.widget.SetSashPosition(self.sash_pos)
        if (not modified or "sash_gravity" in modified) and self.widget and self.check_prop("sash_gravity"):
            self.widget.SetSashGravity(self.sash_gravity)
        if (not modified or "min_pane_size" in modified) and self.widget and self.check_prop("min_pane_size"):
            self.widget.SetMinimumPaneSize(self.min_pane_size)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)

        if modified and "orientation" in modified:
            # update horizontal/vertical icons
            common.app_tree.refresh(self, refresh_label=False, refresh_image=True)
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

    def _get_parent_tooltip(self, pos):
        return "%s splitter pane:"%self._get_label(pos)

    ####################################################################################################################
    # methods moved from SplitterWindowSizer:
    def add_item(self, child, pos=None):
        if pos is not None and self.widget: self._window_old = self.children[pos]
        ManagedBase.add_item(self, child, pos)
        self._add_slots(pos_max=pos)

    def _free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if self.widget and self.children[pos] and self.children[pos].widget:
            self.widget.Unsplit(self.children[pos].widget)
        old_child = self.children[pos]
        slot = Slot(self, pos)
        self.split()
        return slot

    def item_properties_modified(self, widget, modified=None, force_layout=True):
        "Updates the layout of the item"
        if self.widget and self._window_old:
            # a child was replaced
            if self._window_old.widget:
                self.widget.Unsplit(self._window_old.widget)
            elif self.widget.IsSplit(): # the child widget may have been delete meanwhile by tree remove_rec
                self.widget.Unsplit()
        self._window_old = None
        if self.children[0] and self.children[1]:
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


def builder(parent, pos):
    "Factory function for EditSplitterWindow objects"
    dialog = wcodegen.WidgetStyleSelectionDialog( dlg_title, box_title, choices, ["Create panels"],[True])
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    orientation = dialog.get_selection().split(" ")[0]
    create_panels = dialog.get_options()[0]
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = common.root.get_next_name('window_%d', parent)
    with parent.frozen():
        editor = EditSplitterWindow(name, parent, orientation, pos)
        editor.properties["style"].set_to_default()
        if create_panels:
            pane1 = EditPanel(name + '_pane_1', editor, 0)
            pane2 = EditPanel(name + '_pane_2', editor, 1)

        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")

        if parent.widget: editor.create()

        return editor


def xml_builder(attrs, parent, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditSplitterWindow(name, parent, 'wxSPLIT_VERTICAL', pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSplitterWindow'] = EditSplitterWindow
    common.widgets['EditSplitterWindow'] = builder
    common.widgets_from_xml['EditSplitterWindow'] = xml_builder

    import os.path
    from tree import WidgetTree
    WidgetTree.images['EditSplitterSlot-Left']   = os.path.join( config.icons_path, 'splitter_slot-left.xpm' )
    WidgetTree.images['EditSplitterSlot-Right']  = os.path.join( config.icons_path, 'splitter_slot-right.xpm' )
    WidgetTree.images['EditSplitterSlot-Top']    = os.path.join( config.icons_path, 'splitter_slot-top.xpm' )
    WidgetTree.images['EditSplitterSlot-Bottom'] = os.path.join( config.icons_path, 'splitter_slot-bottom.xpm' )
    WidgetTree.images['EditSplitterWindow']      = os.path.join( config.icons_path, 'splitter_window.xpm' )
    WidgetTree.images['EditSplitterWindow-h']    = os.path.join( config.icons_path, 'splitter_window-h.xpm' )

    return common.make_object_button('EditSplitterWindow', 'splitter_window.xpm')
