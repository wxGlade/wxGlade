"""
wxSplitterWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, compat, config, misc
import wcodegen
from tree import Node, SlotNode, WidgetTree
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot
from panel import EditPanel


class ChildWidgetNameProperty(np.Property):
    def __init__(self, child_att_name):
        self.child_att_name = child_att_name
        np.Property.__init__(self, None, default_value=None, name=None)

    def get(self):
        child = getattr(self.owner, self.child_att_name)
        if child is None: return self.value
        if isinstance(child, SizerSlot): return None
        return child.name

    def write(self, output, tabs=0):
        value = self.get()
        if value is not None:
            output.extend( common.format_xml_tag(self.name, value, tabs) )



class SplitterWindowSizer(Sizer):
    "'Virtual sizer' responsible for the management of a SplitterWindow"
    PROPERTIES = []
    def item_properties_modified(self, widget, modified=None, force_layout=True):
        "Updates the layout of the item"
        if self.window.widget and self.window.window_old:
            if self.window.window_old.widget:
                self.window.widget.Unsplit(self.window.window_old.widget)
            elif self.window.widget.IsSplit(): # the child widget may have been delete meanwhile by tree remove_rec
                self.window.widget.Unsplit()
        self.window.window_old = None
        if self.window._window_1 and self.window._window_2:
            self.window.split()

    def set_item_best_size(self, widget, size=None, force_layout=True):
        pass

    def add_item(self, item, pos=None, proportion=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self.window"
        if pos == 1:
            self.window.window_old = self.window._window_1
            self.window._window_1 = item
            self.window.properties["window_1"].set(item.name)
        else:
            self.window.window_old = self.window._window_2
            self.window._window_2 = item
            self.window.properties["window_2"].set(item.name)

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if self.window.orientation=="wxSPLIT_VERTICAL":
            labels = ("SLOT Left","SLOT Right")
        else:
            labels = ("SLOT Top","SLOT Bottom")
        if pos == 1:
            if self.window.widget and self.window._window_1 and self.window._window_1.widget:
                self.window.widget.Unsplit(self.window._window_1.widget)
            old_node = self.window._window_1.node
            slot = SizerSlot(self.window, self, pos, labels[0]) # XXX no node, no tree visualization?
            self.window._window_1 = slot
            w = self.window._window_1
        else:
            if self.window.widget and self.window._window_2 and self.window._window_2.widget:
                self.window.widget.Unsplit(self.window._window_1.widget)
            old_node = self.window._window_2.node
            slot = SizerSlot(self.window, self, pos, labels[1]) # XXX no node, no tree visualization?
            self.window._window_2 = slot
            w = self.window._window_2
        w.node = node = SlotNode(w)
        common.app_tree.change_node( old_node, w, node )
        self.window.split()
        return slot

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        name= attrs.get("original_name", None)
        if name is None: name = attrs['name']
        if name==self.window.properties["window_1"].value:
            return 1
        if name==self.window.properties["window_2"].value:
            return 2
        return None

    def is_virtual(self):
        return True

    def is_fixed(self):
        "exactly two slots"
        return True



class EditSplitterWindow(ManagedBase, EditStylesMixin):
    "Class to handle wxSplitterWindow objects; orientation: Orientation of the widget as string e.g. 'wxSPLIT_VERTICAL'"

    _custom_base_classes = True

    _PROPERTIES = ["Widget", "no_custom_class", "style", "sash_pos", "sash_gravity", "min_pane_size"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase._EXTRA_PROPERTIES
    _PROPERTY_LABELS = {'no_custom_class':"Don't generate code for this class",
                        'sash_pos':"Sash position"}
    _PROPERTY_HELP = {'no_custom_class':"Don't generate code for this class",
                      'sash_gravity':"0.0: only the bottom/right window is automatically resized\n"
                                     "0.5: both windows grow by equal size\n"
                                     "1.0: only left/top window grows"}

    def __init__(self, name, parent, id, win_1, win_2, orientation, sizer, pos):
        ManagedBase.__init__(self, name, 'wxSplitterWindow', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)
        self.sash_pos = np.SpinPropertyD(0, default_value="")
        if hasattr(wx, "SpinCtrlDouble"):
            self.sash_gravity = np.SpinDoublePropertyD(0.5, (0.0,1.0), default_value=0.0, immediate=True)
        else:
            self.sash_gravity = np.FloatPropertyD(0.5, (0.0,1.0), default_value=0.0)
        self.min_pane_size = np.SpinProperty(20)

        # hidden properties: orientation string, window_1, window_2
        self.orientation = np.Property(orientation)
        self.window_1 = ChildWidgetNameProperty("_window_1")
        self.window_2 = ChildWidgetNameProperty("_window_2")

        self.virtual_sizer = SplitterWindowSizer(self)
        labels = ("SLOT Left","SLOT Right") if orientation=="wxSPLIT_VERTICAL" else ("SLOT Top","SLOT Bottom")
        self._window_1 = win_1 or SizerSlot(self, self.virtual_sizer, 1, label=labels[0])
        self._window_2 = win_2 or SizerSlot(self, self.virtual_sizer, 2, label=labels[1])

    def create_widget(self):
        self.widget = wx.SplitterWindow(self.parent.widget, self.id, style=self.style)
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
            min_pane_size.set_value( self.widget.GetMinimumPaneSize() )

        self.widget.Bind(wx.EVT_SPLITTER_SASH_POS_CHANGED, self.on_sash_pos_changed )
        if self._window_1 and self._window_1.widget:
            if self.orientation=="wxSPLIT_VERTICAL":
                compat.SetToolTip(self._window_1.widget, _("Left splitter pane:\nAdd a sizer here") )
            else:
                compat.SetToolTip(self._window_1.widget, _("Top splitter pane:\nAdd a sizer here") )
        if self._window_2 and self._window_2.widget:
            if self.orientation=="wxSPLIT_VERTICAL":
                compat.SetToolTip(self._window_2.widget, _("Right splitter pane:\nAdd a sizer here") )
            else:
                compat.SetToolTip(self._window_2.widget, _("Bottom splitter pane:\nAdd a sizer here") )

    def on_set_focus(self, event):
        misc.set_focused_widget(self)
        # here we must call event.Skip() also on Win32 as this we should be able to move the sash
        event.Skip()

    def split(self):
        if not self.widget or not self._window_1 or not self._window_2: return
        self._window_1.create()
        self._window_2.create()
        
        orientation = self.orientation
        sash_pos_p = self.properties['sash_pos']
        if sash_pos_p.is_active():
            sash_pos = sash_pos_p.get()
        else:
            max_pos = self.widget.GetClientSize() [0 if orientation=='wxSPLIT_VERTICAL' else 1]
            sash_pos = max_pos // 2
        if orientation == 'wxSPLIT_VERTICAL':
            self.widget.SplitVertically  (self._window_1.widget, self._window_2.widget, sash_pos)
        else:
            self.widget.SplitHorizontally(self._window_1.widget, self._window_2.widget, sash_pos)

        if hasattr(self._window_1, 'sel_marker'): self._window_1.sel_marker.update()
        if hasattr(self._window_2, 'sel_marker'): self._window_2.sel_marker.update()

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
            labels = ("SLOT Left","SLOT Right") if self.orientation=="wxSPLIT_VERTICAL" else ("SLOT Top","SLOT Bottom")
            common.app_tree.refresh(self.node, refresh_label=False, refresh_image=True)
            if isinstance(self._window_1, SizerSlot):
                self._window_1.label = labels[0]
                common.app_tree.refresh(self._window_1.node)
            if isinstance(self._window_2, SizerSlot):
                self._window_2.label = labels[1]
                common.app_tree.refresh(self._window_2.node)

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

    def on_sash_pos_changed(self, event):
        self.properties['sash_pos'].set( self.widget.GetSashPosition() )
        event.Skip()

    def on_mouse_events(self, event):
        # resize instead of drag & drop
        event.Skip()

    def check_compatibility(self, widget, typename=None, report=False):
        return (False,"No objects can be pasted here; paste to empty slots instead.")


editor_class = EditSplitterWindow
editor_name = 'EditSplitterWindow'
editor_style = 'wxSPLIT_VERTICAL'

dlg_title = _('wxSplitterWindow')
box_title = _('Orientation')
choices = 'wxSPLIT_VERTICAL (left/right)|wxSPLIT_HORIZONTAL (top/bottom)'
tmpl_label = 'window'


def builder(parent, sizer, pos, number=[1]):
    "Factory function for EditSplitterWindow objects"
    dialog = wcodegen.WidgetStyleSelectionDialog( dlg_title, box_title, choices, ["Create panels"],[True])
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    orientation = dialog.get_selection().split(" ")[0]
    create_panels = dialog.get_options()[0]
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    label = '%s_%d' % (tmpl_label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (tmpl_label, number[0])

    with parent.frozen():
        widget = editor_class(label, parent, -1, None, None, orientation, sizer, pos)
        widget.properties["style"].set_to_default()
        if create_panels:
            widget._window_1 = pane1 = EditPanel(label + '_pane_1', widget, wx.NewId(), widget.virtual_sizer, 1)
            widget._window_2 = pane2 = EditPanel(label + '_pane_2', widget, wx.NewId(), widget.virtual_sizer, 2)
    
        node = Node(widget)
        widget.node = node
        widget.virtual_sizer.node = node
    
        widget.properties["proportion"].set(1)
        widget.properties["flag"].set("wxEXPAND")
    
        common.app_tree.insert(node, sizer.node, pos-1)

        if create_panels:
            node2 = Node(widget._window_1)
            node3 = Node(widget._window_2)
        else:
            node2 = SlotNode(widget._window_1)
            node3 = SlotNode(widget._window_2)
        widget._window_1.node = node2
        common.app_tree.add(node2, widget.node)
        widget._window_2.node = node3
        common.app_tree.add(node3, widget.node)

    
        if parent.widget: widget.create()
        #sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.NewId(), None, None, editor_style, sizer, pos)
    #sizer.set_item(widget.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node

    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)

    node2 = SlotNode(widget._window_1)
    widget._window_1.node = node2
    common.app_tree.add(node2, widget.node)

    node3 = SlotNode(widget._window_2)
    widget._window_2.node = node3
    common.app_tree.add(node3, widget.node)

    return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder

    import os.path
    WidgetTree.images['EditSplitterSlot-Left']   = os.path.join( config.icons_path, 'splitter_slot-left.xpm' )
    WidgetTree.images['EditSplitterSlot-Right']  = os.path.join( config.icons_path, 'splitter_slot-right.xpm' )
    WidgetTree.images['EditSplitterSlot-Top']    = os.path.join( config.icons_path, 'splitter_slot-top.xpm' )
    WidgetTree.images['EditSplitterSlot-Bottom'] = os.path.join( config.icons_path, 'splitter_slot-bottom.xpm' )
    WidgetTree.images['EditSplitterWindow']    = os.path.join( config.icons_path, 'splitter_window.xpm' )
    WidgetTree.images['EditSplitterWindow-h']    = os.path.join( config.icons_path, 'splitter_window-h.xpm' )

    return common.make_object_button(editor_name, 'splitter_window.xpm')
