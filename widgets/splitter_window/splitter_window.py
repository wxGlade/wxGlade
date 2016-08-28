"""
wxSplitterWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common
import compat
import wcodegen
from tree import Tree, Node
import new_properties as np
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot

#try:
    #from panel import EditPanel
    #_has_panel = True
#except ImportError:
    #_has_panel = False


class SplitterWindowSizer(Sizer):
    "'Virtual sizer' responsible for the management of a SplitterWindow"

    def set_item(self, pos, proportion=None, flag=None, border=None, size=None, force_layout=True):
        "Updates the layout of the item at the given pos"
        #self._logger.debug('set_item()')
        if self.window.widget and self.window.window_old and self.window.window_old.widget:
            self.window.widget.Unsplit(self.window.window_old.widget)
            self.window.window_old = None
        if self.window._window_1 and self.window._window_2:
            self.window.split()

    def add_item(self, item, pos=None, proportion=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self.window"
        #self._logger.debug('add_item(): %s', item.name)
        if pos == 1:
            self.window.window_old = self.window._window_1
            self.window._window_1 = item
            self.window.properties['window_1'].set(item.name)
        else:
            self.window.window_old = self.window._window_2
            self.window._window_2 = item
            self.window.properties['window_2'].set(item.name)

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        if pos == 1:
            if self.window.widget and self.window._window_1 and self.window._window_1.widget:
                self.window.widget.Unsplit(self.window.window_1.widget)
            self.window.window_1 = SizerSlot(self.window, self, pos) # XXX no node, no tree visualization?
            w = self.window._window_1
        else:
            if self.window.widget and self.window._window_2 and self.window._window_2.widget:
                self.window.widget.Unsplit()
            self.window._window_2 = SizerSlot(self.window, self, pos) # XXX no node, no tree visualization?
            w = self.window._window_2
        self.window.split()
        w.widget.SetFocus()

    def get_itempos(self, attrs):
        "Get position of sizer item (used in xml_parse)"
        #window_1_p = self.window.properties['window_1']
        #if hasattr(window_1_p, 'value') and attrs['name']==window_1_p.value:
        if attrs['name']==self.window.window_1:
            pos = 1
        else:
            pos = 2
        return pos

    def is_virtual(self):
        return True



class EditSplitterWindow(ManagedBase, EditStylesMixin):
    "Class to handle wxSplitterWindow objects; orientation: Orientation of the widget as string e.g. 'wxSPLIT_VERTICAL'"

    _custom_base_classes = True

    _PROPERTIES = ["Widget", "no_custom_class", "style", "sash_pos", "min_pane_size"]
    _PROPERTY_LABELS = {'no_custom_class':"Don't generate code for this class",
                        'sash_pos':"Sash position"}

    def __init__(self, name, parent, id, style, win_1, win_2, orientation, sizer, pos, show=True):
        ManagedBase.__init__(self, name, 'wxSplitterWindow', parent, id, sizer, pos, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)
        self.sash_pos = np.SpinPropertyD(0, default_value=0)
        self.min_pane_size = np.SpinProperty(20)

        # hidden properties: orientation string, window_1, window_2
        self.orientation = np.Property(orientation)
        self.window_1 = np.Property(win_1 and win_1.name or "")  # these string properties hold the window names
        self.window_2 = np.Property(win_2 and win_2.name or "")  # or empty strings for slots
        
        self.virtual_sizer = SplitterWindowSizer(self)
        self._window_1 = win_1 or SizerSlot(self, self.virtual_sizer, 1) # XXX no node?
        self._window_2 = win_2 or SizerSlot(self, self.virtual_sizer, 2) # XXX no node?

        if style: self.properties["style"].set(style)

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

        min_pane_size_p = self.properties['min_pane_size']
        if min_pane_size_p.is_active():
            self.widget.SetMinimumPaneSize( min_pane_size_p.get() )
        else:
            min_pane_size.set_value( self.widget.GetMinimumPaneSize() )

        wx.EVT_SPLITTER_SASH_POS_CHANGED( self.widget, self.widget.GetId(), self.on_sash_pos_changed )

    def on_set_focus(self, event):
        self.show_properties()
        # here we must call event.Skip() also on Win32 as this we should be able to move the sash
        event.Skip()

    def split(self):
        if not self.widget or not self._window_1 or not self._window_2: return
        self._window_1.show_widget(True)
        self._window_2.show_widget(True)
        
        orientation = self.orientation
        sash_pos_p = self.properties['sash_pos']
        if sash_pos_p.is_active():
            sash_pos = sash_pos_p.get()
        else:
            max_pos = self.widget.GetClientSize() [0 if orientation=='wxSPLIT_VERTICAL' else 0]
            sash_pos = max_pos // 2
        if orientation == 'wxSPLIT_VERTICAL':
            self.widget.SplitVertically  (self._window_1.widget, self._window_2.widget, sash_pos)
        else:
            self.widget.SplitHorizontally(self._window_1.widget, self._window_2.widget, sash_pos)

        if hasattr(self._window_1, 'sel_marker'): self._window_1.sel_marker.update()
        if hasattr(self._window_2, 'sel_marker'): self._window_2.sel_marker.update()

    def properties_changed(self, modified):
        if not modified or "sash_pos" in modified and self.widget:
            self.widget.SetSashPosition(self.sash_pos)
        if not modified or "min_pane_size" in modified and self.widget:
            self.widget.SetMinimumPaneSize(self.min_pane_size)

        ManagedBase.properties_changed(self, modified)

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
                self.sash_pos = self.widget.GetSashPosition()
                self.properties['sash_pos'].set_value(self.sash_pos)
        except (AttributeError, KeyError):
            pass
        ManagedBase.on_size(self, event)

    def on_sash_pos_changed(self, event):
        self.sash_pos = self.widget.GetSashPosition()
        self.properties['sash_pos'].set_value(self.sash_pos)
        event.Skip()


editor_class = EditSplitterWindow
editor_icon = 'splitter_window.xpm'
editor_name = 'EditSplitterWindow'
editor_style = 'wxSPLIT_VERTICAL'

dlg_title = _('wxSplitterWindow')
box_title = _('Orientation')
choices = 'wxSPLIT_VERTICAL|wxSPLIT_HORIZONTAL'
tmpl_label = 'window'


def builder(parent, sizer, pos, number=[1]):
    "Factory function for EditSplitterWindow objects"
    dialog = wcodegen.WidgetStyleSelectionDialog( dlg_title, box_title, choices)
    res = dialog.ShowModal()
    orientation = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    label = '%s_%d' % (tmpl_label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (tmpl_label, number[0])

    pane1 = EditPanel(label + '_pane_1', widget, wx.NewId(), widget.virtual_sizer, 1)
    pane2 = EditPanel(label + '_pane_2', widget, wx.NewId(), widget.virtual_sizer, 2)
    widget = editor_class(label, parent, -1, None, panel1, panel2, orientation, sizer, pos, show=False)

    node = Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node

    widget.properties["proportion"].set(1)
    widget.properties["flag"].set("wxEXPAND")
    widget.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos-1)

    node2 = Node(widget._window_1)
    widget._window_1.node = node2
    common.app_tree.add(node2, widget.node)

    node3 = Node(widget._window_2)
    widget._window_2.node = node3
    common.app_tree.add(node3, widget.node)

    sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.NewId(), None, None, None, editor_style, sizer, pos)
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
