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
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot

try:
    from panel import EditPanel
    _has_panel = True
except ImportError:
    _has_panel = False


class SplitterWindowSizer(Sizer):
    """\
    "Virtual sizer" responsible for the management of a SplitterWindow.
    """

    def set_item(self, pos, option=None, flag=None, border=None, size=None,
                 force_layout=True):
        """\
        Updates the layout of the item at the given pos.
        """
        #self._logger.debug('set_item()')
        if self.window.widget and \
                self.window.window_old and self.window.window_old.widget:
            self.window.widget.Unsplit(self.window.window_old.widget)
            self.window.window_old = None
        if self.window.window_1 and self.window.window_2:
            self.window.split()

    def add_item(self, item, pos=None, option=0, flag=0, border=0, size=None,
                 force_layout=True):
        """\
        Adds an item to self.window.
        """
        #self._logger.debug('add_item(): %s', item.name)
        if pos == 1:
            self.window.window_old = self.window.window_1
            self.window.window_1 = item
            self.window.properties['window_1'].set_value(item.name)
        else:
            self.window.window_old = self.window.window_2
            self.window.window_2 = item
            self.window.properties['window_2'].set_value(item.name)

    def free_slot(self, pos, force_layout=True):
        """\
        Replaces the element at pos with an empty slot
        """
        if pos == 1:
            if self.window.widget and \
                    self.window.window_1 and self.window.window_1.widget:
                self.window.widget.Unsplit(self.window.window_1.widget)
            self.window.window_1 = SizerSlot(self.window, self, pos)
            w = self.window.window_1
        else:
            if self.window.widget and \
                    self.window.window_2 and self.window.window_2.widget:
                self.window.widget.Unsplit()
            self.window.window_2 = SizerSlot(self.window, self, pos)
            w = self.window.window_2
        self.window.split()
        w.widget.SetFocus()

    def get_itempos(self, attrs):
        """\
        Get position of sizer item (used in xml_parse)
        """
        if hasattr(self.window.properties['window_1'], 'value') and \
                attrs['name'] == self.window.properties['window_1'].value:
            pos = 1
        else:
            pos = 2
        return pos

    def is_virtual(self):
        return True

# end of class SplitterWindowSizer


class EditSplitterWindow(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxSplitterWindow objects

    @ivar orientation: Orientation of the widget e.g. wxSPLIT_VERTICAL
    @type orientation: str | unicode
    """

    _custom_base_classes = True

    def __init__(self, name, parent, id, style, win_1, win_2, orientation,
                 sizer, pos, property_window, show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxSplitterWindow', parent, id,
                             sizer, pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.set_style(style)
        self.virtual_sizer = SplitterWindowSizer(self)
        self.window_1 = win_1
        self.window_2 = win_2
        self.orientation = orientation
        self.sash_pos = 0
        self.min_pane_size = 20

        # initialise properties remaining staff
        self.access_functions['min_pane_size'] = (self.get_min_pane_size,
                                                  self.set_min_pane_size)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['sash_pos'] = (self.get_sash_pos,
                                             self.set_sash_pos)
        prop = self.properties
        prop['style'] = CheckListProperty(self, 'style', self.widget_writer)
        self.access_functions['orientation'] = (self.get_orientation,
                                                self.set_orientation)
        prop['orientation'] = HiddenProperty(
            self, 'orientation', label=_("Orientation"))

        self.access_functions['window_1'] = (self.get_win_1, lambda v: None)
        self.access_functions['window_2'] = (self.get_win_2, lambda v: None)
        prop['window_1'] = HiddenProperty(self, 'window_1')
        prop['window_2'] = HiddenProperty(self, 'window_2')
        self.window_1 = SizerSlot(self, self.virtual_sizer, 1)
        self.window_2 = SizerSlot(self, self.virtual_sizer, 2)

        prop['min_pane_size'] = SpinProperty(self, 'min_pane_size',
                                             can_disable=True, r=(10, 2000),
                                             label=_('Minimum pane size'))

        prop['sash_pos'] = SpinProperty(self, 'sash_pos', r=(0, 400),
                                        can_disable=True,
                                        label=_("Sash position"))
        self.no_custom_class = False
        self.access_functions['no_custom_class'] = (self.get_no_custom_class,
                                                    self.set_no_custom_class)
        prop['no_custom_class'] = CheckBoxProperty(
            self, 'no_custom_class',
            label=_("Don't generate code for this class"))

    def create_widget(self):
        self.widget = wx.SplitterWindow(self.parent.widget, self.id,
                                        style=self.get_int_style())
        self.split()

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self,
                                           sel_marker_parent=self.widget)

        sp = self.properties['sash_pos']
        if sp.is_active():
            sp.set_value(self.sash_pos)
            self.widget.SetSashPosition(self.sash_pos)
        else:
            sp.set_value(self.widget.GetSashPosition())

        min_pane_size = self.properties['min_pane_size']
        if sp.is_active():
            min_pane_size.set_value(self.min_pane_size)
            self.widget.SetMinimumPaneSize(self.min_pane_size)
        else:
            min_pane_size.set_value(self.widget.GetMinimumPaneSize())

        wx.EVT_SPLITTER_SASH_POS_CHANGED(self.widget, self.widget.GetId(),
                                         self.on_sash_pos_changed)

    def on_set_focus(self, event):
        self.show_properties()
        # here we must call event.Skip() also on Win32 as this we should be
        # able to move the sash
        event.Skip()

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.properties['no_custom_class'].display(panel)
        self.properties['style'].display(panel)
        self.properties['sash_pos'].display(panel)
        self.properties['min_pane_size'].display(panel)
        sizer.Add(self.properties['no_custom_class'].panel, 0,
                  wx.ALL | wx.EXPAND, 3)
        sizer.Add(self.properties['style'].panel, 0, wx.EXPAND)
        sizer.Add(self.properties['sash_pos'].panel, 0, wx.EXPAND)
        sizer.Add(self.properties['min_pane_size'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def split(self):
        if not self.widget:
            return
        if self.window_1 and self.window_2:
            self.window_1.show_widget(True)
            self.window_2.show_widget(True)
            sp = self.properties['sash_pos'].get_value()
            if not self.properties['sash_pos'].is_active():
                if self.orientation == 'wxSPLIT_VERTICAL':
                    max_pos = self.widget.GetClientSize()[0]
                else:
                    max_pos = self.widget.GetClientSize()[1]
                sp = max_pos / 2
            if self.orientation == 'wxSPLIT_VERTICAL':
                self.widget.SplitVertically(self.window_1.widget,
                                            self.window_2.widget, sp)
            else:
                self.widget.SplitHorizontally(self.window_1.widget,
                                              self.window_2.widget, sp)
            for window in self.window_1, self.window_2:
                if hasattr(window, 'sel_marker'):
                    window.sel_marker.update()

    def get_sash_pos(self):
        return self.sash_pos

    def set_sash_pos(self, value):
        try:
            value = int(value)
        except ValueError:
            return
        self.sash_pos = value
        if self.widget:
            self.widget.SetSashPosition(value)

    def get_min_pane_size(self):
        return self.min_pane_size

    def set_min_pane_size(self, value):
        try:
            value = int(value)
        except ValueError:
            return
        self.min_pane_size = value
        if self.widget:
            self.widget.SetMinimumPaneSize(value)

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
                self.widget.SetSashPosition(max_pos / 2)
                self.sash_pos = self.widget.GetSashPosition()
                self.properties['sash_pos'].set_value(self.sash_pos)
        except (AttributeError, KeyError):
            pass
        ManagedBase.on_size(self, event)

    def on_sash_pos_changed(self, event):
        self.sash_pos = self.widget.GetSashPosition()
        self.properties['sash_pos'].set_value(self.sash_pos)
        event.Skip()

    def get_orientation(self):
        """\
        Return the attribute name of the selected orientation.

        @rtype: str | unicode
        """
        return self.orientation

    def set_orientation(self, value):
        """\
        Select a orientation

        @param value: Attribute name e.g. 'wxSPLIT_HORIZONTAL'
        @type value:  str | unicode
        """
        assert value in ['wxSPLIT_HORIZONTAL', 'wxSPLIT_VERTICAL']
        self.orientation = value

    def get_win_1(self):
        if not isinstance(self.window_1, SizerSlot):
            return self.window_1.name
        return ''

    def get_win_2(self):
        if not isinstance(self.window_2, SizerSlot):
            return self.window_2.name
        return ''

    def get_no_custom_class(self):
        return self.no_custom_class

    def set_no_custom_class(self, value):
        self.no_custom_class = bool(int(value))

# end of class EditSplitterWindow


editor_class = EditSplitterWindow
editor_icon = 'splitter_window.xpm'
editor_name = 'EditSplitterWindow'
editor_style = 'wxSPLIT_VERTICAL'

dlg_title = _('wxSplitterWindow')
box_title = _('Orientation')
choices = 'wxSPLIT_VERTICAL|wxSPLIT_HORIZONTAL'
tmpl_label = 'window'


def builder(parent, sizer, pos, number=[1]):
    """\
    Factory function for EditSplitterWindow objects.
    """
    dialog = wcodegen.WidgetStyleSelectionDialog(
            dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    label = '%s_%d' % (tmpl_label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (tmpl_label, number[0])
    widget = editor_class(label, parent, wx.NewId(), None, None, None, style,
                          sizer, pos, common.property_panel, show=False)
    if _has_panel:
        pane1 = EditPanel(label + '_pane_1', widget, wx.NewId(),
                          widget.virtual_sizer, 1, common.property_panel)
        pane2 = EditPanel(label + '_pane_2', widget, wx.NewId(),
                          widget.virtual_sizer, 2, common.property_panel)
        widget.window_1 = pane1
        widget.window_2 = pane2

    node = Tree.Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node

    widget.set_option(1)
    widget.set_style("wxEXPAND")
    widget.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos - 1)

    if _has_panel:
        node2 = Tree.Node(widget.window_1)
        widget.window_1.node = node2
        common.app_tree.add(node2, widget.node)

        node3 = Tree.Node(widget.window_2)
        widget.window_2.node = node3
        common.app_tree.add(node3, widget.node)

    sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    Factory to build editor objects from a XML file.
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.NewId(), None, None, None,
                          editor_style, sizer, pos, common.property_panel)
    sizer.set_item(widget.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(widget)
    widget.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return widget


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
