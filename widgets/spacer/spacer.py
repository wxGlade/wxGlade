"""\
Spacers to use in sizers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
from tree import Node
import new_properties as np
from edit_windows import ManagedBase


class EditSpacer(ManagedBase):
    "Class to handle spacers for sizers"
    _PROPERTIES = ["Layout", "width", "height", "pos", "proportion", "border", "flag"]
    PROPERTIES = _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, width, height, sizer, pos):
        ManagedBase.__init__(self, name, 'spacer', parent, id, sizer, pos)

        # initialise instance properties
        self.width  = np.SpinProperty(width,  immediate=True)
        self.height = np.SpinProperty(height, immediate=True)

    def create_widget(self):
        style = wx.SIMPLE_BORDER | wx.FULL_REPAINT_ON_RESIZE
        self.widget = wx.Window(self.parent.widget, self.id, size=(self.width, self.height), style=style)
        self.widget.GetBestSize = self.widget.GetSize
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)

    def on_paint(self, event):
        dc = wx.PaintDC(self.widget)
        brush = wx.TheBrushList.FindOrCreateBrush( self.widget.GetBackgroundColour() )
        dc.SetBrush(brush)
        dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 1, wx.SOLID))
        dc.SetBackground(brush)
        dc.Clear()
        w, h = self.widget.GetClientSize()
        dc.DrawLine(0, 0, w, h)
        dc.DrawLine(w, 0, 0, h)
        text = _('Spacer')
        tw, th = dc.GetTextExtent(text)
        x = (w - tw) // 2
        y = (h - th) // 2
        dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 0, wx.TRANSPARENT))
        dc.DrawRectangle(x-1, y-1, tw+2, th+2)
        dc.DrawText(text, x, y)

    def properties_changed(self, modified):
        if not modified or "width" in modified or "height" in modified:
            size = (self.width, self.height)
            if self.widget: self.widget.SetSize(size)
            self.sizer.set_item_best_size(self, size=size)
        ManagedBase.properties_changed(self, modified)


class _Dialog(wx.Dialog):
    def __init__(self, parent):
        pos = wx.GetMousePosition()
        wx.Dialog.__init__(self, misc.get_toplevel_parent(parent), -1, _("Enter size"), pos)
        # the controls
        self.width  = wx.SpinCtrl(self, -1, "20")
        self.height = wx.SpinCtrl(self, -1, "20")
        self.width.SetFocus()
        self.width.SetSelection(-1, -1)
        self.height.SetSelection(-1, -1)
        # the main sizer
        sizer = wx.BoxSizer(wx.VERTICAL)
        # grid sizer with the controls
        gsizer = wx.FlexGridSizer(cols=2)
        for label, control in [("Width", self.width), ("Height", self.height)]:
            gsizer.Add(wx.StaticText(self, -1, _(label)), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
            gsizer.Add(control, 0, wx.ALL | wx.EXPAND | wx.ALIGN_CENTER_VERTICAL, 3)
        sizer.Add(gsizer)
        # horizontal sizer for action buttons
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add( wx.Button(self, wx.ID_CANCEL, _('Cancel')), 1, wx.ALL, 5)
        btn = wx.Button(self, wx.ID_OK, _('OK') )
        btn.SetDefault()
        hsizer.Add(btn, 1, wx.ALL, 5)
        sizer.Add(hsizer, 0, wx.EXPAND|wx.ALIGN_CENTER )

        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)


def builder(parent, sizer, pos):
    "factory function for EditSpacer objects"
    dialog = _Dialog(parent)
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    width  = dialog.width.GetValue()
    height = dialog.height.GetValue()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = 'spacer'
    with parent.frozen():
        spacer = EditSpacer( name, parent, wx.NewId(), width, height, sizer, pos )
        node = Node(spacer)
        spacer.node = node
        if parent.widget: spacer.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditSpacer objects from a XML file"
    from xml_parse import XmlParsingError
    if not sizer or not sizeritem:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    spacer = EditSpacer('spacer', parent, wx.NewId(), 1, 1, sizer, pos)
    #sizer.set_item(spacer.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(spacer)
    spacer.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return spacer


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditSpacer'] = builder
    common.widgets_from_xml['EditSpacer'] = xml_builder

    return common.make_object_button('EditSpacer', 'spacer.xpm')
