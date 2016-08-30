"""\
Spacers to use in sizers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
from tree import Tree, Node
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
        size = (self.width, self.height)
        self.widget = wx.Window(self.parent.widget, self.id, size=size, style=wx.SIMPLE_BORDER)
        self.widget.GetBestSize = self.widget.GetSize
        wx.EVT_PAINT(self.widget, self.on_paint)

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
            self.sizer.set_item(self.pos, size=size)
        ManagedBase.properties_changed(self, modified)


class _Dialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, misc.get_toplevel_parent(parent), -1, _("Enter size"))

        import widget_properties as wp
        self.width  = wp.SpinProperty(self, 'width', self, label=_("width"))
        self.height = wp.SpinProperty(self, 'height', self, label=_("height"))
        self.width.set_value(20)
        self.width.spin.SetFocus()
        self.width.spin.SetSelection(-1, -1)
        self.height.set_value(20)

        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.width.panel, 0, wx.EXPAND)
        szr.Add(self.height.panel, 0, wx.EXPAND)
        sz = wx.BoxSizer(wx.HORIZONTAL)
        sz.Add(wx.Button(self, wx.ID_OK, _('OK')))
        szr.Add(sz, 0, wx.ALL|wx.ALIGN_CENTER, 4)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        self.CenterOnScreen()

    def __getitem__(self, name):
        return lambda : 0, lambda v: None



def builder(parent, sizer, pos):
    "factory function for EditSpacer objects"
    dialog = _Dialog(parent)
    res = dialog.ShowModal()
    width  = dialog.width.get_value()
    height = dialog.height.get_value()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = 'spacer'
    spacer = EditSpacer( name, parent, wx.NewId(), width, height, sizer, pos )
    node = Node(spacer)
    spacer.node = node
    spacer.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditSpacer objects from a XML file"
    from xml_parse import XmlParsingError
    if not sizer or not sizeritem:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    spacer = EditSpacer('spacer', parent, wx.NewId(), 1, 1, sizer, pos)
    sizer.set_item(spacer.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
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
