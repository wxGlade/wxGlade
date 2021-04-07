"""\
Spacers to use in sizers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
import new_properties as np
from edit_windows import ManagedBase


class EditSpacer(ManagedBase):
    "Class to handle spacers for sizers"
    WX_CLASS = 'spacer'
    IS_NAMED = False
    _PROPERTIES = ["Layout", "width", "height", "proportion", "border", "flag"]
    PROPERTIES = _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, parent, index, width, height):
        ManagedBase.__init__(self, 'spacer', parent, index)

        # initialise instance properties
        self.width  = np.SpinProperty(width,  immediate=True)
        self.height = np.SpinProperty(height, immediate=True)

    def create_widget(self):
        style = wx.SIMPLE_BORDER | wx.FULL_REPAINT_ON_RESIZE
        self.widget = wx.Window(self.parent_window.widget, wx.ID_ANY, size=(self.width, self.height), style=style)
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

    def _properties_changed(self, modified, actions):
        if not modified or "width" in modified or "height" in modified:
            size = (self.width, self.height)
            if self.widget: self.widget.SetSize(size)
            self.parent.set_item_best_size(self, size=size)
            actions.add("layout")
        ManagedBase._properties_changed(self, modified, actions)


class _Dialog(wx.Dialog):
    def __init__(self):
        wx.Dialog.__init__(self, common.main, -1, _("Enter size"), wx.GetMousePosition())
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
        sizer.Add(hsizer, 0, wx.EXPAND)

        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)

def builder(parent, index):
    "factory function for EditSpacer objects"
    dialog = _Dialog()
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    width  = dialog.width.GetValue()
    height = dialog.height.GetValue()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    with parent.frozen():
        editor = EditSpacer( parent, index, width, height )
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditSpacer objects from a XML file"
    return EditSpacer(parent, index, 1, 1)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditSpacer'] = EditSpacer
    common.widgets['EditSpacer'] = builder
    common.widgets_from_xml['EditSpacer'] = xml_builder

    return common.make_object_button('EditSpacer', 'spacer.png')
