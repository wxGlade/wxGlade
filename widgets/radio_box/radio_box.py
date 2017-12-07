"""\
wxRadioBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat
from edit_windows import ManagedBase
from tree import Node
import new_properties as np
from misc import wxGladeRadioButton

from ChoicesProperty import *



class EditRadioBox(ManagedBase):
    _PROPERTIES = ["Widget", "label", "style", "dimension", "selection", "choices"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, label, choices, major_dim, style, sizer, pos):
        "Class to handle wxRadioBox objects"
        ManagedBase.__init__(self, name, 'wxRadioBox', parent, id, sizer, pos)
        self.static_box = None
        
        # initialise instance properties
        self.label     = np.TextProperty("", multiline="grow")
        self.dimension = np.SpinProperty(major_dim)
        self.selection = np.SpinProperty(0, val_range=(0,len(choices)-1), immediate=True )
        self.choices   = ChoicesProperty( choices, [(_('Label'), np.GridProperty.STRING)] )
        style = style or wx.RA_SPECIFY_ROWS
        styles = [wx.RA_SPECIFY_ROWS, wx.RA_SPECIFY_COLS]
        aliases = ["wxRA_SPECIFY_ROWS","wxRA_SPECIFY_COLS"]  # labels and aliases
        self.style = np.RadioProperty(style, styles, aliases, aliases=aliases, columns=2)

        self.buttons = None  # list of wx.RadioButton

    # widget creation / updates ########################################################################################
    def create_widget(self):
        self.widget = wx.Panel(self.parent.widget, self.id)
        self.widget.GetBestSize = self.GetBestSize
        self.widget.SetForegroundColour = self.SetForegroundColour
        self.widget.SetBackgroundColour = self.SetBackgroundColour
        self.widget.SetFont = self.SetFont
        self._set_choices()  # calls also _do_layout()
        self._set_selection()

    def _create_button(self, label):
        r = wxGladeRadioButton(self.widget, -1, label)
        r.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        r.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        return r

    def _create_static_box(self):
        sb = wx.StaticBox(self.widget, -1, self.label)
        sb.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        sb.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        return sb

    def _do_layout(self):
        "Lays out the radio buttons according to the values of self.style and self.major_dim"
        if not self.widget:
            return
        buttons_layout = self.buttons
        if self.dimension:
            if self.style & wx.RA_SPECIFY_COLS:
                cols = self.dimension
                rows = 0
            else:
                cols = 0
                rows = self.dimension
            sizer = wx.GridSizer(rows, cols, 0, 0)
            if wx.Platform == '__WXGTK__':
                # we need to reorder self.buttons 'cos wxRadioBox lays out its
                # elements by colums, while wxGridSizer by rows
                import math
                if not rows:
                    step = int(math.ceil(1.0*len(self.buttons)/cols))
                else:
                    step = rows
                tmp = [[] for i in range(step)]
                for i, button in enumerate(self.buttons):
                    tmp[i%step].append(button)
                buttons_layout = []
                for t in tmp:
                    buttons_layout.extend(t)
        else:
            sizer = wx.BoxSizer(wx.VERTICAL)
        for button in buttons_layout:
            w, h = button.GetBestSize()
            sizer.Add(button, 0, wx.EXPAND)
            sizer.SetItemMinSize(button, w, h)
        self.widget.SetAutoLayout(True)
        sb_sizer = wx.StaticBoxSizer(self.static_box, wx.VERTICAL)
        self.widget.SetSizer(sb_sizer)
        sb_sizer.Add(sizer, 1, wx.EXPAND)
        sb_sizer.SetMinSize(sizer.GetMinSize())
        sb_sizer.Fit(self.widget)
        self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())

    def _set_label(self):
        if not self.widget or not self.static_box: return
        label = self.label
        self.static_box.SetLabel(label)
        if not self.properties['size'].is_active():
            self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())

    def _set_choices(self):
        if not self.widget: return
        for b in self.buttons or []:
            if b: compat.DestroyLater(b)
        self.static_box = self._create_static_box()
        self.buttons = [self._create_button(c) for (c,) in self.choices]
        self._do_layout()

    def _set_selection(self):
        if not self.widget: return
        selection = self.selection
        for i, b in enumerate(self.buttons):
            b.SetValue( i==selection )

    def GetBestSize(self):
        w, h = self.widget.GetSizer().GetMinSize()
        w2 = self.static_box.GetBestSize()[0]
        return max(w, w2), h

    def SetBackgroundColour(self, colour):
        wx.Panel.SetBackgroundColour(self.widget, colour)
        self.static_box.SetBackgroundColour(colour)
        for b in self.buttons:
            b.SetBackgroundColour(colour)
        self.widget.Refresh()

    def SetForegroundColour(self, colour):
        wx.Panel.SetForegroundColour(self.widget, colour)
        self.static_box.SetForegroundColour(colour)
        for b in self.buttons:
            b.SetForegroundColour(colour)
        self.widget.Refresh()

    def SetFont(self, font):
        wx.Panel.SetFont(self.widget, font)
        self.static_box.SetFont(font)
        for b in self.buttons:
            b.SetFont(font)
        self.widget.Refresh()

    ####################################################################################################################
    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def properties_changed(self, modified):
        # self.selection needs to be in range (0,len(self.choices)-1)
        choices = self.choices
        max_selection = len(choices)-1 if choices else 0
        set_selection = False
        if not modified or "choices" in modified:
            # adjust range of selection
            self.properties['selection'].set_range(0, max_selection)
            if self.selection>max_selection:
                set_selection = True

        if not modified or "selection" in modified or set_selection:
            if self.selection>max_selection:
                self.properties['selection'].set(max_selection)
            set_selection = True
        
        if not modified or "choices" in modified or "dimension" in modified:
            self._set_choices()  # does also update label
        elif not modified or "label" in modified:
            self._set_label()
            common.app_tree.refresh(self.node, refresh_label=True)

        if self.widget and set_selection:
            self._set_selection()

        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditRadioBox objects"
    label = u'radio_box_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'radio_box_%d' % number[0]
    with parent.frozen():
        radio_box = EditRadioBox(label, parent, wx.NewId(), label, [[u'choice 1'],], 1, 0, sizer, pos)
        node = Node(radio_box)
        radio_box.node = node
        if parent.widget: radio_box.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditRadioBox objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    radio_box = EditRadioBox(label, parent, wx.NewId(), '', [], 1, 0, sizer, pos)
    #size.set_item(radio_box.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(radio_box)
    radio_box.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return radio_box


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditRadioBox'] = builder
    common.widgets_from_xml['EditRadioBox'] = xml_builder

    return common.make_object_button('EditRadioBox', 'radio_box.xpm')
