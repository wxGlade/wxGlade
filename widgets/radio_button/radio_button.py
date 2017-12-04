"""\
wxRadioButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, config
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np
from misc import wxGladeRadioButton


class EditRadioButton(ManagedBase, EditStylesMixin):
    "Class to handle wxRadioButton objects"
    update_widget_style = False

    _PROPERTIES = ["Widget", "label", "clicked", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, label, sizer, pos):
        ManagedBase.__init__(self, name, 'wxRadioButton', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label   = np.TextProperty("", multiline="grow")
        self.clicked = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wxGladeRadioButton(self.parent.widget, self.id, self.label)
        self.widget.SetValue(self.clicked)
        self.widget.Bind(wx.EVT_CHECKBOX, lambda e: self.widget.SetValue(self.value))

    def _set_label(self):
        if not self.widget: return
        self.widget.SetLabel(self.label)
        if not self.properties['size'].is_active():  # XXX changed this: '-1, -1' is identical to not active
            self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())

    def properties_changed(self, modified):
        resize = False

        if not modified or "label" in modified:
            self._set_label()
            common.app_tree.refresh(self.node, refresh_label=True)

        if not modified or "clicked" in modified and self.widget:
            self.widget.SetValue(self.clicked)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditRadioButton objects"
    label = u'radio_btn_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'radio_btn_%d' % number[0]
    with parent.frozen():
        radio = EditRadioButton(label, parent, wx.NewId(), label, sizer, pos)
        radio.properties["style"].set_to_default()
        radio.check_defaults()
        node = Node(radio)
        radio.node = node
        if parent.widget: radio.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditRadioButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    radio = EditRadioButton(label, parent, wx.NewId(), "", sizer, pos)
    #sizer.set_item(radio.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(radio)
    radio.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return radio


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder

    return common.make_object_button('EditRadioButton', 'radio_button.xpm')
