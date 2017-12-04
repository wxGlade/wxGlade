"""\
wxCheckBox objects

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


class EditCheckBox(ManagedBase, EditStylesMixin):
    "Class to handle wxCheckBox objects"

    _PROPERTIES = ["Widget", "label", "checked", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_LABELS = {"checked":"wxCheckBox state"}

    # Convert the position of "checked" RadioProperty to wxCheckBoxState
    index2state = { 0: wx.CHK_UNCHECKED, 1: wx.CHK_CHECKED, 2: wx.CHK_UNDETERMINED }

    def __init__(self, name, parent, id, label, sizer, pos):
        "Class to handle wxCheckBox objects"
        ManagedBase.__init__(self, name, 'wxCheckBox', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label = np.TextProperty("", multiline="grow")

        # value: Checkbox state (0 = unchecked, 1 = checked, 2 = undetermined)
        values = [0,1,2]
        labels = [_('Unchecked'), _('Checked'), _('Undetermined')]
        self.value = np.IntRadioProperty(0, values, labels, columns=3, default_value=0, name="checked") # rename to value?

    def create_widget(self):
        self.widget = wx.CheckBox(self.parent.widget, self.id, self.label)
        self.widget.SetValue(self.value)
        def on_checkbox(event):
            value = 1 if event.IsChecked() else 0
            self.properties["checked"].set(value)
        self.widget.Bind(wx.EVT_CHECKBOX, on_checkbox, id=self.id)

    def properties_changed(self, modified):
        resize = False

        if not modified or "style" in modified:
            checked_p = self.properties['checked']
            if 'wxCHK_3STATE' in self.properties["style"].value_set:
                checked_p.enable_item(2, True)
            else:
                checked_p.enable_item(2, False)
                if checked_p.value == 2:
                    checked_p.set(0)

        if not modified or "label" in modified:
            if self.widget:
                self.widget.SetLabel(self.label)
                resize = True
            common.app_tree.refresh(self.node, refresh_label=True)

        if not modified or "checked" in modified:
            if self.widget:
                if self.widget.Is3State():
                    self.widget.Set3StateValue(self.index2state[self.value])
                else:
                    self.widget.SetValue(self.value)

        if resize: self._set_widget_best_size()

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)




def builder(parent, sizer, pos, number=[1]):
    "factory function for EditCheckBox objects"
    label = 'checkbox_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'checkbox_%d' % number[0]
    with parent.frozen():
        checkbox = EditCheckBox(label, parent, wx.NewId(), label, sizer, pos)
        checkbox.properties["style"].set_to_default()
        checkbox.check_defaults()
        node = Node(checkbox)
        checkbox.node = node
        if parent.widget: checkbox.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditCheckBox objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    checkbox = EditCheckBox( label, parent, wx.NewId(), "", sizer, pos)
    #sizer.set_item(checkbox.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(checkbox)
    checkbox.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return checkbox


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditCheckBox'] = builder
    common.widgets_from_xml['EditCheckBox'] = xml_builder

    return common.make_object_button('EditCheckBox', 'checkbox.xpm')
