"""\
wxCheckBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import config
import misc
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class RadioPropertyNumericValue(RadioProperty):
    """\
    Class derived from L{widget_properties.RadioProperty} to write the
    numeric value of this property instead of the string value.
    """

    def write(self, outfile, tabs=0):
        """\
        Write the numeric value of this property instead of the string value.

        The numeric value is compatible with older versions

        @see: L{widget_properties.RadioProperty.write()}
        """
        if self.is_active():
            value = self.get_value()
            if value:
                stmt = common.format_xml_tag(self.name, value, tabs)
                outfile.write(stmt)


class EditCheckBox(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxCheckBox objects

    @ivar label: Checkbox label
    @type label: str
    @ivar value: Checkbox state (0 = unchecked, 1 = checked,
                 2 = undetermined)
    @type value: int
    """

    index2state = {
        0: wx.CHK_UNCHECKED,
        1: wx.CHK_CHECKED,
        2: wx.CHK_UNDETERMINED,
        }
    """\
    Convert the position of "checked" RadioProperty to wxCheckBoxState
    """

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxCheckBox objects
        """
        ManagedBase.__init__(self, name, 'wxCheckBox', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.label = label
        self.value = 0

        # initialise properties remaining staff
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['checked'] = (self.get_value, self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)

        self.properties['label'] = TextProperty(
            self, 'label', multiline=True, label=_("label"))
        self.properties['checked'] = RadioPropertyNumericValue(
            self, "checked", None,
            [_('Unchecked'), _('Checked'), _('Undetermined')],
            columns=3, label=_("wxCheckBox state"))
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

    def _activate_elements(self):
        """\
        Activate / deactivate widget elements by re-setting styles and value
        """
        self.set_style(self.get_style())
        self.set_value(self.value)

    def create_widget(self):
        label = self.label.replace('\\n', '\n')
        self.widget = wx.CheckBox(self.parent.widget, self.id, label)
        self._activate_elements()

        def on_checkbox(event):
            self.set_value(self.value)
        wx.EVT_CHECKBOX(self.widget, self.id, on_checkbox)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        self.properties['label'].display(panel)
        self.properties['checked'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['checked'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        self._activate_elements()

    def get_label(self):
        return self.label

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            self.label = value
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

    def get_value(self):
        return self.value

    def set_value(self, value):
        self.value = int(value)
        if self.widget:
            if self.widget.Is3State():
                self.widget.Set3StateValue(self.index2state[self.value])
            else:
                self.widget.SetValue(self.value)
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

    def set_style(self, value):
        super(EditCheckBox, self).set_style(value)
        if self.widget:
            prop = self.properties['checked']
            if 'wxCHK_3STATE' in self.get_string_style():
                prop.enable_item(_('Undetermined'), True)
            else:
                if prop.get_str_value() == _('Undetermined'):
                    prop.set_str_value(_('Unchecked'))
                prop.enable_item(_('Undetermined'), False)

# end of class EditCheckBox


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditCheckBox objects.
    """
    label = 'checkbox_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'checkbox_%d' % number[0]
    checkbox = EditCheckBox(label, parent, wx.NewId(), label, sizer, pos,
                            common.property_panel)
    node = Tree.Node(checkbox)
    checkbox.node = node
    checkbox.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditCheckBox objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    checkbox = EditCheckBox(
        label, parent, wx.NewId(), "", sizer, pos,
        common.property_panel, show=False)
    sizer.set_item(checkbox.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(checkbox)
    checkbox.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return checkbox


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditCheckBox'] = builder
    common.widgets_from_xml['EditCheckBox'] = xml_builder

    return common.make_object_button('EditCheckBox', 'checkbox.xpm')
