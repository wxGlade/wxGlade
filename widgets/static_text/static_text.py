"""\
wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import wx.lib.stattext
import common, config
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np


class EditStaticText(ManagedBase, EditStylesMixin):
    "Class to handle wxStaticText objects"
    _PROPERTIES = ["Widget", "label", "style", "attribute"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_HELP ={"attribute":'Store instance as attribute of window class; e.g. self.label_1 = wx.StaticText(...)\n'
                                 'Without this, you can not access the label from your program.'}

    def __init__(self, name, parent, id, label, sizer, pos):
        ManagedBase.__init__(self, name, 'wxStaticText', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label     = np.TextProperty(label, multiline=True)
        self.attribute = np.CheckBoxProperty(False, default_value=False)

        if config.preferences.default_border:
            self.properties["border"].set( config.preferences.default_border_size )
            self.properties["flag"].set( wx.ALL )

    def create_widget(self):
        self.widget = wx.lib.stattext.GenStaticText(self.parent.widget, self.id, self.label)

    def properties_changed(self, modified):
        if not modified or "label" in modified:
            if self.widget:
                self.widget.SetLabel(self.label)
                self._set_widget_best_size()

        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditStaticText objects"
    label = u'label_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'label_%d' % number[0]
    static_text = EditStaticText(label, parent, wx.NewId(), label, sizer, pos)
    node = Node(static_text)
    static_text.node = node
    if parent.widget: static_text.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditStaticText objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    static_text = EditStaticText(label, parent, wx.NewId(), "", sizer, pos)
    sizer.set_item(static_text.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(static_text)
    static_text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return static_text


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditStaticText'] = builder
    common.widgets_from_xml['EditStaticText'] = xml_builder

    return common.make_object_button('EditStaticText', 'static_text.xpm')
