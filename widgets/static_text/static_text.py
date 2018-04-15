"""\
wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
#import wx.lib.stattext
import common, config
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np


class EditStaticText(ManagedBase, EditStylesMixin):
    "Class to handle wxStaticText objects"
    _PROPERTIES = ["Widget", "label", "style", "attribute", "wrap"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_HELP ={"attribute":'Store instance as attribute of window class; e.g. self.label_1 = wx.StaticText(...)\n'
                                 'Without this, you can not access the label from your program.',
                     "wrap":     'Wrap text to at most the given width.\nThe lines will be broken at word boundaries.'}

    def __init__(self, name, parent, id, label, sizer, pos):
        ManagedBase.__init__(self, name, 'wxStaticText', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label     = np.TextProperty(label, multiline="grow")
        self.attribute = np.CheckBoxProperty(False, default_value=False)
        self.wrap      = np.SpinPropertyD(100, val_range=(1,100000), immediate=True, default_value=-1)

    def create_widget(self):
        # up to 0.8 GenStaticText was used; it seems that nowadays StaticText handles mouse events on gtk as well
        #self.widget = wx.lib.stattext.GenStaticText(self.parent.widget, self.id, self.label)
        self.widget = wx.StaticText(self.parent.widget, self.id, self.label)
        if self.wrap:
            self.widget.Wrap(self.wrap)

    def properties_changed(self, modified):
        if not modified or "label" in modified:
            common.app_tree.refresh(self.node, refresh_label=True)
            if self.widget:
                p = self.properties["label"]
                if self.wrap!=-1 or (p.previous_value and len(p.previous_value)>len(self.label)):
                    # re-create as otherwise the size would not be reduced
                    self.recreate_widget()
                    return
                self.widget.SetLabel(self.label)
                self._set_widget_best_size()

        if (not modified or "wrap" in modified) and self.widget:
            self.recreate_widget()  # calling .Wrap(self.wrap) would only work once and not set the size correctly

            return

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditStaticText objects"
    label = u'label_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'label_%d' % number[0]
    with parent.frozen():
        static_text = EditStaticText(label, parent, wx.NewId(), label, sizer, pos)
        static_text.properties["style"].set_to_default()
        static_text.check_defaults()
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
    #sizer.set_item(static_text.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
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
