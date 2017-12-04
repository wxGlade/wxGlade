"""\
wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import wx
import common, compat, config
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np
import decorators

if compat.IS_PHOENIX:
    import wx.adv
    from wx.adv import HyperlinkCtrl
else:
    from wx import HyperlinkCtrl


class EditHyperlinkCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxHyperlinkCtrl objects"
    _PROPERTIES = ["Widget", "label", "style", "url", "attribute"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_HELP = { 'label':"Label of the hyperlink",
                       'url':"URL associated with the given label",
                       "attribute":'Store instance as attribute of window class; e.g. self.bitmap_1 = wx.wxStaticBitmap'
                                   '(...)\nWithout this, you can not access the bitmap from your program.'}

    def __init__(self, name, parent, id, label, sizer, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxHyperlinkCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label = np.TextProperty(label, multiline=True)
        self.url   = np.TextProperty("")
        self.attribute = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = HyperlinkCtrl(self.parent.widget, self.id, self.label, self.url)

    def properties_changed(self, modified):
        if not modified or "label" in modified:
            if self.widget:
                self.widget.SetLabel(self.label)
                self._set_widget_best_size()
            common.app_tree.refresh(self.node, refresh_label=True)

        if not modified or "url" in modified:
            if self.widget:
                self.widget.SetURL(self.url)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)

    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        cn = self.codegen.get_class(self.codegen.cn(name))
        module = wx if compat.IS_CLASSIC else wx.adv
        return getattr(module, cn)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditHyperlinkCtrl objects"
    name = u'hyperlink_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = u'hyperlink_%d' % number[0]
    with parent.frozen():
        hyperlink_ctrl = EditHyperlinkCtrl(name, parent, wx.NewId(), name, sizer, pos)
        hyperlink_ctrl.properties["style"].set_to_default()
        hyperlink_ctrl.properties["attribute"].set(True)  # allow to modificate it later on...
        hyperlink_ctrl.check_defaults()
        node = Node(hyperlink_ctrl)
        hyperlink_ctrl.node = node
        if parent.widget: hyperlink_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditHyperlinkCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    hyperlink_ctrl = EditHyperlinkCtrl(name, parent, wx.NewId(), "", sizer, pos)
    #sizer.set_item(hyperlink_ctrl.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(hyperlink_ctrl)
    hyperlink_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return hyperlink_ctrl


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditHyperlinkCtrl'] = builder
    common.widgets_from_xml['EditHyperlinkCtrl'] = xml_builder

    return common.make_object_button('EditHyperlinkCtrl', 'hyperlink_ctrl.xpm')
