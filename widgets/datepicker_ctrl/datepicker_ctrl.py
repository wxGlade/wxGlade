"""\
wxDatePickerCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common, compat, config
import decorators


if compat.IS_PHOENIX:
    #import wx.adv
    from wx.adv import DatePickerCtrl
else:
    #import wx.calendar
    from wx import DatePickerCtrl

class EditDatePickerCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxDatePickerCtrl objects"
    # XXX unify with EditCalendarCtrl?

    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxDatePickerCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        # TODO add all the other parameters for the DatePickerCtrl initial date
        self.widget = DatePickerCtrl(self.parent.widget, self.id, style=self.style)

    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        cn = self.codegen.get_class(self.codegen.cn(name))
        module = wx if compat.IS_CLASSIC else wx.adv
        return getattr(module, cn)

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditDatePickerCtrl objects"
    label = 'datepicker_ctrl_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'datepicker_ctrl_%d' % number[0]
    with parent.frozen():
        datepicker_ctrl = EditDatePickerCtrl(label, parent, wx.NewId(), sizer, pos)
        datepicker_ctrl.properties["style"].set_to_default()
        datepicker_ctrl.check_defaults()
        node = Node(datepicker_ctrl)
        datepicker_ctrl.node = node
        if parent.widget: datepicker_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditDatePickerCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    datepicker_ctrl = EditDatePickerCtrl(label, parent, wx.NewId(), sizer, pos)
    #sizer.set_item(datepicker_ctrl.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(datepicker_ctrl)
    datepicker_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return datepicker_ctrl


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditDatePickerCtrl'] = builder
    common.widgets_from_xml['EditDatePickerCtrl'] = xml_builder

    return common.make_object_button('EditDatePickerCtrl', 'datepicker_ctrl.xpm')
