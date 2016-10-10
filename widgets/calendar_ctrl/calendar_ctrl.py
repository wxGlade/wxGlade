"""\
wxCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common, compat, config
import new_properties as np
import decorators

if compat.IS_PHOENIX:
    import wx.adv
    from wx.adv import CalendarCtrl
else:
    import wx.calendar
    from wx.calendar import CalendarCtrl


class EditCalendarCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxCalendarCtrl objects"

    _PROPERTIES = ["Widget", "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxCalendarCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.default = np.CheckBoxProperty(False, default_value=False)

        if config.preferences.default_border:
            self.border.set( config.preferences.default_border_size )
            self.flag.set( wx.ALL )

    def create_widget(self):
        # TODO add all the other parameters for the CalendarCtrl especially style=self.style and the initial date
        self.widget = CalendarCtrl(self.parent.widget, self.id, style=self.style)


    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        assert name.startswith('wx')

        cn = self.codegen.get_class(self.codegen.cn(name))
        if compat.IS_PHOENIX:
            attr = getattr(wx.adv, cn)
        else:
            attr = getattr(wx.calendar, cn)
        return attr


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditCalendarCtrl objects"
    label = 'calendar_ctrl_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'calendar_ctrl_%d' % number[0]
    calendar_ctrl = EditCalendarCtrl(label, parent, wx.NewId(), sizer, pos)
    node = Node(calendar_ctrl)
    calendar_ctrl.node = node
    if parent.widget: calendar_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditCalendarCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    calendar_ctrl = EditCalendarCtrl(label, parent, wx.NewId(), sizer, pos)
    sizer.set_item(calendar_ctrl.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(calendar_ctrl)
    calendar_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return calendar_ctrl


def initialize():
    "initialization function for the module; returns an icon to be added to the main palette"
    common.widgets['EditCalendarCtrl'] = builder
    common.widgets_from_xml['EditCalendarCtrl'] = xml_builder

    return common.make_object_button('EditCalendarCtrl', 'calendar_ctrl.xpm')
