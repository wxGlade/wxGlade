"""\
wxCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
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

    WX_CLASS = "wxCalendarCtrl"
    _PROPERTIES = ["Widget", "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, pos, instance_class=None):
        # Initialise parent classes
        ManagedBase.__init__(self, name, parent, pos, instance_class)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.default = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        # TODO add all the other parameters for the CalendarCtrl especially style=self.style and the initial date
        self.widget = CalendarCtrl(self.parent_window.widget, self.id, style=self.style)

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

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditCalendarCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('calendar_ctrl_%d')
    with parent.frozen():
        editor = EditCalendarCtrl(name, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parent, pos, attrs):
    attrs.set_editor_class(EditCalendarCtrl)
    name, instance_class = attrs.get_attributes("name", "instance_class")
    return EditCalendarCtrl(name, parent, pos, instance_class)


def initialize():
    "initialization function for the module; returns an icon to be added to the main palette"
    common.widget_classes['EditCalendarCtrl'] = EditCalendarCtrl
    common.widgets['EditCalendarCtrl'] = builder
    common.widgets_from_xml['EditCalendarCtrl'] = xml_builder

    return common.make_object_button('EditCalendarCtrl', 'calendar_ctrl.xpm')
