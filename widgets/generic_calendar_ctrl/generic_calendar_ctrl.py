"""\
wxGenericCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, compat, config
import new_properties as np
import decorators

if compat.IS_PHOENIX:
    from wx.adv import GenericCalendarCtrl
else:
    from wx.calendar import GenericCalendarCtrl


class EditGenericCalendarCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxGenericCalendarCtrl objects"
    # XXX unify with EditCalendarCtrl?

    WX_CLASS = "wxGenericCalendarCtrl"
    _PROPERTIES = ["Widget", "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxGenericCalendarCtrl', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.default = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        # TODO add all the other parameters for the GenericCalendarCtrl especially style=self.style and the initial date
        self.widget = GenericCalendarCtrl(self.parent_window.widget, self.id, style=self.style)

    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        assert name.startswith('wx')

        cn = self.codegen.get_class(self.codegen.cn(name))
        if compat.IS_PHOENIX:
            attr = getattr(wx.adv, cn)
        else:
            attr = getattr(wx, cn)
        return attr

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditGenericCalendarCtrl objects"
    name = common.root.get_next_name('generic_calendar_ctrl_%d', parent)
    with parent.frozen():
        editor = EditGenericCalendarCtrl(label, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditGenericCalendarCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditGenericCalendarCtrl(label, parent, pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditGenericCalendarCtrl'] = builder
    common.widgets_from_xml['EditGenericCalendarCtrl'] = xml_builder

    return common.make_object_button('EditGenericCalendarCtrl', 'calendar_ctrl.xpm')
