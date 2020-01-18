"""\
wxDatePickerCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
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

    WX_CLASS = "wxDatePickerCtrl"
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxDatePickerCtrl', parent, pos)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        # TODO add all the other parameters for the DatePickerCtrl initial date
        self.widget = DatePickerCtrl(self.parent_window.widget, self.id, style=self.style)

    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        cn = self.codegen.get_class(self.codegen.cn(name))
        module = wx if compat.IS_CLASSIC else wx.adv
        return getattr(module, cn)

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditDatePickerCtrl objects"
    name = common.root.get_next_name('datepicker_ctrl_%d', parent)
    with parent.frozen():
        editor = EditDatePickerCtrl(name, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditDatePickerCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditDatePickerCtrl(name, parent, pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditDatePickerCtrl'] = EditDatePickerCtrl
    common.widgets['EditDatePickerCtrl'] = builder
    common.widgets_from_xml['EditDatePickerCtrl'] = xml_builder

    return common.make_object_button('EditDatePickerCtrl', 'datepicker_ctrl.xpm')
