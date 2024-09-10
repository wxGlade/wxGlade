"""\
wxDirPickerCtrl objects
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, compat, config
import decorators


if compat.IS_PHOENIX:
    #import wx.adv
    from wx.adv import DirPickerCtrl
else:
    #import wx.calendar
    from wx import DirPickerCtrl

class EditDirPickerCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxDirPickerCtrl objects"
    # XXX unify with EditCalendarCtrl?

    WX_CLASS = "wxDirPickerCtrl"
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index):
        # Initialise parent classes
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        self.widget = DirPickerCtrl(self.parent_window.widget, wx.ID_ANY, "", "", wxDefaultPosition, wxDefaultSize, style=self.style)

    # handle compatibility:
    @decorators.memoize
    def wxname2attr(self, name):
        cn = self.codegen.get_class(self.codegen.cn(name))
        module = wx if compat.IS_CLASSIC else wx.adv
        return getattr(module, cn)

    def _properties_changed(self, modified, actions):
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditDirPickerCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('dirpicker_ctrl_%d')
    with parent.frozen():
        editor = EditDirPickerCtrl(name, parent, index)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditDirPickerCtrl objects from a XML file"
    return EditDirPickerCtrl(name, parent, index)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditDirPickerCtrl'] = EditDirPickerCtrl
    common.widgets['EditDirPickerCtrl'] = builder
    common.widgets_from_xml['EditDirPickerCtrl'] = xml_builder

    return common.make_object_button('EditDirPickerCtrl', 'datepicker_ctrl.png')
