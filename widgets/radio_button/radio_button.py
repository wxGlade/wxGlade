"""\
wxRadioButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, config
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from misc import wxGladeRadioButton


class EditRadioButton(ManagedBase, EditStylesMixin):
    "Class to handle wxRadioButton objects"
    update_widget_style = False

    WX_CLASS = 'wxRadioButton'
    _PROPERTIES = ["Widget", "label", "clicked", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, label=""):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label   = np.TextProperty(label, multiline="grow")
        self.clicked = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wxGladeRadioButton(self.parent_window.widget, wx.ID_ANY, self.label)
        self.widget.SetValue(self.clicked)
        self.widget.Bind(wx.EVT_CHECKBOX, lambda e: self.widget.SetValue(self.value))

    def _set_label(self):
        if not self.widget: return
        self.widget.SetLabel(self.label)

    def _properties_changed(self, modified, actions):
        if not modified or "label" in modified:
            self._set_label()
            actions.add("layout")

        if not modified or "clicked" in modified and self.widget:
            self.widget.SetValue(self.clicked)

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditRadioButton objects"
    import dialogs, misc
    name = parent.toplevel_parent.get_next_contained_name('radio_btn_%d')
    dlg = dialogs.WidgetStyleSelectionDialog("RadioButton", None, None, ["?Label"])
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dlg.ShowModal()
    label, = dlg.get_options()
    dlg.Destroy()
    if res != wx.ID_OK: return

    with parent.frozen():
        editor = EditRadioButton(name, parent, index, label)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditRadioButton objects from a XML file"
    return EditRadioButton(name, parent, index, "")


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditRadioButton'] = EditRadioButton
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder

    return common.make_object_button('EditRadioButton', 'radio_button.png')
