"""\
wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
#import wx.lib.stattext
import common
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditStaticText(ManagedBase, EditStylesMixin):
    "Class to handle wxStaticText objects"
    WX_CLASS = 'wxStaticText'
    _PROPERTIES = ["Widget", "label", "style", "attribute", "wrap"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_HELP ={"attribute":'Store instance as attribute of window class; e.g. self.label_1 = wx.StaticText(...)\n'
                                 'Without this, you can not access the label from your program.',
                     "wrap":     'Wrap text to at most the given width.\nThe lines will be broken at word boundaries.'}

    def __init__(self, name, parent, index, label):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label     = np.TextProperty(label, multiline="grow")
        self.attribute = np.CheckBoxProperty(False, default_value=False)
        self.wrap      = np.SpinPropertyD(100, val_range=(1,100000), immediate=True, default_value=-1)

    def create_widget(self):
        # up to 0.8 GenStaticText was used; it seems that nowadays StaticText handles mouse events on gtk as well
        #self.widget = wx.lib.stattext.GenStaticText(self.parent_window.widget, wx.ID_ANY, self.label)
        self.widget = wx.StaticText(self.parent_window.widget, wx.ID_ANY, self.label, style=self.style)
        # self.wrap is now handled in finish_widget_creation

    def _properties_changed(self, modified, actions):
        if modified and "label" in modified:
            actions.add("label")

        if not modified or "label" in modified:
            if self.widget:
                p = self.properties["label"]
                if self.wrap!=-1 or (p.previous_value and len(p.previous_value)>len(self.label)):
                    # re-create as otherwise the size would not be reduced
                    actions.add("recreate2")
                    return
                self.widget.SetLabel(self.label)
                actions.add("layout")

        if (not modified or "wrap" in modified) and self.widget:
            actions.add("recreate2")  # calling .Wrap(self.wrap) would only work once and not set the size correctly
            return

        if modified and wx.Platform!="__WXMSW__":
            if "style" in modified or "font" in modified or "foreground" in modified:
                actions.add("recreate2")
                return

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditStaticText objects"
    import dialogs, misc
    name = parent.toplevel_parent.get_next_contained_name('label_%d')
    dlg = dialogs.WidgetStyleSelectionDialog(_("Static Text / Label"), None, None, ["?Label"])
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dlg.ShowModal()
    label, = dlg.get_options()
    dlg.Destroy()
    if res != wx.ID_OK: return

    with parent.frozen():
        editor = EditStaticText(name, parent, index, label)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditStaticText objects from a XML file"
    return EditStaticText(name, parent, index, "")


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditStaticText'] = EditStaticText
    common.widgets['EditStaticText'] = builder
    common.widgets_from_xml['EditStaticText'] = xml_builder

    return common.make_object_button('EditStaticText', 'static_text.png')
