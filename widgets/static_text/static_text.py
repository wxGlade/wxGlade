"""\
wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
#import wx.lib.stattext
import common, config
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
        #self.widget = wx.lib.stattext.GenStaticText(self.parent_window.widget, self.id, self.label)
        self.widget = wx.StaticText(self.parent_window.widget, self.id, self.label)
        # now in finish_widget_creation
        #if self.wrap:
            #self.widget.Wrap(self.wrap)

    def properties_changed(self, modified):
        if not modified or "label" in modified:
            if common.app_tree: common.app_tree.refresh(self, refresh_label=True)
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
            if self.widget and self.parent.IS_SIZER:
                self.parent.layout()
            else:
                self.parent.widget.Refresh()
            return

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, index):
    "factory function for EditStaticText objects"
    name = parent.toplevel_parent.get_next_contained_name('label_%d')
    with parent.frozen():
        editor = EditStaticText(name, parent, index, name)
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

    return common.make_object_button('EditStaticText', 'static_text.xpm')
