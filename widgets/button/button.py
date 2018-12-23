"""
wxButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import config, common, compat
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from .button_stockitems import *
from gui_mixins import BitmapMixin


class EditButton(ManagedBase, EditStylesMixin, BitmapMixin):
    "Class to handle wxButton objects"

    WX_CLASS = "wxButton"
    STOCKITEMS = sorted( ButtonStockItems.stock_ids.keys())
    _PROPERTIES = ["Widget", "label", "stockitem",
                   "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_HELP = {"default":"This sets the button to be the default item for the panel or dialog box.",
                      "stockitem":"Standard IDs for button identifiers"}

    def __init__(self, name, parent, label, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxButton', parent, pos)
        EditStylesMixin.__init__(self)
        BitmapMixin.__init__(self)

        # initialise instance properties
        self.label     = np.TextProperty(label, default_value="", multiline="grow")
        self.default   = np.CheckBoxProperty(False, default_value=False)
        self.stockitem = np.ComboBoxPropertyD(self.STOCKITEMS[0], choices=self.STOCKITEMS)

        self.bitmap          = np.BitmapPropertyD(min_version=(3,0))
        self.disabled_bitmap = np.BitmapPropertyD(min_version=(3,0))
        self.pressed_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.current_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.focus_bitmap    = np.BitmapPropertyD(min_version=(3,0))

    def create_widget(self):
        stockitem_p = self.properties["stockitem"]
        if stockitem_p.is_active():
            label = ButtonStockItems.stock_ids[stockitem_p.get()]
        else:
            label = self.label
        self.widget = wx.Button(self.parent_window.widget, self.id, label, style=self.style)
        if compat.IS_PHOENIX:
            self._set_preview_bitmaps()

    def properties_changed(self, modified=None):
        "update label (and size if label/stockitem have changed)"

        label_modified = not modified or "label" in modified

        if not modified or "stockitem" in modified:
            # if stockitem is set, label needs to be deactivated and window id is wxID_...
            if self.properties["stockitem"].is_active():
                self.properties["label"].set_blocked(True)
                new_id = "wxID_" + self.stockitem
                self.properties["id"].set( new_id, deactivate=True )
                #self.properties["id"].default_value = new_id  # avoid this value to be written to XML

                l = ButtonStockItems.stock_ids[self.stockitem]
                if self.widget:
                    self.widget.SetLabel(l)
            else:
                self.properties["label"].set_blocked(False)
                #self.properties["id"].default_value = "wxID_ANY"
                label_modified = True

        if label_modified and self.properties["label"].is_active():
            if self.widget:
                self.widget.SetLabel(self.label)

        if label_modified or "name" in modified:
            common.app_tree.refresh(self, refresh_label=True, refresh_image=False)

        BitmapMixin._properties_changed(self, modified)
        self._set_widget_best_size()
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, pos):
    "factory function for EditButton objects"
    name = common.root.get_next_name('button_%d', parent)
    with parent.frozen():
        editor = EditButton(name, parent, name, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    editor = EditButton(name, parent, '', pos)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder

    return common.make_object_button('EditButton', 'button.xpm')
