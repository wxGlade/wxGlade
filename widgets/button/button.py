"""
wxButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from .button_stockitems import *
from gui_mixins import BitmapMixin


class EditButton(BitmapMixin, ManagedBase, EditStylesMixin):
    "Class to handle wxButton objects"

    WX_CLASS = "wxButton"
    STOCKITEMS = sorted( ButtonStockItems.stock_ids.keys())
    _PROPERTIES = ["Widget", "label", "stockitem",
                   "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "default", "style", "bitmap_dir"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_HELP = {"default":"This sets the button to be the default item for the toplevel window.\n"
                                "(On Windows this is only supported for Dialogs.)",
                      "stockitem":"Standard IDs for button identifiers.\n\n"
                                  "You can edit these in the Tree view as well.\n\n"
                                  "If stockitem buttons like OK and CANCEL are placed in a StdDialogButtonSizer, "
                                  "they will be re-ordered according to the platform style guide.",
                      "bitmap_dir":"If bitmap is set: Position of the bitmap in the button."}

    def __init__(self, name, parent, index, label):
        # Initialise parent classes
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)
        BitmapMixin.__init__(self)

        # initialise instance properties
        self.label     = np.TextProperty(label, default_value="", multiline="grow")
        self.default   = np.CheckBoxProperty(False, default_value=False)
        self.stockitem = np.ListBoxPropertyD(self.STOCKITEMS[0], choices=self.STOCKITEMS)

        self.bitmap          = np.BitmapPropertyD(min_version=(3,0))
        self.disabled_bitmap = np.BitmapPropertyD(min_version=(3,0))
        self.pressed_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.current_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.focus_bitmap    = np.BitmapPropertyD(min_version=(3,0))

        values = [wx.LEFT, wx.RIGHT, wx.TOP, wx.BOTTOM]
        aliases = ["wxLEFT", "wxRIGHT", "wxTOP", "wxBOTTOM"]
        p = self.bitmap_dir = np.RadioProperty(wx.LEFT, values, columns=4, aliases=aliases, default_value=wx.LEFT)
        p.min_version = (3,0)
        p.blocked = True

    def create_widget(self):
        stockitem_p = self.properties["stockitem"]
        if stockitem_p.is_active():
            label = ButtonStockItems.stock_ids[stockitem_p.get()]
        else:
            label = self.label
        self.widget = wx.Button(self.parent_window.widget, wx.ID_ANY, label, style=self.style)
        if compat.version[0]>=3 and int(common.root.for_version[0])>2:
            self._set_preview_bitmaps()
            if self.bitmap_dir!=wx.LEFT:
                self.widget.SetBitmapPosition(self.bitmap_dir)

    def _properties_changed(self, modified, actions):
        "update label (and size if label/stockitem have changed)"

        if "bitmap" in modified:
            self.properties["bitmap_dir"].set_blocked( not self.check_prop_truth("bitmap") )

        label_modified = not modified or ("label" in modified or "font" in modified)

        if not modified or "stockitem" in modified:
            # if stockitem is set, label needs to be deactivated and window id is wxID_...
            if self.properties["stockitem"].is_active():
                self.properties["label"].set_blocked(True)
                new_id = "wxID_" + self.stockitem
                if common.history: common.history.monitor_property( self.properties["id"] )
                self.properties["id"].set( new_id, deactivate=True )
                #self.properties["id"].default_value = new_id  # avoid this value to be written to XML

                l = ButtonStockItems.stock_ids[self.stockitem]
                if self.widget: self.widget.SetLabel(l)
            else:
                self.properties["label"].set_blocked(False)
                label_modified = True

        if modified and "font" in modified and wx.Platform == '__WXGTK__':
            # on GTK setting a smaller font would fail
            actions.update(("recreate2","label","sizeevent"))
            return

        if label_modified and self.properties["label"].is_active() and self.widget:
            self.widget.SetLabel(self.label)
            label_modified = True

        if label_modified or (modified and "stockitem" in modified):
            actions.update(("layout","label","sizeevent"))

        if modified and ("label" in modified or "stockitem" in modified): actions.add("label")

        if "bitmap_dir" in modified and self.widget:
            self.widget.SetBitmapPosition(self.bitmap_dir)

        BitmapMixin._properties_changed(self, modified, actions)
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditButton objects"
    name = parent.toplevel_parent.get_next_contained_name('button_%d')
    with parent.frozen():
        editor = EditButton(name, parent, index, name)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditButton objects from a XML file"
    return EditButton(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditButton'] = EditButton
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder

    return common.make_object_button('EditButton', 'button.png')
