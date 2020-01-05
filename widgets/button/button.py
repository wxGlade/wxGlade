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
from tree import Node
import new_properties as np
from .button_stockitems import *
from gui_mixins import BitmapMixin


class EditButton(ManagedBase, EditStylesMixin, BitmapMixin):
    "Class to handle wxButton objects"

    STOCKITEMS = sorted( ButtonStockItems.stock_ids.keys())
    _PROPERTIES = ["Widget", "label", "stockitem",
                   "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_HELP = {"default":"This sets the button to be the default item for the panel or dialog box.",
                      "stockitem":"Standard IDs for button identifiers"}

    def __init__(self, name, parent, id, label, sizer, pos):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxButton', parent, id, sizer, pos)
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

    def create_widget(self):
        stockitem_p = self.properties["stockitem"]
        if stockitem_p.is_active():
            label = ButtonStockItems.stock_ids[stockitem_p.get()]
        else:
            label = self.label
        self.widget = wx.Button(self.parent.widget, self.id, label, style=self.style)
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
            common.app_tree.refresh(self.node, refresh_label=True)

        BitmapMixin._properties_changed(self, modified)
        self._set_widget_best_size()
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditButton objects"
    name = u'button_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = u'button_%d' % number[0]
    with parent.frozen():
        button = EditButton(name, parent, wx.NewId(), name, sizer, pos)
        button.properties["style"].set_to_default()
        button.check_defaults()
        node = Node(button)
        button.node = node
        if parent.widget: button.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    button = EditButton(name, parent, wx.NewId(), '', sizer, pos)
    #sizer.set_item(button.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(button)
    button.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return button


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder

    return common.make_object_button('EditButton', 'button.xpm')
