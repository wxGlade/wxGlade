"""\
wxPropertyGridManager objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.propgrid import *
import common, compat
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node


class EditPropertyGridManager(ManagedBase, EditStylesMixin):
    "Class to handle wxPropertyGridManager objects"
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, id, sizer, pos):
        ManagedBase.__init__(self, name, 'wxPropertyGridManager', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        self.widget = PropertyGridManager(self.parent.widget, self.id, (200,200), style=self.style)

        # property grid does handle left and right mouse clicks and emits these events:
        self.widget.Bind(EVT_PG_SELECTED, self.on_set_focus)
        self.widget.Bind(EVT_PG_RIGHT_CLICK, self.on_right_click)

        self._create_dummy_content()

    def on_right_click(self, event):
        self.popup_menu( event, self.widget.ScreenToClient( wx.GetMousePosition() ) )

    def _create_dummy_content(self):
        # some dummy content from the wxPython demo
        import wx.propgrid as wxpg
        pg = self.widget
        pg.AddPage( "Page 1 - Dummy" )

        pg.Append( wxpg.PropertyCategory("1 - Basic Properties") )
        pg.Append( wxpg.StringProperty("String",value="Some Text") )

        sp = pg.Append( wxpg.StringProperty('StringProperty w/ Password flag', value='ABadPassword') )
        sp.SetAttribute('Hint', 'This is a hint')
        sp.SetAttribute('Password', True)

        pg.Append( wxpg.IntProperty("Int",value=123) )
        pg.Append( wxpg.FloatProperty("Float",value=123.4) )
        pg.Append( wxpg.BoolProperty("Bool",value=True) )
        boolprop = pg.Append( wxpg.BoolProperty("Bool_with_Checkbox",value=True) )
        pg.SetPropertyAttribute( "Bool_with_Checkbox", "UseCheckbox", True)
        pg.Append( wxpg.PropertyCategory("2 - More Properties") )
        pg.Append( wxpg.LongStringProperty("LongString", value="This is a\nmulti-line string\nwith\ttabs\nmixed\tin."))
        pg.Append( wxpg.DirProperty("Dir",value=r"C:\Windows") )
        pg.Append( wxpg.FileProperty("File",value=r"C:\Windows\system.ini") )
        pg.Append( wxpg.ArrayStringProperty("ArrayString",value=['A','B','C']) )

        pg.Append( wxpg.EnumProperty("Enum","Enum", ['wxPython Rules', 'wxPython Rocks', 'wxPython Is The Best'],
                                                    [10,11,12], 0) )
        pg.Append( wxpg.EditEnumProperty("EditEnum","EditEnumProperty", ['A','B','C'], [0,1,2], "Text Not in List") )

        pg.Append( wxpg.PropertyCategory("3 - Advanced Properties") )
        pg.Append( wxpg.DateProperty("Date",value=wx.DateTime.Now()) )
        pg.Append( wxpg.FontProperty("Font",value=self.widget.GetFont()) )
        pg.Append( wxpg.ColourProperty("Colour", value=self.widget.GetBackgroundColour()) )
        pg.Append( wxpg.SystemColourProperty("SystemColour") )
        pg.Append( wxpg.ImageFileProperty("ImageFile") )
        pg.Append( wxpg.MultiChoiceProperty("MultiChoice", choices=['wxWidgets','QT','GTK+']) )

        pg.Append( wxpg.PropertyCategory("4 - Additional Properties") )
        pg.Append( wxpg.IntProperty("IntWithSpin",value=256) )
        pg.SetPropertyEditor("IntWithSpin","SpinCtrl")

        pg.SetPropertyAttribute( "File", wxpg.PG_FILE_SHOW_FULL_PATH, 0 )
        pg.SetPropertyAttribute( "File", wxpg.PG_FILE_INITIAL_PATH, r"C:\Program Files\Internet Explorer" )
        if compat.IS_PHOENIX:
            pg.SetPropertyAttribute( "Date", wxpg.PG_DATE_PICKER_STYLE, wx.adv.DP_DROPDOWN|wx.adv.DP_SHOWCENTURY )

        pg.AddPage( "Page 2 - Almost Empty" )
        pg.Append( wxpg.PropertyCategory("1 - Basic Properties") )
        pg.Append( wxpg.StringProperty("String 2",value="Some Text") )

    def get_property_handler(self, name):
        return ManagedBase.get_property_handler(self, name)

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditPropertyGridManager objects"
    label = 'property_grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'property_grid_%d' % number[0]
    with parent.frozen():
        property_grid_manager = EditPropertyGridManager(label, parent, wx.NewId(), sizer, pos)
        property_grid_manager.properties["style"].set_to_default()
        # A grid should be wx.EXPANDed and 'option' should be 1, or you can't see it.
        property_grid_manager.properties["proportion"].set(1)
        property_grid_manager.properties["flag"].set("wxEXPAND")
        node = Node(property_grid_manager)
        property_grid_manager.node = node
        if parent.widget: property_grid_manager.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditPropertyGridManager objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    property_grid_manager = EditPropertyGridManager( label, parent, wx.NewId(), sizer, pos )
    #sizer.set_item( property_grid_manager.pos, proportion=sizeritem.proportion, flag=sizeritem.flag,
    #                border=sizeritem.border)
    node = Node(property_grid_manager)
    property_grid_manager.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return property_grid_manager


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditPropertyGridManager'] = builder
    common.widgets_from_xml['EditPropertyGridManager'] = xml_builder

    return common.make_object_button('EditPropertyGridManager', 'grid.xpm')

