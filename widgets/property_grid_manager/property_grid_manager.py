"""\
wxPropertyGridManager objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.propgrid import *
import common, compat
from edit_windows import ManagedBase, EditStylesMixin


class EditPropertyGridManager(ManagedBase, EditStylesMixin):
    "Class to handle wxPropertyGridManager objects"
    WX_CLASS = 'wxPropertyGridManager'
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, index):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        self.widget = PropertyGridManager(self.parent_window.widget, wx.ID_ANY, (200,200), style=self.style)

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
        pg.Append( wxpg.BoolProperty("Bool_with_Checkbox",value=True) )
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

    def _properties_changed(self, modified, actions):
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditPropertyGridManager objects"
    name = parent.toplevel_parent.get_next_contained_name('property_grid_%d')
    with parent.frozen():
        editor = EditPropertyGridManager(name, parent, index)
        editor.properties["style"].set_to_default()
        # A grid should be wx.EXPANDed and 'option' should be 1, or you can't see it.
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditPropertyGridManager objects from a XML file"
    return EditPropertyGridManager( name, parent, index )


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditPropertyGridManager'] = EditPropertyGridManager
    common.widgets['EditPropertyGridManager'] = builder
    common.widgets_from_xml['EditPropertyGridManager'] = xml_builder

    return common.make_object_button('EditPropertyGridManager', 'grid.png')

