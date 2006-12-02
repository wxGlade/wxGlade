# button.py: wxButton objects
# $Id: button.py,v 1.21 2006/12/02 10:49:56 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import wx
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditButton(ManagedBase):

    events = ['EVT_BUTTON']

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxButton objects
        """
        import config
        self.label = label
        self.default = False
        ManagedBase.__init__(self, name, 'wxButton', parent, id, sizer, pos,
                             property_window, show=show)
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.properties['label'] = TextProperty(self, 'label', None,
                                                multiline=True)
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['default'] = CheckBoxProperty(self, 'default', None)
        self.style_pos = (wx.BU_LEFT, wx.BU_RIGHT, wx.BU_TOP, wx.BU_BOTTOM,
            wx.BU_EXACTFIT,wx.NO_BORDER)
        style_labels = ('#section#Style', 'wxBU_LEFT', 'wxBU_RIGHT', 
            'wxBU_TOP', 'wxBU_BOTTOM', 'wxBU_EXACTFIT','wxNO_BORDER')
	
	#The tooltips tuple
        style_tooltips=("Left-justifies the label. Windows and GTK+ only.",
            "Right-justifies the bitmap label. Windows and GTK+ only.",
	    "Aligns the label to the top of the button. Windows and GTK+ only.",
            "Aligns the label to the bottom of the button. Windows and GTK+ only.",
	    "Creates the button as small as possible instead of making it of the standard size (which is the default behaviour ).",
	    "Creates a flat button. Windows and GTK+ only.")
        self.properties['style'] = CheckListProperty(self, 'style', None,
            style_labels, tooltips=style_tooltips) #the tooltips tuple is passed as the last argument
        # 2003-09-04 added default_border
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        self.properties['label'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['default'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_label(self):
        return self.label

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())
            self.label = value

    def create_widget(self):
        try:
            self.widget = wx.Button(self.parent.widget, self.id, self.label, style=self.style)
        except AttributeError:
            self.widget = wx.Button(self.parent.widget, self.id, self.label)

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

# end of class EditButton
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditButton objects.
    """
    label = 'button_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'button_%d' % number[0]
    button = EditButton(label, parent, wx.NewId(), misc._encode(label), sizer,
                        pos, common.property_panel)
    node = Tree.Node(button)
    button.node = node
    button.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditButton objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    button = EditButton(label, parent, wx.NewId(), '', sizer,
                        pos, common.property_panel, show=False)
    node = Tree.Node(button)
    button.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return button


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder

    return common.make_object_button('EditButton', 'icons/button.xpm')
