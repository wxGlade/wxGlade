"""\
wxDatePickerCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, StylesMixin
from tree import Tree
import common
import config
from widget_properties import *


class EditDatePickerCtrl(ManagedBase, StylesMixin):
    """\
    Class to handle wxDatePickerCtrl objects
    """

    events = ['EVT_DATE_CHANGED']

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxDatePickerCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        StylesMixin.__init__(self)

        # initialise instance variables
        self.default = False
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['default'] = CheckBoxProperty(self, 'default', None, label=_("default"))
        style_labels = ('#section#' + _('Style'), 'wxDP_SPIN', 'wxDP_DROPDOWN',
                        'wxDP_DEFAULT', 'wxDP_ALLOWNONE', 'wxDP_SHOWCENTURY')
        self.gen_style_pos(style_labels)
        self.tooltips = (_("Creates a control without a month calendar drop down but with spin-control-like arrows to change individual date components. This style is not supported by the generic version."),
                         _("Creates a control with a month calendar drop-down part from which the user can select a date."),
                         _("Creates a control with the style that is best supported for the current platform (currently wxDP_SPIN under Windows and wxDP_DROPDOWN elsewhere)."),
                         _("With this style, the control allows the user to not enter any valid date at all. Without it - the default - the control always has some valid date."),
                         _("Forces display of the century in the default date format. Without this style the century could be displayed, or not, depending on the default date representation in the system."))
        self.properties['style'] = CheckListProperty(
            self, 'style', None, style_labels, tooltips=self.tooltips)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        #self.properties['label'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        #szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['default'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def create_widget(self):
        try:
            #TODO add all the other parameters for the DatePickerCtrl intial date
            self.widget = wx.DatePickerCtrl(self.parent.widget, self.id, style=self.style)
        except AttributeError:
            self.widget = wx.DatePickerCtrl(self.parent.widget, self.id)

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))

# end of class EditDatePickerCtrl
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditDatePickerCtrl objects.
    """
    label = 'datepicker_ctrl_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'datepicker_ctrl_%d' % number[0]
    datepicker_ctrl = EditDatePickerCtrl(label, parent, wx.NewId(), sizer,
                        pos, common.property_panel)
    node = Tree.Node(datepicker_ctrl)
    datepicker_ctrl.node = node
    datepicker_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditDatePickerCtrl objects from an xml file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    datepicker_ctrl = EditDatePickerCtrl(label, parent, wx.NewId(), sizer,
                        pos, common.property_panel, show=False)
    node = Tree.Node(datepicker_ctrl)
    datepicker_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return datepicker_ctrl


def initialize():
    """\
    initialization function for the module.
    @rtype: wxBitmapButton
    @return: an icon to be added to the main palette. 
    """
    common.widgets['EditDatePickerCtrl'] = builder
    common.widgets_from_xml['EditDatePickerCtrl'] = xml_builder

    return common.make_object_button('EditDatePickerCtrl', 'icons/datepicker_ctrl.xpm')
