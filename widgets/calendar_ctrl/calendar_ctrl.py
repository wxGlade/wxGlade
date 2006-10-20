# calendar_ctrl.py: wxCalendarCtrl objects
# $Header: /home/alb/tmp/wxglade_cvs_backup/wxGlade/widgets/calendar_ctrl/calendar_ctrl.py,v 1.6 2006/10/20 09:33:03 guyru Exp $

# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from edit_windows import ManagedBase
from tree import Tree
import common, misc
from widget_properties import *
#import needed modules for the wxCalendarCtrl
from wxPython.calendar import *

class EditCalendarCtrl(ManagedBase):

    events = [
    'EVT_CALENDAR',
    'EVT_CALENDAR_SEL_CHANGE',
    'EVT_CALENDAR_DAY',
    'EVT_CALENDAR_MONTH',
    'EVT_CALENDAR_YEAR',
    'EVT_CALENDAR_WEEKDAY_CLICKED']

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxCalendarCtrl objects
        """
        import config
        self.default = False
        ManagedBase.__init__(self, name, 'wxCalendarCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        #self.access_functions['label'] = (self.get_label, self.set_label)
        #self.properties['label'] = TextProperty(self, 'label', None,
        #                                       multiline=True)
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['default'] = CheckBoxProperty(self, 'default', None)
        style_labels = ('#section#Style', 'wxCAL_SUNDAY_FIRST', 'wxCAL_MONDAY_FIRST', 
            'wxCAL_SHOW_HOLIDAYS', 'wxCAL_NO_YEAR_CHANGE', 'wxCAL_NO_MONTH_CHANGE',
            'wxCAL_SHOW_SURROUNDING_WEEKS','wxCAL_SEQUENTIAL_MONTH_SELECTION')
        self.style_pos = (wxCAL_SUNDAY_FIRST, wxCAL_MONDAY_FIRST, 
            wxCAL_SHOW_HOLIDAYS, wxCAL_NO_YEAR_CHANGE, wxCAL_NO_MONTH_CHANGE,
            wxCAL_SHOW_SURROUNDING_WEEKS,wxCAL_SEQUENTIAL_MONTH_SELECTION)
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)
        
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wxALL

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        #self.properties['label'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wxBoxSizer(wxVERTICAL)
        #szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['default'].panel, 0, wxEXPAND)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def create_widget(self):
        try:
            #TODO add all the other parameters for the CalendarCtrl especialy style=self.style and the itial date
            self.widget = wxCalendarCtrl(self.parent.widget, self.id ,style=self.style)
        except AttributeError:
            self.widget = wxCalendarCtrl(self.parent.widget, self.id)

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


# end of class EditCalendarCtrl
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditCalendarCtrl objects.
    """
    label = 'calendar_ctrl_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'calendar_ctrl_%d' % number[0]
    calendar_ctrl = EditCalendarCtrl(label, parent, wxNewId(), sizer,
                        pos, common.property_panel)
    node = Tree.Node(calendar_ctrl)
    calendar_ctrl.node = node
    calendar_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditCalendarCtrl objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    calendar_ctrl = EditCalendarCtrl(label, parent, wxNewId(), sizer,
                        pos, common.property_panel, show=False)
    node = Tree.Node(calendar_ctrl)
    calendar_ctrl.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return calendar_ctrl


def initialize():
    """\
    initialization function for the module.
    @rtype: wxBitmapButton
    @return: an icon to be added to the main palette. 
    """
    common.widgets['EditCalendarCtrl'] = builder
    common.widgets_from_xml['EditCalendarCtrl'] = xml_builder

    return common.make_object_button('EditCalendarCtrl', 'icons/calendar_ctrl.xpm')
