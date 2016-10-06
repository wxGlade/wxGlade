"""\
Mixin class for 'events' property

@copyright: 2002-2004 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import wx.grid
import re

import common
import config
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np



class NewEventsProperty(np.GridProperty):
    "Class EventsProperty"
    LABEL = 'Events'
    validation_res = [False, re.compile(r'^\s*[\w-]+\s*$')]
    def __init__(self, events):
        # initialise instance
        cols = [(_('Event'), np.GridProperty.STRING),
                (_('Handler'), np.GridProperty.STRING)]
        value = [[name, ''] for name in sorted(events)]
        np.GridProperty.__init__( self, value, cols, can_add=False, can_remove=False, can_insert=False)

    def create_editor(self, panel, sizer):
        np.GridProperty.create_editor(self, panel, sizer)
        attr = wx.grid.GridCellAttr()
        attr.SetReadOnly(True)
        self.grid.SetColAttr(0, attr)
        self.grid.AutoSizeColumn(0, False)
        self.grid.AutoSizeColumn(1, False)

    def set_value_dict(self, values_dict):
        for row in self.value:
            row[1] = values_dict.get(row[0], "")
        self.update_display()

    def write(self, outfile, tabs):
        inner_xml = u''
        for event, handler in self.get():
            handler = handler.strip()
            if handler:
                inner_xml += common.format_xml_tag('handler', handler, tabs+1, event=event)
        if inner_xml:
            stmt = common.format_xml_tag(u'events', inner_xml, tabs, is_xml=True)
            outfile.write(stmt)



class EventsPropertyHandler(BaseXmlBuilderTagHandler):
    "Class EventsPropertyHandler"
    strip_char_data = True
    
    def __init__(self, owner):
        super(EventsPropertyHandler, self).__init__()
        # initialise instance
        self.owner = owner
        self.current_event = None
        self.handlers = {}  # Dictionary of event names and event handlers

    def start_elem(self, name, attrs):
        if name == 'handler':
            self.current_event = attrs['event']

    def end_elem(self, name):
        if name == 'handler':
            if self.current_event and self._content:
                char_data = self.get_char_data()
                self.handlers[self.current_event] = char_data
        elif name == 'events':
            if self.handlers:
                self.owner.properties["events"].set_value_dict(self.handlers)
            return True  # to remove this handler



class EventsMixin(object):
    "Class mixin to handle events"
    _PROPERTIES = ["Events", "events"]

    def __init__(self):
        # get events known by this widget
        try:
            events = list( config.widget_config[self.klass]['events'].keys() )
            if 'default' in events: events.remove('default')
            events.sort()
        except KeyError:
            events = []  # no default handler

        # create Property
        self.events = NewEventsProperty(events) if events else None

    def get_property_handler(self, name):
        if name == 'events':
            return EventsPropertyHandler(self)
        return None
