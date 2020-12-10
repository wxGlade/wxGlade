"""\
Mixin class for 'events' property

@copyright: 2002-2004 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import wx.grid
import re

import common, config
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np



class EventsProperty(np.GridProperty):
    "Class EventsProperty"
    LABEL = 'Events'
    TOOLTIP = ("Enter a handler name for each event that should be handled.\n"
               "wxGlade will then create a method with that name and register it as event handler.\n"
               "\nFor Python, you may also enter a lambda function as handler.\n\n"
               "If you need to handle event types that are not listed here,\n"
               "then you need to create and register the handler yourself.\n"
               "You can do so in your own source that you derive from the\n"
               "wxGlade generated code or within wxGlade on the 'Code' tab.")
    validation_res = [re.compile(r'^([a-zA-Z_][a-zA-Z0-9_\.]*\.)?EVT_[A-Z0-9_]+$'),  # XXX avoid ..
                      re.compile(r'^(([a-zA-Z_]+[a-zA-Z0-9_-]*)|()|(lambda .*))$')]
    EDITABLE_COLS = [0,1]
    UPPERCASE_COLS = [None,None]
    SKIP_EMPTY = True
    IS_KEY_VALUE = True
    CAN_ADD_GROUPS = True
    def __init__(self, events):
        # initialise instance
        cols = [(_('Event'), np.GridProperty.STRING),
                (_('Handler'), np.GridProperty.STRING)]
        value = [[name, ''] for name in sorted(events)]
        np.GridProperty.__init__( self, value, cols, can_add=True, can_remove=True, can_insert=True,
                                  immediate=True)

    def create_editor(self, panel, sizer):
        np.GridProperty.create_editor(self, panel, sizer)
        self.grid.AutoSizeColumn(0, False)
        self.grid.AutoSizeColumn(1, False)

    def set_value_dict(self, values_dict):
        default_events = set()
        for row in self.value:
            row[1] = values_dict.get(row[0], "")
            default_events.add(row[0])
        # handle non-default events
        for key, value in values_dict.items():
            if key in default_events: continue
            self.value.append([key,value])
        self.update_display()

    def write(self, output, tabs):
        inner_xml = []
        for event, handler in self.get():
            if handler:
                inner_xml += common.format_xml_tag('handler', handler, tabs+1, event=event)
        if inner_xml:
            output.extend( common.format_xml_tag(u'events', inner_xml, tabs, is_xml=True) )



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
            #events = list( config.widget_config[self.klass]['events'].keys() )
            events = list( config.widget_config[self.WX_CLASS]['events'].keys() )
            if 'default' in events: events.remove('default')
            events.sort()
        except KeyError:
            events = []  # no default handler

        # create Property
        self.events = EventsProperty(events) if events is not None else None

    def get_property_handler(self, name):
        if name == 'events':
            return EventsPropertyHandler(self)
        return None
