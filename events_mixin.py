"""\
Mixin class for 'events' property

@copyright: 2002-2004 Alberto Griggio
@copyright: 2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx
import wx.grid

import re
import config

from widget_properties import GridProperty
from xml.sax.saxutils import escape, quoteattr
import common


class EventsProperty(GridProperty):
    """\
    Class EventsProperty

    @ivar _logger: Class specific logging instance
    """

    def __init__(self, owner):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        cols = [(_('Event'), GridProperty.STRING),
                (_('Handler'), GridProperty.STRING)]
        GridProperty.__init__(self, owner, 'events', None, cols,
                              len(owner.events), False, False, False,
                              label=_('Events'))
        self.validator_re = re.compile(r'^\s*[\w-]+\s*$')
        event_list = [name for name in owner.events]
        event_list.sort()
        self.set_value([[name, ''] for name in event_list])

    def display(self, parent):
        GridProperty.display(self, parent)
        attr = wx.grid.GridCellAttr()
        attr.SetReadOnly(True)
        self.grid.SetColAttr(0, attr)
        self.grid.AutoSizeColumn(0, False)
        self.grid.AutoSizeColumn(1, False)
        wx.grid.EVT_GRID_CELL_CHANGE(self.grid, self.on_change_val)
        szr = self.panel.GetSizer()
        szr.Show(self.btn_sizer, False)
        szr.Layout()

    def set_value_dict(self, values_dict):
        val = self.get_value()
        for row in val:
            row[1] = values_dict.get(row[0], "")
        self.set_value(val)

    def write(self, outfile, tabs):
        if self.getter:
            handlers = self.getter()
        else:
            handlers = self.owner[self.name][0]()
        if handlers:
            written = False
            write = outfile.write
            stab = '    ' * (tabs+1)
            for event, handler in handlers:
                if handler:
                    if not written:
                        written = True
                        write('    ' * tabs + '<events>\n')
                    write('%s<handler event=%s>%s</handler>\n' %
                          (stab, quoteattr(event), escape(handler.strip())))
            if written:
                write('    ' * tabs + '</events>\n')

    def on_change_val(self, event):
        val = self.get_value()
        for i in range(len(val)):
            handler = val[i][1].strip()
            if handler and self.validator_re.match(handler) is None:
                self.set_value(self.val)
                return event.Skip()
        GridProperty.on_change_val(self, event)

# end of class EventsProperty


class EventsPropertyHandler(object):
    """\
    Class EventsPropertyHandler

    @ivar curr_handler: Content of the current XML element
    @type curr_handler: list[str]

    @ivar handlers: Dictionary of event names and event handlers
    @type handlers: dict[str, str]
    """

    def __init__(self, owner):
        # initialise instance
        self.owner = owner
        self.handlers = {}
        self.event_name = None
        self.curr_handler = []
        
    def start_elem(self, name, attrs):
        if name == 'handler':
            self.event_name = attrs['event']

    def end_elem(self, name):
        if name == 'handler':
            if self.event_name and self.curr_handler:
                self.handlers[self.event_name] = ''.join(self.curr_handler)
            self.event_name = None
            self.curr_handler = []
        elif name == 'events':
            self.owner.properties['events'].set_value_dict(self.handlers)
            self.owner.set_events_dict(self.handlers)
            return True  # to remove this handler

    def char_data(self, data):
        data = data.strip()
        if data:
            self.curr_handler.append(data)

# end of class EventsPropertyHandler


class EventsMixin(object):
    """\
    Class mixin to handle events

    @ivar events: Events known by this widget
    @type events: list[str]

    @ivar handlers: Dictionary of event names and event handlers
    @type handlers: dict[str, str]
    """

    def __init__(self):
        self.handlers = {}
        try:
            self.events = config.widget_config[self.klass]['events'].keys()
            if 'default' in self.events:
                self.events.remove('default')
            self.events.sort()
        except KeyError:
            self.events = []                # no default handler

        if self.events:
            self.access_functions['events'] = self.get_events, self.set_events
            self.properties['events'] = EventsProperty(self)

    def get_events(self):
        """\
        Returns list of two-value tuples of event name and event handler

        @rtype: list[(str, str)]
        """
        ret = []
        for event_name in self.events:
            ret.append([event_name, self.handlers.get(event_name, '')])
        return ret

    def set_events(self, handlers_list):
        """\
        Replace the existing event handlers by new handlers

        @param handlers_list: List of two-value tuples of event name and
                              event handler
        @type handlers_list: list[(str, str)]
        """
        self.handlers = {}
        for event_name, event_handler in handlers_list:
            if event_handler.strip():
                self.handlers[event_name] = event_handler

    def set_events_dict(self, handlers):
        """\
        Replace the existing event handlers by new handlers

        @param handlers: Dictionary of event names and event handlers
        @type handlers: dict[str, str]
        """
        self.handlers = handlers

    def create_events_property(self):
        if not self.events:
            return
        panel = wx.Panel(self.notebook, -1) 
        self.properties['events'].display(panel)
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(self.properties['events'].panel, 1, wx.ALL|wx.EXPAND, 5)
        panel.SetSizerAndFit(sizer)
        self.notebook.AddPage(panel, _('Events'))

    def get_property_handler(self, name):
        if name == 'events':
            return EventsPropertyHandler(self)
        return None

# end of class EventsMixin
