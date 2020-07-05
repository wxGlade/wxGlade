"""\
wxGenericCalendarCtrl widget configuration

@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxGenericCalendarCtrl',
    'supported_by': ('wx3',),
    'style_defs': {
        'wxCAL_SUNDAY_FIRST': {
            'desc': _('Show Sunday as the first day in the week'),
            'exclude': 'wxCAL_MONDAY_FIRST'
        },
        'wxCAL_MONDAY_FIRST': {
            'desc': _('Show Monday as the first day in the week'),
            'exclude': 'wxCAL_SUNDAY_FIRST'
        },
        'wxCAL_SHOW_HOLIDAYS': {
            'desc': _('Highlight holidays in the calendar (only generic)')
        },
        'wxCAL_NO_YEAR_CHANGE': {
            'desc': _('Disable the year changing')
        },
        'wxCAL_NO_MONTH_CHANGE': {
            'desc': _('Disable the month (and, implicitly, the year) changing')
        },
        'wxCAL_SHOW_SURROUNDING_WEEKS': {
            'desc': _('Show the neighbouring weeks in the previous and next months')
        },
        'wxCAL_SEQUENTIAL_MONTH_SELECTION': {
            'desc': _('Use alternative, more compact, style for the month and year selection controls.')
        },
    },
    'default_style': 'wxCAL_SHOW_HOLIDAYS',
    'style_list': ['wxCAL_SUNDAY_FIRST', 'wxCAL_MONDAY_FIRST',
                   'wxCAL_SHOW_HOLIDAYS', 'wxCAL_NO_YEAR_CHANGE',
                   'wxCAL_NO_MONTH_CHANGE', 'wxCAL_SHOW_SURROUNDING_WEEKS',
                   'wxCAL_SEQUENTIAL_MONTH_SELECTION'],
    'events': {
        'default': {
            'type': 'wxCalendarEvent',
        },
        'EVT_CALENDAR': {},
        'EVT_CALENDAR_SEL_CHANGED': {},
        'EVT_CALENDAR_DAY': {},
        'EVT_CALENDAR_MONTH': {},
        'EVT_CALENDAR_YEAR': {},
        'EVT_CALENDAR_WEEKDAY_CLICKED': {},
    },
}
