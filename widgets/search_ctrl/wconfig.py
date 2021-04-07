"""\
wxSearchCtrl widget configuration

@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSearchCtrl',
    'style_defs': {
        'wxTE_PROCESS_ENTER': {
            'desc': _('The control will generate the event wxEVT_TEXT_ENTER (otherwise pressing Enter key is '
                      'either processed internally by the control or used for navigation between dialog controls).'),
        },
        'wxTE_PROCESS_TAB': {
            'desc': _('The control will receive wxEVT_CHAR events for TAB '
                      'pressed - normally, TAB is used for passing to the '
                      'next control in a dialog instead. For the control '
                      'created with this style, you can still use '
                      'Ctrl-Enter to pass to the next control from the '
                      'keyboard.'),
        },
        'wxTE_NOHIDESEL': {
            'desc': _("By default, the Windows text control doesn't show "
                      "the selection when it doesn't have focus - use this "
                      "style to force it to always show it. It doesn't do "
                      "anything under other platforms."),
        },
        'wxTE_LEFT': {
            'desc': _('The text in the control will be left-justified (default).'),
        },
        'wxTE_CENTRE': {
            'desc': _('The text in the control will be centered (currently wxMSW and wxGTK2 only).'),
        },
        'wxTE_RIGHT': {
            'desc': _('The text in the control will be right-justified (currently wxMSW and wxGTK2 only).'),
        },
    },
    'style_list': ['wxTE_PROCESS_ENTER', 'wxTE_PROCESS_TAB',
                   'wxTE_NOHIDESEL', 'wxTE_LEFT', 'wxTE_CENTRE', 'wxTE_RIGHT'],
    'events': {
        'EVT_TEXT': {},
        'EVT_TEXT_ENTER': {},
        'EVT_TEXT_MAXLEN': {},  # XXX?
        'EVT_SEARCHCTRL_SEARCH_BTN': {},
        'EVT_SEARCHCTRL_CANCEL_BTN': {},
    },
}

