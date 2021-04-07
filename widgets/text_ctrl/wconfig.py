"""\
wxTextCtrl widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep synchronous between wxSpinCtrl and wxTextCtrl
config = {
    'wxklass': 'wxTextCtrl',
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
        'wxTE_MULTILINE': {
            'desc': _('The text control allows multiple lines. If this '
                      'style is not specified, line break characters '
                      'should not be used in the controls value.'),
        },
        'wxTE_PASSWORD': {
            'desc': _('The text will be echoed as asterisks.'),
        },
        'wxTE_READONLY': {
            'desc': _('The text will not be user-editable.'),
        },
        'wxTE_RICH': {
            'desc': _('Use rich text control under Win32, this allows to '
                      'have more than 64KB of text in the control even '
                      'under Win9x. This style is ignored under other '
                      'platforms.'),
        },
        'wxTE_RICH2': {
            'desc': _('Use rich text control version 2.0 or 3.0 under '
                      'Win32, this style is ignored under other platforms'),
        },
        'wxTE_AUTO_URL': {
            'desc': _('Highlight the URLs and generate the wxTextUrlEvents '
                      'when mouse events occur over them. This style is '
                      'only supported for wxTE_RICH Win32 and multi-line '
                      'wxGTK2 text controls.'),
        },
        'wxTE_NOHIDESEL': {
            'desc': _("By default, the Windows text control doesn't show "
                      "the selection when it doesn't have focus - use this "
                      "style to force it to always show it. It doesn't do "
                      "anything under other platforms."),
        },
        'wxHSCROLL': {
            'desc': _("A horizontal scrollbar will be created and used, so "
                      "that text won't be wrapped. No effect under wxGTK1."),
        },
        'wxTE_NO_VSCROLL': {
            'desc': _('For multiline controls only: vertical scrollbar will '
                      'never be created. This limits the amount of text '
                      'which can be entered into the control to what can '
                      'be displayed in it under MSW but not under GTK2. '
                      'Currently not implemented for the other platforms.'),
            'supported_by': ('wx3',),
        },
        'wxTE_LEFT': {
            'desc': _('The text in the control will be left-justified (default).'),
            'exclude': 'wxTE_CENTRE|wxTE_RIGHT',
        },
        'wxTE_CENTRE': {
            'desc': _('The text in the control will be centered (currently wxMSW and wxGTK2 only).'),
            'exclude': 'wxTE_LEFT|wxTE_RIGHT',
        },
        'wxTE_RIGHT': {
            'desc': _('The text in the control will be right-justified (currently wxMSW and wxGTK2 only).'),
            'exclude': 'wxTE_LEFT|wxTE_CENTRE',
        },
        'wxTE_DONTWRAP': {
            'desc': _("Same as wxHSCROLL style: don't wrap at all, show horizontal scrollbar instead."),
        },
        'wxTE_LINEWRAP': {
            'desc': _('Wrap the lines too long to be shown entirely at any position (wxUnix and wxGTK2 only).'),
            'supported_by': ('wx2',),
            'rename_to': 'wxTE_CHARWRAP',
        },
        'wxTE_CHARWRAP': {
            'desc': _('Wrap the lines too long to be shown entirely at any position (wxUnix and wxGTK2 only).'),
        },
        'wxTE_WORDWRAP': {
            'desc': _('Wrap the lines too long to be shown entirely at word boundaries (wxUnix and wxGTK2 only).'),
        },
        'wxTE_BESTWRAP': {
            'desc': _('Wrap the lines at word boundaries or at any other character if there are words longer than '
                      'the window width (this is the default).'),
        },
    },
    'style_list': ['wxTE_PROCESS_ENTER', 'wxTE_PROCESS_TAB',
                   'wxTE_MULTILINE', 'wxTE_PASSWORD', 'wxTE_READONLY',
                   'wxTE_RICH', 'wxTE_RICH2', 'wxTE_AUTO_URL',
                   'wxTE_NOHIDESEL', 'wxHSCROLL', 'wxTE_NO_VSCROLL',
                   'wxTE_LEFT', 'wxTE_CENTRE', 'wxTE_RIGHT',
                   'wxTE_DONTWRAP', 'wxTE_LINEWRAP', 'wxTE_CHARWRAP', 'wxTE_WORDWRAP', 'wxTE_BESTWRAP',
                   'wxBORDER_NONE'],
    'events': {
        'EVT_TEXT': {},
        'EVT_TEXT_ENTER': {},
        'EVT_TEXT_URL': {},
        'EVT_TEXT_MAXLEN': {},
    },
}

