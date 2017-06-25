"""\
wxDatePickerCtrl widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxDatePickerCtrl',
    'style_defs': {
        'wxDP_SPIN': {
            'desc': _('Creates a control without a month calendar drop down but with spin-control-like arrows to change'
                      ' individual date components. This style is not supported by the generic version.'),
        },
        'wxDP_DROPDOWN': {
            'desc': _('Creates a control with a month calendar drop-down part from which the user can select a date.'),
        },
        'wxDP_DEFAULT': {
            'desc': _('Creates a control with the style that is best supported for the current platform (currently '
                      'wxDP_SPIN under Windows and wxDP_DROPDOWN elsewhere).'),
        },
        'wxDP_ALLOWNONE': {
            'desc': _('With this style, the control allows the user to not enter any valid date at all. Without it - '
                      'the default - the control always has some valid date.'),
        },
        'wxDP_SHOWCENTURY': {
            'desc': _('Forces display of the century in the default date format. Without this style the century could be '
                      'displayed, or not, depending on the default date representation in the system.'),
        },
    },
    'default_style': 'wxDP_DEFAULT|wxDP_SHOWCENTURY',
    'style_list': ['wxDP_SPIN', 'wxDP_DROPDOWN', 'wxDP_DEFAULT', 'wxDP_ALLOWNONE', 'wxDP_SHOWCENTURY'],
    'events': {
        'EVT_DATE_CHANGED': {'type': 'wxDateEvent',},
    },
}





