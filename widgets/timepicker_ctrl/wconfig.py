"""\
wxTimePickerCtrl widget configuration

@copyright: 2023 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxTimePickerCtrl',
    'style_defs': {
        'wxTP_DEFAULT': {
            'desc': _('Default style for time picker. Actually, no styles are supported.'),
        },
    },
    'default_style': 'wxTP_DEFAULT',
    'style_list': ['wxTP_DEFAULT'],
    'events': {
        'EVT_TIME_CHANGED': {'type': 'wxDateEvent',},
    },
}





