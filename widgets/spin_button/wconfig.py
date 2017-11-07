"""\
wxSpinButton widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSpinButton',
    'style_defs': {
        'wxSP_HORIZONTAL': {
            'desc': _('Specifies a horizontal spin button (note that this style is not supported in wxGTK).'),
            'exclude': 'wxSP_VERTICAL',
        },
        'wxSP_VERTICAL': {
            'desc': _('Specifies a vertical spin button.'),
            'exclude': 'wxSP_HORIZONTAL',
        },
        'wxSP_ARROW_KEYS': {
            'desc': _('The user can use arrow keys to change the value.'),
        },
        'wxSP_WRAP': {
            'desc': _('The value wraps at the minimum and maximum.'),
        },
    },
    'default_style': 'wxSP_VERTICAL',
    'style_list': ['wxSP_HORIZONTAL', 'wxSP_VERTICAL', 'wxSP_ARROW_KEYS', 'wxSP_WRAP'],
    'events': {
        'default': {
            'type': 'wxSpinEvent',
        },
        'EVT_SPIN': {},
        'EVT_SPIN_UP': {},
        'EVT_SPIN_DOWN': {},
    },
}
