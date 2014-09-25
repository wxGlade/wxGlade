"""\
wxSpinButton widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSpinButton',
    'style_defs': {
        'wxSP_HORIZONTAL': {
            'desc': _('Specifies a horizontal spin button (note that this '
                      'style is not supported in wxGTK).'),
        },
        'wxSP_VERTICAL': {
            'desc': _('Specifies a vertical spin button.'),
        },
        'wxSP_ARROW_KEYS': {
            'desc': _('The user can use arrow keys to change the value.'),
        },
        'wxSP_WRAP': {
            'desc': _('The value wraps at the minimum and maximum.'),
        },
    },
    'box_label': _('Style'),
    'style_list': ['wxSP_HORIZONTAL', 'wxSP_VERTICAL', 'wxSP_ARROW_KEYS',
                   'wxSP_WRAP'],
}
