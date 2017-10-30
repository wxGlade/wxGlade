"""\
wxComboBox widget configuration

@copyright: 2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxRadioBox',
    'style_defs': {
        'wxRA_SPECIFY_ROWS': {
            'desc': _('The major dimension parameter refers to the maximum number of rows.'),
            'exclude': 'wxRA_SPECIFY_COLS',
        },
        'wxRA_SPECIFY_COLS': {
            'desc': _('The major dimension parameter refers to the maximum number of columns.'),
            'exclude': 'wxRA_SPECIFY_ROWS',
        },
    },
    'style_list': ['wxRA_SPECIFY_ROWS', 'wxRA_SPECIFY_COLS'],
    'events': {
        'EVT_RADIOBOX': {},
    },
}




