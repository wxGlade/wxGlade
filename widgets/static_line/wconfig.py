"""\
wxStaticLine widget configuration

@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxStaticLine',
    'style_defs': {
        'wxLI_HORIZONTAL': {
            'desc': _('Creates a horizontal line.'),
        },
        'wxLI_VERTICAL': {
            'desc': _('Creates a vertical line.'),
        },
    },
    'box_label': _('Style'),
    'default_style': 'wxLI_HORIZONTAL',
    'style_list': ['wxLI_HORIZONTAL', 'wxLI_VERTICAL']
}
