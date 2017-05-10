"""\
wxStaticLine widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxStaticLine',
    'style_defs': {
        'wxLI_HORIZONTAL': {
            'desc': _('Creates a horizontal line.'),
            'exclude': 'wxLI_VERTICAL',
        },
        'wxLI_VERTICAL': {
            'desc': _('Creates a vertical line.'),
            'exclude': 'wxLI_HORIZONTAL',
        },
    },
    'default_style': 'wxLI_HORIZONTAL',
    'style_list': ['wxLI_HORIZONTAL', 'wxLI_VERTICAL']
}
