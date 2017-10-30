"""\
wxGauge widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxGauge',
    'style_defs': {
        'wxGA_HORIZONTAL': {
            'desc': _('Creates a horizontal gauge.'),
            'exclude': 'wxGA_VERTICAL',
        },
        'wxGA_VERTICAL': {
            'desc': _('Creates a vertical gauge. Not supported under Mac OS.'),
            'exclude': 'wxGA_HORIZONTAL',
        },

        'wxGA_SMOOTH': {
            'desc': _('Creates smooth progress bar with one pixel wide '
                      'update step (not supported by all platforms).'),
        },
        'wxGA_PROGRESSBAR': {
            'desc': _('Under Windows 95, creates a horizontal progress bar.'),
            'supported_by': ('wx2',),
            'obsolete':_('obsolete')
        },
    },
    'default_style': 'wxGA_HORIZONTAL',
    'style_list': ['wxGA_HORIZONTAL', 'wxGA_VERTICAL', 'wxGA_PROGRESSBAR', 'wxGA_SMOOTH'],
}


