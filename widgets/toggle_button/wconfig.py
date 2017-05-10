"""\
wxToggleButton widget configuration

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxToggleButton',
    'style_defs': {
        'wxBU_AUTODRAW': {
            'desc': _('If this is specified, the button will be drawn '
                      'automatically using the label bitmap only, providing '
                      'a 3D-look border. If this style is not specified, '
                      'the button will be drawn without borders and using '
                      'all provided bitmaps. WIN32 only.'),
            },
        'wxBU_BOTTOM': {
            'desc': _('Aligns the bitmap label to the bottom of the button. '
                      'WIN32 only.'),
            },
        'wxBU_LEFT': {
            'desc': _('Left-justifies the bitmap label. WIN32 only.'),
            },
        'wxBU_RIGHT': {
            'desc': _('Right-justifies the bitmap label. WIN32 only.'),
            },
        'wxBU_TOP': {
            'desc': _('Aligns the bitmap label to the top of the button. '
                      'WIN32 only.'),
            },
        'wxBU_EXACTFIT': {
            'desc': _('Creates the button as small as possible instead of '
                      'making it of the standard size (which is the default '
                      'behaviour ).'),
            },
        },
    'style_list': ['wxBU_AUTODRAW', 'wxBU_LEFT', 'wxBU_RIGHT', 'wxBU_TOP',
                   'wxBU_BOTTOM', 'wxBU_EXACTFIT', 'wxNO_BORDER'],
    'events': {
        'EVT_TOGGLEBUTTON': {},
    },
}




