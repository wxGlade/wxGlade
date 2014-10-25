"""\
wxSlider widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSlider',
    'style_defs': {
        'wxSL_HORIZONTAL': {
            'desc': _('Displays the slider horizontally (this is the '
                      'default).'),
        },
        'wxSL_VERTICAL': {
            'desc': _('Displays the slider vertically.'),
        },
        'wxSL_AUTOTICKS': {
            'desc': _('Displays tick marks. Windows only.'),
        },
        'wxSL_MIN_MAX_LABELS': {
            'desc': _('Displays minimum, maximum labels (new since '
                      'wxWidgets 2.9.1).'),
            'supported_by': ('wx3',),
        },
        'wxSL_VALUE_LABEL': {
            'desc': _('Displays value label (new since wxWidgets 2.9.1).'),
            'supported_by': ('wx3',),
        },
        'wxSL_LABELS': {
            'desc': _('Displays minimum, maximum and value labels (same as '
                      'wxSL_VALUE_LABEL and wxSL_MIN_MAX_LABELS together).'),
        },
        'wxSL_LEFT': {
            'desc': _('Displays ticks on the left and forces the slider '
                      'to be vertical.'),
        },
        'wxSL_RIGHT': {
            'desc': _('Displays ticks on the right and forces the slider '
                      'to be vertical.'),
        },
        'wxSL_TOP': {
            'desc': _('Displays ticks on the top.'),
        },
        'wxSL_BOTTOM': {
            'desc': _('Displays ticks on the bottom (this is the default).'),
        },
        'wxSL_SELRANGE': {
            'desc': _('Allows the user to select a range on the slider. '
                      'Windows only.'),
        },
        'wxSL_INVERSE': {
            'desc': _('Inverses the minimum and maximum endpoints on the '
                      'slider. Not compatible with wxSL_SELRANGE.'),
        },
    },
    'box_label': _('Style'),
    'default_style': 'wxSL_HORIZONTAL',
    'style_list': ['wxSL_HORIZONTAL', 'wxSL_VERTICAL',
                   'wxSL_AUTOTICKS',
                   'wxSL_MIN_MAX_LABELS', 'wxSL_VALUE_LABEL',

                   'wxSL_LABELS', 'wxSL_LEFT',
                   'wxSL_RIGHT', 'wxSL_TOP', 'wxSL_BOTTOM',
                   'wxSL_SELRANGE', 'wxSL_INVERSE'],
}


