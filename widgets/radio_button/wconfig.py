"""\
wxRadioButton widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxRadioButton',
    'style_defs': {
        'wxRB_GROUP': {
            'desc': _('Marks the beginning of a new group of radio buttons.'),
        },
        'wxRB_SINGLE': {
            'desc': _('In some circumstances, radio buttons that are not '
                      'consecutive siblings trigger a hang bug in Windows '
                      '(only). If this happens, add this style to mark the '
                      'button as not belonging to a group, and implement '
                      'the mutually-exclusive group behaviour yourself.'),
        },
        'wxRB_USE_CHECKBOX': {
            'desc': _('Use a checkbox button instead of radio button (currently supported only on PalmOS).'),
            'supported_by': ('wx2',),
        },
    },
    'style_list': ['wxRB_GROUP', 'wxRB_SINGLE', 'wxRB_USE_CHECKBOX'],
    'events': {
        'EVT_RADIOBUTTON': {},
    },
}




