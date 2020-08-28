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
    },
    'style_list': ['wxRB_GROUP', 'wxRB_SINGLE'],
    'events': {
        'EVT_RADIOBUTTON': {},
    },
}




