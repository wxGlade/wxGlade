"""\
wxCheckBox widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxCheckBox',
    'style_defs': {
        'wxCHK_2STATE': {
            'desc': _('Create a 2-state checkbox. This is the default.'),
            'exclude': 'wxCHK_3STATE',
        },
        'wxCHK_3STATE': {
            'desc': _('Create a 3-state checkbox. Not implemented in wxOS2 '
                      'and wxGTK built against GTK+ 1.2.'),
            'exclude': 'wxCHK_2STATE',
        },
        'wxCHK_ALLOW_3RD_STATE_FOR_USER': {
            'desc': _("By default a user can't set a 3-state checkbox to the "
                      "third state. It can only be done from code. Using "
                      "this flags allows the user to set the checkbox to "
                      "the third state by clicking."),
            'require': 'wxCHK_3STATE',
        },
        'wxALIGN_RIGHT': {
            'desc': _('Makes the text appear on the left of the checkbox.')
        }
    },
    'style_list': ['wxCHK_2STATE', 'wxCHK_3STATE',
                   'wxCHK_ALLOW_3RD_STATE_FOR_USER', 'wxALIGN_RIGHT'],

    # mapping for selected values to checkbox states (wxCheckBoxState)
    'number2state': {
        0: 'wxCHK_UNCHECKED',
        1: 'wxCHK_CHECKED',
        2: 'wxCHK_UNDETERMINED',
    },
    'events': {
        'EVT_CHECKBOX': {},
    },
}




