"""\
wxChoice widget configuration

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxChoice',
    'style_defs': {
        'wxCB_SORT': {
            'desc': _('Sorts the entries alphabetically.'),
            'supported_by': ('wx3',)
            },
    },
    'style_list': ['wxCB_SORT'],
    'events': {
        'EVT_CHOICE': {},
    },
}



