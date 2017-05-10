"""\
wxListBox widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxListBox',
    'style_defs': {
        'wxLB_SINGLE': {
            'desc': _('Single-selection list.'),
            'exclude': 'wxLB_EXTENDED|wxLB_MULTIPLE',
        },
        'wxLB_MULTIPLE': {
            'desc': _('Multiple-selection list: the user can toggle multiple '
                      'items on and off. This is the same as wxLB_EXTENDED '
                      'in wxGTK2 port.'),
            'exclude': 'wxLB_EXTENDED|wxLB_SINGLE',
        },
        'wxLB_EXTENDED': {
            'desc': _('Extended-selection list: the user can extend the '
                      'selection by using SHIFT or CTRL keys together with '
                      'the cursor movement keys or the mouse.'),
            'exclude': 'wxLB_SINGLE|wxLB_MULTIPLE',
        },
        'wxLB_HSCROLL': {
            'desc': _('Create horizontal scrollbar if contents are too wide '
                      '(Windows only).'),
        },
        'wxLB_ALWAYS_SB': {
            'desc': _('Always show a vertical scrollbar.'),
        },
        'wxLB_NEEDED_SB': {
            'desc': _('Only create a vertical scrollbar if needed.'),
        },
        'wxLB_NO_SB': {
            'desc': _("Don't create vertical scrollbar (wxMSW only)."),
            'supported_by': ('wx3',),
        },
        'wxLB_SORT': {
            'desc': _('The listbox contents are sorted in alphabetical order.')
        },
    },
    'default_style': 'wxLB_SINGLE',
    'style_list': ['wxLB_SINGLE', 'wxLB_MULTIPLE', 'wxLB_EXTENDED',
                   'wxLB_HSCROLL', 'wxLB_ALWAYS_SB', 'wxLB_NEEDED_SB',
                   'wxLB_NO_SB', 'wxLB_SORT'],
    'events': {
        'EVT_LISTBOX': {},
        'EVT_LISTBOX_DCLICK': {},
    },
}



