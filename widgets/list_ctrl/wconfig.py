"""\
wxListCtrl widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxListCtrl',
    'style_defs': {
        'wxLC_LIST': {
            'desc': _("Multicolumn list view, with optional small icons. Columns are computed automatically, i.e. "
                      "you don't set columns as in wxLC_REPORT. In other words, the list wraps, unlike a wxListBox."),
            'exclude': 'wxLC_REPORT|wxLC_VIRTUAL|wxLC_ICON|wxLC_SMALL_ICON',
        },
        'wxLC_REPORT': {
            'desc': _('Single or multicolumn report view, with optional header.'),
            'exclude': 'wxLC_LIST|wxLC_ICON|wxLC_SMALL_ICON',
        },
        'wxLC_VIRTUAL': {
            'desc': _('The application provides items text on demand. May only be used with wxLC_REPORT.'),
            'disabled': 'wxLC_LIST|wxLC_ICON|wxLC_SMALL_ICON',
        },
        'wxLC_ICON': {
            'desc': _('Large icon view, with optional labels.'),
            'exclude': 'wxLC_LIST|wxLC_REPORT|wxLC_VIRTUAL|wxLC_SMALL_ICON',
        },
        'wxLC_SMALL_ICON': {
            'desc': _('Small icon view, with optional labels.'),
            'exclude': 'wxLC_LIST|wxLC_REPORT|wxLC_VIRTUAL|wxLC_ICON',
        },
        'wxLC_ALIGN_TOP': {
            'desc': _('Icons align to the top. Win32 default, Win32 only.')
        },
        'wxLC_ALIGN_LEFT': {
            'desc': _('Icons align to the left.')
        },
        'wxLC_AUTOARRANGE': {
            'desc': _('Icons arrange themselves. Win32 only.')
        },
        'wxLC_EDIT_LABELS': {
            'desc': _('Labels are editable: the application will be notified when editing starts.')
        },
        'wxLC_NO_HEADER': {
            'desc': _('No header in report mode.')
        },
        'wxLC_SINGLE_SEL': {
            'desc': _('Single selection (default is multiple).')
        },
        'wxLC_SORT_ASCENDING': {
            'desc':_('Sort in ascending order. (You must still supply a comparison callback in wxListCtrl::SortItems.)')
        },
        'wxLC_SORT_DESCENDING': {
            'desc': _('Sort in descending order. (You must still supply a '
                      'comparison callback in wxListCtrl::SortItems.)')
        },
        'wxLC_HRULES': {
            'desc': _('Draws light horizontal rules between rows in report mode.')
        },
        'wxLC_VRULES': {
            'desc': _('Draws light vertical rules between columns in report mode.')
        },
    },
    'default_style': 'wxLC_ICON',
    'style_list': ['wxLC_LIST', 'wxLC_REPORT', 'wxLC_VIRTUAL', 'wxLC_ICON',
                   'wxLC_SMALL_ICON', 'wxLC_ALIGN_TOP', 'wxLC_ALIGN_LEFT',
                   'wxLC_AUTOARRANGE', 'wxLC_EDIT_LABELS', 'wxLC_NO_HEADER',
                   'wxLC_SINGLE_SEL', 'wxLC_SORT_ASCENDING',
                   'wxLC_SORT_DESCENDING', 'wxLC_HRULES', 'wxLC_VRULES',

                   'wxBORDER_DEFAULT',
                   'wxSIMPLE_BORDER', 'wxBORDER_SIMPLE',
                   'wxSUNKEN_BORDER', 'wxBORDER_SUNKEN',
                   'wxDOUBLE_BORDER', 'wxBORDER_DOUBLE',
                   'wxRAISED_BORDER', 'wxBORDER_RAISED',
                   'wxSTATIC_BORDER', 'wxBORDER_STATIC',
                   'wxBORDER_THEME',
                   'wxNO_BORDER', 'wxBORDER_NONE',

                   'wxWANTS_CHARS',
                   'wxFULL_REPAINT_ON_RESIZE'],
    'events': {
        'default': {
            'type': 'wxListEvent',
        },
        'EVT_LIST_BEGIN_DRAG': {},
        'EVT_LIST_BEGIN_RDRAG': {},
        'EVT_LIST_BEGIN_LABEL_EDIT': {},
        'EVT_LIST_END_LABEL_EDIT': {},
        'EVT_LIST_DELETE_ITEM': {},
        'EVT_LIST_DELETE_ALL_ITEMS': {},
        'EVT_LIST_ITEM_SELECTED': {},
        'EVT_LIST_ITEM_DESELECTED': {},
        'EVT_LIST_ITEM_ACTIVATED': {},
        'EVT_LIST_ITEM_FOCUSED': {},
        'EVT_LIST_ITEM_MIDDLE_CLICK': {},
        'EVT_LIST_ITEM_RIGHT_CLICK': {},
        'EVT_LIST_KEY_DOWN': {},
        'EVT_LIST_INSERT_ITEM': {},
        'EVT_LIST_COL_CLICK': {},
        'EVT_LIST_COL_RIGHT_CLICK': {},
        'EVT_LIST_COL_BEGIN_DRAG': {},
        'EVT_LIST_COL_DRAGGING': {},
        'EVT_LIST_COL_END_DRAG': {},
        'EVT_LIST_CACHE_HINT': {},
    },
}




