"""\
wxTreeCtrl widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxTreeCtrl',
    'style_defs': {
        'wxTR_EDIT_LABELS': {
            'desc': _('Use this style if you wish the user to be able to '
                      'edit labels in the tree control.'),
        },
        'wxTR_NO_BUTTONS': {
            'desc': _('For convenience to document that no buttons are to '
                      'be drawn.'),
        },
        'wxTR_HAS_BUTTONS': {
            'desc': _('Use this style to show + and - buttons to the left '
                      'of parent items.'),
        },
        'wxTR_TWIST_BUTTONS': {
            'desc': _('Selects alternative style of +/- buttons and shows '
                      'rotating ("twisting") arrows instead. Currently '
                      'this style is only implemented under Microsoft '
                      'Windows Vista and later Windows versions and is '
                      'ignored under the other platforms. Notice that '
                      'under Vista this style results in the same '
                      'appearance as used by the tree control in Explorer '
                      'and other built-in programs and so using it may '
                      'be preferable to the default style.'),
        },
        'wxTR_NO_LINES': {
            'desc': _('Use this style to hide vertical level connectors.'),
        },
        'wxTR_FULL_ROW_HIGHLIGHT': {
            'desc': _('Use this style to have the background colour and '
                      'the selection highlight extend over the entire '
                      'horizontal row of the tree control window. (This '
                      'flag is ignored under Windows unless you specify '
                      'wxTR_NO_LINES as well.)'),
            },
        'wxTR_LINES_AT_ROOT': {
            'desc': _('Use this style to show lines between root nodes. '
                      'Only applicable if wxTR_HIDE_ROOT is set and '
                      'wxTR_NO_LINES is not set.'),
        },
        'wxTR_HIDE_ROOT': {
            'desc': _('Use this style to suppress the display of the root '
                      'node, effectively causing the first-level nodes '
                      'to appear as a series of root nodes.'),
        },
        'wxTR_ROW_LINES': {
            'desc': _('Use this style to draw a contrasting border '
                      'between displayed rows.'),
        },
        'wxTR_HAS_VARIABLE_ROW_HEIGHT': {
            'desc': _('Use this style to cause row heights to be just '
                      'big enough to fit the content. If not set, all '
                      'rows use the largest row height. The default is '
                      'that this flag is unset. Generic only.'),
        },
        'wxTR_SINGLE': {
            'desc': _('For convenience to document that only one item may '
                      'be selected at a time. Selecting another item '
                      'causes the current selection, if any, to be '
                      'deselected. This is the default.'),
        },
        'wxTR_MULTIPLE': {
            'desc': _('Use this style to allow a range of items to be '
                      'selected. If a second range is selected, the '
                      'current range, if any, is deselected.'),
        },
        'wxTR_EXTENDED': {
            'desc': _('Use this style to allow disjoint items to be '
                      'selected. (Only partially implemented; may not '
                      'work in all cases.)'),
            'supported_by': ('wx2',),
        },
        'wxTR_DEFAULT_STYLE': {
            'desc': _('The set of flags that are closest to the defaults '
                      'for the native control for a particular toolkit.'),
        },
    },
    'default_style': 'wxTR_HAS_BUTTONS',
    'style_list': ['wxTR_EDIT_LABELS', 'wxTR_NO_BUTTONS', 'wxTR_HAS_BUTTONS',
                   'wxTR_TWIST_BUTTONS', 'wxTR_NO_LINES',
                   'wxTR_FULL_ROW_HIGHLIGHT', 'wxTR_LINES_AT_ROOT',
                   'wxTR_HIDE_ROOT', 'wxTR_ROW_LINES',
                   'wxTR_HAS_VARIABLE_ROW_HEIGHT', 'wxTR_SINGLE',
                   'wxTR_MULTIPLE', 'wxTR_EXTENDED', 'wxTR_DEFAULT_STYLE',
                   'wxSIMPLE_BORDER', 'wxBORDER_SIMPLE',
                   'wxSUNKEN_BORDER', 'wxBORDER_SUNKEN',
                   'wxRAISED_BORDER', 'wxBORDER_RAISED',
                   'wxSTATIC_BORDER', 'wxBORDER_STATIC',
                   'wxDOUBLE_BORDER', 'wxBORDER_DOUBLE',
                   'wxNO_BORDER',     'wxBORDER_NONE',
                   'wxWANTS_CHARS',
                   'wxFULL_REPAINT_ON_RESIZE'],
    'events': {
        'default': {
            'type': 'wxTreeEvent',
        },
        'EVT_TREE_BEGIN_DRAG': {},
        'EVT_TREE_BEGIN_RDRAG': {},
        'EVT_TREE_END_DRAG': {},
        'EVT_TREE_END_RDRAG': {},
        'EVT_TREE_BEGIN_LABEL_EDIT': {},
        'EVT_TREE_END_LABEL_EDIT': {},
        'EVT_TREE_DELETE_ITEM': {},
        'EVT_TREE_GET_INFO': {},
        'EVT_TREE_SET_INFO': {},
        'EVT_TREE_ITEM_ACTIVATED': {},
        'EVT_TREE_ITEM_COLLAPSED': {},
        'EVT_TREE_ITEM_COLLAPSING': {},
        'EVT_TREE_ITEM_EXPANDED': {},
        'EVT_TREE_ITEM_EXPANDING': {},
        'EVT_TREE_SEL_CHANGED': {},
        'EVT_TREE_SEL_CHANGING': {},
        'EVT_TREE_KEY_DOWN': {},
        'EVT_TREE_ITEM_GETTOOLTIP': {},
    },
}



