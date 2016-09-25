"""\
wxPanel widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxPanel',
    'style_defs': {},
    'box_label': _('Style'),
    'style_list': ['wxSIMPLE_BORDER', 'wxBORDER_SIMPLE',
                   'wxDOUBLE_BORDER', 'wxBORDER_DOUBLE',
                   'wxSUNKEN_BORDER', 'wxBORDER_SUNKEN',
                   'wxRAISED_BORDER', 'wxBORDER_RAISED',
                   'wxSTATIC_BORDER', 'wxBORDER_STATIC',
                   'wxNO_BORDER', 'wxBORDER_NONE',
                   'wxNO_3D', 'wxTAB_TRAVERSAL', 'wxWANTS_CHARS',
                   'wxNO_FULL_REPAINT_ON_RESIZE', 'wxFULL_REPAINT_ON_RESIZE',
                   'wxCLIP_CHILDREN'],
    'events': {
        'EVT_NAVIGATION_KEY': {
            'type': 'wxNavigationKeyEvent',
            'supported_by': ('wx3',),
        }
    },
}
