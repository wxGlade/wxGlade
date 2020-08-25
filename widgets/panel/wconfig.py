"""\
wxPanel widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxPanel',
    'style_defs': {
        'wxHSCROLL': {'obsolete':True},
        'wxVSCROLL': {'obsolete':True},
    },
    'style_list': ['wxSIMPLE_BORDER', 'wxBORDER_SIMPLE',
                   'wxDOUBLE_BORDER', 'wxBORDER_DOUBLE',
                   'wxSUNKEN_BORDER', 'wxBORDER_SUNKEN',
                   'wxRAISED_BORDER', 'wxBORDER_RAISED',
                   'wxSTATIC_BORDER', 'wxBORDER_STATIC',
                   'wxBORDER_THEME',
                   'wxNO_BORDER', 'wxBORDER_NONE',
                   'wxTAB_TRAVERSAL', 'wxWANTS_CHARS',
                   'wxFULL_REPAINT_ON_RESIZE',
                   'wxCLIP_CHILDREN'],
    'events': {
        'EVT_NAVIGATION_KEY': {
            'type': 'wxNavigationKeyEvent',
            'supported_by': ('wx3',),
        }
    },
}
