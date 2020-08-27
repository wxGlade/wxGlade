"""\
wxStaticBitmap widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxStaticBitmap',
    'style_defs': {
    },
    'style_list': ['wxSIMPLE_BORDER', 'wxBORDER_SIMPLE',
                   'wxSUNKEN_BORDER', 'wxBORDER_SUNKEN',
                   'wxRAISED_BORDER', 'wxBORDER_RAISED',
                   'wxSTATIC_BORDER', 'wxBORDER_STATIC',
                   'wxBORDER_THEME',
                   'wxDOUBLE_BORDER', 'wxBORDER_DOUBLE',
                   'wxNO_BORDER',     'wxBORDER_NONE',
                   'wxTAB_TRAVERSAL', 'wxWANTS_CHARS',
                   'wxFULL_REPAINT_ON_RESIZE', 'wxCLIP_CHILDREN'],
}
