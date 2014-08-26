"""\
wxSplitterWindow widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSplitterWindow',
    'styles': {
        'wxSP_3D': {
          'desc': 'Draws a 3D effect border and sash',
          'combination': 'wxSP_3DBORDER|wxSP_3DSASH',
        },
        'wxSP_3DBORDER': {
            'synonym': 'wxSP_BORDER',
        },
    },
}

