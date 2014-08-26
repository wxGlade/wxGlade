"""\
wxFrame widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxFrame',
    'styles': {
        'wxRESIZE_BOX': {
            'desc': 'deprecated since wx28',
            'alternative': 'wxMAXIMIZE_BOX',
        },
        'wxDEFAULT_FRAME_STYLE': {
          'desc': 'Default frame style',
          'combination': 'wxMINIMIZE_BOX|wxMAXIMIZE_BOX|wxRESIZE_BORDER|'
                         'wxSYSTEM_MENU|wxCAPTION|wxCLOSE_BOX|'
                         'wxCLIP_CHILDREN',
        },
    },
}
