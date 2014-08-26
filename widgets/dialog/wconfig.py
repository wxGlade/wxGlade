"""\
wxDialog widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxDialog',
    'styles': {
        'wxRESIZE_BOX': {
            'desc': 'deprecated since wx28',
            'alternative': 'wxMAXIMIZE_BOX',
        },
        'wxTHICK_FRAME': {
            'desc': 'deprecated since wx28',
            'alternative': 'wxRESIZE_BORDER',
        },
    },
}

