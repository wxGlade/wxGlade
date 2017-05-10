"""\
wxSplitterWindow widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxSplitterWindow',
    'style_defs': {
        'wxSP_3D': {
            'desc': _('Draws a 3D effect border and sash'),
            'combination': 'wxSP_3DBORDER|wxSP_3DSASH',
        },
        'wxSP_THIN_SASH': {
            'desc': _('Draws a thin sash.'),
            'supported_by': ('wx3',),
        },
        'wxSP_3DSASH': {
            'desc': _('Draws a 3D effect sash.'),
        },
        'wxSP_3DBORDER': {
            'desc': _('Draws a standard border.'),
            'synonym': 'wxSP_BORDER',
            },
        'wxSP_BORDER': {
            'desc': _('Draws a standard border.'),
        },
        'wxSP_NOBORDER': {
            'desc': _('No border (default).'),
        },
        'wxSP_NO_XP_THEME': {
            'desc': _('Under Windows XP, switches off the attempt to draw '
                      'the splitter using Windows XP theming, so the '
                      'borders and sash will take on the pre-XP look.'),
        },
        'wxSP_PERMIT_UNSPLIT': {
            'desc': _('Always allow to unsplit, even with the minimum '
                      'pane size other than zero.'),
        },
        'wxSP_LIVE_UPDATE': {
            'desc': _("Don't draw XOR line but resize the child windows "
                      "immediately."),
        },

    },
    'default_style': 'wxSP_3D',
    'style_list': ['wxSP_3D', 'wxSP_THIN_SASH', 'wxSP_3DSASH',
                   'wxSP_3DBORDER', 'wxSP_BORDER',
                   'wxSP_NOBORDER', 'wxSP_PERMIT_UNSPLIT',
                   'wxSP_LIVE_UPDATE', 'wxCLIP_CHILDREN'],
    'events': {
        'default': {
            'type': 'wxSplitterEvent',
        },
        'EVT_SPLITTER_SASH_POS_CHANGING': {},
        'EVT_SPLITTER_SASH_POS_CHANGED': {},
        'EVT_SPLITTER_UNSPLIT': {},
        'EVT_SPLITTER_DCLICK': {},
    },
}
