"""\
wxSplitterWindow widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxToolBar',
    'style_defs': {
        'wxTB_FLAT':       {'desc':_('Gives the toolbar a flat look (Windows and GTK only).')},
        'wxTB_DOCKABLE':   {'desc':_('Makes the toolbar floatable and dockable (GTK only).')},
        
        'wxTB_HORIZONTAL': {'desc':_('Specifies horizontal layout (default).\n(alias: wxTB_TOP)'),
                            'exclude':'wxTB_VERTICAL|wxTB_BOTTOM|wxTB_RIGHT'},
        'wxTB_VERTICAL':   {'desc':_('Specifies vertical layout.\n(alias: wxTB_LEFT)'),
                            'exclude':'wxTB_HORIZONTAL|wxTB_BOTTOM|wxTB_RIGHT|wxTB_DEFAULT_STYLE'},
        'wxTB_BOTTOM':     {'desc':_('Align the toolbar at the bottom of parent window.'),
                            'exclude':'wxTB_HORIZONTAL|wxTB_VERTICAL|wxTB_RIGHT|wxTB_DEFAULT_STYLE'
                            },
        'wxTB_RIGHT':      {'desc':_('Align the toolbar at the right side of parent window.'),
                            'exclude':'wxTB_HORIZONTAL|wxTB_VERTICAL|wxTB_BOTTOM|wxTB_DEFAULT_STYLE'},

        'wxTB_TEXT':       {'desc':_('Shows the text in the toolbar buttons; by default only icons are shown.')},
        'wxTB_NOICONS':    {'desc':_('Specifies no icons in the toolbar buttons; by default they are shown.')},
        'wxTB_NODIVIDER':  {'desc':_('Specifies no divider (border) above the toolbar (Windows only)')},
        'wxTB_NOALIGN':    {'desc':_('Specifies no alignment with the parent window (Windows only, not very useful).')},
        'wxTB_HORZ_LAYOUT':{'desc':_('Shows the text and the icons alongside, not vertically stacked '
                                     '(Windows and GTK 2 only). This style must be used with wxTB_TEXT.')},
        'wxTB_HORZ_TEXT':  {'desc':_('Combination of wxTB_HORZ_LAYOUT and wxTB_TEXT.'),
                            'combination': 'wxTB_HORZ_LAYOUT|wxTB_TEXT' },
        'wxTB_NO_TOOLTIPS':{'desc':_("Don't show the short help tooltips for the tools when the mouse hovers over them.")},

        'wxTB_LEFT':       {'rename_to':'wxTB_VERTICAL'},
        'wxTB_TOP':        {'rename_to':'wxTB_HORIZONTAL'},

        'wxTB_DEFAULT_STYLE': {
            'desc': _('Flags that are closest to the native look. Currently identical to wxTB_HORIZONTAL.\n'
                      'This style is new since wxWidgets 2.9.5.'),
            'combination': 'wxTB_HORIZONTAL',
            'exclude':'wxTB_VERTICAL|wxTB_BOTTOM|wxTB_RIGHT',
            'supported_by': ('wx3',) },
        'wxTB_3DBUTTONS':  {'desc':_('show 3D buttons (wxToolBarSimple only)')},
    },
    'style_list': ['wxTB_DEFAULT_STYLE', 'wxTB_FLAT', 'wxTB_DOCKABLE',
                   'wxTB_HORIZONTAL', 'wxTB_VERTICAL', 'wxTB_BOTTOM', 'wxTB_RIGHT', 'wxTB_TEXT',
                   'wxTB_NOICONS', 'wxTB_NODIVIDER', 'wxTB_NOALIGN', 'wxTB_HORZ_LAYOUT', 'wxTB_HORZ_TEXT',
                   'wxTB_NO_TOOLTIPS', 'wxTB_3DBUTTONS']
}
