"""\
wxStatusBar widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxStatusBar',
    'style_defs': {
        'wxST_SIZEGRIP': {
            'desc': _('Displays a gripper at the right-hand side of the '
                      'status bar which can be used to resize the parent '
                      'window.'),
            'supported_by': ('wx28',),
        },
        'wxSTB_SIZEGRIP': {
            'desc': _('Displays a gripper at the right-hand side of the '
                      'status bar which can be used to resize the parent '
                      'window.'),
            'supported_by': ('wx3',),
        },
        'wxSTB_SHOW_TIPS': {
            'desc': _("Displays tooltips for those panes whose status text "
                      "has been ellipsized/truncated because the status "
                      "text doesn't fit the pane width. Note that this "
                      "style has effect only on wxGTK (with GTK+ >= 2.12) "
                      "currently."),
            'supported_by': ('wx3',),
        },
        'wxSTB_ELLIPSIZE_START': {
            'desc': _("Replace the beginning of the status texts with an "
                      "ellipsis when the status text widths exceed the "
                      "status bar pane's widths (uses "
                      "wxControl::Ellipsize)."),
            'supported_by': ('wx3',),
        },
        'wxSTB_ELLIPSIZE_MIDDLE': {
            'desc': _("Replace the middle of the status texts with an "
                      "ellipsis when the status text widths exceed the "
                      "status bar pane's widths (uses "
                      "wxControl::Ellipsize)."),
            'supported_by': ('wx3',),
        },
        'wxSTB_ELLIPSIZE_END': {
            'desc': _("Replace the end of the status texts with an ellipsis "
                      "when the status text widths exceed the status bar "
                      "pane's widths (uses wxControl::Ellipsize)."),
            'supported_by': ('wx3',),
        },
        'wxSTB_DEFAULT_STYLE': {
            'desc': _('The default style: includes wxSTB_SIZEGRIP|'
                      'wxSTB_SHOW_TIPS|wxSTB_ELLIPSIZE_END|wxFULL_REPAINT_ON_RESIZE.'),
            'supported_by': ('wx3',),
            'combination': ('wxSTB_SIZEGRIP|wxSTB_SHOW_TIPS|wxSTB_ELLIPSIZE_END|wxFULL_REPAINT_ON_RESIZE'),
        },
    },
    'style_list': ['wxST_SIZEGRIP', 'wxSTB_SIZEGRIP', 'wxSTB_SHOW_TIPS',
                   'wxSTB_ELLIPSIZE_START', 'wxSTB_ELLIPSIZE_MIDDLE',
                   'wxSTB_ELLIPSIZE_END', 'wxSTB_DEFAULT_STYLE']
}

