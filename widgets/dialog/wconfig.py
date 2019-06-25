"""\
wxDialog widget configuration

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxDialog',
    'style_defs': {
        'wxDEFAULT_DIALOG_STYLE': {
            'desc': 'from wxDialog',
            'combination': 'wxCAPTION|wxCLOSE_BOX|wxSYSTEM_MENU',
            },
        'wxDIALOG_MODAL': {
            'desc': _('Show a modal dialog'),
            'obsolete': _("This style is obsolete and doesn't do anything any more, don't use it in any new code."),
            'supported_by': ('wx2',),
        },
        'wxRESIZE_BOX': {
            'desc': _('Displays a maximize box on the dialog.'),
            'rename_to': 'wxMAXIMIZE_BOX',
            'supported_by': ('wx2',),
        },
        'wxTHICK_FRAME': {
            'desc': 'Display a thick frame around the window.',
            'rename_to': 'wxRESIZE_BORDER',
        },
        'wxDIALOG_NO_PARENT': {
            'desc': _("By default, a dialog created with a NULL parent "
                      "window will be given the application's top level "
                      "window as parent. Use this style to prevent this "
                      "from happening and create an orphan dialog. This "
                      "is not recommended for modal dialogs."),
        },
        # generic styles from wxWindow (from common.py):
        # - wxFULL_REPAINT_ON_RESIZE
        # - wxNO_FULL_REPAINT_ON_RESIZE
        # - wxCLIP_CHILDREN
    },
    'style_list': ['wxDEFAULT_DIALOG_STYLE', 'wxDIALOG_MODAL', 'wxCAPTION',
                   'wxSYSTEM_MENU', 'wxCLOSE_BOX', 'wxRESIZE_BOX',
                   'wxMAXIMIZE_BOX', 'wxMINIMIZE_BOX', 'wxTHICK_FRAME',
                   'wxRESIZE_BORDER', 'wxSTAY_ON_TOP',
                   'wxDIALOG_NO_PARENT',
                   'wxFULL_REPAINT_ON_RESIZE', 'wxCLIP_CHILDREN'],
    'events': {
        'EVT_CLOSE': {'type':'wxCloseEvent'},
        'EVT_INIT_DIALOG': {'type':'wxInitDialogEvent'},
    },
}
