"""\
wxFrame widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxFrame',
    'style_defs': {
        'wxDEFAULT_FRAME_STYLE': {
            'desc': _('Defined as wxMINIMIZE_BOX | wxMAXIMIZE_BOX | '
                      'wxRESIZE_BORDER | wxSYSTEM_MENU | wxCAPTION | '
                      'wxCLOSE_BOX | wxCLIP_CHILDREN.'),
            'combination': 'wxMINIMIZE_BOX|wxMAXIMIZE_BOX|wxRESIZE_BORDER|'
                           'wxSYSTEM_MENU|wxCAPTION|wxCLOSE_BOX|'
                           'wxCLIP_CHILDREN',},
        'wxICONIZE':  {'desc': _('Display the frame iconized (minimized). Windows only.')},
        'wxMINIMIZE': {'desc': _('Identical to wxICONIZE. Windows only.')},
        'wxMAXIMIZE': {'desc': _('Displays the frame maximized. Windows and GTK+ only.')},
        'wxFRAME_TOOL_WINDOW': {
            'desc': _('Causes a frame with a small title bar to be created; '
                      'the frame does not appear in the taskbar under '
                      'Windows or GTK+.'),
        },
        'wxFRAME_NO_TASKBAR': {
            'desc': _('Creates an otherwise normal frame but it does not '
                      'appear in the taskbar under Windows or GTK+ (note '
                      'that it will minimize to the desktop window under '
                      'Windows which may seem strange to the users and '
                      'thus it might be better to use this style only '
                      'without wxMINIMIZE_BOX style). In wxGTK, the flag '
                      'is respected only if the window manager supports '
                      '_NET_WM_STATE_SKIP_TASKBAR hint.'),
        },
        'wxFRAME_FLOAT_ON_PARENT': {
            'desc': _('The frame will always be on top of its parent '
                      '(unlike wxSTAY_ON_TOP). A frame created with '
                      'this style must have a non-NULL parent.'),
        },
        'wxFRAME_SHAPED': {
            'desc': _('Windows with this style are allowed to have their shape changed with the SetShape method.')},

        # plus generic styles from wxWindow (from common.py):
    },
    #'default_style': 'wxDEFAULT_FRAME_STYLE', # don't define here, set in builder(...)
    'style_list': ['wxDEFAULT_FRAME_STYLE', 'wxICONIZE', 'wxCAPTION',
                   'wxMINIMIZE', 'wxMINIMIZE_BOX', 'wxCLOSE_BOX',
                   'wxMAXIMIZE', 'wxMAXIMIZE_BOX', 'wxSTAY_ON_TOP',
                   'wxSYSTEM_MENU',
                   'wxSIMPLE_BORDER', 'wxBORDER_SIMPLE', 'wxRESIZE_BORDER',
                   'wxFRAME_TOOL_WINDOW', 'wxFRAME_NO_TASKBAR',
                   'wxFRAME_FLOAT_ON_PARENT', 'wxNO_BORDER',
                   'wxFULL_REPAINT_ON_RESIZE',
                   'wxTAB_TRAVERSAL', 'wxCLIP_CHILDREN'],
    'events': {
        'EVT_CLOSE': {'type':'wxCloseEvent'},
        'EVT_MENU_OPEN': {'type':'MenuEvent'},
        'EVT_MENU_CLOSE': {'type':'MenuEvent'},
        'EVT_MENU_HIGHLIGHT': {'type':'MenuEvent'},
        'EVT_MENU_HIGHLIGHT_ALL': {'type':'MenuEvent'},
    },
}
