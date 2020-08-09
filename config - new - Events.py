"""
Configuration related stuff

@note: Don't place code with gettext or dependencies to other wxGlade
       parts here!

see: preferencesdialog
@copyright: 2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os, sys


# default configuration values #########################################################################################
default_app_name = 'app'           # application name

default_cpp_app_name = 'main.cpp'  # name for C++ application file
default_header_extension = 'h'     # extension of C++ header files
default_source_extension = 'cpp'   # extension of C++ source files

default_language = 'python'        # Default language if no specified

default_output_file = './wxglade_out.py'  # output file
default_output_path = './'                # output path"

default_encoding = 'UTF-8'   # value for encoding; see: encoding"

default_indent_symbol = ' '  # value for indentation symbol
default_indent_amount = 4    # value for indentation amount

default_multiple_files = 0   # value for writing multiple files (each class in a separate file)
default_overwrite = 1        # value for overwriting existing sources
default_use_gettext = False  # value to usage of gettext

for_version = (2, 8) # version to generate code for


# these paths, file names and strings will be set during initialisation: ###############################################
appdata_path = ''                    # wxGlades application data like file history and templates
credits_file = ''                    # Path of the credits file "CREDITS.txt"

widgets_path = 'widgets'             # Path to wxGlade "built-in" widgets
wxglade_path = '.'                   # Program path, set in wxglade.py
docs_path = 'docs'                   # Path to wxGlade documentation (e.g. html manual, LICENSE.txt, CREDITS.txt)
home_path = ''                       # Users home directory
icons_path = 'icons'                 # Path to wxGlade icons
templates_path = 'templates'         # System template path
license_file = ''                    # Path of the license file "LICENSE.txt"
manual_file = 'docs/html/index.html' # Path to wxGlade HTML manual
tutorial_file = 'docs/tutorial.html' # Path to wxGlade HTML Tutorial
bmp_manual_file = 'docs/html/bitmaps.html' # Path to bitmaps help

platform = 'not_set'                 # Current platform string (mostly wx.Platform)
version = 'not_set'                  # wxGlade version string; see: get_version()
py_version = sys.version.split()[0]  # Python version string
wx_version = 'not_set'               # wxPython version string

rc_file = ''                         # Path to the rc / ini file to store user preferences in it
history_file = ''                    # Path to the history file, if used
log_file = ''                        # Path to wxGlade log file



use_gui = True                 # If True, wxGlade runs in "GUI" mode, if False, in "batch" mode for generating code only
use_file_history =  True       # Flag to use a file history


backed_up = {}      # Set of file names already backed up during this session (a dictionary);  see: common.save_file()
preferences = None  # User preferences;  type common.Preferences


label_width = 96     # width of labels in Property window

tooltip_time = 3    # Number of seconds a tooltip will be shown
tooltip_width = 50  # Maximum width to split tooltips into

debugging = ('WINGDB_ACTIVE' in os.environ)  # if True, at many places exceptions will be raised instead of handled
testing = False  # to be set by the testing framework

########################################################################################################################
# Dictionary to store widget generic widget details like tooltips, different names, ...
# see below for examples and documentation

if __name__=="__main__":
    def _(s): s

widget_config = {
    'generic_styles': {

        # generic styles from wxSizer
        'wxALL':    { 'desc': _('from wxSizer'),
                      'combination': 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM' },
        'wxTOP':    { 'desc': _('Apply the border to the top.') },
        'wxBOTTOM': { 'desc': _('Apply the border to the bottom.') },
        'wxLEFT':   { 'desc': _('Apply the border to the left.') },
        'wxRIGHT':  { 'desc': _('Apply the border to the right.') },
        'wxALIGN_LEFT':   {'desc': _('Align the item to the left.') },
        'wxALIGN_RIGHT':  {'desc': _('Align the item to the right.') },
        'wxALIGN_CENTER': { 'desc': _('Centre the item (horizontally).'),
                            'combination': 'wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL' },
        'wxALIGN_CENTRE': { 'desc': _('Centre the item (horizontally).'),
                            'synonym': 'wxALIGN_CENTER',
                            'rename_to': 'wxALIGN_CENTER',
                            'combination': 'wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL' },
        'wxALIGN_TOP':               { 'desc': _('Align the item to the top.') },
        'wxALIGN_BOTTOM':            { 'desc': _('Align the item to the bottom.') },
        'wxALIGN_CENTER_VERTICAL':   { 'desc': _('Centre the item vertically.') },
        'wxALIGN_CENTRE_VERTICAL':   { 'desc': _('Centre the item vertically.'),
                                       'synonym': 'wxALIGN_CENTER_VERTICAL',
                                       'rename_to': 'wxALIGN_CENTER_VERTICAL' },
        'wxALIGN_CENTER_HORIZONTAL': { 'desc': _('Centre the item horizontally.') },
        'wxALIGN_CENTRE_HORIZONTAL': { 'desc': _('Centre the item horizontally.'),
                                       'synonym': 'wxALIGN_CENTER_HORIZONTAL',
                                       'rename_to': 'wxALIGN_CENTER_HORIZONTAL' },
        'wxEXPAND': { 'desc': _('The item will be expanded to fill the space assigned to the item.') },
        'wxSHAPED':{'desc':_('The item will be expanded as much as possible while also maintaining its aspect ratio.\n'
                             'Proportion must be 0 in this case.')},
        'wxADJUST_MINSIZE': { 'desc': _('This style was used in wxWidgets 2.4. Since wxWidgets 2.6 the behaviour is '
                                        'default. Select wxFIXED_MINSIZE to use the old behaviour.'),
                              'supported_by': ('wx2',) },
        'wxFIXED_MINSIZE':  { 'desc': _('Normally wxSizers will use GetAdjustedBestSize() to determine what the '
                                        'minimal size of window items should be, and will use that size to calculate '
                                        'the layout. This allows layouts to adjust when an item changes and its best '
                                        'size becomes different. If you would rather have a window item stay '
                                        'the size it started with then use wxFIXED_MINSIZE.') },
        'wxRESERVE_SPACE_EVEN_IF_HIDDEN': { 'desc': _("Normally wxSizers don't allocate space for hidden "
                                                      "windows or other items. This flag overrides this "
                                                      "behaviour so that sufficient space is allocated for "
                                                      "the window even if it isn't visible. This makes it "
                                                      "possible to dynamically show and hide controls "
                                                      "without resizing parent dialog, for example. "
                                                      "This function is new since wxWidgets version 2.8."),
                                            'supported_by': ('wx3',) },

        # generic styles from wxWindow
        'wxTAB_TRAVERSAL': { 'desc': _('Use this to enable tab traversal for non-dialog windows.'), },
        'wxFULL_REPAINT_ON_RESIZE':    { 'desc': _('Use this style to force a complete redraw of the '
                                                   'window whenever it is resized instead of redrawing '
                                                   'just the part of the window affected by resizing. ') },
        'wxNO_FULL_REPAINT_ON_RESIZE': { 'desc': _('On Windows, this style used to disable repainting '
                                                   'the window completely when its size is changed. '
                                                   'Since this behaviour is now the default, the style '
                                                   'is now obsolete and no longer has an effect.'),
                                         'obsolete': _('obsolete')},
        'wxCLIP_CHILDREN': { 'desc': _('Use this style to eliminate flicker caused by the background being repainted, '
                                       'then children being painted over them. Windows only.') },
        'wxWANTS_CHARS': { 'desc': _("Use this to indicate that the window wants to get all char/key events for all "
                                     "keys - even for keys like TAB or ENTER which are usually used for "
                                     "dialog navigation and which wouldn't be generated without this style. "
                                     "If you need to use this style in order to get the arrows or etc., but would still"
                                     " like to have normal keyboard navigation take place, you should call Navigate in "
                                     "response to the key events for Tab and Shift-Tab.") },

        # Generic border styles
        'wxBORDER_DEFAULT': { 'desc': _('The window class will decide the kind of border to show, if any.'),
                              'supported_by': ('wx3',) },
        'wxSIMPLE_BORDER':  { 'desc': _('Displays a thin border around the window. '
                                        'wxSIMPLE_BORDER is the old name for this style.'),
                              'rename_to': 'wxBORDER_SIMPLE' },
        'wxBORDER_SIMPLE':  { 'desc': _('Displays a thin border around the window. '
                                        'wxSIMPLE_BORDER is the old name for this style.') },
        'wxSUNKEN_BORDER':  { 'desc': _('Displays a sunken border. wxSUNKEN_BORDER is the old name for this style.'),
                              'rename_to': 'wxBORDER_SUNKEN' },
        'wxBORDER_SUNKEN':  { 'desc': _('Displays a sunken border. wxSUNKEN_BORDER is the old name for this style.') },
        'wxRAISED_BORDER':  { 'desc': _('Displays a raised border. wxRAISED_BORDER is the old name for this style.'),
                              'rename_to': 'wxBORDER_RAISED' },
        'wxBORDER_RAISED':  { 'desc': _('Displays a raised border. wxRAISED_BORDER is the old name for this style.') },
        'wxSTATIC_BORDER':  { 'desc': _('Displays a border suitable for a static control. '
                                        'wxSTATIC_BORDER is the old name for this style. Windows only.'),
                              'rename_to': 'wxBORDER_STATIC' },
        'wxBORDER_STATIC':  { 'desc': _('Displays a border suitable for a static control. '
                                        'wxSTATIC_BORDER is the old name for this style. Windows only.') },
        'wxBORDER_THEME':   { 'desc': _('Displays a native border suitable for a control, on the current platform. '
                                        'On Windows XP or Vista, this will be a themed border; '
                                        'on most other platforms a sunken border will be used. '
                                        'For more information for themed borders on Windows, please see Themed borders '
                                        'on Windows.') },
        'wxNO_BORDER': { 'desc': _('Displays no border, overriding the default border '
                                   'style for the window. wxNO_BORDER is the old name for this style.'),
                         'rename_to': 'wxBORDER_NONE' },
        'wxBORDER_NONE': { 'desc': _('Displays no border, overriding the default border style for the window.'
                                     ' wxNO_BORDER is the old name for this style.'),
        },
        'wxDOUBLE_BORDER': { 'desc':_('Displays a double border. wxDOUBLE_BORDER is the old name for this style. '
                                      'Windows and Mac only.'),
                             'rename_to': 'wxBORDER_DOUBLE' },
        'wxBORDER_DOUBLE': { 'desc':_('Displays a double border. wxDOUBLE_BORDER is the old name for this style. '
                                      'Windows and Mac only.'),
                             'obsolete': _('since wx3.0') },

        # wxDialog styles
        'wxNO_3D': { 'desc': _('Under Windows, specifies that the child controls should not have 3D borders unless '
                               'specified in the control.'),
                     'obsolete': _("This style is obsolete and doesn't do anything any more, don't use it in any new code."),
                     'supported_by': ('wx2',) },
        'wxCAPTION': { 'desc': _('Puts a caption on the dialog box.') },
        'wxCLOSE_BOX':    { 'desc': _('Displays a close box on the frame.') },
        'wxMAXIMIZE_BOX': { 'desc': _('Displays a maximize box on the dialog.') },
        'wxMINIMIZE_BOX': { 'desc': _('Displays a minimize box on the dialog.') },
        'wxRESIZE_BORDER': { 'desc': _('Display a thick frame around the window.') },
        'wxSTAY_ON_TOP': { 'desc': _('The dialog stays on top of all other windows.') },
        'wxSYSTEM_MENU': { 'desc': _('Display a system menu.') },
    },
    # https://wiki.wxwidgets.org/EventTypes_and_Event-Table_Macros
    'event_types': {# the first groups are the most useful ones to be added to arbitrary widgets:
                    # EVT_MOUSE_EVENTS
                    'EVT_MOUSE_EVENTS':{'type':'wxMouseEvent'},
                    'EVT_LEFT_DOWN':{'type':'wxMouseEvent'},
                    'EVT_LEFT_UP':{'type':'wxMouseEvent'},
                    'EVT_MIDDLE_DOWN':{'type':'wxMouseEvent'},
                    'EVT_MIDDLE_UP':{'type':'wxMouseEvent'},
                    'EVT_RIGHT_DOWN':{'type':'wxMouseEvent'},
                    'EVT_RIGHT_UP':{'type':'wxMouseEvent'},
                    'EVT_MOTION':{'type':'wxMouseEvent'},
                    'EVT_LEFT_DCLICK':{'type':'wxMouseEvent'},
                    'EVT_MIDDLE_DCLICK':{'type':'wxMouseEvent'},
                    'EVT_RIGHT_DCLICK':{'type':'wxMouseEvent'},
                    'EVT_ENTER_WINDOW':{'type':'wxMouseEvent'},
                    'EVT_LEAVE_WINDOW':{'type':'wxMouseEvent'},
                    'EVT_MOUSEWHEEL':{'type':'wxMouseEvent'},
                    # more mouse events
                    'EVT_MOUSE_AUX1_DOWN':{'type':'wxMouseEvent'},
                    'EVT_MOUSE_AUX1_UP':{'type':'wxMouseEvent'},
                    'EVT_MOUSE_AUX1_DCLICK':{'type':'wxMouseEvent'},
                    'EVT_MOUSE_AUX2_DOWN':{'type':'wxMouseEvent'},
                    'EVT_MOUSE_AUX2_UP':{'type':'wxMouseEvent'},
                    'EVT_MOUSE_AUX2_DCLICK':{'type':'wxMouseEvent'},

                    # mouse command events
                    'EVT_COMMAND_LEFT_CLICK':{'type':'wxCommandEvent'},
                    'EVT_COMMAND_LEFT_DCLICK':{'type':'wxCommandEvent'},
                    'EVT_COMMAND_RIGHT_CLICK':{'type':'wxCommandEvent'},
                    'EVT_COMMAND_RIGHT_DCLICK':{'type':'wxCommandEvent'},

                    # key(board)
                    'EVT_CHAR':{'type':'wxKeyEvent'},
                    'EVT_CHAR_HOOK':{'type':'wxKeyEvent'},
                    'EVT_KEY_DOWN':{'type':'wxKeyEvent'},
                    'EVT_KEY_UP':{'type':'wxKeyEvent'},

                    'EVT_COMMAND_ENTER':{'type':'wxCommandEvent'},

                    # focus
                    'EVT_SET_FOCUS':{'type':'wxFocusEvent'},
                    'EVT_KILL_FOCUS':{'type':'wxFocusEvent'},
                    'EVT_COMMAND_KILL_FOCUS':{'type':'wxCommandEvent'},
                    'EVT_COMMAND_SET_FOCUS':{'type':'wxCommandEvent'},
                    'EVT_CHILD_FOCUS':{'type':'wxChildFocusEvent'},

                    # EVT_SCROLL, EVT_COMMAND_SCROLL
                    'EVT_SCROLL':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_TOP':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_BOTTOM':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_PAGEUP':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_PAGEDOWN':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_THUMBRELEASE':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_ENDSCROLL':{'type':'wxScrollEvent'},

                    'EVT_SCROLL_CHANGED':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_LINEDOWN':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_LINEUP':{'type':'wxScrollEvent'},
                    'EVT_SCROLL_THUMBTRACK':{'type':'wxScrollEvent'},

                    'EVT_COMMAND_SCROLL_BOTTOM':{'type':'wxScrollEvent'},  # from standalone ScrollBar and Slider
                    'EVT_COMMAND_SCROLL_CHANGED':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_ENDSCROLL':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_LINEDOWN':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_LINEUP':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_PAGEDOWN':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_PAGEUP':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_THUMBRELEASE':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_THUMBTRACK':{'type':'wxScrollEvent'},
                    'EVT_COMMAND_SCROLL_TOP':{'type':'wxScrollEvent'},


                    # EVT_JOYSTICK_EVENTS
                    'EVT_JOYSTICK_EVENTS':{'type':'wxJoystickEvent'},
                    'EVT_JOY_BUTTON_DOWN':{'type':'wxJoystickEvent'},
                    'EVT_JOY_BUTTON_UP':{'type':'wxJoystickEvent'},
                    'EVT_JOY_MOVE':{'type':'wxJoystickEvent'},
                    'EVT_JOY_ZMOVE':{'type':'wxJoystickEvent'},

                    # EVT_SCROLLWIN
                    'EVT_SCROLLWIN':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_TOP':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_BOTTOM':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_LINEUP':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_LINEDOWN':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_PAGEUP':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_PAGEDOWN':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_THUMBTRACK':{'type':'wxScrollWinEvent'},
                    'EVT_SCROLLWIN_THUMBRELEASE':{'type':'wxScrollWinEvent'},

                    'EVT_ACTIVATE':{'type':'wxActivateEvent'},
                    'EVT_ACTIVATE_APP':{'type':'wxActivateEvent'},

                    # toplevel windows or just frames?
                    'EVT_CLOSE':{'type':'wxCloseEvent'},
                    'EVT_ICONIZE':{'type':'wxIconizeEvent'},
                    'EVT_IDLE':{'type':'wxIdleEvent'},
                    'EVT_MAXIMIZE':{'type':'wxMaximizeEvent'},

                    # any widget
                    'EVT_PAINT':{'type':'wxPaintEvent'},

                    # toolbar
                    'EVT_TOOL':{'type':'wxCommandEvent'},
                    'EVT_TOOL_DROPDOWN':{'type':'wxCommandEvent'},
                    'EVT_TOOL_ENTER':{'type':'wxCommandEvent'},
                    'EVT_TOOL_RANGE':{'type':'wxCommandEvent'},
                    'EVT_TOOL_RCLICKED':{'type':'wxCommandEvent'},
                    'EVT_TOOL_RCLICKED_RANGE':{'type':'wxCommandEvent'},

                    'EVT_QUERY_END_SESSION':{'type':'wxCloseEvent'},  # from app derived only
                    'EVT_QUERY_NEW_PALETTE':{'type':'wxCloseEvent'},  # from app derived only

                    'EVT_COLLAPSIBLEPANE_CHANGED':{'type':'wxCollapsiblePaneEvent'},  # from CollapsiblePane
                    'EVT_COLOURPICKER_CHANGED':{'type':'wxColourPickerEvent'},  # from ColourPickerCtrl

                    'EVT_CONTEXT_MENU':{'type':'wxContextMenuEvent'},
                    'EVT_DETAILED_HELP':{'type':'wxHelpEvent'},
                    'EVT_DETAILED_HELP_RANGE':{'type':'wxHelpEvent'},

                    'EVT_DIRCTRL_FILEACTIVATED':{'type':'wxTreeEvent'},  # from GenericDirCtrl
                    'EVT_DIRCTRL_SELECTIONCHANGED':{'type':'wxTreeEvent'},

                    'EVT_DIRPICKER_CHANGED':{'type':'wxFileDirPickerEvent'},  #  FilePickerCtrl and DirPickerCtrl
                    'EVT_FILEPICKER_CHANGED':{'type':'wxFileDirPickerEvent'},

                    'EVT_DISPLAY_CHANGED':{'type':'wxDisplayChangedEvent'},
                    'EVT_DROP_FILES':{'type':'wxDropFilesEvent'},
                    'EVT_END_PROCESS':{'type':'wxProcessEvent'},
                    'EVT_END_SESSION':{'type':'wxCloseEvent'},
                    'EVT_ERASE_BACKGROUND':{'type':'wxEraseEvent'},

                    'EVT_FILECTRL_FILEACTIVATED':{'type':'wxFileCtrlEvent'},  # from FileCtrl
                    'EVT_FILECTRL_FILTERCHANGED':{'type':'wxFileCtrlEvent'},
                    'EVT_FILECTRL_FOLDERCHANGED':{'type':'wxFileCtrlEvent'},
                    'EVT_FILECTRL_SELECTIONCHANGED':{'type':'wxFileCtrlEvent'},

                    'EVT_FIND':{'type':'wxFindDialogEvent'},  # from FindReplaceDialog
                    'EVT_FIND_CLOSE':{'type':'wxFindDialogEvent'},
                    'EVT_FIND_NEXT':{'type':'wxFindDialogEvent'},
                    'EVT_FIND_REPLACE':{'type':'wxFindDialogEvent'},
                    'EVT_FIND_REPLACE_ALL':{'type':'wxFindDialogEvent'},

                    'EVT_FONTPICKER_CHANGED':{'type':'wxFontPickerEvent'},  # FontPickerCtrl
                    'EVT_FSWATCHER':{'type':'wxFileSystemWatcherEvent'},

                    'EVT_HEADER_BEGIN_REORDER':{'type':'wxHeaderCtrlEvent'},  # from grid, listctrl and datagrid?
                    'EVT_HEADER_BEGIN_RESIZE':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_CLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_DCLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_DRAGGING_CANCELLED':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_END_REORDER':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_END_RESIZE':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_MIDDLE_CLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_MIDDLE_DCLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_RESIZING':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_RIGHT_CLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_RIGHT_DCLICK':{'type':'wxHeaderCtrlEvent'},
                    'EVT_HEADER_SEPARATOR_DCLICK':{'type':'wxHeaderCtrlEvent'},

                    'EVT_HELP':{'type':'wxHelpEvent'},
                    'EVT_HELP_RANGE':{'type':'wxHelpEvent'},

                    'EVT_HOTKEY':{'type':'wxCharEvent'},
                    'EVT_INIT_DIALOG':{'type':'wxInitDialogEvent'},

                    'EVT_MENU':{'type':'wxCommandEvent'},
                    'EVT_MENU_RANGE':{'type':'wxCommandEvent'},

                    'EVT_MENU_CLOSE':{'type':'wxMenuEvent'},
                    'EVT_MENU_HIGHLIGHT':{'type':'wxMenuEvent'},
                    'EVT_MENU_HIGHLIGHT_ALL':{'type':'wxMenuEvent'},
                    'EVT_MENU_OPEN':{'type':'wxMenuEvent'},

                    'EVT_MOUSE_CAPTURE_CHANGED':{'type':'wxMouseCaptureChangedEvent'},
                    'EVT_MOUSE_CAPTURE_LOST':{'type':'wxMouseCaptureLostEvent'},

                    # for toplevel windows on MSW
                    'EVT_MOVE':{'type':'wxMoveEvent'},
                    'EVT_MOVE_END':{'type':'wxMoveEvent'},
                    'EVT_MOVE_START':{'type':'wxMoveEvent'},

                    'EVT_SIZE':{'type':'wxSizeEvent'},

                    # for toplevel windows when being dragged
                    'EVT_SIZING':{'type':'wxSizeEvent'},
                    'EVT_MOVING':{'type':'wxMoveEvent'},

                    # e.g. for tab and page down
                    'EVT_NAVIGATION_KEY':{'type':'wxNavigationKeyEvent'},

                    'EVT_NC_PAINT':{'type':'wxNcPaintEvent'},

                    'EVT_PALETTE_CHANGED':{'type':'wxPaletteChangedEvent'},
                    'EVT_SYS_COLOUR_CHANGED':{'type':'wxSysColourChangedEvent'},

                    'EVT_SET_CURSOR':{'type':'wxSetCursorEvent'},
                    'EVT_SHOW':{'type':'wxShowEvent'},

                    'EVT_TEXT_COPY':{'type':'wxClipboardTextEvent'},  # from TextCtrl and on MSW from ComboBox
                    'EVT_TEXT_CUT':{'type':'wxClipboardTextEvent'},
                    'EVT_TEXT_PASTE':{'type':'wxClipboardTextEvent'},

                    'EVT_UPDATE_UI':{'type':'wxUpdateUIEvent'},
                    'EVT_UPDATE_UI_RANGE':{'type':'wxUpdateUIEvent'},
                    'EVT_VLBOX':{'type':'wxCommandEvent'},
                    'EVT_WINDOW_CREATE':{'type':'wxWindowCreateEvent'},
                    'EVT_WINDOW_DESTROY':{'type':'wxWindowDestroyEvent'},
                    'EVT_WINDOW_MODAL_DIALOG_CLOSED':{'type':'wxWindowModalDialogEvent'},

                    'EVT_THREAD':{'type':'wxThreadEvent'},
                    'EVT_TIMER':{'type':'wxThreadEvent'},

                    'EVT_HIBERNATE':{'type':'wxActivateEvent'},
                    'EVT_POWER_RESUME':{'type':'wxPowerEvent'},
                    'EVT_POWER_SUSPENDED':{'type':'wxPowerEvent'},
                    'EVT_POWER_SUSPENDING':{'type':'wxPowerEvent'},
                    'EVT_POWER_SUSPEND_CANCEL':{'type':'wxPowerEvent'},


                    # command events per core widget type:
                    'EVT_BUTTON':{'type':'wxCommandEvent'},
                    'EVT_CHECKBOX':{'type':'wxCommandEvent'},
                    'EVT_CHECKLISTBOX':{'type':'wxCommandEvent'},
                    'EVT_CHOICE':{'type':'wxCommandEvent'},
                    'EVT_COMBOBOX':{'type':'wxCommandEvent'},
                    'EVT_COMBOBOX_CLOSEUP':{'type':'wxCommandEvent'},
                    'EVT_COMBOBOX_DROPDOWN':{'type':'wxCommandEvent'},
                    'EVT_LISTBOX':{'type':'wxCommandEvent'},
                    'EVT_LISTBOX_DCLICK':{'type':'wxCommandEvent'},
                    'EVT_RADIOBOX':{'type':'wxCommandEvent'},
                    'EVT_RADIOBUTTON':{'type':'wxCommandEvent'},
                    'EVT_SCROLLBAR':{'type':'wxCommandEvent'},
                    'EVT_SEARCHCTRL_CANCEL_BTN':{'type':'wxCommandEvent'},
                    'EVT_SEARCHCTRL_SEARCH_BTN':{'type':'wxCommandEvent'},

                    'EVT_SLIDER':{'type':'wxCommandEvent'},
                    'EVT_TEXT':{'type':'wxCommandEvent'},
                    'EVT_TEXT_ENTER':{'type':'wxCommandEvent'},
                    'EVT_TEXT_MAXLEN':{'type':'wxCommandEvent'},
                    'EVT_TEXT_URL':{'type':'wxTextUrlEvent'},
                    'EVT_TOGGLEBUTTON':{'type':'wxCommandEvent'},

                    'EVT_SPIN':{'type':'wxSpinEvent'},
                    'EVT_SPIN_UP':{'type':'wxSpinEvent'},
                    'EVT_SPIN_DOWN':{'type':'wxSpinEvent'},
                    'EVT_SPINCTRL':{'type':'wxSpinEvent'},
                    'EVT_SPINCTRLDOUBLE':{'type':'wxSpinDoubleEvent'},

                    'EVT_SPLITTER_DCLICK':{'type':'wxSplitterEvent'},
                    'EVT_SPLITTER_DOUBLECLICKED':{'type':'wxSplitterEvent'},  # deprecated version of DCLICK?
                    'EVT_SPLITTER_SASH_POS_CHANGED':{'type':'wxSplitterEvent'},
                    'EVT_SPLITTER_SASH_POS_CHANGING':{'type':'wxSplitterEvent'},
                    'EVT_SPLITTER_UNSPLIT':{'type':'wxSplitterEvent'},

                    'EVT_LIST_BEGIN_DRAG':{'type':'wxListEvent'},
                    'EVT_LIST_BEGIN_LABEL_EDIT':{'type':'wxListEvent'},
                    'EVT_LIST_BEGIN_RDRAG':{'type':'wxListEvent'},
                    'EVT_LIST_CACHE_HINT':{'type':'wxListEvent'},
                    'EVT_LIST_COL_BEGIN_DRAG':{'type':'wxListEvent'},
                    'EVT_LIST_COL_CLICK':{'type':'wxListEvent'},
                    'EVT_LIST_COL_DRAGGING':{'type':'wxListEvent'},
                    'EVT_LIST_COL_END_DRAG':{'type':'wxListEvent'},
                    'EVT_LIST_COL_RIGHT_CLICK':{'type':'wxListEvent'},
                    'EVT_LIST_DELETE_ALL_ITEMS':{'type':'wxListEvent'},
                    'EVT_LIST_DELETE_ITEM':{'type':'wxListEvent'},
                    'EVT_LIST_END_LABEL_EDIT':{'type':'wxListEvent'},
                    'EVT_LIST_INSERT_ITEM':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_ACTIVATED':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_DESELECTED':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_FOCUSED':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_MIDDLE_CLICK':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_RIGHT_CLICK':{'type':'wxListEvent'},
                    'EVT_LIST_ITEM_SELECTED':{'type':'wxListEvent'},
                    'EVT_LIST_KEY_DOWN':{'type':'wxListEvent'},

                    'EVT_NOTEBOOK_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_NOTEBOOK_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_LISTBOOK_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_LISTBOOK_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_BOOKCTRL_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_BOOKCTRL_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_CHOICEBOOK_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_CHOICEBOOK_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_TOOLBOOK_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_TOOLBOOK_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_TREEBOOK_NODE_COLLAPSED':{'type':'wxBookCtrlEvent'},
                    'EVT_TREEBOOK_NODE_EXPANDED':{'type':'wxBookCtrlEvent'},
                    'EVT_TREEBOOK_PAGE_CHANGED':{'type':'wxBookCtrlEvent'},
                    'EVT_TREEBOOK_PAGE_CHANGING':{'type':'wxBookCtrlEvent'},

                    'EVT_TREE_BEGIN_DRAG':{'type':'wxTreeEvent'},
                    'EVT_TREE_BEGIN_LABEL_EDIT':{'type':'wxTreeEvent'},
                    'EVT_TREE_BEGIN_RDRAG':{'type':'wxTreeEvent'},
                    'EVT_TREE_DELETE_ITEM':{'type':'wxTreeEvent'},
                    'EVT_TREE_END_DRAG':{'type':'wxTreeEvent'},
                    'EVT_TREE_END_LABEL_EDIT':{'type':'wxTreeEvent'},
                    'EVT_TREE_GET_INFO':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_ACTIVATED':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_COLLAPSED':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_COLLAPSING':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_EXPANDED':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_EXPANDING':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_GETTOOLTIP':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_MENU':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_MIDDLE_CLICK':{'type':'wxTreeEvent'},
                    'EVT_TREE_ITEM_RIGHT_CLICK':{'type':'wxTreeEvent'},
                    'EVT_TREE_KEY_DOWN':{'type':'wxTreeEvent'},
                    'EVT_TREE_SEL_CHANGED':{'type':'wxTreeEvent'},
                    'EVT_TREE_SEL_CHANGING':{'type':'wxTreeEvent'},
                    'EVT_TREE_SET_INFO':{'type':'wxTreeEvent'},
                    'EVT_TREE_STATE_IMAGE_CLICK':{'type':'wxTreeEvent'},

                    # from wx.grid; we use only the CMD version as it allows to specify the source widget
                    'EVT_GRID_EDITOR_CREATED':{'type':'wx.grid.GridEditorCreatedEvent'},
                    'EVT_GRID_CMD_EDITOR_CREATED':{'type':'wx.grid.GridEditorCreatedEvent'},

                    'EVT_GRID_CELL_BEGIN_DRAG': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_CHANGED': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_CHANGING': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_LEFT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_LEFT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_RIGHT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CELL_RIGHT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_BEGIN_DRAG': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_CHANGED': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_CHANGING': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_LEFT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_LEFT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_RIGHT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_CELL_RIGHT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_COL_MOVE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_COL_SIZE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_COL_SORT': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_EDITOR_CREATED': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_EDITOR_HIDDEN': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_EDITOR_SHOWN': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_LABEL_LEFT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_LABEL_LEFT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_LABEL_RIGHT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_LABEL_RIGHT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_RANGE_SELECT': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_ROW_SIZE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_SELECT_CELL': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_CMD_TABBING': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_COL_MOVE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_COL_SIZE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_COL_SORT': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_EDITOR_HIDDEN': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_EDITOR_SHOWN': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_LABEL_LEFT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_LABEL_LEFT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_LABEL_RIGHT_CLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_LABEL_RIGHT_DCLICK': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_RANGE_SELECT': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_ROW_SIZE': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_SELECT_CELL': {'type':'wx.grid.GridEvent'},
                    'EVT_GRID_TABBING': {'type':'wx.grid.GridEvent'},

                    # from wx.html.HtmlWindow
                    'EVT_HTML_CELL_HOVER': {'type':'wx.html.HtmlCellEvent'},  
                    'EVT_HTML_CELL_CLICKED': {'type':'wx.html.HtmlCellEvent'},
                    'EVT_HTML_LINK_CLICKED': {'type':'wx.html.HtmlLinkEvent'},

                    # from wx.adv.HyperlinkCtrl
                    'EVT_HYPERLINK:':  {'type':'wx.adv.HyperlinkEvent'},

                    # from wx.propgrid.PropertyGrid
                    'EVT_PG_CHANGED': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_CHANGING': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_COL_BEGIN_DRAG': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_COL_DRAGGING': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_COL_END_DRAG': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_DOUBLE_CLICK': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_HIGHLIGHTED': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_ITEM_COLLAPSED': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_ITEM_EXPANDED': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_LABEL_EDIT_BEGIN': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_LABEL_EDIT_ENDING': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_PAGE_CHANGED': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_RIGHT_CLICK': {'type':'wx.propgrid.PropertyGridEvent'},
                    'EVT_PG_SELECTED': {'type':'wx.propgrid.PropertyGridEvent'},
                    
                    # from a wx.ribbon.RibbonButtonBar
                    'EVT_RIBBONBAR_HELP_CLICK': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_PAGE_CHANGED': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_PAGE_CHANGING': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TAB_LEFT_DCLICK': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TAB_MIDDLE_DOWN': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TAB_MIDDLE_UP': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TAB_RIGHT_DOWN': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TAB_RIGHT_UP': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBAR_TOGGLED': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBUTTONBAR_CLICKED': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONBUTTONBAR_DROPDOWN_CLICKED': {'type':'wx.ribbon.RibbonButtonBarEvent'},
                    'EVT_RIBBONGALLERY_CLICKED': {'type':'wx.ribbon.RibbonGalleryEvent'},
                    'EVT_RIBBONGALLERY_HOVER_CHANGED': {'type':'wx.ribbon.RibbonGalleryEvent'},
                    'EVT_RIBBONGALLERY_SELECTED': {'type':'wx.ribbon.RibbonGalleryEvent'},
                    'EVT_RIBBONPANEL_EXTBUTTON_ACTIVATED': {'type':'wx.ribbon.RibbonPanelEvent'},
                    'EVT_RIBBONTOOLBAR_CLICKED': {'type':'wx.ribbon.RibbonToolBarEvent'},
                    'EVT_RIBBONTOOLBAR_DROPDOWN_CLICKED': {'type':'wx.ribbon.RibbonToolBarEvent'},
                    
                    # from wx.adv.Sash
                    'EVT_SASH_DRAGGED': {'type':'wx.adv.SashEvent'},
                    'EVT_SASH_DRAGGED_RANGE': {'type':'wx.adv.SashEvent'},

                    # from wx.stc.StyledTextCtrl 
                    'EVT_STC_AUTOCOMP_CANCELLED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_AUTOCOMP_CHAR_DELETED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_AUTOCOMP_SELECTION': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_CALLTIP_CLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_CHANGE': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_CHARADDED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_DOUBLECLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_DO_DROP': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_DRAG_OVER': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_DWELLEND': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_DWELLSTART': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_HOTSPOT_CLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_HOTSPOT_DCLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_HOTSPOT_RELEASE_CLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_INDICATOR_CLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_INDICATOR_RELEASE': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_KEY': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_MACRORECORD': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_MARGINCLICK': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_MODIFIED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_NEEDSHOWN': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_PAINTED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_ROMODIFYATTEMPT': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_SAVEPOINTLEFT': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_SAVEPOINTREACHED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_START_DRAG': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_STYLENEEDED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_UPDATEUI': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_URIDROPPED': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_USERLISTSELECTION': {'type':'wx.stc.StyledTextEvent'},
                    'EVT_STC_ZOOM': {'type':'wx.stc.StyledTextEvent'},
                },
    # for event property 'Add group' button:
    #'generic_event_groups':
    # list here at least these which are no CommandEvents, as only CommandEvents will be bound to the containing class
    # others will be bound to the widgets
    'event_classes': {'wxMouseEvent':'wxEvent',
                    'wxKeyEvent':'wxEvent',
                    'wxCharEvent':'wxKeyEvent',
                    'wxMouseCaptureLostEvent':'wxEvent',
                    'wxMouseCaptureChangedEvent':'wxEvent',
                    'wxJoystickEvent':'wxEvent',
                    'wxIdleEvent':'wxEvent',
                    'wxFocusEvent':'wxEvent',
                    'wxCloseEvent':'wxEvent',
                    'wxDropFilesEvent':'wxEvent',
                    'wxEraseEvent':'wxEvent',
                    'wxHelpEvent':'wxCommandEvent',
                    'wxContextMenuEvent':'wxCommandEvent',
                    'wxChildFocusEvent':'wxCommandEvent',
                    'wxActivateEvent':'wxEvent',
                    'wxPaintEvent':'wxEvent',
                    'wxPowerEvent':'wxEvent',
                    'wxScrollEvent':'wxCommandEvent',
                    'wxScrollWinEvent':'wxEvent',
                    'wxSetCursorEvent':'wxEvent',
                    'wxSizeEvent':'wxEvent',
                    'wxSysColourChangedEvent':'wxEvent',
                    'wxTreeEvent':'wxCommandEvent',
                    'wxTextUrlEvent':'wxCommandEvent',
                    'wxClipboardTextEvent':'wxEvent',
                    'wxBookCtrlEvent': 'wxCommandEvent',  # actually NotifyEvent -> CommandEvent
                    'wxCollapsiblePaneEvent': 'wxCommandEvent',
                    'wxColourPickerEvent':'wxCommandEvent',
                    'wxFileCtrlEvent':'wxCommandEvent',
                    'wxFileDirPickerEvent':'wxCommandEvent',
                    'wxFindDialogEvent':'wxCommandEvent',
                    'wxFontPickerEvent':'wxCommandEvent',
                    'wxWindowCreateEvent':'wxCommandEvent',
                    'wxDisplayChangedEvent':'wxEvent',
                    'wxProcessEvent':'wxEvent',

                    'wxHeaderCtrlEvent':'wxNotifyEvent',
                    'wxSpinEvent':'wxNotifyEvent',
                    'wxSpinDoubleEvent':'wxNotifyEvent',
                    'wxSplitterEvent':'wxNotifyEvent',
                    'wxListEvent':'wxNotifyEvent',
                    'wxNotifyEvent':'wxCommandEvent',

                    'wxFileSystemWatcherEvent':'wxEvent',
                    'wxIconizeEvent':'wxEvent',
                    'wxInitDialogEvent':'wxEvent',
                    'wxMaximizeEvent':'wxEvent',
                    'wxMenuEvent':'wxEvent',
                    'wxMoveEvent':'wxEvent',
                    'wxNavigationKeyEvent':'wxEvent',
                    'wxNcPaintEvent':'wxEvent',
                    'wxPaletteChangedEvent':'wxEvent',
                    'wxShowEvent':'wxEvent',
                    'wxThreadEvent':'wxEvent',
                    'wxUpdateUIEvent':'wxCommandEvent',
                    'wxWindowDestroyEvent':'wxCommandEvent',
                    'wxWindowModalDialogEvent':'wxCommandEvent',
                    
                    'wx.adv.DateEvent':'wxCommandEvent',  # wx.adv.DatePickerCtrl
                    'wx.adv.CalendarEvent':'wxCommandEvent', # actually wx.adv.DateEvent -> CommandEvent
                    'wx.grid.GridEditorCreatedEvent':'wxCommandEvent',
                    'wx.grid.GridEvent':'wxCommandEvent',
                    'wx.html.HtmlCellEvent':'wxCommandEvent',
                    'wx.html.HtmlLinkEvent':'wxCommandEvent',
                    'wx.adv.HyperlinkEvent':'wxCommandEvent',
                    'wx.propgrid.PropertyGridEvent':'wxCommandEvent',
                    'wx.ribbon.RibbonButtonBarEvent':'wxCommandEvent',
                    'wx.ribbon.RibbonGalleryEvent':'wxCommandEvent',
                    'wx.ribbon.RibbonPanelEvent':'wxCommandEvent',
                    'wx.ribbon.RibbonToolBarEvent':'wxCommandEvent',
                    'wx.adv.SashEvent':'wxCommandEvent',
                    'wx.stc.StyledTextEvent':'wxCommandEvent',
                    }
}
"""
Example:
    config = { 'wxSplitterWindow': { 'supported_by': ('wx28', 'wx3'),
                                      'style_defs': { 'wxSP_3D': { 'desc': _('Draws a 3D effect border and sash'),
                                                                   'combination': 'wxSP_3DBORDER|wxSP_3DSASH' } },
               'wxHyperlinkCtrl':  { 'supported_by': ('wx28', 'wx3') },
               'wxDialog':         { 'style_defs': { 'wxNO_3D': { 'desc': _('Under Windows, specifies that the child '
                                                                            'controls should not have 3D borders unless'
                                                                            ' specified in the control.'),
                                                                  'supported_by': ('wx2',) } } },
              'generic_styles': { 'wxALL': { 'desc': _('from wxSizer'),
                                             'combination': 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM' } }
    }

Elements:
  - supported_by   - This widget is only available at the listed wx versions. An empty list or a non-existing entry
                     means the widgets is always available.
  - styles         - Dictionary with style specific settings
  - generic_styles - Generic item to concentrate styles that are not part of a specific widget e.g. sizer styles.
  - default_style  - Default style for new created widgets
  - style_list     - List of all styles to show within the style box
  - events         - Dictionary with event specific settings

Style attributes:
  - 'desc':         <description> - Short style description
  - 'combination':  <styles joined by '|'> - The style is defined as a combination of different other styles
  - 'exclude':      <styles joined by '|'> - The listed styles will be removed by selecting this style
  - 'include':      <styles joined by '|'> - The style requires additional styles. The listed styles are a soft
                                             requirement - these styles are added even if the "requesting" style will be
                                             delete somehow or other.
  - 'obsolete':     <text>                 - This style is obsolete. A short notice will shown in the style tooltip
  - 'rename_to:     <new style name>       - The style will be renamed into the given style name
  - 'require':      <styles joined by '|'> - The style requires additional styles.
                                             The listed styles are a hard requirement - these styles are added only in
                                             together with the "requesting" style.
                                             If the "requesting" style will be deleted, these styles will be not added.
  - 'supported_by': (<supported version>)  - List of versions supporting this style
  - 'synonym':      <alternative name>     - Short notice about an alternative style name shown in style tooltip

Event attributes:
  - 'type':          <event prototype>     - Event prototype, fallback is wxCommandEvent
  - 'type_wx2':      <event prototype>     - Event prototype for wx 2.X,
                                             use this attribute if the event exists in all supported wx versions
  - 'type_wx3':      <event prototype>     - Event prototype for wx 3.X,
                                             use this attribute if the event exists in all supported wx versions
  - 'supported_by': (<supported version>)  - List of versions supporting this event

All event attributes are optional. If no attributes are given, wxCommandEvent will be used as event type.

Use gettext ( _() ) for the attributes content of "desc" and "obsolete".

The style processing is described in gui_mixins.StylesMixin.cn_f()."""


def read_version_file():
    """Read the version information from file "RELEASE-VERSION".
    see: write_version_file() and get_version()"""
    try:
        import version
        return version.__version__.strip()
    except ImportError:
        return None


def write_version_file(release):
    """Write the given version string into file "version.py".

    release: version string to write

    see: read_version_file(), get_version()"""
    fh = open('version.py', 'w')
    fh.write("""\
#
# This is an automatically generated file. Manual changes will be
# overwritten without warning.
#

__version__ = "%s"
""" % release)
    fh.close()


def get_hg_version():
    "Query the local hg repository to get the current release or return None"
    try:
        from mercurial.hg import repository
        from mercurial.ui import ui
        from mercurial.node import short
        from mercurial.error import RepoError
    except:
        return None

    # try to open local hg repository
    try:
        repo = repository(ui(), os.path.dirname(__file__))
    except RepoError:
        # no mercurial repository found
        return None

    release = ''
    context = repo[None]
    parents = context.parents()
    repo_changed = context.files() + context.deleted()
    if len(parents) == 1 and not repo_changed:
        # release tag isn't at tip it's -2 (one below tip)
        parents = parents[0].parents()
        node = parents[0].node()
        tags = repo.nodetags(node)
        # look for the special 'rel_X_X_X' or 'rel_X_X' tag
        for tag in tags:
            if tag.startswith('rel_') and len(tag) > 4:
                release = tag[4:].replace('_', '.')
                break
        # handle untagged release e.g. tip
        if not release:
            release = short(node)
    else:
        release = '%s' % '+'.join([short(p.node()) for p in parents])

    suffix_changed = repo_changed and '+' or ''

    ver = '%s%s' % (release, suffix_changed)
    return ver


def get_version(suffix=True):
    """Return the release string.

    The release will determinate in three steps:
     1. read from release file (see read_version_file() )
     2. Queried from local hg repo (see get_hg_version() )
     3. Set to "not found"

    The release string contains a suffix if wxGlade runs as standalone edition.

    suffix: Append suffix for standalone edition (bool)

    see: read_version_file(), get_hg_version()"""
    release = read_version_file()
    if not release:
        release = get_hg_version()
    if not release:
        release = 'not found'

    if suffix and hasattr(sys, 'frozen'):
        release = '%s (standalone edition)' % release

    return release


if __name__=="__main__":
    # check whether each event type hierarchy ends with wxCommandEvent or wxEvent
    classes = widget_config["event_classes"]
    def get_base(event_class):
        ret = event_class
        while ret in classes:
            ret = classes[ret]
        return ret
    for event, event_config in widget_config["event_types"].items():
        base = get_base(event_config["type"])
        assert base in ("wxCommandEvent", "wxEvent")
