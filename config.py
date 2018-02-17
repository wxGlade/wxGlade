"""
Configuration related stuff

@note: Don't place code with gettext or dependencies to other wxGlade
       parts here!

@see: L{preferencesdialog}
@copyright: 2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2018 Dietmar Schwertberger
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
preferences = None  # User preferences;  @type: common.Preferences   @see: L{common.Preferences}


label_width = 96     # width of labels in Property window

tooltip_time = 3    # Number of seconds a tooltip will be shown
tooltip_width = 50  # Maximum width to split tooltips into

debugging = ('WINGDB_ACTIVE' in os.environ)  # if True, at many places exceptions will be raised instead of handled
testing = False  # to be set by the testing framework

########################################################################################################################
# Dictionary to store widget generic widget details like tooltips, different names, ...
# see below for examples and documentation
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
