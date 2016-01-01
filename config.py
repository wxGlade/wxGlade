"""
Configuration related stuff

@note: Don't place code with gettext or dependencies to other wxGlade
       parts here!

@see: L{preferencesdialog}
@copyright: 2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import os
import sys

# default configuration values
default_app_name = 'app'
"""\
Default value for the application name

@type: str
"""

default_cpp_app_name = 'main.cpp'
"""\
Default application name for C++ files

@type: str
@see: L{default_header_extension}
@see: L{default_source_extension}
"""

default_encoding = 'UTF-8'
"""\
Default value for encoding

@type: str
@see: L{encoding}
"""

default_header_extension = '.h'
"""\
Default extension of the header file

@type: str
@see: L{default_cpp_app_name}
@see: L{default_source_extension}
"""

default_indent_amount = 4
"""\
Default value for indentation

@type: int
"""

default_indent_symbol = ' '
"""\
Default value for indentation symbol

@type: str
"""

default_language = 'python'
"""\
Default language of no specified

@type: str
"""

default_multiple_files = 0
"""\
Default value for writing multiple files (each class in a separate file)

@type: int
"""

default_overwrite = 1
"""\
Default value for overwriting existing sources

@type: int
"""

default_output_file = './wxglade_out.py'
"""\
Default output file

@type: str
"""

default_output_path = './'
"""\
Default output path

@type: str
"""

default_source_extension = '.cpp'
"""\
Default extension of the source file

@type: str
@see: L{default_cpp_app_name}
@see: L{default_header_extension}
"""

default_use_gettext = 1
"""\
Default value to usage of gettext

@type: int
"""

encoding = None
"""\
System default character encoding.

The default application L{default_encoding} is the fallback only.

@type: str | None
@see: L{default_encoding}
@see: L{wxglade.init_localization()}
"""

for_version = (2, 8)
"""\
Default version to generate code for

@type: (int, int)
"""

appdata_path = ''
"""\
Directory to wxGlades application data like file history and templates

@type: str
@note: This path will be set during initialisation
"""

credits_file = ''
"""\
Path of the credits file "CREDITS.txt"

@type: str
"""

docs_path = 'docs'
"""\
Path to wxGlade documentation (e.g. html manual, LICENSE.txt, CREDITS.txt)

@type: str
@note: This path will be set during initialisation
"""

home_path = ''
"""\
Users home directory

@type: str
@note: This path will be set during initialisation
"""

icons_path = 'icons'
"""\
Path to wxGlade icons

@type: str
@note: This path will be set during initialisation
"""

license_file = ''
"""\
Path of the license file "LICENSE.txt"

@type: str
@note: This path will be set during initialisation
"""

manual_file = 'docs/html/index.html'
"""\
Path to wxGlade HTML manual

@type: str
@note: This path will be set during initialisation
"""

platform = 'not_set'
"""\
Current platform (mostly wx.Platform)

@type: str
@note: This path will be set during initialisation
"""

version = 'not_set'
"""\
wxGlade version string

@type: str
@note: This path will be set during initialisation
@see: L{get_version()}
"""

py_version = sys.version.split()[0]
"""\
Python version

@type: str
"""

wx_version = 'not_set'
"""\
wxPython version

@type: str
"""

templates_path = 'templates'
"""\
System template path

@type: str
@note: This path will be set during initialisation
"""

tutorial_file = 'docs/tutorial.html'
"""\
Path to wxGlade HTML Tutorial

@type: str
@note: This path will be set during initialisation
"""

use_gui = True
"""\
If True, wxGlade runs in "GUI" mode. If False, the program is invoked
from the command-line in "batch" mode for generating code only.

@type: bool
"""

widgets_path = 'widgets'
"""\
Path to wxGlade "built-in" widgets

@type: str
@note: This path will be set during initialisation
"""

wxglade_path = '.'
"""\
Program path, set in wxglade.py

@type: str
"""

backed_up = {}
"""\
Set of file names already backed up during this session

@type: dict
@see: L{common.save_file()}
"""

preferences = None
"""\
User preferences

@type: common.Preferences
@see: L{common.Preferences}
"""

rc_file = ''
"""\
Path to the rc / ini file to store user preferences in it

@type: str
@note: This path will be set during initialisation
"""

history_file = ''
"""\
Path to the history file, if used

@type: str
@note: This path will be set during initialisation
"""

use_file_history = False
"""\
Flag to use a file history

@type: bool
"""
if use_gui:
    use_file_history = True

log_file = ''
"""\
Path to wxGlade log file

@type: str
@note: This path will be set during initialisation
"""

label_initial_width = 5
"""\
Initial width of new created labels
"""

tooltip_time = 3
"""\
Number of seconds a tooltip will be shown

@type: int
"""

tooltip_width = 50
"""\
Maximum width to split tooltips into

@type: int
"""

widget_config = {
    'generic_styles': {

        # generic styles from wxSizer
        'wxALL': {
            'desc': _('from wxSizer'),
            'combination': 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM',
        },
        'wxTOP': {
            'desc': _('Apply the border to the top.'),
        },
        'wxBOTTOM': {
            'desc': _('Apply the border to the bottom.'),
        },
        'wxLEFT': {
            'desc': _('Apply the border to the left.'),
        },
        'wxRIGHT': {
            'desc': _('Apply the border to the right.'),
        },
        'wxALIGN_LEFT': {
            'desc': _('Align the item to the left.'),
        },
        'wxALIGN_RIGHT': {
            'desc': _('Align the item to the right.'),
        },
        'wxALIGN_CENTER': {
            'desc': _('Centre the item (horizontally).'),
            'combination': 'wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL',
        },
        'wxALIGN_CENTRE': {
            'desc': _('Centre the item (horizontally).'),
            'synonym': 'wxALIGN_CENTER',
            'rename_to': 'wxALIGN_CENTER',
            'combination': 'wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL',
        },
        'wxALIGN_TOP': {
            'desc': _('Align the item to the top.'),
        },
        'wxALIGN_BOTTOM': {
            'desc': _('Align the item to the bottom.'),
        },
        'wxALIGN_CENTER_VERTICAL': {
            'desc': _('Centre the item vertically.'),
        },
        'wxALIGN_CENTRE_VERTICAL': {
            'desc': _('Centre the item vertically.'),
            'synonym': 'wxALIGN_CENTER_VERTICAL',
            'rename_to': 'wxALIGN_CENTER_VERTICAL',
        },
        'wxALIGN_CENTER_HORIZONTAL': {
            'desc': _('Centre the item horizontally.'),
        },
        'wxALIGN_CENTRE_HORIZONTAL': {
            'desc': _('Centre the item horizontally.'),
            'synonym': 'wxALIGN_CENTER_HORIZONTAL',
            'rename_to': 'wxALIGN_CENTER_HORIZONTAL',
        },
        'wxEXPAND': {
            'desc': _('The item will be expanded to fill the space '
                      'assigned to the item.'),
        },
        'wxSHAPED': {
            'desc': _('The item will be expanded as much as possible while '
                      'also maintaining its aspect ratio.'),
        },
        'wxADJUST_MINSIZE': {
            'desc': _('This style was used in wxWidgets 2.4. Since wxWidgets '
                      '2.6 the behaviour is default. Select wxFIXED_MINSIZE '
                      'to use the old behaviour.'),
            'supported_by': ('wx2',)
        },
        'wxFIXED_MINSIZE': {
            'desc': _('Normally wxSizers will use GetAdjustedBestSize() '
                      'to determine what the minimal size of window items '
                      'should be, and will use that size to calculate the '
                      'layout. This allows layouts to adjust when an item '
                      'changes and its best size becomes different. If you '
                      'would rather have a window item stay the size it '
                      'started with then use wxFIXED_MINSIZE.'),
        },
        'wxRESERVE_SPACE_EVEN_IF_HIDDEN': {
            'desc': _("Normally wxSizers don't allocate space for hidden "
                      "windows or other items. This flag overrides this "
                      "behaviour so that sufficient space is allocated for "
                      "the window even if it isn't visible. This makes it "
                      "possible to dynamically show and hide controls "
                      "without resizing parent dialog, for example. "
                      "This function is new since wxWidgets version 2.8."),
            'supported_by': ('wx3',),
        },

        # generic styles from wxWindow
        'wxTAB_TRAVERSAL': {
            'desc': _('Use this to enable tab traversal for non-dialog '
                      'windows.'),
        },
        'wxFULL_REPAINT_ON_RESIZE': {
            'desc': _('Use this style to force a complete redraw of the '
                      'window whenever it is resized instead of redrawing '
                      'just the part of the window affected by resizing. '),
        },
        'wxNO_FULL_REPAINT_ON_RESIZE': {
            'desc': _('On Windows, this style used to disable repainting '
                      'the window completely when its size is changed. '
                      'Since this behaviour is now the default, the style '
                      'is now obsolete and no longer has an effect.'),
        },
        'wxCLIP_CHILDREN': {
            'desc': _('Use this style to eliminate flicker caused by the '
                      'background being repainted, then children being '
                      'painted over them. Windows only.'),
        },
        'wxWANTS_CHARS': {
            'desc': _("Use this to indicate that the window wants to get "
                      "all char/key events for all keys - even for keys "
                      "like TAB or ENTER which are usually used for "
                      "dialog navigation and which wouldn't be generated "
                      "without this style. If you need to use this style "
                      "in order to get the arrows or etc., but would still "
                      "like to have normal keyboard navigation take place, "
                      "you should call Navigate in response to the key "
                      "events for Tab and Shift-Tab."),
        },

        # Generic border styles
        'wxBORDER_DEFAULT': {
            'desc': _('The window class will decide the kind of border to '
                      'show, if any.'),
            'supported_by': ('wx3',)
        },
        'wxSIMPLE_BORDER': {
            'desc': _('Displays a thin border around the window. '
                      'wxSIMPLE_BORDER is the old name for this style.'),
            'rename_to': 'wxBORDER_SIMPLE',
        },
        'wxBORDER_SIMPLE': {
            'desc': _('Displays a thin border around the window. '
                      'wxSIMPLE_BORDER is the old name for this style.'),
        },
        'wxSUNKEN_BORDER': {
            'desc': _('Displays a sunken border. wxSUNKEN_BORDER is the '
                      'old name for this style.'),
            'rename_to': 'wxBORDER_SUNKEN'
        },
        'wxBORDER_SUNKEN': {
            'desc': _('Displays a sunken border. wxSUNKEN_BORDER is the '
                      'old name for this style.'),
        },
        'wxRAISED_BORDER': {
            'desc': _('Displays a raised border. wxRAISED_BORDER is the '
                      'old name for this style.'),
            'rename_to': 'wxBORDER_RAISED',
        },
        'wxBORDER_RAISED': {
            'desc': _('Displays a raised border. wxRAISED_BORDER is the '
                      'old name for this style.'),
        },
        'wxSTATIC_BORDER': {
            'desc': _('Displays a border suitable for a static control. '
                      'wxSTATIC_BORDER is the old name for this style. '
                      'Windows only.'),
            'rename_to': 'wxBORDER_STATIC',
        },
        'wxBORDER_STATIC': {
            'desc': _('Displays a border suitable for a static control. '
                      'wxSTATIC_BORDER is the old name for this style. '
                      'Windows only.'),
            'rename_to': '',
        },
        'wxBORDER_THEME': {
            'desc': _('Displays a native border suitable for a control, on '
                      'the current platform. On Windows XP or Vista, this '
                      'will be a themed border; on most other platforms a '
                      'sunken border will be used. For more information for '
                      'themed borders on Windows, please see Themed borders '
                      'on Windows.'),
        },
        'wxNO_BORDER': {
            'desc': _('Displays no border, overriding the default border '
                      'style for the window. wxNO_BORDER is the old name '
                      'for this style.'),
            'rename_to': 'wxBORDER_NONE',
        },
        'wxBORDER_NONE': {
            'desc': _('Displays no border, overriding the default border '
                      'style for the window. wxNO_BORDER is the old name '
                      'for this style.'),
        },
        'wxDOUBLE_BORDER': {
            'desc': _('Displays a double border. wxDOUBLE_BORDER is the '
                      'old name for this style. Windows and Mac only.'),
            'rename_to': 'wxBORDER_DOUBLE',
        },
        'wxBORDER_DOUBLE': {
            'desc': _('Displays a double border. wxDOUBLE_BORDER is the '
                      'old name for this style. Windows and Mac only.'),
            'obsolete': _('since wx3.0'),
        },

        # wxDialog styles
        'wxNO_3D': {
            'desc': _('Under Windows, specifies that the child '
                      'controls should not have 3D borders unless '
                      'specified in the control.'),
            'obsolete': _("This style is obsolete and doesn't do anything "
                          "any more, don't use it in any new code."),
            'supported_by': ('wx2',),
        },
        'wxCAPTION': {
            'desc': _('Puts a caption on the dialog box.'),
        },
        'wxCLOSE_BOX': {
            'desc': _('Displays a close box on the frame.'),
        },
        'wxMAXIMIZE_BOX': {
            'desc': _('Displays a maximize box on the dialog.'),
        },
        'wxMINIMIZE_BOX': {
            'desc': _('Displays a minimize box on the dialog.'),
        },
        'wxRESIZE_BORDER': {
            'desc': _('Display a thick frame around the window.'),
        },
        'wxSTAY_ON_TOP': {
            'desc': _('The dialog stays on top of all other windows.'),
        },
        'wxSYSTEM_MENU': {
            'desc': _('Display a system menu.'),
        },
    }
}
"""\
Dictionary to store widget generic widget details like tooltips, different
names, ...

Example::
    config = {
        'wxSplitterWindow' = {
            'supported_by': ('wx28', 'wx3'),
            'style_defs': {
                'wxSP_3D': {
                    'desc': _('Draws a 3D effect border and sash'),
                    'combination': 'wxSP_3DBORDER|wxSP_3DSASH',
            },
        },
        'wxHyperlinkCtrl': {
            'supported_by': ('wx28', 'wx3'),
        },
        'wxDialog': {
            'style_defs': {
                'wxNO_3D': {
                    'desc': _('Under Windows, specifies that the child '
                              'controls should not have 3D borders unless '
                              'specified in the control.'),
                    'supported_by': ('wx2',),
                },
            },
        },
        'generic_styles': {
            'wxALL': {
                'desc': _('from wxSizer'),
                'combination': 'wxLEFT|wxRIGHT|wxTOP|wxBOTTOM',
            },
        },
    }

Elements:
  - I{supported_by} - This widget is only available at the listed wx
    versions. An empty list or a non-existing entry means the widgets is
    always available.

  - I{styles} - Dictionary with style specific settings

  - I{generic_styles} - Generic item to concentrate styles that are not
    part of a specific widget e.g. sizer styles.

  - I{box_label} - Title of the widget style box

  - I{default_style} - Default style for new created widgets

  - I{style_list} - List of all styles to show within the style box

  - I{events} - Dictionary with event specific settings

Style attributes:
  - I{'desc':} I{<description>} - Short style description

  - I{'combination':} I{<styles joined by '|'>} - The style is defined as
    a combination of different other styles

  - I{'exclude':} I{<styles joined by '|'>} - The listed styles will be
    removed by selecting this style

  - I{'include':} I{<styles joined by '|'>} - The style requires additional
    styles. The listed styles are a soft requirement - these styles are
    added even if the "requesting" style will be delete somehow or other.

  - I{'obsolete':} I{<text>} - This style is obsolete. A short notice will
    shown in the style tooltip

  - I{'rename_to:} I{<new style name>} - The style will be renamed into the
    given style name

  - I{'require':} I{<styles joined by '|'>} - The style requires additional
    styles. The listed styles are a hard requirement - these styles are
    added only in together with the "requesting" style. If the "requesting"
    style will be deleted, these styles will be not added.

  - I{'supported_by':} I(<supported version>) - List of versions
    supporting this style

  - I{'synonym':} I{<alternative name>} - Short notice about an alternative
    style name shown in style tooltip

Event attributes:
  - I{'type':} I{<event prototype>} - Event prototype, fallback is
    C{wxCommandEvent}

  - I{'type_wx2':} I{<event prototype>} - Event prototype for wx 2.X, use
    this attribute if the event exists in all supported wx versions

  - I{'type_wx3':} I{<event prototype>} - Event prototype for wx 3.X, use
    this attribute if the event exists in all supported wx versions

  - I{'supported_by':} I{(<supported version>)} - List of versions
    supporting this event

All event attributes are optional. If no attributes are given,
C{wxCommandEvent} will be used as event type.

Use gettext (C{_()}) for the attributes content of "box_label", "desc" and
"obsolete".

The style processing is described in L{gui_mixins.StylesMixin.cn_f()}.

@type: dict
"""


def read_version_file():
    """\
    Read the version information from file "RELEASE-VERSION".

    @rtype: str | None

    @see: L{write_version_file()}
    @see: L{get_version()}
    """

    try:
        import version
        return version.__version__.strip()
    except ImportError:
        return None


def write_version_file(release):
    """\
    Write the given version string into file "version.py".

    @param release: version string to write
    @type release:  str

    @see: L{read_version_file()}
    @see: L{get_version()}
    """
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
    """
    Query the local hg repository to get the current release or return None.

    @rtype: str | None
    """
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
        # look for the special 'rel_X.X' tag
        for tag in tags:
            if tag.startswith('rel_') and len(tag) > 4:
                release = tag[4:]
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
    """\
    Return the release string.

    The release will determinate in three steps:
     1. read from release file (see L{read_version_file()})
     2. Queried from local hg repo (see L{get_hg_version()})
     3. Set to "not found"

    The release string contains a suffix if wxGlade runs as standalone
    edition.

    @param suffix: Append suffix for standalone edition
    @type suffix:  bool

    @rtype: str

    @see: L{read_version_file()}
    @see: L{get_hg_version()}
    """
    release = read_version_file()
    if not release:
        release = get_hg_version()
    if not release:
        release = 'not found'

    if suffix and hasattr(sys, 'frozen'):
        release = '%s (standalone edition)' % release

    return release
