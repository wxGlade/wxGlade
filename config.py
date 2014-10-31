"""
Configuration related stuff

@note: Don't place code with gettext or dependencies to other wxGlade
       parts here!

@see: L{preferencesdialog}
@copyright: 2007 Alberto Griggio
@copyright: 2013-2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
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

default_encoding = 'UTF-8'
"""\
Default value for encoding

@type: str
@see: L{encoding}
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
@see: L{wxglade.init_stage1()}
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
Path of the credits file "credits.txt"

@type: str
"""

docs_path = 'docs'
"""\
Path to wxGlade documentation (e.g. html tutorial, license.txt, credits.txt)

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
Path of the license file "license.txt"

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

tutorial_file = 'docs/html/index.html'
"""\
Path to wxGlade tutorial (HTML)

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

_backed_up = {}
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


def read_version_file():
    """\
    Read the version information from file "RELEASE-VERSION".

    @rtype: str | None

    @see: L{write_version_file()}
    @see: L{get_version()}
    """
    try:
        fh = open('RELEASE-VERSION', 'r')
        try:
            release = fh.readlines()[0]
            return release.strip()
        finally:
            fh.close()
    except:
        return None


def write_version_file(release):
    """\
    Write the given version string into file "RELEASE-VERSION".

    @param release: version string to write
    @type release:  str

    @see: L{read_version_file()}
    @see: L{get_version()}
    """
    fh = open('RELEASE-VERSION', 'w')
    fh.write("%s\n" % release)
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
