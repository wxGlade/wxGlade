"""
Configuration related stuff

@see: L{configdialog}
@copyright: 2007 Alberto Griggio
@copyright: 2013 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
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
@see: L{common.set_version()}
"""

version_nohgfound = 'HG'
"""\
Version number to return if no hg repo has been found

@type: str
@note: This path will be set during initialisation
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

@type: Instance of L{common.Preferences}
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
