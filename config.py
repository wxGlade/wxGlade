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
default_encoding = 'UTF-8'
"""\
Default value for encoding

@type: String
@see: L{encoding}
"""

default_indent_amount = 4
"""\
Default value for indentation

@type: Integer
"""

default_indent_symbol = ' '
"""\
Default value for indentation symbol

@type: String
"""

default_multiple_files = 0
"""\
Default value for writing multiple files (each class in a separate file)

@type: Integer
"""

default_overwrite = 1
"""\
Default value for overwriting existing sources

@type: Integer
"""

default_use_gettext = 1
"""\
Default value to usage of gettext

@type: Integer
"""

encoding = None
"""\
System default character encoding.

The default application L{default_encoding} is the fallback only.

@type: String or None
@see: L{default_encoding}
@see: L{wxglade.init_stage1()}
"""

appdata_path = ''
"""\
Directory to wxGlades application data like file history and templates
"""

credits_file = ''
"""\
Path of the credits file "credits.txt"
"""

docs_path = 'docs'
"""\
Path to wxGlade documentation (e.g. html tutorial, license.txt, credits.txt)

@note: This path will be set during initialisation
"""

home_path = ''
"""\
Users home directory
"""

icons_path = 'icons'
"""\
Path to wxGlade icons

@note: This path will be set during initialisation
"""

license_file = ''
"""\
Path of the license file "license.txt"
"""

platform = None
"""\
Current platform (mostly wx.Platform)
"""

py_version = sys.version.split()[0]
"""\
Python version
"""

templates_path = 'templates'
"""\
System template path

@note: This path will be set during initialisation
"""

tutorial_file = 'docs/html/index.html'
"""\
Path to wxGlade tutorial (HTML)

@note: This path will be set during initialisation
"""

use_gui = True
"""\
If False, the program is invoked from the command-line in "batch" mode
(for code generation only)
"""

version = 'not_set'
"""\
wxGlade version string
@see: L{common.set_version()}
"""

version_nohgfound = 'HG'
"""\
Version number to return if no hg repo has been found
"""

widgets_path = 'widgets'
"""\
Path to wxGlade "built-in" widgets

@note: This path will be set during initialisation
"""

wxglade_path = '.'
"""\
Program path, set in wxglade.py
"""

_backed_up = {}
"""\
Set of file names already backed up during this session

@see: L{common.save_file()}
"""

preferences = None
"""\
Instance of user preferences

@see: L{common.Preferences}
"""

rc_file = ''
"""\
Path to the rc / ini file to store user preferences in it
"""

history_file = ''
"""\
Path to the history file, if used
"""

use_file_history = False
"""\
Flag to use a file history
"""
if use_gui:
    use_file_history = True
