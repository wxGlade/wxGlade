
# Dietmar Schwertberger 2023:
# building a binary distribution for Windows using embeddable interpreter:
"""
download and unpack embedable archive, e.g. https://www.python.org/ftp/python/3.12.0/python-3.12.0-embed-amd64.zip

unpack into wxglade/python

# in python312._pth uncomment:
import site

Download get-pip.py: https://pip.pypa.io/en/stable/installation/#get-pip-py

# run
python get-pip.py

# install wxPython:
python -mpip install wxpython

# install win32 extensions:
python -mpip install pywin32

# run this script in the wxGlade directory to create wxglade.exe and wxgladew.exe:
python/python.exe install/make_exe.py

replace icons using e.g. ResourceHacker.exe

merge .exe files and directory python into wxglade source code
remove unused libraries, e.g. pip, setuptools, distutils

"""



# see https://stackoverflow.com/questions/35412392/how-can-i-use-setuptools-to-create-an-exe-launcher

from pip._vendor.distlib.scripts import ScriptMaker

sm = ScriptMaker(
    None,    # source_dir - not needed when using an entry spec
    target_dir = ".", # folder that your exe should end up inside
    add_launchers = True   # create exe launchers, not just python scripts
)

# create only the main variant (not the one with X.Y suffix)
sm.variants = [""]

# set the python executable manually; don't use pythonw
#sm.executable = sys.executable.replace("pythonw", "python")
sm.executable = "python\\python.exe"


# argument options: keys 'interpreter_args' and 'gui' are available

options = {'gui':True}
# provide an entry specification string here, just like in pyproject.toml
sm.make("wxgladew = wxglade:run_main", options)  # use pythonw

sm.executable = "python\\python.exe"
# provide an entry specification string here, just like in pyproject.toml
sm.make("wxglade = wxglade:run_main")
