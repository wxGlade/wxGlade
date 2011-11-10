# -*- mode: python -*-
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import os

# increase the visibility of DEBUG because file is 'executed' using execfile
global DEBUG
global debug
global WARNFILE

WARNFILE = os.path.join(BUILDPATH, 'warnwxglade.txt')
"""\
Set the correct path for the WARNFILE.

PyInstaller 1.5.1 sets the wrong WARNFILE path if custom buildpaths are used
"""

REPO_DIR = os.getcwd()
'''\
Main directory of the wxGlade source tree
'''

PATHEX = [os.path.join(REPO_DIR, 'install\\pyinstaller'),]
'''\
Extend the search path to wxGlade base directory
'''

DEBUG = False
'''\
Print additional information about integrated and analysed but removed files
'''

ONEFILE = False
'''\
Build one file installer

True generates a single exe file with all dependencies inside.
False generates an exe file with all pure python inside and additional
dlls as well as compiled python modules in a directory.
'''

SHOW_CONSOLE = True
'''\
Use the console executable, or the Windows subsystem executable.
'''

ICON = os.path.join(REPO_DIR, 'install\\pyinstaller\\mondrian.ico')
'''\
EXE Icon
'''

BUILD_TARGET = os.path.join(REPO_DIR, 'dist')
'''\
Directory to store the created single exe file.

Directory is used temporarly only because the exe file will connect and
copied to DIST_TARGET too.
'''

DIST_TARGET = os.path.join(REPO_DIR, 'bdist')
'''\
Directory to store all collected files.
'''

NAME_EXE = 'wxglade.exe'
'''\
Name of the created executable
'''

FILES2INCLUDE = [
    'CHANGES.txt',
    'credits.txt',
    'license.txt',
    'README.txt',
    'TODO.txt',
    'widgets\widgets.txt',
    ]
'''\
(DATA) files to add the collection process without processing
path is relative to REPO_DIR
'''

DIRS2INCLUDE = [
    ('codegen', '*.py'),
    ('docs', '*'),
    ('edit_sizers', '*.py'),
    ('icons', '*'),
    ('locale', '*'),
    ('po', '*'),
    ('res', '*'),
    ('templates', '*'),
    ('widgets',  '*.py'),
    ]
'''\
(DATA) directories to add to the collection process without processing
path is relative to REPO_DIR
'''

# =========== NO changes below this line ===========

def debug(msg):
    '''
    Show debug information
    '''
    if DEBUG:
        print 'DEBUG: %s' % msg

def rescursiveFilelist(rootdir, pattern='*.py'):
    '''
    Returns a list of all files matching pattern in the directory

    Directories will be parsed recursively

    @param rootdir: Root directory to list recursively
    @param pattern: Pattern of all files to list
    '''
    # import module late due execfile
    import fnmatch

    fl = []
    for root, dirs, files in os.walk(rootdir):
        fl.extend([os.path.join(root, filename) for filename in files])

    return fnmatch.filter(fl, pattern)

def purgePath(toc, path):
    '''
    Remove all elements below path from the toc.

    @param toc:  TOC to filter
    @param path: Path to drop
    '''
    path = os.path.normpath(path)
    for entry in toc[:]:
        epath = os.path.normpath(entry[1])
        if epath.startswith(path):
            debug('Remove element %s' % entry[0])
            toc -= [entry]

    return toc

files2analyse = [
    os.path.join(HOMEPATH, 'support\\_mountzlib.py'),
    os.path.join(HOMEPATH, 'support\\useUnicode.py'),
    'wxglade.py',
    ]
'''\
Scripts to run
'''

files2analyse += rescursiveFilelist('codegen')
files2analyse += rescursiveFilelist('edit_sizers')
files2analyse += rescursiveFilelist('widgets')
if debug:
    for filename in files2analyse:
        debug('File to analyse: %s' % filename)

# expand content of DIRS2INCLUDE and add them to FILES2INCLUDE
for dir, pattern in DIRS2INCLUDE:
    FILES2INCLUDE.extend(rescursiveFilelist(dir, pattern))

# generate a list of (DATA) files for the collection process
files2collect = [ (entry, os.path.join(REPO_DIR, entry), 'DATA') for entry in FILES2INCLUDE]
for entry in files2collect:
    debug('Additional file: %s' % entry[0])

a = Analysis(
    files2analyse,
    pathex=PATHEX,
    hookspath=PATHEX,
    )

# remove plugin-related code
for dir in ['codegen', 'edit_sizers', 'widgets']:
    a.pure    = purgePath(a.pure,    os.path.join(REPO_DIR, dir))
    a.scripts = purgePath(a.scripts, dir)

# print found files
for s in a.pure:
    debug('a.pure: %s' % str(s))
for s in a.scripts:
    debug('a.scripts: %s' % str(s))

pyz = PYZ(a.pure)

if DEBUG:
    a.scripts += [('v', '', 'OPTION')]

if ONEFILE:
    exe = EXE(
        pyz,
        a.scripts,
        a.binaries,
        a.zipfiles,
        a.datas,
        name=os.path.join(BUILD_TARGET, NAME_EXE),
        debug=DEBUG,
        strip=False,
        upx=True,
        console=SHOW_CONSOLE,
        )
    coll = COLLECT(
        exe,
        files2collect,
        strip=False,
        name=DIST_TARGET,
        )
else:
    exe = EXE(
        pyz,
        a.scripts,
        exclude_binaries=1,
        name=os.path.join(BUILD_TARGET, NAME_EXE),
        debug=DEBUG,
        strip=False,
        console=SHOW_CONSOLE,
        icon=ICON,
        upx=True,
        )
    coll = COLLECT(
        exe,
        a.binaries,
        files2collect,
        strip=False,
        name=DIST_TARGET,
        )
