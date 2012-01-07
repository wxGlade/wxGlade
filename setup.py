# -*- coding: Latin-1 -*-
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from distutils.core import setup
import distutils.command.sdist
from distutils.util import convert_path

import os
from types import *
from glob import glob
import common

# distutils sdisk command is broken because it doesn't copy data_files
# Bug: http://bugs.python.org/issue2279
def add_defaults_fixed(self):
    """Add all the default files to self.filelist:
      - README or README.txt
      - setup.py
      - test/test*.py
      - all pure Python modules mentioned in setup script
      - all files pointed by package_data (build_py)
      - all files defined in data_files.
      - all files defined as scripts.
      - all C sources listed as part of extensions or C libraries
        in the setup script (doesn't catch C headers!)
    Warns if (README or README.txt) or setup.py are missing; everything
    else is optional.
    """
    standards = [('README', 'README.txt'), self.distribution.script_name]
    for fn in standards:
        if isinstance(fn, tuple):
            alts = fn
            got_it = False
            for fn in alts:
                if os.path.exists(fn):
                    got_it = True
                    self.filelist.append(fn)
                    break

            if not got_it:
                self.warn("standard file not found: should have one of " +
                          ', '.join(alts))
        else:
            if os.path.exists(fn):
                self.filelist.append(fn)
            else:
                self.warn("standard file '%s' not found" % fn)

    optional = ['test/test*.py', 'setup.cfg']
    for pattern in optional:
        files = filter(os.path.isfile, glob(pattern))
        self.filelist.extend(files)

    # build_py is used to get:
    #  - python modules
    #  - files defined in package_data
    build_py = self.get_finalized_command('build_py')

    # getting python files
    if self.distribution.has_pure_modules():
        self.filelist.extend(build_py.get_source_files())

    # getting package_data files
    # (computed in build_py.data_files by build_py.finalize_options)
    for pkg, src_dir, build_dir, filenames in build_py.data_files:
        for filename in filenames:
            self.filelist.append(os.path.join(src_dir, filename))

    # getting distribution.data_files
    if self.distribution.has_data_files():
        for item in self.distribution.data_files:
            if isinstance(item, str): # plain file
                item = convert_path(item)
                if os.path.isfile(item):
                    self.filelist.append(item)
            else:    # a (dirname, filenames) tuple
                dirname, filenames = item
                for f in filenames:
                    f = convert_path(f)
                    if os.path.isfile(f):
                        self.filelist.append(f)

    if self.distribution.has_ext_modules():
        build_ext = self.get_finalized_command('build_ext')
        self.filelist.extend(build_ext.get_source_files())

    if self.distribution.has_c_libraries():
        build_clib = self.get_finalized_command('build_clib')
        self.filelist.extend(build_clib.get_source_files())

    if self.distribution.has_scripts():
        build_scripts = self.get_finalized_command('build_scripts')
        self.filelist.extend(build_scripts.get_source_files())


# Replace old implementation by the new own
distutils.command.sdist.sdist.add_defaults = add_defaults_fixed

def is_package(path):
    return (
        os.path.isdir(path) and
        os.path.isfile(os.path.join(path, '__init__.py'))
        )

def find_packages(path, base="" ):
    """ Find all packages in path """
    packages = {}
    for item in os.listdir(path):
        dir = os.path.join(path, item)
        if is_package( dir ):
            if base:
                module_name = "%(base)s.%(item)s" % vars()
            else:
                module_name = item
            packages[module_name] = dir
            packages.update(find_packages(dir, module_name))
    return packages

classifiers = """\
Development Status :: 4 - Beta
Intended Audience :: Developers
License :: OSI Approved :: MIT License
Natural Language :: English
Operating System :: OS Independent (Written in an interpreted language)
Programming Language :: Python :: 2
Topic :: Software Development :: Code Generators
User Interface :: Textual :: Command-line
User Interface :: Toolkits/Libraries :: wxWidgets"""

description = \
    'GUI designer written in Python with the popular GUI toolkit wxPython'

data_files = [[
        'share/doc/wxglade',
        ['CHANGES.txt', 'credits.txt', 'epydoc.conf', 'license.txt',
         'Makefile', 'NEWS.txt', 'README.txt', 'TODO.txt',],
    ],[
       'share/doc/wxglade/doc',
       glob('docs/*.html'),
    ],[
       'share/doc/wxglade/doc/img',
       glob('docs/img/*.*'),
    ],[
       'share/doc/wxglade/doc',
       glob('docs/*.txt'),
    ],[
       'share/doc/wxglade/doc/html',
       glob('docs/html/*.*'),
    ],[
       'share/doc/wxglade/doc/pdf',
       glob('docs/pdf/*.pdf'),
    ],[
       'share/man/man1',
       ['docs/man/wxglade.1'],
    # documentation source files :-)
    ],[
       'share/doc/wxglade',
       ['docs/man/manpage.xml'],
    ],[
       'share/doc/wxglade',
       ['docs/src/manual.xml'],
    ]]


scripts = ['wxglade',]

packages = find_packages(path=".", base='wxglade').keys()
packages.append('wxglade')

version = common.version

setup(
    name='wxGlade',
    version=version,
    author='Alberto Griggio',
    author_email='agriggio@users.sourceforge.net',
    url='http://wxglade.sourceforge.net/',
    classifiers=classifiers.split("\n"),
    description=description,
    license='MIT License',
    platforms = ['WIN32', 'OSX', 'POSIX'],
    scripts=scripts,
    packages=packages,
    package_dir={'wxglade': '.'},
    package_data={'wxglade.widgets': ['widgets.txt'],
                  'wxglade' : ['icons/*.*', 
                              'icons/gtk/*.*',
                              'icons/msw/*.*',
                              'res/*.*',
                              ]},
    data_files=data_files,
    )
