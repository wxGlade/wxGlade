# -*- coding: utf-8 -*-
"""
Setup script to create release packages

@copyright: 2011-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from setuptools import setup, find_packages

import os
from glob import glob
import fnmatch
import sys

# gettext support is needed by config module
import gettext
t = gettext.NullTranslations()
t.install()

import config


# Filter output of the original modules list to exclude the file "test.py"
# from binary packages
def filter_modules(self, package, package_dir):
    modules = orig_find_package_modules(self, package, package_dir)
    for entry in modules[:]:
        package, module, filename = entry
        if os.path.normpath(filename) == 'test.py':
            modules.remove(entry)
    return modules

# inject own implementation
from distutils.command.build_py import build_py
orig_find_package_modules = build_py.find_package_modules
build_py.find_package_modules = filter_modules


def recursive(path, pattern='*'):
    """\
    Returns a list of files found in the path matching the pattern

    @param path:    Path to list matching files
    @type path:     str
    @param pattern: Filter pattern
    @type pattern:  str
    @rtype: list[str]
    """
    matches = []
    for dirpath, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, pattern):
            matches.append(os.path.join(dirpath, filename))
    return matches


classifiers = """\
Development Status :: 5 - Production/Stable
Intended Audience :: Developers
License :: OSI Approved :: MIT License
Natural Language :: English
Operating System :: OS Independent
Operating System :: MacOS
Operating System :: Microsoft :: Windows
Operating System :: POSIX
Operating System :: Unix
Programming Language :: Python
Programming Language :: Python :: 2
Programming Language :: Python :: 2.4
Programming Language :: Python :: 2.5
Programming Language :: Python :: 2.6
Programming Language :: Python :: 2.7
Programming Language :: Python :: 2 :: Only
Topic :: Software Development :: Code Generators
Topic :: Software Development :: User Interfaces
User Interface :: Textual :: Command-line
User Interface :: Toolkits/Libraries :: wxWidgets"""

description = 'GUI designer written in Python with the popular GUI toolkit wxPython'

long_description = """\
wxGlade is a GUI designer written in Python with the popular GUI toolkit
wxPython, that helps you create wxWidgets/wxPython user interfaces. At
the moment it can generate Python, C++, Perl, Lisp and XRC (wxWidgets'
XML resources) code."""

text_files = ['CONTRIBUTING.txt', 'CREDITS.txt',
              'LICENSE.txt', 'NEWS.txt', 'README.txt', 'docs/Todo.txt',]

data_files = [
    ['share/wxglade/icons', glob('icons/*.*')],
    ['share/wxglade/icons/gtk', glob('icons/gtk/*')],
    ['share/wxglade/icons/msw', glob('icons/msw/*')],
    ['share/wxglade/templates', glob('templates/*')],
    ['share/doc/wxglade', text_files],
    ['share/doc/wxglade/tutorial', glob('docs/Tutorial.html')],
    ['share/doc/wxglade/tutorial/img', glob('docs/img/*.*')],
    ['share/doc/wxglade/manual_html', glob('docs/html/*.*')],
    ['share/doc/wxglade/manual_pdf', glob('docs/pdf/*.pdf')]
]

packages = ['wxglade.%s' % pkg for pkg in find_packages(exclude=['tests'])]
packages.append('wxglade')

# write and handle version file
version = config.get_version(False)
if not os.path.exists('version.py'):
    config.write_version_file(version)

if 'sdist' in sys.argv:
    package_data_files = []
    package_data_files.extend(text_files)
    package_data_files.extend(['Makefile',
                               'epydoc.conf',
                               'pylintrc',
                               'wxGlade.desktop',
                               'wxglade.pyw',
                               'widgets/widgets.txt',
                               '__init__.py',
                               'test.py'
                               ])

    # add content of listed directories to the package_data_file list
    for data_dir in ['docs', 'install', 'icons', 'locale', 'po', 'res', 'tests', 'templates']:
        package_data_files.extend(recursive(data_dir))

    package_data = {
        'wxglade': package_data_files,
    }
else:
    package_data = {
        'wxglade.widgets': ['widgets.txt'],
        'wxglade': ['res/*.*'],
    }

setup(
    name='wxGlade',
    version=version,
    author='Alberto Griggio, Carsten Grohmann, Dietmar Schwertberger and the wxGlade developers',
    author_email='wxglade-general@lists.sourceforge.net',
    maintainer='Dietmar Schwertberger',
    maintainer_email='wxglade@schwertberger.de',
    url='http://wxglade.sourceforge.net/',
    classifiers=classifiers.split("\n"),
    description=description,
    long_description=long_description,
    license='MIT License',
    platforms=['WIN32', 'OSX', 'POSIX'],
    scripts=['wxglade'],
    packages=packages,
    package_dir={'wxglade': '.'},
    data_files=data_files,
    install_requires=['wxPython >=2.8'],
    package_data=package_data,
)
