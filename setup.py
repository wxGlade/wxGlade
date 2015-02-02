# -*- coding: utf-8 -*-
"""
Setup script to create release packages

@copyright: 2011-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from setuptools import setup

import os
from glob import glob
import config


def is_package(path):
    return os.path.isdir(path) and \
        os.path.isfile(os.path.join(path, '__init__.py'))


def find_packages(path, base=""):
    """ Find all packages in path """
    packages = {}
    for item in os.listdir(path):
        dir = os.path.join(path, item)
        if is_package(dir):
            if base:
                module_name = "%(base)s.%(item)s" % vars()
            else:
                module_name = item
            packages[module_name] = dir
            packages.update(find_packages(dir, module_name))
    return packages

classifiers = """\
Development Status :: 5 - Production/Stable
Intended Audience :: Developers
License :: OSI Approved :: MIT License
Natural Language :: English
Operating System :: OS Independent (Written in an interpreted language)
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

description = \
    'GUI designer written in Python with the popular GUI toolkit wxPython'

long_description = """\
wxGlade is a GUI designer written in Python with the popular GUI toolkit
wxPython, that helps you create wxWidgets/wxPython user interfaces. At
the moment it can generate Python, C++, Perl, Lisp and XRC (wxWidgets'
XML resources) code."""

text_files = ['CHANGES.txt', 'credits.txt', 'epydoc.conf', 'license.txt',
              'Makefile', 'NEWS.txt', 'README.txt', 'TODO.txt',
              'appdata.xml', 'wxGlade.desktop']

data_files = [
    ['share/wxglade/icons', glob('icons/*.*')],
    ['share/wxglade/icons/gtk', glob('icons/gtk/*')],
    ['share/wxglade/icons/msw', glob('icons/msw/*')],
    ['share/doc/wxglade', text_files],
    ['share/doc/wxglade/doc', glob('docs/*.html')],
    ['share/doc/wxglade/doc/img', glob('docs/img/*.*')],
    ['share/doc/wxglade/doc', glob('docs/*.txt')],
    ['share/doc/wxglade/doc/html', glob('docs/html/*.*')],
    ['share/doc/wxglade/doc/pdf', glob('docs/pdf/*.pdf')],
    ['share/man/man1', ['docs/man/wxglade.1']],
    ['share/doc/wxglade', ['docs/man/manpage.xml']],
    ['share/doc/wxglade', ['docs/src/manual.xml']],
    ['share/doc/wxglade/install', glob('install/*.*')],
    ['share/doc/wxglade/install/rpm', glob('install/rpm/*.*')],
    ['share/doc/wxglade/install/pyinstaller',
     glob('install/pyinstaller/*.*')],
]

scripts = ['wxglade', 'wxglade.pyw']

packages = find_packages(path=".", base='wxglade').keys()
packages.append('wxglade')

# write and handle version file
version = config.get_version(False)
if not os.path.exists('RELEASE-VERSION'):
    config.write_version_file(version)
text_files.append('RELEASE-VERSION')

setup(
    name='wxGlade',
    version=version,
    author='Alberto Griggio, Carsten Grohmann and the wxGlade developers',
    author_email='wxglade-general@lists.sourceforge.net',
    maintainer='Carsten Grohmann',
    maintainer_email='mail@carstengrohmann.de',
    url='http://wxglade.sourceforge.net/',
    classifiers=classifiers.split("\n"),
    description=description,
    long_description=long_description,
    license='MIT License',
    platforms=['WIN32', 'OSX', 'POSIX'],
    scripts=scripts,
    packages=packages,
    package_dir={'wxglade': '.'},
    package_data={'wxglade.widgets': ['widgets.txt'],
                  'wxglade.tests': ['casefiles/*.*'],
                  'wxglade': ['res/*.*']},
    data_files=data_files,
    install_requires=['wxPython >=2.8'],
    setup_requires=["setuptools_hg"],
    )
