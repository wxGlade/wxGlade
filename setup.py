# -*- coding: utf-8 -*-
"""
Setup script to create release packages

@copyright: 2011-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from setuptools import setup, find_packages

import os
from glob import glob

# gettext support is needed by config module
import gettext
t = gettext.NullTranslations()
t.install()

import config

# Hack: use own implementation to filter module file "test.py"
def find_package_modules2(self, package, package_dir):
    self.check_package(package, package_dir)
    module_files = glob(os.path.join(package_dir, "*.py"))
    modules = []
    setup_script = os.path.abspath(self.distribution.script_name)

    for f in module_files:
        abs_f = os.path.abspath(f)
        if package == 'wxglade' and os.path.normpath(f) == 'test.py':
            self.debug_print("excluding %s" % f)
        elif abs_f != setup_script:
            module = os.path.splitext(os.path.basename(f))[0]
            modules.append((package, module, f))
        else:
            self.debug_print("excluding %s" % setup_script)
    return modules

# inject own implementation
from distutils.command.build_py import build_py
build_py.find_package_modules = find_package_modules2


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

description = \
    'GUI designer written in Python with the popular GUI toolkit wxPython'

long_description = """\
wxGlade is a GUI designer written in Python with the popular GUI toolkit
wxPython, that helps you create wxWidgets/wxPython user interfaces. At
the moment it can generate Python, C++, Perl, Lisp and XRC (wxWidgets'
XML resources) code."""

text_files = ['CHANGES.txt', 'CONTRIBUTING.txt', 'credits.txt',
              'license.txt', 'NEWS.txt', 'README.txt', 'TODO.txt', ]

data_files = [
    ['share/wxglade/icons', glob('icons/*.*')],
    ['share/wxglade/icons/gtk', glob('icons/gtk/*')],
    ['share/wxglade/icons/msw', glob('icons/msw/*')],
    ['share/wxglade/templates', glob('templates/*')],
    ['share/doc/wxglade', text_files],
    ['share/doc/wxglade/tutorial', glob('docs/tutorial.html')],
    ['share/doc/wxglade/tutorial/img', glob('docs/img/*.*')],
    ['share/doc/wxglade/manual_html', glob('docs/html/*.*')],
    ['share/doc/wxglade/manual_pdf', glob('docs/pdf/*.pdf')],
    ['share/man/man1', ['docs/man/wxglade.1']],
]

packages = ['wxglade.%s' % pkg for pkg in find_packages(exclude=['tests'])]
packages.append('wxglade')

# write and handle version file
version = config.get_version(False)
if not os.path.exists('version.py'):
    config.write_version_file(version)

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
    scripts=['wxglade'],
    packages=packages,
    package_dir={'wxglade': '.'},
    package_data={'wxglade.widgets': ['widgets.txt'],
                  'wxglade': ['res/*.*']},
    data_files=data_files,
    install_requires=['wxPython >=2.8'],
    setup_requires=["setuptools_hg"],
)
