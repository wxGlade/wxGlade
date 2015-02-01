"""
@copyright: 2014-2015 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import cStringIO
import os
import re

# import test base class
from tests import WXGladeBaseTest


class TestCodeGen(WXGladeBaseTest):
    """\
    Test code generation
    """

    def test_bug163(self):
        """\
        Test bug #163 - Don't convert first char of a XRC extraproperty to
        upper case.

        That's the test case for SF bug #163.
        """
        self._test_all('bug163')

    def test_bug165(self):
        """\
        Test bug #165 - Can't rename notebook widget class - internal error
        on Preview

        That's the test case for SF bug #165.
        """
        self._test_all('bug165')

    def test_bug167(self):
        """\
        Test bug #167 - ascii codec error - UnicodeDecodeError will be raised
        if existing files will be changed (and not overwritten) and those files
        contains non-ASCII characters.

        That's the test case for SF bug #167.
        """
        self._test_all('bug167')
        self._test_all('bug167_utf8')
