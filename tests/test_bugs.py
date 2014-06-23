"""
@copyright: 2014 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import cStringIO
import os
import re

# import test base class
from tests import WXGladeBaseTest

# import project modules
import common
import errors
import misc


class TestCodeGen(WXGladeBaseTest):
    """\
    Test code generation
    """

    def tearDown(self):
        common.code_writers['python'].use_new_namespace = 1
        WXGladeBaseTest.tearDown(self)

    def test_bug163(self):
        """\
        Test bug #163 - Don't convert first char of a XRC extraproperty to
        upper case.

        That's the test case for SF bug #163.
        """
        self._test_all('bug163')
