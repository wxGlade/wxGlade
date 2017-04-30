"""\
Test external code

@copyright: 2014-2016 Carsten Grohmann

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import unittest
from copy import deepcopy


class TestExternal(unittest.TestCase):
    """\
    Test different external code
    """

    def test_OrderedDict_deepcopy(self):
        """\
        Test deep copying OrderedDict
        """
        from collections import OrderedDict
        a = OrderedDict([('A', 'A'), ('B', 'B'), (1, 2)])
        a[2] = {'a': 1}
        b = deepcopy(a)
        self.assertEqual(a, b)
        self.assertFalse(a is b)
        self.assertFalse(a[2] is b[2])


