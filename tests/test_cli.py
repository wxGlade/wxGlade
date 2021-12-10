"""
@copyright: 2020 Dietmar Schwertberger

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from testsupport_new import WXGladeCLITest

import common, wxglade
import unittest, os


class TestCodegen(WXGladeCLITest):
    "Test CLI code generation"

    def test_codegen_AllWidgets_30(self):
        self.generate('AllWidgets_30', excluded=["lisp"])

    def test_codegen_AllWidgets_28(self):
        self.generate('AllWidgets_28')

    def test_Issue502_codegen(self):
        self.generate('Issue502_codegen_fail', included=["perl"])

if __name__ == '__main__':
    unittest.main(exit=False)
