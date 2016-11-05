"""\
Suite to test compilation of automatically generated C++ code.

@copyright: 2016 Carsten Grohmann

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest
import os
import subprocess
import tempfile


class TestCompile(WXGladeBaseTest):
    """\
    Suite to test compilation of automatically generated C++ code.

    @ivar libs28: Link flags for wx2.8 source code
    @type libs28: str
    @ivar libs30: Link flags for wx3.0 source code
    @type libs30: str

    @ivar cxxflags28: Compile flags for wx2.8 source code
    @type cxxflags28: str
    @ivar cxxflags30: Compile flags for wx3.0 source code
    @type cxxflags30: str

    @ivar _tmpfile: Temporary filename used for the generated binary
    @type _tmpfile: str | None
    """

    def __init__(self, methodName='runTest'):
        super(TestCompile, self).__init__(methodName)

        self.libs28 = ''
        self.cxxflags28 = ''
        self.libs30 = ''
        self.cxxflags30 = ''
        self._tmpfile = None

        for program in ['wx-config', 'wx-config-2.8', 'wx-config-3.0']:
            try:
                release = subprocess.check_output(
                    "%s --release" % program,
                    stderr=subprocess.STDOUT,
                    shell=True
                )
                if release.startswith('2.8'):
                    cmd = "%s --libs" % program
                else:
                    cmd = "%s --libs all" % program
                libs = subprocess.check_output(
                    cmd,
                    stderr=subprocess.STDOUT,
                    shell=True
                )
                cxxflags = subprocess.check_output(
                    "%s --cxxflags" % program,
                    stderr=subprocess.STDOUT,
                    shell=True
                )
                if release.startswith('2.8'):
                    self.libs28 = libs.strip()
                    self.cxxflags28 = cxxflags.strip()
                elif release.startswith('3.0'):
                    self.libs30 = libs.strip()
                    self.cxxflags30 = cxxflags.strip()
            except subprocess.CalledProcessError:
                continue

    def setUp(self):
        super(TestCompile, self).setUp()
        self._tmpfile = tempfile.mktemp()

    def tearDown(self):
        super(TestCompile, self).tearDown()
        if self._tmpfile and os.path.exists(self._tmpfile):
            os.unlink(self._tmpfile)
            self._tmpfile = None

    def _compile28(self, source):
        """\
        Compile the given file with wx28 settings

        @param source: Sourcefile to compile
        @type source:  str
        """
        if not self.libs28:
            self.skipTest('No wx-config for wx28 found')

        self._compile(source, self.libs28, self.cxxflags28)

    def _compile30(self, source):
        """\
        Compile the given file with wx30 settings

        @param source: Sourcefile to compile
        @type source:  str
        """
        if not self.libs30:
            self.skipTest('No wx-config for wx30 found')

        self._compile(source, self.libs30, self.cxxflags30)

    def _compile(self, source, libs, cxxflags):
        """\
        Compile a C++ file

        @param source: Sourcefile to compile
        @type source:  str
        @param libs: Linker flags
        @type libs:  str
        @param cxxflags: C++ compiler flags
        @type cxxflags: str
        """
        abs_filename = self._get_abs_filename(source)

        cmd = 'g++ %s %s %s -o %s' % (
            abs_filename, libs, cxxflags, self._tmpfile)
        try:
            subprocess.check_output(
                cmd,
                stderr=subprocess.STDOUT,
                shell=True)
        except subprocess.CalledProcessError, details:
            self.fail(
                '''\
Can't compile %s:
Return code: %s
Command:     %s
Output:
%s''' % (source, details.returncode, cmd, details.output))
        self.failUnless(
            os.path.exists(self._tmpfile),
            'The application binary was not created!'
        )

    def test_compile_AllWidgets(self):
        """\
        Test compilation of AllWidgets for wx28 and wx30
        """
        self._compile28('AllWidgets_28.cpp')
        self._compile30('AllWidgets_30.cpp')

    def test_compile_ComplexExample(self):
        """\
        Test compilation of ComplexExample for wx28 and wx30
        """
        self._compile28('ComplexExample.cpp')
        self._compile30('ComplexExample_30.cpp')

    def test_compile_FontColour(self):
        """\
        Test compilation of FontColour for wx28 and wx30
        """
        self._compile28('FontColour.cpp')
        self._compile30('FontColour.cpp')

    def test_compile_CPPOgg3(self):
        """\
        Test compilation of CPPOgg3 for wx28 and wx30
        """
        self._compile28('CPPOgg3.cpp')
        self._compile30('CPPOgg3.cpp')

    def test_compile_HyperlinkCtrl_28(self):
        """\
        Test compilation of HyperlinkCtrl for wx28 and wx30
        """
        self._compile28('HyperlinkCtrl_28.cpp')
        self._compile30('HyperlinkCtrl_28.cpp')

    def test_bug183(self):
        """\
        Test compilation for SF bug #183
        """
        self._compile28('bug183.cpp')
        self._compile30('bug183.cpp')

    def test_bug186(self):
        """\
        Test compilation for SF bug #186
        """
        self._compile28('bug186.cpp')
        self._compile30('bug186.cpp')

    def test_bug188(self):
        """\
        Test compilation for SF bug #188
        """
        # broken jet
        return
        self._compile28('bug188_included_toolbar.cpp')
        self._compile30('bug188_included_toolbar.cpp')
        self._compile28('bug188_standalone_toolbar.cpp')
        self._compile30('bug188_standalone_toolbar.cpp')

    def test_bars_wo_parent(self):
        """\
        Test AttributeError during code generation of toplevel menubars
        """
        self._compile28('bars_wo_parent.cpp')
        self._compile30('bars_wo_parent.cpp')

    def test_bug192(self):
        """Test compilation for SF bug #192"""
        self._compile28('bug192.cpp')
        self._compile30('bug192.cpp')

    def test_bug194(self):
        """Test compilation for SF bug #194"""
        self._compile28('bug194.cpp')
        self._compile30('bug194.cpp')