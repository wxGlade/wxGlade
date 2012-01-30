"""
@copyright: 2012 Carsten Grohmann

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import test base class
from tests import WXGladeBaseTest

# import project modules
import wxglade

# import general python modules
import cStringIO
import os.path
import sys


class TestGui(WXGladeBaseTest):
    """\
    Test GUI functionality
    """

    init_stage1 = True
    init_use_gui = True

    def mockMessageBox(self, message, caption, *args, **kwargs):
        """\
        Mock object for wx.MessageBox
        """
        self._messageBox = [message, caption]

    def setUp(self):
        # fake stdout
        sys.stdout = cStringIO.StringIO()

        # initialse base class
        WXGladeBaseTest.setUp(self)

        import common
        import main
        import wx

        # inject mock object for wxMessageBox
        self._messageBox = []
        wx.MessageBox = self.mockMessageBox

        # create an simply application
        self.app = wx.PySimpleApp()
        wx.InitAllImageHandlers()
        wx.ArtProvider.PushProvider(main.wxGladeArtProvider())
        self.frame = main.wxGladeFrame()

        # hide all windows
        self.frame.Hide()
        self.frame.hide_all()

    def tearDown(self):
        self.frame.Destroy()

    def testNotebookWithoutTabs(self):
        infile = cStringIO.StringIO(
            self.loadFile('Notebook_wo_tabs', '.wxg')
            )
        self.frame._open_app(
            infilename=infile,
            use_progress_dialog=False,
            is_filelike=True,
            add_to_history=False,
            )
        err_msg = u'Error loading file None: Notebook widget' \
                  ' "notebook_1" does not have any tabs! ' \
                  '_((line: 18, column:  20))'
        err_caption = u'Error'
        self.failUnless(
            [err_msg, err_caption] == self._messageBox,
            '''Expected wxMessageBox(message=%s, caption=%s)''' % (err_msg, err_caption)
            )
        infile = cStringIO.StringIO(
            self.loadFile('Notebook_w_tabs', '.wxg')
            )
        self.frame._open_app(
            infilename=infile,
            use_progress_dialog=False,
            is_filelike=True,
            add_to_history=False,
            )
