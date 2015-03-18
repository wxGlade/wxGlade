"""\
Dialog to show details of internal errors.

@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import bugdialog_ui
import log

import logging
import sys
import wx


class BugReport(bugdialog_ui.UIBugDialog):
    """\
    Dialog to show details of internal errors.
    """

    _disabled = False
    """\
    Flag to prevent dialog popups during test runs.

    @type: bool
    """

    def __init__(self):
        self._disabled = getattr(sys, '_called_from_test', False)
        bugdialog_ui.UIBugDialog.__init__(self, None, -1, "")

    def SetContent(self, msg, exc):
        """\
        Prepare given exception information and show it as dialog content.

        @param msg: Short description of the action that has raised this error
        @type msg:  str
        @param exc: Caught exception
        @type exc:  Exception

        @see: L{SetContentEI()}
        """
        if self._disabled:
            return

        exc_type = exc.__class__.__name__
        exc_msg = str(exc)
        header = self.st_header.GetLabel() % {'action': msg}
        log.exception_orig(header)
        self._fill_dialog(exc_msg, exc_type, header)

    def SetContentEI(self, exc_type, exc_value, exc_tb, msg=None):
        """\
        Format given exception and add details to dialog.

        @param exc_type: Exception type
        @param exc_value: Exception value
        @param exc_tb: Exception traceback

        @param msg: Short description of the exception
        @type msg:  str | None

        @see: L{SetContent()}
        """
        if self._disabled:
            return

        header = _("An internal error occurred")
        # don't use exception() because it overwrites exc_info with 1
        logging.error(header, exc_info=(exc_type, exc_value, exc_tb))
        self._fill_dialog(msg, exc_type, header)

    def _fill_dialog(self, exc_msg, exc_type, header):
        """\
        Fill the bug dialog

        @param exc_msg: Short exception summary
        @type exc_msg: str | None
        @param exc_type: Exception type
        @type exc_type: str
        @param header: Initial message
        @type header: str

        @see: L{SetContent()}
        @see: L{SetContentEI()}
        """
        details = log.getBufferAsString()

        if not exc_msg:
            exc_msg = _('No summary available')

        summary = self.st_summary.GetLabel() % {
            'exc_type': exc_type,
            'exc_msg': exc_msg}

        self.st_header.SetLabel(header)
        self.st_summary.SetLabel(summary)
        self.tc_details.SetValue(details)

    def OnCopy(self, event):
        """\
        Copy the dialog content to the clipboard
        """
        text = self.tc_details.GetValue()
        if not text:
            return
        data = wx.TextDataObject(text)
        if wx.TheClipboard.Open():
            wx.TheClipboard.SetData(data)
            wx.TheClipboard.Close()
        else:
            wx.MessageBox("Unable to open the clipboard", "Error")

    def ShowModal(self, **kwargs):
        if getattr(sys, '_called_from_test', False):
            return wx.ID_OK
        super(BugReport, self).ShowModal(**kwargs)
