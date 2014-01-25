"""\
Dialog to show details of internal errors.

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import bugdialog_ui
import log

import logging
import wx


class BugReport(bugdialog_ui.UIBugDialog):
    """\
    Dialog to show details of internal errors.
    """

    def __init__(self):
        bugdialog_ui.UIBugDialog.__init__(self, None, -1, "")

    def SetContent(self, action, exc):
        """\
        @param action: Short description of the action that has raised this error
        @type action:  String
        @param exc: Caught exception
        @type exc:  Exception
        """
        exc_type = exc.__class__.__name__
        exc_msg = str(exc)
        if not exc_msg:
            exc_msg = _('no summary available')

        header = self.st_header.GetLabel() % {'action': action}
        summary = self.st_summary.GetLabel() % {
            'exc_type': exc_type,
            'exc_msg': exc_msg,
        }

        # deactivate the general msg dialog and reactivate it later again
        app = wx.GetApp()
        app.show_dialog = False
        logging.exception(header)
        details = log.getBufferAsString(clean=True)
        app.show_dialog = True

        self.st_header.SetLabel(header)
        self.st_summary.SetLabel(summary)
        self.tc_details.SetValue(details)

    def OnCopy(self, event):
        text = self.tc_details.GetValue()
        if not text:
            return
        data = wx.TextDataObject()
        data.SetText(text)
        if wx.TheClipboard.Open():
            wx.TheClipboard.SetData(data)
            wx.TheClipboard.Close()
        else:
            wx.MessageBox("Unable to open the clipboard", "Error")
