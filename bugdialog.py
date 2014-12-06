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

    def SetContent(self, action=None, exc=None, ei=None):
        """\
        Prepare given exception information and show it as dialog content.

        Use parameters 'action' and 'exec' or just 'ei'.

        @param action: Short description of the action that has raised this
                       error
        @type action:  str
        @param exc: Caught exception
        @type exc:  Exception
        @param ei: Exception information
        @type ei: (exc_type, exc_value, exc_tb)
        """
        assert (action and exc) or ei

        if exc:
            exc_type = exc.__class__.__name__
            exc_msg = str(exc)
        else:
            exc_type = ei[0]
            exc_msg = None

        if not exc_msg:
            exc_msg = _('No summary available')

        if action:
            header = self.st_header.GetLabel() % {'action': action}
        else:
            header = _("An internal error occurred")

        summary = self.st_summary.GetLabel() % {
            'exc_type': exc_type,
            'exc_msg': exc_msg}

        # deactivate the general msg dialog and reactivate it later again
        app = wx.GetApp()
        app.show_dialog = False

        if exc:
            logging.exception(header)
        else:
            logging.error(header, exc_info=ei)

        details = log.getBufferAsString()

        app.show_dialog = True

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