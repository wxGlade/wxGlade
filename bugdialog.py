"""\
Dialog to show details of internal errors.

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import bugdialog_ui
import config
import log

import logging
import sys
import wx


class BugReport(bugdialog_ui.UIBugDialog):
    "Dialog to show details of internal errors"

    _disabled = False  # Flag to prevent dialog popups during test runs.

    def __init__(self):
        self._disabled = getattr(sys, '_called_from_test', False)
        bugdialog_ui.UIBugDialog.__init__(self, None, -1, "")

    def SetContent(self, msg, exc):
        """Prepare given exception information and show it as dialog content.

        msg: Short description of the action that has raised this error
        exc: Caught exception (Exception instance)
        see: SetContentEI()"""
        if self._disabled:
            return

        exc_type = exc.__class__.__name__
        exc_msg = str(exc)
        header = self.st_header.GetLabel() % {'action': msg}
        log.exception_orig(header)
        self._fill_dialog(exc_msg, exc_type, header)

    def SetContentEI(self, exc_type, exc_value, exc_tb, msg=_('An internal error occurred')):
        """Format given exception and add details to dialog.

        exc_type: Exception type
        exc_value: Exception value
        exc_tb: Exception traceback

        msg: Short description of the exception

        see: SetContent()"""
        if self._disabled:
            return

        # don't use exception() because it overwrites exc_info with 1
        logging.error(msg, exc_info=(exc_type, exc_value, exc_tb))
        self._fill_dialog(msg, exc_type, _('An internal error occurred'))

    def _fill_dialog(self, exc_msg, exc_type, header):
        """Fill the bug dialog

        exc_msg: Short exception summary
        exc_type: Exception type as string
        header: Initial message

        see: SetContent(), SetContentEI()"""
        details = log.getBufferAsString()

        if not exc_msg:
            exc_msg = _('No summary available')

        summary = self.st_summary.GetLabel() % { 'exc_type':exc_type, 'exc_msg':exc_msg }

        self.st_header.SetLabel(header)
        self.st_summary.SetLabel(summary)
        self.tc_details.SetValue(details)
        howto = self.tc_howto_report.GetValue()
        howto = howto % {'log_file': config.log_file}
        self.tc_howto_report.SetValue(howto)

    def OnCopy(self, event):
        "Copy the dialog content to the clipboard"
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


def Show(msg, exc):
    """Wrapper for creating a BugReport dialog and show the details of the given exception instance.

    msg: Short description of the action that has raised this error
    exc: Caught exception

    see ShowEI(), BugReport.SetContent()"""
    dialog = BugReport()
    dialog.SetContent(msg, exc)
    dialog.ShowModal()
    dialog.Destroy()


def ShowEI(exc_type, exc_value, exc_tb, msg=None):
    """Wrapper for creating a BugReport dialog and show the given exception details.

    exc_type: Exception type
    exc_value: Exception value
    exc_tb: Exception traceback
    msg: Short description of the exception

    see: Show(), BugReport.SetContent()"""
    dialog = BugReport()
    dialog.SetContentEI(exc_type, exc_value, exc_tb, msg)
    dialog.ShowModal()
    dialog.Destroy()


def ShowEnvironmentError(msg, inst):
    """Show EnvironmentError exceptions detailed and user-friendly

    msg:  Error message
    inst: The caught exception"""

    details = {'msg':msg, 'type':inst.__class__.__name__}

    if inst.filename:
        details['filename'] = _('Filename:   %s') % inst.filename

    if inst.errno is not None and inst.strerror is not None:
        details['error'] = '%s - %s' % (inst.errno, inst.strerror)
    else:
        details['error'] = str(inst.args)

    text = _("""%(msg)s

Error type: %(type)s
Error code: %(error)s
%(filename)s""") % details

    wx.MessageBox(text, _('Error'), wx.OK | wx.CENTRE | wx.ICON_ERROR)
