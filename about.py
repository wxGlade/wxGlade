"""
About box with general info

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import bugdialog
import codecs
import wx
import wx.html
import wx.lib.wxpTag
import config
import misc
import os.path


class wxGladeAboutBox(wx.Dialog):
    text = '''
    <html>
    <body bgcolor="%s">
    <!-- <font size="-1"> -->
    <center>
    <table align="center" border="2" cellspacing="0">
    <tr>
    <td align="center"><img src="%s">
    </td></tr>
    <tr><td bgcolor="#000000" align="center">
    <font color="#ffffff">Version %s on Python %s and wxPython %s</font>
    </td></tr>
    </table>
    </center>
    <!-- </font> -->
    <table border="0" cellpadding="0" cellspacing="0">
    <tr><td width="50"></td><td>
    <!-- <font size="-1"> -->
    <b><p>License: MIT (see <a href="show_license">LICENSE.txt</a>)</b><br>
    <!-- wxPyColourChooser code copyright (c) 2002-2004 <br>Michael Gilfix 
    (wxWindows license) -->
    <p>Home page:
    <a href="http://wxglade.sourceforge.net">http://wxglade.sourceforge.net</a>
    <p>For credits, see
    <a href="show_credits">CREDITS.txt</a>.<!-- </font> --></td>
    </tr></table>
    </body>
    </html>
    '''

    def __init__(self, parent=None):
        wx.Dialog.__init__(self, parent, -1, _('About wxGlade'))
        html = wx.html.HtmlWindow(self, -1, size=(480, 250))
        html.Bind(wx.html.EVT_HTML_LINK_CLICKED, self.OnLinkClicked)
        # it's recommended at least for GTK2 based wxPython
        if "gtk2" in wx.PlatformInfo:
            html.SetStandardFonts()
        bgcolor = misc.color_to_string(self.GetBackgroundColour())
        icon_path = os.path.join(config.icons_path, 'wxglade_small.png')
        html.SetPage( self.text % (bgcolor, icon_path, config.version, config.py_version, config.wx_version) )
        ir = html.GetInternalRepresentation()
        ir.SetIndent(0, wx.html.HTML_INDENT_ALL)
        html.SetSize((ir.GetWidth(), ir.GetHeight()))
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(html, 0, wx.TOP|wx.ALIGN_CENTER, 10)
        szr.Add(wx.StaticLine(self, -1), 0, wx.LEFT|wx.RIGHT|wx.EXPAND, 20)
        szr2 = wx.BoxSizer(wx.HORIZONTAL)
        btn = wx.Button(self, wx.ID_OK, _("OK"))
        btn.SetDefault()
        szr2.Add(btn)
        if wx.Platform == '__WXGTK__':
            extra_border = 5  # border around a default button
        else:
            extra_border = 0
        szr.Add(szr2, 0, wx.ALL|wx.ALIGN_RIGHT, 20 + extra_border)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        self.Layout()
        if parent: self.CenterOnParent()
        else: self.CenterOnScreen()

    def OnLinkClicked(self, event):
        href = event.GetLinkInfo().GetHref()
        if href == 'show_license':
            if config.license_file:
                from wx.lib.dialogs import ScrolledMessageDialog
                try:
                    license_file = codecs.open(config.license_file, encoding='UTF-8')
                    dlg = ScrolledMessageDialog( self, license_file.read(), "wxGlade - License" )
                    license_file.close()
                    dlg.ShowModal()
                    dlg.Destroy()
                except EnvironmentError as inst:
                    bugdialog.ShowEnvironmentError(
                        _('''Can't read the file "LICENSE.txt".\n\nYou can get a license copy at\n'''
                          '''http://www.opensource.org/licenses/mit-license.php'''), inst)
            else:
                wx.MessageBox(_('File "LICENSE.txt" not found!\nYou can get a license copy at\n'
                                'http://www.opensource.org/licenses/mit-license.php'),
                              _('Error'), wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION)
        elif href == 'show_credits':
            if config.credits_file:
                from wx.lib.dialogs import ScrolledMessageDialog
                try:
                    credits_file = codecs.open( config.credits_file, encoding='UTF-8' )
                    dlg = ScrolledMessageDialog( self, credits_file.read(), _("wxGlade - Credits") )
                    credits_file.close()
                    dlg.ShowModal()
                    dlg.Destroy()
                except EnvironmentError as inst:
                    bugdialog.ShowEnvironmentError(_('''Can't read the file "CREDITS.txt"'''), inst)
            else:
                wx.MessageBox(_('File "CREDITS.txt" not found!'), _('Error'),
                              wx.OK | wx.CENTRE | wx.ICON_EXCLAMATION)
        else:
            import webbrowser
            webbrowser.open(href, new=True)


