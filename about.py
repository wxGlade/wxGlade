# about.py: about box with general info
# $Id: about.py,v 1.24 2007/03/27 07:02:07 agriggio Exp $
#
# Copyright (c) 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import wx
import wx.html
import wx.lib.wxpTag
import common, misc, os.path

class wxGladeAboutBox(wx.Dialog):
    text = '''
    <html>
    <body bgcolor="%s">
    <!-- <font size="-1"> -->
    <center>
    <table align="center" width="380" border="2" cellspacing="0">
    <tr>
    <td align="center"><img src="%s">
    </td></tr>
    <tr><td bgcolor="#000000" align="center">
    <font color="#ffffff">Version %s on Python %s and wxPython %s
    </font>
    </td></tr>
    </table>
    </center>
    <!-- </font> -->
    <table border="0" cellpadding="0" cellspacing="0">
    <tr><td width="50"></td><td>
    <!-- <font size="-1"> -->
    <b><p>License: MIT (see <a href="show_license">license.txt</a>)</b><br>
    <!-- wxPyColourChooser code copyright (c) 2002-2004 <br>Michael Gilfix 
    (wxWindows license) -->
    <p>Home page:
    <a href="http://wxglade.sourceforge.net">http://wxglade.sourceforge.net</a>
    <p>For credits, see
    <a href="show_credits">credits.txt</a>.<!-- </font> --></td>
    </tr></table>
    </body>
    </html>
    '''

    def __init__(self, parent=None):
        wx.Dialog.__init__(self, parent, -1, _('About wxGlade'))
        class HtmlWin(wx.html.HtmlWindow):
            def OnLinkClicked(self, linkinfo):
                href = linkinfo.GetHref()
                if href == 'show_license':
                    if common.license_file:
                        from wx.lib.dialogs import ScrolledMessageDialog
                        try:
                            license_file = open(common.license_file)
                            dlg = ScrolledMessageDialog(
                                self,
                                license_file.read(),
                                _("wxGlade - License")
                                )
                            license_file.close()
                            dlg.ShowModal()
                            dlg.Destroy()
                        except IOError:
                            wx.MessageBox(_("Can't find the license!\n"
                                         "You can get a copy at \n"
                                         "http://www.opensource.org/licenses/"
                                         "mit-license.php"), _("Error"),
                                         wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION)
                    else:
                        wx.MessageBox(_("Can't find the license!\n"
                                     "You can get a copy at \n"
                                     "http://www.opensource.org/licenses/"
                                     "mit-license.php"), _("Error"),
                                     wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION)
                elif href == 'show_credits':
                    if common.credits_file:
                        from wx.lib.dialogs import ScrolledMessageDialog
                        try:
                            credits_file = open(common.credits_file)
                            dlg = ScrolledMessageDialog(
                                self,
                                credits_file.read(),
                                _("wxGlade - Credits")
                                )
                            credits_file.close()
                            dlg.ShowModal()
                            dlg.Destroy()
                        except IOError:
                            wx.MessageBox(_("Can't find the credits file!\n"), _("Error"),
                                         wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION)
                    else:
                        wx.MessageBox(_("Can't find the credits file!\n"), _("Error"),
                                     wx.OK|wx.CENTRE|wx.ICON_EXCLAMATION)
                else:
                    import webbrowser
                    webbrowser.open(linkinfo.GetHref(), new=True)
        html = HtmlWin(self, -1, size=(400, -1))
        # it's recommended at least for GTK2 based wxPython
        if "gtk2" in wx.PlatformInfo:
            html.SetStandardFonts()
        bgcolor = misc.color_to_string(self.GetBackgroundColour())
        icon_path = os.path.join(common.icons_path, 'wxglade_small.png')
        html.SetPage(self.text % (bgcolor, icon_path, common.version,
                                  common.py_version, wx.__version__))
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
            extra_border = 5 # border around a default button
        else: extra_border = 0
        szr.Add(szr2, 0, wx.ALL|wx.ALIGN_RIGHT, 20 + extra_border)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        self.Layout()
        if parent: self.CenterOnParent()
        else: self.CenterOnScreen()

# end of class wxGladeAboutBox


if __name__ == '__main__':
    wx.InitAllImageHandlers()
    app = wx.PySimpleApp()
    d = wxGladeAboutBox()
    app.SetTopWindow(d)
    d.ShowModal()
    
