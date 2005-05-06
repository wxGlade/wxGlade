# about.py: about box with general info
# $Id: about.py,v 1.21 2005/05/06 21:48:26 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from wxPython.html import *
import wxPython.lib.wxpTag
import common, misc, os.path, sys

class wxGladeAboutBox(wxDialog):
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
        wxDialog.__init__(self, parent, -1, 'About wxGlade')
        class HtmlWin(wxHtmlWindow):
            def OnLinkClicked(self, linkinfo):
                href = linkinfo.GetHref()
                if href == 'show_license':
                    from wxPython.lib.dialogs import wxScrolledMessageDialog
                    try:
                        license = open(os.path.join(common.wxglade_path,
                                                    'license.txt'))
                        dlg = wxScrolledMessageDialog(self, license.read(),
                                                      "wxGlade - License")
                        license.close()
                        dlg.ShowModal()
                        dlg.Destroy()
                    except IOError:
                        wxMessageBox("Can't find the license!\n"
                                     "You can get a copy at \n"
                                     "http://www.opensource.org/licenses/"
                                     "mit-license.php", "Error",
                                     wxOK|wxCENTRE|wxICON_EXCLAMATION)
                elif href == 'show_credits':
                    from wxPython.lib.dialogs import wxScrolledMessageDialog
                    try:
                        credits = open(os.path.join(common.wxglade_path,
                                                    'credits.txt'))
                        dlg = wxScrolledMessageDialog(self, credits.read(),
                                                      "wxGlade - Credits")
                        credits.close()
                        dlg.ShowModal()
                        dlg.Destroy()
                    except IOError:
                        wxMessageBox("Can't find the credits file!\n", "Oops!",
                                     wxOK|wxCENTRE|wxICON_EXCLAMATION)
                else:
                    import webbrowser
                    webbrowser.open(linkinfo.GetHref(), new=True)
        html = HtmlWin(self, -1, size=(400, -1))
        if misc.check_wx_version(2, 5, 3):
            try:
                html.SetStandardFonts()
            except AttributeError:
                pass
        py_version = sys.version.split()[0]
        bgcolor = misc.color_to_string(self.GetBackgroundColour())
        icon_path = os.path.join(common.wxglade_path,
                                 'icons/wxglade_small.png')
        html.SetPage(self.text % (bgcolor, icon_path, common.version,
                                  py_version, wx.__version__))
        ir = html.GetInternalRepresentation()
        ir.SetIndent(0, wxHTML_INDENT_ALL)
        html.SetSize((ir.GetWidth(), ir.GetHeight()))
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(html, 0, wxTOP|wxALIGN_CENTER, 10)
        szr.Add(wxStaticLine(self, -1), 0, wxLEFT|wxRIGHT|wxEXPAND, 20)
        szr2 = wxBoxSizer(wxHORIZONTAL)
        btn = wxButton(self, wxID_OK, "OK")
        btn.SetDefault()
        szr2.Add(btn)
        if wxPlatform == '__WXGTK__':
            extra_border = 5 # border around a default button
        else: extra_border = 0
        szr.Add(szr2, 0, wxALL|wxALIGN_RIGHT, 20 + extra_border)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        self.Layout()
        if parent: self.CenterOnParent()
        else: self.CenterOnScreen()

# end of class wxGladeAboutBox


if __name__ == '__main__':
    wxInitAllImageHandlers()
    app = wxPySimpleApp()
    d = wxGladeAboutBox()
    app.SetTopWindow(d)
    d.ShowModal()
    
