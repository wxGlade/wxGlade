# about.py: about box with general info
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
from wxPython.html import *
import wxPython.lib.wxpTag
import common, misc

class wxGladeAboutBox(wxDialog):
    text = '''
    <html>
    <body bgcolor="%s">
    <font size="-1">
    <center>
    <table align="center" width="380" border="2" cellspacing="0">
    <tr>
    <td align="center" valign="center"><img src="icons/wxglade_small.png"
    border="0">
    </td></tr>
    <tr><td bgcolor="#000000"
    align="center">
    <font color="#ffffff">Version %s &nbsp;&nbsp;on Python %s and wxPython %s
    </font>
    </td></tr>
    </table>
    </center>
    </font>
    <table border="0" cellpadding="0" cellspacing="0">
    <tr><td width="50"></td><td>
    <font size="-1"><b>
    <p>Copyright (c) 2002 Alberto Griggio<br>
    License: Python 2.2 license (see license.txt)</b>
    <p>Home page: <a href="http://wxglade.sourceforge.net">http://wxglade.sourceforge.net</a>
    <p>Authors:<br>
    &nbsp;&nbsp;&nbsp;Alberto Griggio &lt;albgrig@tiscalinet.it&gt;<br>
    &nbsp;&nbsp;&nbsp;Marco Barisione &lt;marco.bari@vene.ws&gt;
    <p>For credits, see credits.txt</font></td></tr></table>
    </body>
    </html>
    '''

    def __init__(self, parent=None):
        wxDialog.__init__(self, parent, -1, 'About wxGlade')
        class HtmlWin(wxHtmlWindow):
            def OnLinkClicked(self, linkinfo):
                import webbrowser
                webbrowser.open(linkinfo.GetHref(), new=True)
        html = HtmlWin(self, -1, size=(430, -1))
        py_version = sys.version.split()[0]
        bgcolor = misc.color_to_string(self.GetBackgroundColour())
        html.SetPage(self.text % (bgcolor, common.version, py_version,
                                  wx.__version__))
        ir = html.GetInternalRepresentation()
        html.SetSize((ir.GetWidth(), ir.GetHeight()+5))
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(html, 0, wxALIGN_CENTER)
        szr.Add(wxStaticLine(self, -1), 0, wxLEFT|wxRIGHT|wxEXPAND, 20)
        szr2 = wxBoxSizer(wxHORIZONTAL)
        szr2.Add(wxButton(self, wxID_OK, "OK"))
        szr.Add(szr2, 0, wxALL|wxALIGN_RIGHT, 20)
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
    
