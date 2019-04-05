"""Dialog to ask user for base class and class name

@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import wx
import common, compat


class WindowDialog(wx.Dialog):
    # dialog for builder function
    parent = parent_property = None  # used by StandaloneOrChildDialog
    def __init__(self, klass, base_classes=None, message="Select Class", toplevel=True):
        pos = wx.GetMousePosition()
        wx.Dialog.__init__(self, common.main, -1, _(message), pos)

        self.standalone = self.base = None

        if base_classes:
            # base class
            self.base  = wx.RadioBox(self, -1, ("base class"), choices=base_classes, style=wx.RA_VERTICAL)
            self.base.SetSelection(0)

        self.number = 1
        self.class_names = set( common.app_tree.get_all_class_names() )
        self.toplevel_names = set( common.app_tree.get_toplevel_class_names() )
        self.toplevel = toplevel  # if this is True, the name must be unique, as the generated class will have it
        # class
        self._klass = klass
        if common.app_tree.app.language.lower() != 'xrc':
            klass = self.get_next_class_name(klass)
        self.klass = wx.TextCtrl(self, -1, klass)
        self.klass.Bind(wx.EVT_TEXT, self.on_text)  # for validation

        # for frame or standalone? this is used by the derived class below and just for menu bar and tool bar
        if self.parent_property and self.parent:
            # self.parent must be set by the derived class in this case; here we check whether it is set already
            choices = ["Add to %s '%s'"%(self.parent.base[2:], self.parent.name),
                       "Standalone"]
            self.standalone  = wx.RadioBox(self, -1, ("Type"), choices=choices, style=wx.RA_VERTICAL)
            self.standalone.Bind(wx.EVT_RADIOBOX, self.on_standalone)
            if self.parent.properties[self.parent_property].value:
                self.standalone.SetSelection(1)
                self.standalone.Disable()
            else:
                self.standalone.SetSelection(0)
                self.klass.Disable()

        # layout
        szr = wx.BoxSizer(wx.VERTICAL)
        if self.standalone: szr.Add(self.standalone, 0, wx.ALL|wx.EXPAND, 5)
        if self.base:       szr.Add(self.base,       0, wx.ALL|wx.EXPAND, 5)
        hszr = wx.BoxSizer(wx.HORIZONTAL)
        hszr.Add(wx.StaticText(self, -1, _("class"),), 0, wx.ALIGN_CENTRE_VERTICAL|wx.ALIGN_RIGHT|wx.EXPAND|wx.ALL, 3)
        hszr.Add(self.klass, 2)
        szr.Add(hszr, 0, wx.EXPAND)
        # buttons
        btnbox = wx.BoxSizer(wx.HORIZONTAL)
        self.btnOK = btnOK = wx.Button(self, wx.ID_OK, _('OK'))
        btnCANCEL = wx.Button(self, wx.ID_CANCEL, _('Cancel'))
        btnbox.Add(btnOK, 0, wx.ALL, 3)
        btnbox.Add(btnCANCEL, 0, wx.ALL, 3)
        btnOK.SetFocus()
        szr.Add(btnbox, 0, wx.ALL|wx.ALIGN_CENTER, 3)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)

    def get_next_class_name(self, name):
        #names = [c.widget.klass for c in common.app_tree.root.children or []]
        
        if not name in self.class_names: return name
        while True:
            ret = '%s%d'%(name,self.number)
            if not ret in self.class_names: return ret
            self.number += 1

    def get_next_name(self, name):
        names = common.app_tree.get_all_names()
        names = [n for n in names if n.startswith(name)]
        if not name in names: return name
        while True:
            ret = '%s_%d'%(name,self.number)
            if not ret in names: return ret
            self.number += 1

    def on_text(self, event):
        import re
        validation_re = re.compile(r'^[a-zA-Z_]+[\w:.0-9-]*$')
        name = event.GetString()
        OK = bool( validation_re.match( name ) )
        if not OK:
            self.klass.SetBackgroundColour(wx.RED)
            compat.SetToolTip(self.klass, "Class name not valid")
        else:
            #if name in [c.widget.klass for c in common.app_tree.root.children or []]:
            if self.toplevel and name in self.toplevel_names:
                self.klass.SetBackgroundColour( wx.RED )
                compat.SetToolTip(self.klass, "Class name already in use for toplevel window")
                OK = False
            elif name in self.class_names:
                # if the class name is in use already, indicate in yellow
                self.klass.SetBackgroundColour( wx.Colour(255, 255, 0, 255) )
                compat.SetToolTip(self.klass, "Class name not unique")
                if self.toplevel and name in self.toplevel_names: OK = False
            else:
                self.klass.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
                compat.SetToolTip(self.klass, "")
        self.klass.Refresh()
        self.btnOK.Enable(OK)
        event.Skip()

    def on_standalone(self, event):
        self.klass.Enable(event.Selection==1)
        event.Skip()

    def show(self):
        "returns values if user has hit OK, None otherwise"
        res  = self.ShowModal()
        if res != wx.ID_OK: return None

        if self.standalone and self.standalone.GetSelection()==0:
            return True
        klass  = self.klass.GetValue()
        if not self.base:
            return klass
        base = self.base.GetStringSelection()
        return klass, base


class StandaloneOrChildDialog(WindowDialog):
    # for menu and tool bars
    def __init__(self, klass, message, parent, parent_property):
        self.parent = parent
        self.parent_property = parent_property
        WindowDialog.__init__(self, klass, None, message, True)
