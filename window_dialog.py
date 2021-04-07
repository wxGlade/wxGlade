"""Dialog to ask user for base class and class name

@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import wx
import common, compat


def _get_all_class_names(item):
    ret = []
    for c in item.children or []:
        name = getattr(c, "klass", None)
        if name: ret.append(name)
        ret += _get_all_class_names(c)

    return ret


class WindowDialog(wx.Dialog):
    # dialog for builder function
    parent = parent_property = None  # used by StandaloneOrChildDialog
    def __init__(self, klass, base_classes=None, message="Select Class", toplevel=True, options=None, defaults=None,
                 boxes=None):
        pos = wx.GetMousePosition()
        wx.Dialog.__init__(self, common.main, -1, _(message), pos)

        self.standalone = self.base = None

        if base_classes:
            # base class
            self.base  = wx.RadioBox(self, -1, ("base class"), choices=base_classes, style=wx.RA_VERTICAL)
            self.base.SetSelection(0)
            self.Bind(wx.EVT_RADIOBOX, self.on_base_selection)
            self.selected_base = 0

        self.number = 1
        self.class_names = set( _get_all_class_names(common.root) )
        self.toplevel_names = set( c.name for c in common.root.children )
        self.toplevel = toplevel  # if this is True, the name must be unique, as the generated class will have it
        # class
        self._klass = klass
        if common.root.language.lower() != 'xrc':
            klass = self.get_next_class_name(klass)
        self.klass = wx.TextCtrl(self, -1, klass)
        self.klass.Bind(wx.EVT_TEXT, self.on_text)  # for validation

        # for frame or standalone? this is used by the derived class below and just for menu bar and tool bar
        if self.parent_property and self.parent:
            # self.parent must be set by the derived class in this case; here we check whether it is set already
            choices = ["Add to %s '%s'"%(self.parent.WX_CLASS[2:], self.parent.name),
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
        hszr.Add(wx.StaticText(self, -1, _("class"),), 0, wx.EXPAND|wx.ALL, 3)
        hszr.Add(self.klass, 2)
        szr.Add(hszr, 0, wx.EXPAND|wx.ALL, 5)

        if options:
            #line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
            #szr.Add(line, 0, wx.EXPAND|wx.ALL, 5)
            self._create_option_controls(szr, options, defaults, boxes)

        # buttons
        btnbox = wx.StdDialogButtonSizer()
        self.btnOK = btnOK = wx.Button(self, wx.ID_OK)#, _('OK'))
        btnOK.SetDefault()
        btnCANCEL = wx.Button(self, wx.ID_CANCEL)#, _('Cancel'))
        btnbox.AddButton(btnOK)#, 0, wx.ALL, 3)
        btnbox.AddButton(btnCANCEL)#, 0, wx.ALL, 3)
        btnbox.Realize()
        szr.Add(btnbox, 0, wx.ALL|wx.ALIGN_CENTER, 5)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)

    def _create_option_controls(self, szr, options, defaults, boxes):
        self.option_controls = []
        self.option_values = []
        szr_ = szr
        in_box = False
        for o, option in enumerate(options):
            if boxes and option in boxes and boxes[option]:
                # start a static box sizer
                box = wx.StaticBox( self, label=_(boxes[option]) )
                szr_ = wx.StaticBoxSizer(box, wx.VERTICAL)
                in_box = True
            ctrl = wx.CheckBox(self, -1, _(option))
            value = defaults and defaults[o] or False
            ctrl.SetValue(value)
            szr_.Add(ctrl, 0, wx.ALL, 5 if in_box else 10)
            self.option_controls.append(ctrl)
            self.option_values.append(value)
            ctrl.Bind(wx.EVT_CHECKBOX, self.callback)
            if boxes and option in boxes and boxes[option] is None:
                # end static box sizer
                szr.Add(szr_, 0, wx.EXPAND|wx.ALL, 5)
                szr_ = szr
                in_box = False

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        szr.Add(line, 0, wx.EXPAND|wx.TOP|wx.LEFT, 5)

    def on_base_selection(self, event):
        # this can be overridden for widget specific actions
        self.selected_base = self.base.GetStringSelection()
        event.Skip()

    def get_next_class_name(self, name):
        if not name in self.class_names: return name
        while True:
            ret = '%s%d'%(name,self.number)
            if not ret in self.class_names: return ret
            self.number += 1

    def get_next_name(self, name):
        names = [c.name for c in common.root.children]
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
            #if name in [c.widget.klass for c in common.root.children or []]:
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

    def callback(self, event):
        # update values and enable / disable dependent options
        ctrl = event.GetEventObject()
        i = self.option_controls.index(ctrl)
        self.option_values[i] = ctrl.GetValue()

    def get_options(self):
        return self.option_values


class StandaloneOrChildDialog(WindowDialog):
    # for menu and tool bars
    def __init__(self, klass, message, parent, parent_property):
        self.parent = parent
        self.parent_property = parent_property
        WindowDialog.__init__(self, klass, None, message, True)


class DialogOrPanelDialog(WindowDialog):
    # the button types: Affirmative, Negative, Cancel, aPply, Help; on Windows wx will place them in this Order
    all_button_types = ["A", "N", "C", "P", "H"]
    button_names =          ["OK", "CANCEL", "YES", "NO", "SAVE", "APPLY", "CLOSE", "HELP"]
    button_types =          ["A",  "C",      "A",   "N",   "A",   "P",     "C",     "H"]
    default_choices = [True, True, True,     False, False, False, False,   False,   False]
    boxes = {"OK":"Add buttons", "HELP":None}

    def __init__(self, klass):
        base_classes = ['wxDialog', 'wxPanel']
        options = ["Add sizer"] + self.button_names
        WindowDialog.__init__(self, klass, base_classes, 'Select widget type', True, options, self.default_choices,
                              self.boxes)
        self._enable_buttons()

    def get_selected_buttons(self):
        # returns names in the right ordering for Windows: Affirmative, Apply, Negative, Cancel, Help
        if not self.option_values[0]: return [], []
        names = [None]*len(self.all_button_types)
        types = [None]*len(self.all_button_types)
        for o, value in enumerate(self.option_values[1:]):
            if value:
                i = self.all_button_types.index( self.button_types[o] )
                names[i] = self.button_names[o]
                types[i] = self.button_types[o]
        return [n for n in names if n], [t for t in types if t]

    def callback(self, event):
        i = self.option_controls.index( event.GetEventObject() ) - 1
        WindowDialog.callback(self, event)
        self._enable_buttons()

    def _enable_buttons(self):
        names, types = self.get_selected_buttons()
        for b, type_ in enumerate(self.button_types):
            if self.option_values[b+1]:
                enable = True
            else:
                enable = not type_ in types
            if not self.option_values[0] or self.selected_base=="wxPanel": enable = False
            self.option_controls[1+b].Enable(enable)

    def on_base_selection(self, event):
        # enable button actions
        WindowDialog.on_base_selection(self, event)
        self._enable_buttons()
