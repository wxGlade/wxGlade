# widget_properties.py: classes to handle the various properties of the widgets
# (name, size, color, etc.)
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
from wxPython.grid import *
from xml.sax.saxutils import escape

def _mangle(label):
    """\
    returns a mangled version of the str label, suitable for displaying
    the name of a property
    """
    return label.capitalize().replace('_', ' ')

import common
_encode = common._encode_to_xml
## def _encode(label, encoding='latin-1'):
##     """\
##     returns a utf-8 encoded representation of label. This is equivalent to:
##     str(label).decode(encoding).encode('utf-8')
##     """
##     return str(label).decode(encoding).encode('utf-8')


class Property:
    """\
    A class to handle a single property of a widget.
    """
    def __init__(self, owner, name, parent, getter=None, setter=None):
        # owner: the widget this property belongs to
        # parent: the widget inside which the property is displayed
        """\
        Access to the property is made through the getter and setter functions,
        which are invoked also in the default event handler. If they are None,
        they default to owner[name][0] and owner[name][1]
        """
        self.val = None 
        self.parent = parent
        self.owner = owner
        self.getter = getter
        self.setter = setter
        self.name = name

    def on_change_val(self, event, first=[True]):
        """\
        Event handler called to notify owner that the value of the Property
        has changed
        """
        val = self.get_value()
        if self.val != val:
            import common
            common.app_tree.app.saved = False # update the status of the app
            if self.setter: self.setter(val)
            else:
                self.owner[self.name][1](val)
            self.val = val
        first[0] = False
        event.Skip()
        
    def write(self, outfile=None, tabs=0):
        """\
        Writes the xml code for this property onto the given file.
        """
        if self.getter: value = self.getter()
        else: value = self.owner[self.name][0]() 
        if value != '':
            fwrite = outfile.write
            fwrite('    ' * tabs + '<%s>' % self.name)
            fwrite(escape(_encode(value)))
            fwrite('</%s>\n' % self.name)

    def bind_event(self, function):
        """\
        sets the default event handler for this property.
        """
        raise NotImplementedError

    def get_value(self):
        raise NotImplementedError

    def set_value(self, value):
        raise NotImplementedError

# end of class Property


class HiddenProperty(Property):
    """\
    Properties not associated to any control, i.e. not editable by the user.
    """
    def __init__(self, owner, name, value=None):
        try: getter, setter = owner[name]
        except KeyError:
            import traceback; traceback.print_exc()
            if callable(value): getter = value
            else:
                def getter(): return value
            def setter(v): pass
            
        self.panel = None # this is needed to provide an uniform treatment,
                          # but is always None
        Property.__init__(self, owner, name, None, getter, setter)
        if value is not None:
            if callable(value): self.value = value()
            else: self.value = value

    def bind_event(self, function):
        pass

    def get_value(self):
        return self.value

    def set_value(self, val):
        self.value = val

# end of class HiddenProperty


class _activator:
    """\
    a utility class which provides a method, toggle_active, to activate or
    deactivate a Property of a widget
    """
    def __init__(self, target=None, enabler=None):
        # target: name of the object to Enable/Disable
        # enabler: check box which provides the ability to Enable/Disable the
        #          Property
        self._target = target
        self._enabler = enabler
        if self._target: self._active = self._target.IsEnabled()
        else: self._active = True

    def toggle_active(self, active):
        self._active = active
        if not self._target: return
        self._target.Enable(active)
        import common; common.app_tree.app.saved = False
        try: self._enabler.SetValue(active)
        except AttributeError: pass

    def is_active(self):
        if self._target: return self._target.IsEnabled()
        return self._active

# end of class _activator


class TextProperty(Property, _activator):
    """\
    Properties associated to a text control.
    """
    def __init__(self, owner, name, parent=None, can_disable=False,
                 enabled=False, readonly=False, multiline=False):
        Property.__init__(self, owner, name, parent)
        self.val = str(owner[name][0]())
        self.can_disable = can_disable
        self.readonly = readonly
        self.multiline = multiline
        _activator.__init__(self)
        if can_disable: self.toggle_active(enabled)
        self.panel = None
        if parent is not None: self.display(parent)

    def display(self, parent):
        """\
        Actually builds the text control to set the value of the property
        interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)
        if self.readonly: style = wxTE_READONLY
        else: style = 0
        if self.multiline: style |= wxTE_MULTILINE
        val = self.get_value()
        if self.multiline: val = val.replace('\\n', '\n')
        self.text = wxTextCtrl(self.panel, self.id, val, style=style)
        label = wxStaticText(self.panel, -1, _mangle(self.name))
        if self.can_disable:
            self._enabler = wxCheckBox(self.panel, self.id+1, '')
            EVT_CHECKBOX(self.panel, self.id+1,
                         lambda event: self.toggle_active(event.IsChecked()))
            self.text.Enable(self.is_active())
            self._enabler.SetValue(self.is_active())
            self._target = self.text
        sizer = wxBoxSizer(wxHORIZONTAL)
        sizer.Add(label, 2, wxALL|wxALIGN_CENTER, 3)
        sizer.SetItemMinSize(label, *label.GetBestSize())
        try:
            sizer.Add(self._enabler, 1, wxALL|wxALIGN_CENTER, 3)
            option = 4
        except AttributeError:
            option = 5
        sizer.Add(self.text, option, wxALL|wxALIGN_CENTER, 3)
        if self.multiline:
            h = self.text.GetCharHeight()
            sizer.SetItemMinSize(self.text, -1, h*3)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())
        self.bind_event(self.on_change_val)


    def bind_event(self, function):
        def func_2(event):
            if self.text.IsEnabled():
                function(event)
        EVT_KILL_FOCUS(self.text, func_2)

    def get_value(self):
        try:
            val = self.text.GetValue()
            if self.multiline: return val.replace('\n', '\\n')
            return val
        except AttributeError:
            return self.val

    def set_value(self, value):
        value = str(value)
        if self.multiline:
            self.val = value.replace('\n', '\\n')
            value = value.replace('\\n', '\n')
        else: self.val = value
        try: self.text.SetValue(value)
        except AttributeError: pass

    def write(self, outfile, tabs):
        if self.is_active():
            Property.write(self, outfile, tabs)

# end of class TextProperty


class CheckBoxProperty(Property):
    """\
    Properties whose values can be changed by one checkbox.
    """
    def __init__(self, owner, name, parent=None, label=None):
        Property.__init__(self, owner, name, parent)
        self.val = int(owner[name][0]())
        if label is None: label = _mangle(name)
        self.label = label
        self.panel = None
        if parent is not None: self.display(parent)

    def display(self, parent):
        """\
        Actually builds the check box to set the value of the property
        interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)
        self.cb = wxCheckBox(self.panel, self.id, '')
        self.cb.SetValue(self.val)
        label = wxStaticText(self.panel, -1, self.label)
        sizer = wxBoxSizer(wxHORIZONTAL)
        sizer.Add(label, 5, wxALIGN_CENTER|wxALL, 3)
        sizer.SetItemMinSize(label, *label.GetBestSize())        
        sizer.Add(self.cb, 2, wxALIGN_CENTER|wxALL, 3)
        self.panel.SetAutoLayout(True)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())
        self.bind_event(self.on_change_val)
        
    def bind_event(self, function):
        EVT_CHECKBOX(self.panel, self.id, function)

    def get_value(self):
        try: return self.cb.GetValue()
        except AttributeError: return self.val

    def set_value(self, val):
        self.val = int(val)
        try: self.cb.SetValue(self.val)
        except AttributeError: pass

    def write(self, outfile, tabs):
        if self.get_value(): Property.write(self, outfile, tabs)

# end of class CheckBoxProperty


class CheckListProperty(Property):
    """\
    Properties whose values can be changed by a list of checkboxes.
    """
    def __init__(self, owner, name, parent=None, labels=None, writer=None):
        # labels: list of names of the labels of the checkboxes; a
        # label that begins with the string "#section#" is used as the
        # title of a static box that encloses the checkboxes that
        # follow
        Property.__init__(self, owner, name, parent)
        self.values = owner[name][0]()
        self.labels = labels
        # the writer param is a function to customize the generation of the xml
        # for this property
        self.writer = writer
        self.panel = None
        if parent is not None: self.display(parent)

    def display(self, parent):
        """\
        Actually builds the list of checkboxes to set the value of the property
        interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)

        self.choices = []
        tmp_sizer = wxBoxSizer(wxVERTICAL)
        self.panel.SetAutoLayout(True)
        i = j = 0
        tmp = tmp_sizer
        while 1:
            if i >= len(self.labels): break
            if self.labels[i].startswith('#section#'):
                if tmp != tmp_sizer:
                    tmp_sizer.Add(tmp, 1, wxALL|wxEXPAND, 5)
                lbl = _mangle(self.labels[i].replace('#section#', ''))
                tmp = wxStaticBoxSizer(wxStaticBox(self.panel, -1, lbl),
                                       wxVERTICAL)
            else:
                c = wxCheckBox(self.panel, self.id+j, self.labels[i])
                self.choices.append(c)
                tmp.Add(c)
                j += 1
            i += 1
        if tmp != tmp_sizer:
            tmp_sizer.Add(tmp, 1, wxALL|wxEXPAND, 5)

        for i in range(len(self.values)):
            self.choices[i].SetValue(self.values[i])
                  
        self.panel.SetSizer(tmp_sizer)
        self.panel.SetSize(tmp_sizer.GetMinSize())
        self.bind_event(self.on_change_val)
        
    def bind_event(self, function):
        for i in range(len(self.choices)):
            EVT_CHECKBOX(self.panel, self.id+i, function)

    def get_value(self):
        try: return [c.GetValue() for c in self.choices]
        except AttributeError: return self.values

    def set_value(self, value):
        self.values = self.prepare_value(value)
        try:
            for i in range(len(self.values)):
                self.choices[i].SetValue(self.values[i])
        except AttributeError: pass

    def write(self, outfile, tabs):
        if self.writer is not None:
            return self.writer(outfile, tabs)
        val = self.owner[self.name][0]() #self.getter()
        def filter_func(label): return not label.startswith('#section#')
        labels = filter(filter_func, self.labels)
        tmp = '|'.join([ labels[c] for c in range(len(labels)) if val[c] ])
        if tmp: 
            fwrite = outfile.write
            fwrite('    ' * tabs + '<%s>' % self.name)
            fwrite(escape(_encode(tmp)))
            fwrite('</%s>\n' % self.name)

    def prepare_value(self, old_val):
        """\
        turns a string of tokens separated by '|' into a list of
        boolean values
        """
        try: old_val = old_val.split("|")
        except AttributeError: return list(old_val)
        ret = []
        for l in self.labels:
            if l in old_val: ret.append(1)
            elif not l.startswith('#section#'): ret.append(0)
        return ret

# end of class CheckListProperty


class SpinProperty(Property, _activator):
    """\
    Properties associated to a spin control.
    """
    def __init__(self, owner, name, parent=None, can_disable=False,
                 r=None, enabled=False):
        # r = range of the spin (min, max)
        Property.__init__(self, owner, name, parent)
        self.can_disable = can_disable
        _activator.__init__(self)
        if can_disable: self.toggle_active(enabled)
        self.val_range = r
        self.panel = None
        if parent is not None: self.display(parent)
        self.val = owner[name][0]()

    def display(self, parent):
        """\
        Actually builds the spin control to set the value of the property
        interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)
        if self.val_range is None: self.val_range = (0, 1000)
        self.spin = wxSpinCtrl(self.panel, self.id, min=self.val_range[0],
                               max=self.val_range[1])
        val = int(self.owner[self.name][0]())
        if not val:
            self.spin.SetValue(1) # needed for GTK to display a '0'
        self.spin.SetValue(val) #int(self.owner[self.name][0]()))
        label = wxStaticText(self.panel, -1, _mangle(self.name))
        if self.can_disable:
            self._enabler = wxCheckBox(self.panel, self.id+1, '')
            EVT_CHECKBOX(self.panel, self.id+1,
                         lambda event: self.toggle_active(event.IsChecked()))
            self.spin.Enable(self.is_active())
            self._enabler.SetValue(self.is_active())
            self._target = self.spin
        sizer = wxBoxSizer(wxHORIZONTAL)
        sizer.Add(label, 2, wxALL|wxALIGN_CENTER, 3)
        sizer.SetItemMinSize(label, *label.GetBestSize())        
        try:
            sizer.Add(self._enabler, 1, wxALL|wxALIGN_CENTER, 3)
            option = 4
        except AttributeError:
            option = 5
        sizer.Add(self.spin, option, wxALL|wxALIGN_CENTER, 3)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        def func_2(event, function=function, self=self):
            if self.is_active():
                function(event)
        EVT_KILL_FOCUS(self.spin, func_2)

    def get_value(self):
        try: return self.spin.GetValue()
        except AttributeError: return self.val

    def set_value(self, value):
        self.val = int(value)
        try: self.spin.SetValue(int(value))
        except AttributeError: pass

    def set_range(self, min_v, max_v):
        self.val_range = (min_v, max_v)
        try: self.spin.SetRange(min_v, max_v)
        except AttributeError: pass

    def write(self, outfile, tabs):
        if self.is_active(): 
            Property.write(self, outfile, tabs)

# end of class SpinProperty


class DialogProperty(Property, _activator):
    """\
    Property which selection is made through a dialog, which must provide a
    get_value method.
    """
    def __init__(self, owner, name, parent, dialog, can_disable=False,
                 enabled=False):
        Property.__init__(self, owner, name, parent)
        self.dialog = dialog
        self.panel = None
        self.can_disable = can_disable
        _activator.__init__(self)
        if can_disable: self.toggle_active(enabled)
        if parent is not None: self.display(parent)
        self.val = str(owner[name][0]())

    def display(self, parent):
        """\
        Actually builds the panel (with the text ctrl and the button to display
        the dialog) to set the value of the property interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)
        val = str(self.owner[self.name][0]())
        self.text = wxTextCtrl(self.panel, self.id, val)
        self.btn = wxButton(self.panel, self.id+1, "...")
        if self.can_disable:
            self._enabler = wxCheckBox(self.panel, self.id+1, '')
            EVT_CHECKBOX(self.panel, self.id+1,
                         lambda event: self.toggle_active(event.IsChecked()))
            self.text.Enable(self.is_active())
            self.btn.Enable(self.is_active())
            self._enabler.SetValue(self.is_active())
            self._target = self.text
        label = wxStaticText(self.panel, -1, _mangle(self.name))
        EVT_BUTTON(self.panel, self.id+1, self.display_dialog)
        sizer = wxBoxSizer(wxHORIZONTAL)
        sizer.Add(label, 2, wxALL|wxALIGN_CENTER, 3)
        sizer.SetItemMinSize(label, *label.GetBestSize())        
        try:
            sizer.Add(self._enabler, 1, wxALL|wxALIGN_CENTER, 3)
            option = 3
        except AttributeError:
            option = 4
        sizer.Add(self.text, option, wxALL|wxALIGN_CENTER, 3)
        sizer.Add(self.btn, 1, wxALL|wxALIGN_CENTER, 3)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())
        
        self.bind_event(self.on_change_val)

    def display_dialog(self, event):
        if self.dialog.ShowModal() == wxID_OK:
            self.text.SetValue(self.dialog.get_value())
        self.text.ProcessEvent(wxFocusEvent(wxEVT_KILL_FOCUS, self.id))

    def bind_event(self, function):
        EVT_KILL_FOCUS(self.text, function)

    def get_value(self):
        try: return self.text.GetValue()
        except AttributeError: return self.val

    def set_value(self, value):
        self.val = str(value)
        try: self.text.SetValue(self.val)
        except AttributeError: pass

    def write(self, dest_file=None, tabs=0):
        if self.is_active():
            Property.write(self, dest_file, tabs)

    def toggle_active(self, active):
        _activator.toggle_active(self, active)
        try: self.btn.Enable(active)
        except AttributeError: pass

# end of class DialogProperty


class FileDialogProperty(DialogProperty):
    dialog = [None]
    def __init__(self, owner, name, parent=None, wildcard="All Files|*",
                 message="Choose a file", can_disable=True, style=0):
        if not self.dialog[0]:
            self.dialog[0] = wxFileDialog(parent, message,
                                          wildcard=wildcard, style=style)
            self.dialog[0].get_value = self.dialog[0].GetPath
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable)

# end of class FileDialogProperty


class ColorDialogProperty(DialogProperty):
    dialog = [None]
    def __init__(self, owner, name, parent=None, can_disable=True):
        if not self.dialog[0]:
            self.dialog[0] = wxColourDialog(parent)
            def get_value():
                return '#' + reduce(lambda a, b: a+b,
                                map(lambda s: '%02x' % s,
                                    self.dialog.GetColourData().
                                    GetColour().Get()))
            self.dialog[0].get_value = get_value
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable)

# end of class ColorDialogProperty


def _reverse_dict(src):
    ret = {}
    for key, val in src.iteritems():
        ret[val] = key
    return ret

class FontDialogProperty(DialogProperty):
    font_families_to = { 'default': wxDEFAULT, 'decorative': wxDECORATIVE,
                         'roman': wxROMAN, 'swiss': wxSWISS,
                         'script':wxSCRIPT, 'modern': wxMODERN }
    font_families_from = _reverse_dict(font_families_to)
    font_styles_to = { 'normal': wxNORMAL, 'slant': wxSLANT,
                       'italic': wxITALIC }
    font_styles_from = _reverse_dict(font_styles_to)
    font_weights_to = { 'normal': wxNORMAL, 'light': wxLIGHT, 'bold': wxBOLD }
    font_weights_from = _reverse_dict(font_weights_to)
    
    dialog = [None]

    def __init__(self, owner, name, parent=None, can_disable=True):
        if not self.dialog[0]:
            # check wxPython >= 2.3.3
##             v = wx.__version__.split('.', 2)[-1]
##             if v and int(v[0]) > 2:
            import misc
            if misc.check_wx_version(2, 3, 3):
                FontDialogProperty.font_families_to['teletype'] = wxTELETYPE 
                FontDialogProperty.font_families_from[wxTELETYPE] = 'teletype' 

            data = wxFontData()
            self.dialog[0] = wxFontDialog(parent, data)
            def get_value():
                font = self.dialog.GetFontData().GetChosenFont()
                family = font.GetFamily()
                # check wxPython >= 2.3.3
##                 v = wx.__version__.split('.', 2)[-1]
##                 if v and int(v[0]) > 2:
##                     print 'Version: %s' % v
                if misc.check_wx_version(2, 3, 3):
                    for f in (wxVARIABLE, wxFIXED):
                        if family & f: family = family ^ f
                return "['%s', '%s', '%s', '%s', '%s', '%s']" % \
                       (font.GetPointSize(),
                        self.font_families_from[family],
                        self.font_styles_from[font.GetStyle()],
                        self.font_weights_from[font.GetWeight()],
                        font.GetUnderlined(), font.GetFaceName())
            self.dialog[0].get_value = get_value
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable)

    def write(self, outfile=None, tabs=0):
        if self.is_active():
            try: props = [ _encode(s) for s in eval(self.get_value().strip()) ]
            except:
                import traceback
                traceback.print_exc()
                return
            if len(props) < 6:
                print 'error in the value of the property "%s"' % self.name
                return
            fwrite = outfile.write
            fwrite('    ' * tabs + '<%s>\n' % self.name)
            tstr = '    ' * (tabs+1)
            fwrite('%s<size>%s</size>\n' % (tstr, escape(props[0])))
            fwrite('%s<family>%s</family>\n' % (tstr, escape(props[1])))
            fwrite('%s<style>%s</style>\n' % (tstr, escape(props[2])))
            fwrite('%s<weight>%s</weight>\n' % (tstr, escape(props[3])))
            fwrite('%s<underlined>%s</underlined>\n' % (tstr,
                                                        escape(props[4])))
            fwrite('%s<face>%s</face>\n' % (tstr, escape(props[5])))
            fwrite('    ' * tabs + '</%s>\n' % self.name)

# end of class FontDialogProperty


class RadioProperty(Property, _activator):
    """\
    properties controlled by a series of radio buttons.
    """
    def __init__(self, owner, name, parent, choices, can_disable=False,
                 enabled=False, columns=1):
        Property.__init__(self, owner, name, parent)
        self.can_disable = can_disable
        _activator.__init__(self)
        if can_disable: self.toggle_active(enabled)
        self.choices = choices
        self.columns = columns
        self.panel = None
        if parent is not None: self.display(parent)
        self.val = owner[name][0]()

    def display(self, parent):
        """\
        Actually builds the radio box to set the value of the property
        interactively
        """
        self.id = wxNewId()
        self.panel = wxPanel(parent, -1)
        self.panel.SetAutoLayout(True)
        style = wxRA_SPECIFY_COLS|wxNO_BORDER
        if not self.can_disable: 
            szr = wxBoxSizer(wxHORIZONTAL)
            style=wxRA_SPECIFY_COLS
        else: 
            szr = wxStaticBoxSizer(wxStaticBox(self.panel, -1,
                                               _mangle(self.name)),
                                   wxHORIZONTAL)
        self.options = wxRadioBox(self.panel, self.id, _mangle(self.name),
                                  choices=self.choices,
                                  majorDimension=self.columns,
                                  style=style)
        if self.can_disable:
            self._enabler = wxCheckBox(self.panel, self.id+1, "")
            szr.Add(self._enabler)
            EVT_CHECKBOX(self.panel, self.id+1,
                         lambda e: self.toggle_active(e.IsChecked()))
            self.options.Enable(self.is_active())
            self.options.SetLabel("")
            self._enabler.SetValue(self.is_active())
        szr.Add(self.options, 1, wxEXPAND)
        self.panel.SetSizer(szr)
        self.panel.SetSize(szr.GetMinSize())
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        def func_2(event, function=function, self=self):
            if self.options.IsEnabled():
                function(event)
        EVT_RADIOBOX(self.options, self.id, func_2)

    def get_value(self):
        try: return self.options.GetSelection()
        except AttributeError: return self.val

    def get_str_value(self):
        try: return self.options.GetStringSelection()
        except AttributeError:
            if 0 <= self.val < len(self.choices):
                return self.choices[self.val]
            else: return ''

    def set_value(self, value):
        try: self.val = int(value)
        except ValueError: self.val = self.choices.index(value)
        try: self.options.SetSelection(self.val)
        except AttributeError: pass 

    def set_str_value(self, value):
        try:
            self.val = self.choices.index(value)
            self.options.SetSelection(self.val)
        except (AttributeError, ValueError): pass

    def write(self, outfile, tabs):
        if self.is_active():
            outfile.write('    ' * tabs + '<%s>%s</%s>\n' %
                          (self.name, escape(_encode(self.get_str_value())),
                           self.name))

# end of class RadioProperty


class GridProperty(wxPanel, Property):
    """\
    Property whose values are modified through a wxGrid table.
    """
    STRING, INT, FLOAT = 0, 1, 2
    col_format = [lambda g, c: None,
                  lambda g, c: g.SetColFormatNumber(c),
                  lambda g, c: g.SetColFormatFloat(c)]
    def __init__(self, owner, name, parent, cols, rows=1, can_add=True,
                 can_remove=True, can_insert=True):
        # cols: list of 2-tuples with these fields:
        #     - label for the column
        #     - type: GridProperty.STRING, GridProperty.INT, GridProperty.FLOAT
        # rows: number of rows
        Property.__init__(self, owner, name, parent)
        self.val = owner[name][0]()
        self.set_value(self.val)
        self.rows, self.cols = rows, cols
        self.can_add = can_add
        self.can_remove = can_remove
        self.can_insert = can_insert
        self.panel = None
        self.cur_row = 0
        if parent is not None: self.display(parent)

    def display(self, parent):
        """\
        Actually builds the grid to set the value of the property
        interactively
        """
        self.panel = wxPanel(parent, -1)
        self.btn_id = wxNewId()
        self.btn = wxButton(self.panel, self.btn_id, "Update")
        if self.can_add:
            self.add_btn = wxButton(self.panel, self.btn_id+1, "Add")
        if self.can_remove:
            self.remove_btn = wxButton(self.panel, self.btn_id+2, "Remove")
        if self.can_insert:
            self.insert_btn = wxButton(self.panel, self.btn_id+3, "Insert")
        self.grid = wxGrid(self.panel, -1)
        self.grid.CreateGrid(self.rows, len(self.cols))
        self.grid.SetMargins(0, 0)

        for i in range(len(self.cols)):
            self.grid.SetColLabelValue(i, self.cols[i][0])
            GridProperty.col_format[self.cols[i][1]](self.grid, i)
        sizer = wxStaticBoxSizer(wxStaticBox(self.panel, -1,
                                             _mangle(self.name)),
                                 wxVERTICAL)
        self.cols = len(self.cols)
        self.grid.SetRowLabelSize(0)
        self.grid.SetColLabelSize(20)

        self.btn_sizer = wxBoxSizer(wxHORIZONTAL)
        self.btn.SetSize((self.btn.GetCharWidth()*8, -1))
        self.btn_sizer.Add(self.btn)
        if self.can_add:
            self.add_btn.SetSize((self.add_btn.GetCharWidth()*5, -1))
            self.btn_sizer.Add(self.add_btn, 0, wxLEFT|wxRIGHT, 4)
            EVT_BUTTON(self.add_btn, self.btn_id+1, self.add_row)
        if self.can_insert: 
            self.insert_btn.SetSize((self.insert_btn.GetCharWidth()*8, -1))
            self.btn_sizer.Add(self.insert_btn, 0, wxLEFT|wxRIGHT, 4)
            EVT_BUTTON(self.insert_btn, self.btn_id+3, self.insert_row)
        if self.can_remove:
            self.remove_btn.SetSize((self.remove_btn.GetCharWidth()*8, -1))
            self.btn_sizer.Add(self.remove_btn)
            EVT_BUTTON(self.remove_btn, self.btn_id+2, self.remove_row)
        sizer.Add(self.btn_sizer, 0, wxBOTTOM|wxEXPAND, 2)
        sizer.Add(self.grid, 1, wxEXPAND)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())

        EVT_GRID_SELECT_CELL(self.panel, self.on_select_cell)
        self.bind_event(self.on_change_val)
        self.set_value(self.val)

    def on_select_cell(self, event):
        self.cur_row = event.GetRow()
        event.Skip()

    def bind_event(self, function):
        EVT_BUTTON(self.btn, self.btn_id, function)

    def get_value(self):
        if not hasattr(self, 'grid'): return self.val
        l = []
        for i in range(self.rows):
            l2 = []
            for j in range(self.cols):
                l2.append(self.grid.GetCellValue(i, j))
            l.append(l2)
        return l

    def set_value(self, values):
        self.val = values
        if not hasattr(self, 'grid'): return
        # values is a list of lists with the values of the cells
        size = len(values)
        if self.rows < size:
            self.grid.AppendRows(size-self.rows)
            self.rows = size
        elif self.rows != size: self.grid.DeleteRows(size, self.rows-size)
        for i in range(len(values)):
            for j in range(len(values[i])):
                self.grid.SetCellValue(i, j, values[i][j])
            
    def add_row(self, event):
        self.grid.AppendRows()
        self.rows += 1

    def remove_row(self, event):
        if self.rows > 1:
            self.grid.DeleteRows(self.cur_row)
            self.rows -= 1

    def insert_row(self, event):
        self.grid.InsertRows(self.cur_row)
        self.rows += 1

    def set_col_sizes(self, sizes):
        """\
        sets the width of the columns.
        sizes is a list of integers with the size of each column: a value of 0
          stands for a default size, while -1 means to expand the column to fit
          the available space (at most one column can have size -1)
        """
        col_to_expand = -1
        total_w = 0
        for i in range(self.grid.GetNumberCols()):
            try: w = sizes[i]
            except IndexError: return
            if not w:
                self.grid.AutoSizeColumn(i)
                total_w += self.grid.GetColSize(i)
            elif w < 0: col_to_expand = i
            else:
                self.grid.SetColSize(i, w)
                total_w += w
        if col_to_expand >= 0:
            w = self.grid.GetSize()[0] - total_w
            if w >= 0: self.grid.SetColSize(col_to_expand, w)
            else: self.grid.AutoSizeColumn(col_to_expand)

# end of class GridProperty

