"""
Classes to handle the various properties of the widgets (name, size, colour,
etc.)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import textwrap

import wx
import wx.lib.stattext
import wx.grid
import wx.lib.agw.supertooltip as STT

import common
import compat
import config
import misc
from misc import _reverse_dict
from ordereddict import OrderedDict


__all__ = ['CheckBoxProperty', 'CheckListProperty', 'ColorDialogProperty',
           'ComboBoxProperty', 'DialogProperty', 'FileDialogProperty',
           'FontDialogProperty', 'GridProperty', 'HiddenProperty',
           'Property', 'RadioProperty', 'SpinProperty', 'TextProperty', ]


class Property(object):
    """\
    A class to handle a single property of a widget.

    @ivar name:    Property name
    @type name:    str
    @ivar owner:   The widget this property belongs to
    @ivar parent:  The widget inside which the property is displayed
    @ivar tooltip: Tooltip text
    @type tooltip: str
    @ivar _logger: Instance specific logger
    @ivar _tooltip_widgets: All widgets to set tooltips for
    @type _tooltip_widgets: list
    """

    def __init__(self, owner, name, parent, getter=None, setter=None,
                 label=None):
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
        self.tooltip = None
        self._tooltip_widgets = []
        self.name = name
        if label:
            self.dispName = label
        else:
            self.dispName = name
        self._logger = logging.getLogger(self.__class__.__name__)

    def on_change_val(self, event):
        """\
        Event handler called to notify owner that the value of the Property
        has changed
        """
        val = self.get_value()
        if not misc.streq(self.val, val):
            common.app_tree.app.saved = False  # update the status of the app
            if self.setter:
                self.setter(val)
            else:
                self.owner[self.name][1](val)
            self.val = self.get_value()
        event.Skip()

    def write(self, outfile, tabs=0):
        """\
        Writes the xml code for this property onto the given file.

        @param outfile: A file or a file-like object
        @type outfile:  str
        @param tabs: Indention level, Each level are four spaces
        @type tabs:  int
        """
        if self.getter:
            value = self.getter()
        else:
            value = self.owner[self.name][0]()
        if not misc.streq(value, ''):
            stmt = common.format_xml_tag(self.name, value, tabs)
            outfile.write(stmt)

    def bind_event(self, function):
        """\
        Sets the default event handler for this property.
        """
        raise NotImplementedError

    def get_value(self):
        """\
        Return the content of this property.
        """
        raise NotImplementedError

    def set_value(self, value):
        """\
        Set the content of this property to the given value.
        """
        raise NotImplementedError

    def _mangle(self, label):
        """\
        Returns a mangled version of label, suitable for displaying
        the name of a property.

        @type label: str
        @param label: Text to mangle
        """
        return misc.wxstr(misc.capitalize(label).replace('_', ' '))

    def set_tooltip(self, text=None):
        """\
        Set same tooltip text to all widgets in L{self._tooltip_widgets}.
        
        The text is taken from the C{text} parameter or from L{self.tooltip}.

        C{text} will be stored in L{self.tooltip} if it is given.

        @param text: Tooltip text
        @type text: str
        """
        # check parameter first
        if text:
            self.tooltip = text

        # check for attributes
        if not self.tooltip or not self._tooltip_widgets:
            return

        # set tooltip
        for widget in self._tooltip_widgets:
            if widget:
                widget.SetToolTip(wx.ToolTip(self.tooltip))

    def _escape(self, val):
        """\
        Convert newline and tab characters to a character sequences.

        The direction is FROM input widget TO property.

        @param val: String to convert
        @type val: str | Unicode
        @rtype: str | Unicode
        """
        val = val.replace('\n', '\\n')
        val = val.replace('\t', '\\t')
        return val

    def _unescape(self, val):
        """\
        Convert character sequences to a single newline or tab character.

        The direction is FROM property TO input widget.

        @param val: String to convert
        @type val: str | Unicode
        @rtype: str | Unicode
        """
        val = val.replace('\\n', '\n')
        val = val.replace('\\t', '\t')
        return val

# end of class Property


class HiddenProperty(Property):
    """\
    Properties not associated to any control, i.e. not editable by the user.
    """

    def __init__(self, owner, name, value=None, label=None):
        try:
            getter, setter = owner[name]
        except KeyError:
            self._logger.exception(_('Internal Error'))
            if callable(value):
                getter = value
            else:

                def getter():
                    return value

            def setter(v):
                pass

        self.panel = None  # this is needed to provide an uniform treatment,
                           # but is always None
        Property.__init__(self, owner, name, None, getter, setter, label=label)
        if value is not None:
            if callable(value):
                self.value = value()
            else:
                self.value = value

    def bind_event(self, function):
        pass

    def get_value(self):
        return self.value

    def set_value(self, val):
        self.value = val

# end of class HiddenProperty


class _activator:
    """\
    A utility class which provides:
    - a method L{toggle_active()} to (de)activate a Property of a widget
    - a method L{toggle_blocked()} to (un)block enabler of a Property of a widget
    - a method L{prepare_activator()} to assign enabler and target with settings
    
    @ivar _enabler: CheckBox which provides the ability to Enable/Disable
                    the Property
    @type _enabler: CheckBox
    @ivar _target:  Object to enable/disable
    @type _target:  A single widget or a list of widgets
    """

    def __init__(self, target=None, enabler=None, omitter=None):
        """\
        @param target:  Object to Enable/Disable
        @type target:   A single widget or a list of widgets
        @param enabler: CheckBox which provides the ability to Enable/Disable
                        the Property
        @type enabler:  CheckBox
        """
        self._target = target
        self._enabler = enabler
        if self._enabler:
            self._blocked = self.is_blocked()
        else:
            self._blocked = False
        if self.is_blocked():
            self.toggle_active(False, False)
        elif self._target:
            self._active = self.is_active()
        else:
            self._active = True
        self.set_omitter(omitter)

    def toggle_active(self, active=None, refresh=True):
        """\
        Toggle the activation state
        """
        # active is not given when refreshing target and enabler
        if active and self.is_blocked():
            self._active = False  # blocked is always inactive
                                  # (security measure)
        elif active is not None:
            self._active = active
        if not self._target:
            return
        try:
            for target in self._target:
                target.Enable(self._active)
        except TypeError:
            self._target.Enable(self._active)
        if self._active and self._omitter:
            try:
                value = self.owner.access_functions[self.name][0]()
                self.owner.access_functions[self.name][1](value)
            except:
                pass
        if refresh:
            try:
                common.app_tree.app.saved = False
            except AttributeError:
                pass  # why does this happen on win at startup?
        try:
            self._enabler.SetValue(self._active)
        except AttributeError:
            pass

    def is_active(self):
        """\
        Return True for a non-blocked and enabled / active widget.
        """
        if self.is_blocked():
            return False
        if self._target:
            try:
                widget = self._target[0]
            except TypeError:
                widget = self._target
            return compat.wxWindow_IsEnabled(widget)
        return self._active

    def toggle_blocked(self, blocked=None):
        """\
        Toggle the blocked state
        """
        if blocked is not None:
            self._blocked = blocked
        if self._enabler:
            self._enabler.Enable(not self._blocked)
        if self.is_blocked():  # deactivate blocked
            self.toggle_active(False, False)
        elif self._enabler:  # refresh activity of target and value of enabler
            self.toggle_active(refresh=False)
        elif not getattr(self, 'can_disable', None) and self._omitter:
            # enable blocked widget (that would be always active) without
            # _enabler
            if self.owner.get_property_blocking(self._omitter):
                self.toggle_active(True, False)

    def is_blocked(self):
        """\
        Return True for a non-enabled or blocked widget.
        """
        if self._enabler:
            return not compat.wxWindow_IsEnabled(self._enabler)
        return self._blocked

    def prepare_activator(self, enabler=None, target=None):
        if target:
            self._target = target
            if enabler:
                self._enabler = enabler
        self.toggle_blocked()

    def set_omitter(self, omitter):
        self._omitter = omitter
        if omitter:
            self.owner.set_property_blocking(omitter, self.name)

    def remove_omitter(self):
        if self._omitter:
            self.owner.remove_property_blocking(self._omitter, self.name)

# end of class _activator


class TextProperty(Property, _activator):
    """\
    Properties associated to a text control.

    @ivar multiline: Text property has a multiline wxTextCtrl widget,
                     True converts newline sequence to newline character
                     (and back) for use in the text ctrl.
    @type multiline: bool
    """

    def __init__(self, owner, name, parent=None, can_disable=False,
                 enabled=False, readonly=False, multiline=False,
                 label=None, blocked=False, omitter=None):
        Property.__init__(self, owner, name, parent, label=label)
        self.val = misc.wxstr(owner[name][0]())
        self.can_disable = can_disable
        self.readonly = readonly
        self.multiline = multiline
        _activator.__init__(self, omitter=omitter)
        if can_disable:
            self.toggle_active(enabled)
            self.toggle_blocked(blocked)
        self.panel = None
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the text control to set the value of the property
        interactively
        """
        if self.readonly:
            style = wx.TE_READONLY
        else:
            style = 0
        if self.multiline:
            style |= wx.TE_MULTILINE
        val = self.get_value()
        if self.multiline:
            val = self._unescape(val)
        lbl = getattr(self, 'label', None)
        if lbl is None:
            lbl = self._mangle(self.dispName)
        label = wx.lib.stattext.GenStaticText(
            parent, wx.ID_ANY, lbl, size=(config.label_initial_width, -1))
        self.text = wx.TextCtrl(
            parent, wx.ID_ANY, val, style=style, size=(1, -1))
        enabler = None
        if self.can_disable:
            enabler = wx.CheckBox(parent, wx.ID_ANY, '', size=(1, -1))
            enabler.Bind(wx.EVT_CHECKBOX,
                         lambda event: self.toggle_active(event.IsChecked()))
        self._tooltip_widgets = [label, self.text, enabler]
        self.set_tooltip()
        self.prepare_activator(enabler, self.text)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(label, 2, wx.ALL | wx.ALIGN_CENTER, 3)
        if getattr(self, '_enabler', None) is not None:
            sizer.Add(self._enabler, 1, wx.ALL | wx.ALIGN_CENTER, 3)
            option = 4
        else:
            option = 5
        sizer.Add(self.text, option, wx.ALL | wx.ALIGN_CENTER, 3)
        if self.multiline:
            h = self.text.GetCharHeight()
            sizer.SetItemMinSize(self.text, -1, h * 3)
        self.panel = sizer
        self.bind_event(self.on_change_val)
        self.text.Bind(wx.EVT_CHAR, self.on_char)

    def on_char(self, event):
        if event.GetKeyCode() == wx.WXK_ESCAPE:
            self.text.SetValue(self.val)
            self.text.SetInsertionPointEnd()
        event.Skip()

    def bind_event(self, function):
        def func_2(event):
            if self.text.IsBeingDeleted():
                return

            if compat.wxWindow_IsEnabled(self.text):
                function(event)
            event.Skip()
        self.text.Bind(wx.EVT_KILL_FOCUS, func_2)

    def get_value(self):
        try:
            val = self.text.GetValue()
            if self.multiline:
                val = self._escape(val)
            return val
        except AttributeError:
            return self.val

    def set_value(self, value):
        value = misc.wxstr(value)
        if self.multiline:
            self.val = self._escape(value)
            value = self._unescape(value)
        else:
            self.val = value
        try:
            self.text.SetValue(value)
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        if self.is_active():
            Property.write(self, outfile, tabs)

# end of class TextProperty


class CheckBoxProperty(Property, _activator):
    """\
    Properties whose values can be changed by one checkbox.
    """

    def __init__(self, owner, name, parent=None, label=None,
                 write_always=False, omitter=None):
        Property.__init__(self, owner, name, parent, label=label)
        self.val = int(owner[name][0]())
        if label is None or label == name:
            label = self._mangle(name)
        self.label = label
        self.panel = None
        self.write_always = write_always
        _activator.__init__(self, omitter=omitter)
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the check box to set the value of the property
        interactively
        """
        self.cb = wx.CheckBox(parent, wx.ID_ANY, '')
        self.cb.SetValue(self.val)
        label = wx.lib.stattext.GenStaticText(parent, wx.ID_ANY, self.label)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(label, 5, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 3)
        sizer.Add(self.cb, 0, wx.ALIGN_CENTER | wx.ALL, 3)
        self._tooltip_widgets = [label, self.cb]
        self.set_tooltip()
        self.prepare_activator(target=self.cb)
        self.panel = sizer
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        self.cb.Bind(wx.EVT_CHECKBOX, function)

    def get_value(self):
        try:
            return int(self.cb.GetValue())
        except AttributeError:
            return int(self.val)

    def set_value(self, val):
        self.val = int(val)
        try:
            self.cb.SetValue(self.val)
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        if self.write_always or self.get_value():
            if self.getter:
                value = int(self.getter())
            else:
                value = int(self.owner[self.name][0]())
            stmt = common.format_xml_tag(self.name, value, tabs)
            outfile.write(stmt)

# end of class CheckBoxProperty


class CheckListProperty(Property, _activator):
    """\
    Properties whose values can be changed by a list of checkboxes.

    Example of styles dictionary::
        styles = OrderedDict()
        styles[_('Border')] = ['wxALL', 'wxLEFT', 'wxRIGHT', 'wxTOP',
                               'wxBOTTOM']
        styles[_('Alignment')] = ['wxEXPAND', 'wxALIGN_RIGHT',
                                  'wxALIGN_BOTTOM',
                                  'wxALIGN_CENTER_HORIZONTAL',
                                  'wxALIGN_CENTER_VERTICAL', 'wxSHAPED',
                                  'wxADJUST_MINSIZE', 'wxFIXED_MINSIZE']

    @cvar _cache_wrapped_messages: Cache for already wrapped messages of all
                                   instances; see L{_wrap_msg()} for details
    @type _cache_wrapped_messages: dict

    @ivar _choices: Checkboxes representing the styles to select
    @type _choices: list[wx.CheckBox]

    @ivar styles: Ordered dictionary with box labels as key and a
                  list of styles to show as value
    @type styles: OrderedDict

    @ivar style_defs: Style definitions
    @type style_defs: dict[str]

    @ivar tooltips:  Detailed tooltips generated by L{_create_tooltip_text()}
    @type tooltips:  dict[str, str]
    """

    _cache_wrapped_messages = {}

    def __init__(self, owner, name, widget_writer=None, parent=None,
                 styles=None):
        """\
        Initialise this property.

        The styles to show are used from C{widget_writer.style_list} or
        from the styles dictionary.

        @param owner: The widget this property belongs to
        @type owner:  EditBase | SizerBase | PanelBase

        @param name: Property name
        @type name:  str

        @param widget_writer: Widget code writer
        @type widget_writer: wcodegen.BaseWidgetWriter

        @param styles: Ordered dictionary with box labels as key and a
                       list of styles to show as value
        @type styles:  OrderedDict
        """
        assert widget_writer or styles

        Property.__init__(self, owner, name, parent)

        self.values = owner[name][0]()
        self._choices = []
        self._labels = []

        if widget_writer:
            self.style_defs = widget_writer.style_defs
            self.styles = OrderedDict()
            self.styles[widget_writer.box_label] = widget_writer.style_list
        else:
            self.style_defs = config.widget_config['generic_styles']
            self.styles = styles

        for box_label in self.styles.keys():
            self._labels.extend(self.styles[box_label])

        self.tooltips = self._create_tooltip_text()

        self.panel = None
        _activator.__init__(self)

        if parent is not None:
            self.display(parent)

    def _wrap_msg(self, msg):
        """\
        Wrap a message to fit lines into a width of L{config.tooltip_width}.

        textwrap.warp() is terrible slow. Thereby results are are cached in
        L{self._cache_wrapped_messages}. The cache is used by all instances
        of this class.

        @param msg: Message to wrap
        @type msg:  str

        @rtype: str
        """
        msg = msg.strip()
        if msg in self._cache_wrapped_messages:
            return self._cache_wrapped_messages[msg]
        text = '\n'.join(textwrap.wrap(msg, config.tooltip_width))
        self._cache_wrapped_messages[msg] = text
        return text

    def _tooltip_format_flags(self, details):
        """\
        Create a tooltip text for generic style flags (aka attributes).

        @param details: style definitions from L{self.style_defs}
        @type details: dict

        @rtype: str
        """
        info = ''
        for attr_name, msg in [
            ('default_style', _('This style is the default\n')),
            ('obsolete', _('This style is obsolete and should not be '
                           'used.\nDetails: %s\n')),
            ('rename_to', _('This style will be renamed to %s.\n')),
            ('synonym', _('This style name is a synonym for %s.\n')),
        ]:
            if attr_name not in details:
                continue

            if '%s' in msg:
                info = self._wrap_msg(msg % details[attr_name])
            else:
                info = self._wrap_msg(msg)

        return info

    def _tooltip_format_generic(self, details):
        """\
        Create a tooltip text for generic style attributes.

        @param details: style definitions from L{self.style_defs}
        @type details: dict

        @rtype: str
        """
        info = ''
        for attr_name, msg in [
            ('include', _('This style includes: %s\n')),
            ('combination', _('This style is a combination of: %s\n')),
            ('exclude', _('This style excludes: %s\n')),
            ('require', _('This styles requires: %s\n')),
            ]:
            if attr_name not in details:
                continue

            style_list = list(details[attr_name])
            style_list.sort()
            if len(style_list) == 1:
                style_text = style_list[0]
            else:
                first = ', '.join(style_list[:-1])
                last = style_list[-1]
                style_text = _('%s and %s') % (first, last)

            info += self._wrap_msg(msg % style_text)

        return info

    def _tooltip_format_supported_by(self, details):
        """\
        Create a tooltip text for the 'supported_by' style attribute.

        @param details: style definitions from L{self.style_defs}
        @type details: dict

        @rtype: str
        """
        if 'supported_by' not in details:
            return ''

        supported_by = list(details['supported_by'])
        supported_by.sort()
        if len(supported_by) == 1:
            style_text = supported_by[0]
        else:
            first = supported_by[:-1]
            last = supported_by[-1]
            style_text = _('%s and %s') % (', '.join(first), last)
        info = self._wrap_msg(
            _('This style is only supported on %s\n') %
            style_text
        )
        return info

    def _create_tooltip_text(self):
        """\
        Create the texts for all tooltips based on widgets style
        configuration.

        @see: L{_tooltip_format_generic()}
        @see: L{_tooltip_format_flags()}
        @see: L{_tooltip_format_supported_by()}
        """
        tooltips = {}
        for style_name in self.style_defs:
            text = ''
            details = self.style_defs.get(style_name, {})

            if 'desc' in details:
                text += '%s\n' % self._wrap_msg(details['desc'])

            info = self._tooltip_format_generic(details)
            if info:
                if text:
                    text += '\n'
                text += info

            info = self._tooltip_format_supported_by(details)
            if info:
                if text:
                    text += '\n'
                text += info

            info = self._tooltip_format_flags(details)
            if info:
                if text:
                    text += '\n'
                text += info

            tooltips[style_name] = text

        return tooltips

    def display(self, parent):
        """\
        Actually builds the list of checkboxes to set the value of the property
        interactively
        """
        self._choices = []

        # the value for all choices is continuously stored in self.values
        value_pos = 0

        tmp_sizer = wx.BoxSizer(wx.VERTICAL)

        for box_label in self.styles.keys():
            static_box = wx.StaticBox(parent, wx.ID_ANY, box_label,
                                      style=wx.FULL_REPAINT_ON_RESIZE)
            box_sizer = wx.StaticBoxSizer(static_box, wx.VERTICAL)
            for style in self.styles[box_label]:
                checkbox = wx.CheckBox(parent, wx.ID_ANY, style)
                checkbox.SetValue(self.values[value_pos])

                # set the tool-tips for the properties
                if style in self.tooltips:
                    tip = STT.SuperToolTip(self.tooltips[style])
                    tip.SetHeader(style)
                    tip.SetTarget(checkbox)
                    tip.SetDrawHeaderLine(True)
                    tip.ApplyStyle("Beige")
                    tip.SetEndDelay(config.tooltip_time)

                self._choices.append(checkbox)
                box_sizer.Add(checkbox)

                value_pos += 1

            tmp_sizer.Add(box_sizer, 0, wx.ALL | wx.EXPAND, 5)

        self.prepare_activator(target=self._choices)

        self.panel = tmp_sizer
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        for checkbox in self._choices:
            checkbox.Bind(wx.EVT_CHECKBOX, function)

    def get_value(self):
        try:
            value = [choice.GetValue() for choice in self._choices]
            return value
        except AttributeError:
            return self.values

    def set_value(self, value):
        self.values = self.prepare_value(value)
        if not self._choices:
            return
        try:
            for i in range(len(self.values)):
                self._choices[i].SetValue(self.values[i])
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        val = self.owner[self.name][0]()
        value = '|'.join([self._labels[c]
                          for c in range(len(self._labels)) if val[c]])
        if value:
            stmt = common.format_xml_tag(self.name, value, tabs)
            outfile.write(stmt)

    def prepare_value(self, old_val):
        """\
        Convert token string into a list of boolean values.

        The returned list shows the position of a token from old_val in
        self.labels.

        Example::
            >>> self.prepare_value('wxALL|wxALIGN_CENTER_VERTICAL')
            [1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]

        @param old_val: String of tokens separated by '|'
        @type old_val:  str

        @rtype: list[bool]
        """
        try:
            old_val = old_val.split("|")
        except AttributeError:
            return list(old_val)
        ret = []
        for l in self._labels:
            if l in old_val:
                ret.append(1)
            else:
                ret.append(0)
        return ret

# end of class CheckListProperty


class SpinProperty(Property, _activator):
    """\
    Properties associated to a spin control.
    """

    def __init__(self, owner, name, parent=None, can_disable=False,
                 r=None, enabled=False, immediate=False, label=None,
                 blocked=False, omitter=None):
        # r = range of the spin (min, max)
        Property.__init__(self, owner, name, parent, label=label)
        self.can_disable = can_disable
        self.immediate = immediate  # if true, changes to this property have an
                                    # immediate effect (instead of waiting the
                                    # focus change...)
        _activator.__init__(self, omitter=omitter)
        if can_disable:
            self.toggle_active(enabled)
            self.toggle_blocked(blocked)
        if r is not None:
            self.val_range = (r[0], max(r[0], r[1]))
        else:
            self.val_range = None
        self.panel = None
        self.val = owner[name][0]()
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the spin control to set the value of the property
        interactively
        """
        if self.val_range is None:
            self.val_range = (0, 1000)
        lbl = getattr(self, 'label', None)
        if lbl is None:
            lbl = self._mangle(self.dispName)
        label = wx.lib.stattext.GenStaticText(
            parent, wx.ID_ANY, lbl, size=(config.label_initial_width, -1))
        self.spin = wx.SpinCtrl(parent, wx.ID_ANY, min=self.val_range[0],
                                max=self.val_range[1])
        val = int(self.owner[self.name][0]())
        if not val:
            self.spin.SetValue(1)  # needed for GTK to display a '0'
        self.spin.SetValue(val)
        enabler = None
        if self.can_disable:
            enabler = wx.CheckBox(parent, wx.ID_ANY, '', size=(1, -1))
            enabler.Bind(wx.EVT_CHECKBOX,
                         lambda event: self.toggle_active(event.IsChecked()))
        self._tooltip_widgets = [label, self.spin, enabler]
        self.set_tooltip()
        self.prepare_activator(enabler, self.spin)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(label, 2, wx.ALL | wx.ALIGN_CENTER, 3)
        if getattr(self, '_enabler', None) is not None:
            sizer.Add(self._enabler, 1, wx.ALL | wx.ALIGN_CENTER, 3)
            option = 4
        else:
            option = 5
        sizer.Add(self.spin, option, wx.ALL | wx.ALIGN_CENTER, 3)
        self.panel = sizer
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        def func_2(event):
            if self.spin.IsBeingDeleted():
                return

            if self.is_active():
                function(event)
            event.Skip()
        self.spin.Bind(wx.EVT_KILL_FOCUS, func_2)
        if wx.Platform == '__WXMAC__' or self.immediate:
            self.spin.Bind(wx.EVT_TEXT, func_2)
            self.spin.Bind(wx.EVT_SPINCTRL, func_2)

    def get_value(self):
        try:
            return self.spin.GetValue()
        except AttributeError:
            return self.val

    def set_value(self, value):
        self.val = int(value)
        try:
            self.spin.SetValue(int(value))
        except AttributeError:
            pass

    def set_range(self, min_v, max_v):
        self.val_range = (min_v, max(min_v, max_v))
        try:
            self.spin.SetRange(min_v, max_v)
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        if self.is_active():
            Property.write(self, outfile, tabs)

# end of class SpinProperty


class DialogProperty(Property, _activator):
    """\
    Property which selection is made through a dialog, which must provide a
    get_value method.
    """

    def __init__(self, owner, name, parent, dialog, can_disable=False,
                 enabled=False, label=None, blocked=False, omitter=None):
        Property.__init__(self, owner, name, parent, label=label)
        self.dialog = dialog
        self.panel = None
        self.can_disable = can_disable
        _activator.__init__(self, omitter=omitter)
        if can_disable:
            self.toggle_active(enabled)
            self.toggle_blocked(blocked)
        if parent is not None:
            self.display(parent)
        self.val = "%s" % owner[name][0]()

    def display(self, parent):
        """\
        Actually builds the panel (with the text ctrl and the button to display
        the dialog) to set the value of the property interactively
        """
        val = misc.wxstr(self.owner[self.name][0]())
        label = wx.lib.stattext.GenStaticText(
            parent, wx.ID_ANY, self._mangle(self.dispName),
            size=(config.label_initial_width, -1))
        self.text = wx.TextCtrl(parent, wx.ID_ANY, val, size=(1, -1))
        self.btn = wx.Button(parent, wx.ID_ANY, " ... ",
                             size=(config.label_initial_width, -1))
        enabler = None
        if self.can_disable:
            enabler = wx.CheckBox(parent, wx.ID_ANY, '', size=(1, -1))
            enabler.Bind(wx.EVT_CHECKBOX,
                         lambda event: self.toggle_active(event.IsChecked()))
        self.prepare_activator(enabler, self.text)
        self._tooltip_widgets = [label, self.text, self.btn, enabler]
        self.set_tooltip()
        if self.can_disable:
            self.btn.Enable(self.is_active())
        self.btn.Bind(wx.EVT_BUTTON, self.display_dialog)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(label, 2, wx.ALL | wx.ALIGN_CENTER, 3)
        if getattr(self, '_enabler', None) is not None:
            sizer.Add(self._enabler, 1, wx.ALL | wx.ALIGN_CENTER, 3)
            option = 3
        else:
            option = 4
        sizer.Add(self.text, option, wx.ALL | wx.ALIGN_CENTER, 3)
        sizer.Add(self.btn, 1, wx.ALL | wx.ALIGN_CENTER, 3)
        self.panel = sizer

        self.bind_event(self.on_change_val)
        self.text.Bind(wx.EVT_CHAR, self.on_char)

    def on_char(self, event):
        if event.GetKeyCode() == wx.WXK_ESCAPE:
            self.text.SetValue(self.val)
            self.text.SetInsertionPointEnd()
        event.Skip()

    def display_dialog(self, event):
        if self.dialog.ShowModal() == wx.ID_OK:
            self.text.SetValue(misc.wxstr(self.dialog.get_value()))
        self.text.ProcessEvent(
            wx.FocusEvent(wx.wxEVT_KILL_FOCUS, self.text.GetId()))

    def bind_event(self, function):
        def func_2(event):
            if self.text.IsBeingDeleted():
                return

            function(event)
        self.text.Bind(wx.EVT_KILL_FOCUS, func_2)

    def get_value(self):
        try:
            return self.text.GetValue()
        except AttributeError:
            return self.val

    def set_value(self, value):
        self.val = misc.wxstr(value)
        try:
            self.text.SetValue(self.val)
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        if self.is_active():
            Property.write(self, outfile, tabs)

    def toggle_active(self, active=None, refresh=True):
        _activator.toggle_active(self, active, refresh)
        try:
            self.btn.Enable(self.is_active())
        except AttributeError:
            pass

# end of class DialogProperty


class FileDialogProperty(DialogProperty):
    dialog = [None]

    class FileDialog:
        def __init__(self, parent, message, wildcard, style):
            self.parent = parent
            self.message = message
            self.wildcard = wildcard
            self.style = style
            self.value = None

        def ShowModal(self):
            self.value = wx.FileSelector(
                self.message, wildcard=self.wildcard, flags=self.style)
            if self.value:
                return wx.ID_OK

        def get_value(self):
            return self.value

    # end of class FileDialog

    def __init__(self, owner, name, parent=None, wildcard=_("All files|*"),
                 message=_("Choose a file"), can_disable=True, style=0,
                 label=None, blocked=False, omitter=None):
        if not self.dialog[0]:
            self.dialog[0] = self.FileDialog(parent, message, wildcard, style)
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable, label=label, blocked=blocked,
                                omitter=omitter)

# end of class FileDialogProperty


class ColorDialogProperty(DialogProperty):
    str_to_colors = {
        'wxSYS_COLOUR_SCROLLBAR': wx.SYS_COLOUR_SCROLLBAR,
        'wxSYS_COLOUR_BACKGROUND': wx.SYS_COLOUR_BACKGROUND,
        'wxSYS_COLOUR_ACTIVECAPTION': wx.SYS_COLOUR_ACTIVECAPTION,
        'wxSYS_COLOUR_INACTIVECAPTION': wx.SYS_COLOUR_INACTIVECAPTION,
        'wxSYS_COLOUR_MENU': wx.SYS_COLOUR_MENU,
        'wxSYS_COLOUR_WINDOW': wx.SYS_COLOUR_WINDOW,
        'wxSYS_COLOUR_WINDOWFRAME': wx.SYS_COLOUR_WINDOWFRAME,
        'wxSYS_COLOUR_MENUTEXT': wx.SYS_COLOUR_MENUTEXT,
        'wxSYS_COLOUR_WINDOWTEXT': wx.SYS_COLOUR_WINDOWTEXT,
        'wxSYS_COLOUR_CAPTIONTEXT': wx.SYS_COLOUR_CAPTIONTEXT,
        'wxSYS_COLOUR_ACTIVEBORDER': wx.SYS_COLOUR_ACTIVEBORDER,
        'wxSYS_COLOUR_INACTIVEBORDER': wx.SYS_COLOUR_INACTIVEBORDER,
        'wxSYS_COLOUR_APPWORKSPACE': wx.SYS_COLOUR_APPWORKSPACE,
        'wxSYS_COLOUR_HIGHLIGHT': wx.SYS_COLOUR_HIGHLIGHT,
        'wxSYS_COLOUR_HIGHLIGHTTEXT': wx.SYS_COLOUR_HIGHLIGHTTEXT,
        'wxSYS_COLOUR_BTNFACE': wx.SYS_COLOUR_BTNFACE,
        'wxSYS_COLOUR_BTNSHADOW': wx.SYS_COLOUR_BTNSHADOW,
        'wxSYS_COLOUR_GRAYTEXT': wx.SYS_COLOUR_GRAYTEXT,
        'wxSYS_COLOUR_BTNTEXT': wx.SYS_COLOUR_BTNTEXT,
        'wxSYS_COLOUR_INACTIVECAPTIONTEXT': wx.SYS_COLOUR_INACTIVECAPTIONTEXT,
        'wxSYS_COLOUR_BTNHIGHLIGHT': wx.SYS_COLOUR_BTNHIGHLIGHT,
        'wxSYS_COLOUR_3DDKSHADOW': wx.SYS_COLOUR_3DDKSHADOW,
        'wxSYS_COLOUR_3DLIGHT': wx.SYS_COLOUR_3DLIGHT,
        'wxSYS_COLOUR_INFOTEXT': wx.SYS_COLOUR_INFOTEXT,
        'wxSYS_COLOUR_INFOBK': wx.SYS_COLOUR_INFOBK,
        'wxSYS_COLOUR_DESKTOP': wx.SYS_COLOUR_DESKTOP,
        'wxSYS_COLOUR_3DFACE': wx.SYS_COLOUR_3DFACE,
        'wxSYS_COLOUR_3DSHADOW': wx.SYS_COLOUR_3DSHADOW,
        'wxSYS_COLOUR_3DHIGHLIGHT': wx.SYS_COLOUR_3DHIGHLIGHT,
        'wxSYS_COLOUR_3DHILIGHT': wx.SYS_COLOUR_3DHILIGHT,
        'wxSYS_COLOUR_BTNHILIGHT': wx.SYS_COLOUR_BTNHILIGHT
        }

    colors_to_str = _reverse_dict(str_to_colors)

    dialog = [None]

    def __init__(self, owner, name, parent=None, can_disable=True,
                 label=None, blocked=False, omitter=None):
        if not self.dialog[0]:
            from color_dialog import wxGladeColorDialog
            self.dialog[0] = wxGladeColorDialog(self.str_to_colors)
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable, label=label, blocked=blocked,
                                omitter=omitter)

    def display_dialog(self, event):
        self.dialog.set_value(self.get_value())
        DialogProperty.display_dialog(self, event)

    def toggle_active(self, active=None, refresh=True):
        DialogProperty.toggle_active(self, active, refresh)
        if not self.is_active():
            # restore the original value if toggled off
            color = self.owner._original.get(self.name, None)
            if color is not None and self.owner.widget is not None:
                which = 'Set%sColour' % self.name.capitalize()
                func = getattr(self.owner.widget, which, lambda c: None)
                func(color)
                self.owner.widget.Refresh()
        else:
            # restore the saved value
            getval, setval = self.owner[self.name]
            setval(getval())

# end of class ColorDialogProperty


class FontDialogProperty(DialogProperty):
    font_families_to = {'default': wx.DEFAULT,
                        'decorative': wx.DECORATIVE,
                        'roman': wx.ROMAN,
                        'swiss': wx.SWISS,
                        'script': wx.SCRIPT,
                        'modern': wx.MODERN,
                        }
    font_families_from = _reverse_dict(font_families_to)
    font_styles_to = {'normal': wx.NORMAL,
                      'slant': wx.SLANT,
                      'italic': wx.ITALIC,
                      }
    font_styles_from = _reverse_dict(font_styles_to)
    font_weights_to = {'normal': wx.NORMAL,
                       'light': wx.LIGHT,
                       'bold': wx.BOLD,
                       }
    font_weights_from = _reverse_dict(font_weights_to)

    font_families_to['teletype'] = wx.TELETYPE
    font_families_from[wx.TELETYPE] = 'teletype'

    dialog = [None]

    def __init__(self, owner, name, parent=None, can_disable=True,
                 label=None, blocked=False, omitter=None):
        if not self.dialog[0]:
            import font_dialog
            self.dialog[0] = font_dialog.wxGladeFontDialog(parent, -1, "")
        DialogProperty.__init__(self, owner, name, parent, self.dialog[0],
                                can_disable, label=label, blocked=blocked,
                                omitter=omitter)

    def display_dialog(self, event):
        try:
            props = eval(self.get_value())
        except:
            self._logger.exception(_('Internal Error'))
        else:
            if len(props) == 6:
                self.dialog.set_value(props)
        DialogProperty.display_dialog(self, event)

    def write(self, outfile, tabs=0):
        if self.is_active():
            try:
                props = [common.encode_to_unicode(s) for s in
                         eval(self.get_value().strip())]
            except:
                self._logger.exception(_('Internal Error'))
                return
            if len(props) < 6:
                self._logger.error(
                    _('error in the value of the property "%s"'),
                    self.name,
                    )
                return
            inner_xml = common.format_xml_tag(u'size', props[0], tabs + 1)
            inner_xml += common.format_xml_tag(u'family', props[1], tabs + 1)
            inner_xml += common.format_xml_tag(u'style', props[2], tabs + 1)
            inner_xml += common.format_xml_tag(u'weight', props[3], tabs + 1)
            inner_xml += common.format_xml_tag(
                u'underlined', props[4], tabs + 1)
            inner_xml += common.format_xml_tag(u'face', props[5], tabs + 1)
            stmt = common.format_xml_tag(
                self.name, inner_xml, tabs, is_xml=True)
            outfile.write(stmt)

    def toggle_active(self, active=None, refresh=True):
        DialogProperty.toggle_active(self, active, refresh)
        if not self.is_active():
            # restore the original value if toggled off
            font = self.owner._original['font']
            if font is not None and self.owner.widget is not None:
                self.owner.widget.SetFont(font)
                self.owner.widget.Refresh()
        else:
            # restore the saved value
            getval, setval = self.owner[self.name]
            setval(getval())

# end of class FontDialogProperty


class RadioProperty(Property, _activator):
    """\
    Properties controlled by a series of radio buttons.

    @ivar sort: Sort the the choices.
    @type sort: bool

    @ivar capitalize: Capitalise the first character of each entry in
                      C{choices} list. Capitalising the first character is an
                      internal process. It does not affect the functions
                      L{get_str_value()}, L{set_str_value()}, L{set_value()}
                      and L{get_value()}. They still handle the original
                      entries.
    @type capitalize: bool

    @ivar choices: Choices shown in the radiobox
    @type choices: list[str]

    @ivar options: wxRadioBox instance
    @type options: wx.RadioBox | None

    @ivar _cap2orig: Dictionary for reverse mapping between the capitalised
                     entry and the original one.
    @type _cap2orig: dict
    """

    def __init__(self, owner, name, parent, choices, can_disable=False,
                 enabled=False, columns=1, label=None, tooltips=None,
                 blocked=False, omitter=None, sort=False,
                 capitalize=False):
        Property.__init__(self, owner, name, parent, label=label)
        self.can_disable = can_disable
        _activator.__init__(self, omitter=omitter)
        if can_disable:
            self.toggle_active(enabled)
            self.toggle_blocked(blocked)
        self._cap2orig = {}
        self.capitalize = capitalize
        self.choices = choices
        self.columns = columns
        self.panel = None
        self.options = None
        self.label = label
        self.sort = sort
        self.tooltips = tooltips
        self.val = owner[name][0]()
        if label is None:
            self.label = self._mangle(name)
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the radio box to set the value of the property
        interactively
        """
        style = wx.RA_SPECIFY_COLS | wx.NO_BORDER | wx.CLIP_CHILDREN
        if not self.can_disable:
            szr = wx.BoxSizer(wx.HORIZONTAL)
            style = wx.RA_SPECIFY_COLS
        else:
            szr = wx.StaticBoxSizer(wx.StaticBox(parent, wx.ID_ANY, self.label),
                                    wx.HORIZONTAL)
        if self.capitalize:
            new_choices = []
            for orig in self.choices:
                cap = misc.capitalize(orig)
                new_choices.append(cap)
                self._cap2orig[cap] = orig
            self.choices = new_choices
        if self.sort:
            self.choices.sort()
        self.options = wx.RadioBox(parent, wx.ID_ANY, self.label,
                                   choices=self.choices,
                                   majorDimension=self.columns,
                                   style=style)
        # Set the tool-tips for the properties
        if self.tooltips:
            max_tooltips = len(self.choices)
            for i, tooltip in enumerate(self.tooltips):
                if i >= max_tooltips:
                    break
                self.options.SetItemToolTip(i, tooltip)
        try:
            self.options.SetSelection(int(self.val))
        except:
            pass
        enabler = None
        if self.can_disable:
            enabler = wx.CheckBox(parent, wx.ID_ANY, "")
            szr.Add(enabler)
            enabler.Bind(wx.EVT_CHECKBOX,
                         lambda e: self.toggle_active(e.IsChecked()))
            self.options.SetLabel("")
        self.prepare_activator(enabler, self.options)
        szr.Add(self.options, 1, wx.ALL | wx.EXPAND, 5)
        self.panel = szr
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        def func_2(event, function=function, self=self):
            if compat.wxWindow_IsEnabled(self.options):
                function(event)
        self.options.Bind(wx.EVT_RADIOBOX, func_2)

    def get_value(self):
        """\
        Return the zero-based position if the selected entry
        """
        try:
            return self.options.GetSelection()
        except AttributeError:
            return self.val

    def get_str_value(self):
        """\
        Return the content of the selected entry
        """
        try:
            selected_string = self.options.GetStringSelection()
            # reverse lookup for capitalized selections
            if self.capitalize:
                selected_string = self._cap2orig[selected_string]
            return selected_string
        except AttributeError:
            if 0 <= self.val < len(self.choices):
                selected_string = self.choices[self.val]
                # reverse lookup for capitalized selections
                if self.capitalize:
                    selected_string = self._cap2orig[selected_string]
                return selected_string
            else:
                return ''

    def set_value(self, value):
        try:
            self.val = int(value)
        except ValueError:
            if self.capitalize:
                value = misc.capitalize(value)
            self.val = self.choices.index(value)
        try:
            self.options.SetSelection(self.val)
        except AttributeError:
            pass

    def set_str_value(self, value):
        """\
        Select the entry specified by given value.
        """
        if self.capitalize:
            value = misc.capitalize(value)
        try:
            self.val = self.choices.index(value)
            self.options.SetSelection(self.val)
        except (AttributeError, ValueError):
            pass

    def write(self, outfile, tabs=0):
        if self.is_active():
            stmt = common.format_xml_tag(
                self.name, self.get_str_value(), tabs)
            outfile.write(stmt)

    def enable_item(self, item, flag):
        """\
        Enable or disable a single radiobox item.

        @param item: Item text
        @type item:  str

        @param flag: Enable or disable this item
        @type flag:  bool
        """
        if not self.options:
            return
        if self.capitalize:
            item = misc.capitalize(item)
        index = self.choices.index(item)
        self.options.EnableItem(index, flag)

# end of class RadioProperty


class GridProperty(Property, _activator):
    """\
    Property whose values are modified through a wxGrid table.

    @ivar can_add:    Add Button to add a new entry
    @type can_add:    bool
    @ivar can_insert: Add Button to insert a new entry
    @type can_insert: bool
    @ivar can_remove: Add Button to remove a new entry
    @type can_remove: bool
    @ivar can_remove_last: Allow to remove last entry
    @type can_remove_last: bool
    @ivar cols:      Number of columns
    @type cols:      int
    @ivar col_defs:  List of column labels and column types (
                     L{GridProperty.STRING}, L{GridProperty.INT},
                     L{GridProperty.FLOAT} and L{GridProperty.BOOL}).
    @type col_defs:  list[(str, int)]
    @ivar col_sizes:  List of column widths
    @type col_sizes:  list[int]

    @ivar rows:       Number of rows
    @type rows:       int

    @cvar STRING: Column displays string values
    @cvar INT:    Column displays integer values.
    @cvar FLOAT:  Column displays float values.
    @cvar BOOL:   Column displays boolean values.

    @cvar col_format: List of functions to set the column format.
    """
    STRING, INT, FLOAT, BOOL = 0, 1, 2, 3
    col_format = [lambda g, c: None,
                  lambda g, c: g.SetColFormatNumber(c),
                  lambda g, c: g.SetColFormatFloat(c),
                  lambda g, c: g.SetColFormatBool(c)]

    def __init__(self, owner, name, parent, cols, rows=1, can_add=True,
                 can_remove=True, can_insert=True, label=None, omitter=None,
                 col_sizes=None, can_remove_last=True):
        Property.__init__(self, owner, name, parent, label=label)
        self.val = owner[name][0]()
        self.set_value(self.val)
        self.rows = rows
        self.col_defs = cols
        self.cols = len(self.col_defs)
        self.can_add = can_add
        self.can_remove = can_remove
        self.can_insert = can_insert
        self.can_remove_last = can_remove_last
        if col_sizes is None:
            self.col_sizes = []
        else:
            self.col_sizes = col_sizes
        self.panel = None
        self.cur_row = 0
        _activator.__init__(self, omitter=omitter)
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the grid to set the value of the property
        interactively
        """
        children = []
        self.panel = wx.Panel(parent, -1)  # why if the grid is not on this
                                           # panel it is not displayed???
        label = getattr(self, 'label', self._mangle(self.dispName))
        sizer = wx.StaticBoxSizer(wx.StaticBox(self.panel, -1, label),
                                  wx.VERTICAL)
        self.btn = wx.Button(self.panel, wx.ID_ANY, _("  Apply  "),
                             style=wx.BU_EXACTFIT)
        children.append(self.btn)
        if self.can_add:
            self.add_btn = wx.Button(self.panel, wx.ID_ANY, _("  Add  "),
                                     style=wx.BU_EXACTFIT)
            children.append(self.add_btn)
        if self.can_insert:
            self.insert_btn = wx.Button(self.panel, wx.ID_ANY,
                                        _("  Insert  "),
                                        style=wx.BU_EXACTFIT)
            children.append(self.insert_btn)
        if self.can_remove:
            self.remove_btn = wx.Button(self.panel, wx.ID_ANY,
                                        _("  Remove  "),
                                        style=wx.BU_EXACTFIT)
            children.append(self.remove_btn)
        self.grid = wx.grid.Grid(self.panel, -1)
        self.grid.CreateGrid(self.rows, self.cols)
        children.append(self.grid)
        self.grid.SetMargins(0, 0)

        for i, column in enumerate(self.col_defs):
            self.grid.SetColLabelValue(i, misc.wxstr(column[0]))
            GridProperty.col_format[column[1]](self.grid, i)

        self.grid.SetRowLabelSize(0)
        self.grid.SetColLabelSize(20)
        if self.col_sizes:
            self.set_col_sizes(self.col_sizes)

        self.btn_sizer = wx.BoxSizer(wx.HORIZONTAL)
        extra_flag = wx.FIXED_MINSIZE
        self.btn_sizer.Add(self.btn, 0, extra_flag)
        if self.can_add:
            self.btn_sizer.Add(
                self.add_btn, 0, wx.LEFT | wx.RIGHT | extra_flag, 4)
            self.add_btn.Bind(wx.EVT_BUTTON, self.add_row)
        if self.can_insert:
            self.btn_sizer.Add(
                self.insert_btn, 0, wx.LEFT | wx.RIGHT | extra_flag, 4)
            self.insert_btn.Bind(wx.EVT_BUTTON, self.insert_row)
        if self.can_remove:
            self.btn_sizer.Add(self.remove_btn, 0, extra_flag)
            self.remove_btn.Bind(wx.EVT_BUTTON, self.remove_row)
        sizer.Add(self.btn_sizer, 0, wx.BOTTOM | wx.EXPAND, 2)
        sizer.Add(self.grid, 1, wx.EXPAND)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())

        self.prepare_activator(target=children)
        self.grid.Bind(wx.grid.EVT_GRID_SELECT_CELL, self.on_select_cell)
        self.bind_event(self.on_change_val)
        self.set_value(self.val)

    def on_select_cell(self, event):
        self.cur_row = event.GetRow()
        event.Skip()

    def bind_event(self, function):
        def func(event):
            self.grid.SaveEditControlValue()
            function(event)
        self.btn.Bind(wx.EVT_BUTTON, func)

    def get_value(self):
        if not hasattr(self, 'grid'):
            return self.val

        grid_lines = []
        for i in range(self.rows):
            single_line = []
            for j in range(self.cols):
                value = self.grid.GetCellValue(i, j)
                if self.col_defs[j][1] == GridProperty.STRING:
                    unescaped = value
                    value = self._escape(value)
                    if unescaped != value:
                        self.grid.SetCellValue(i, j, value)
                single_line.append(value)
            grid_lines.append(single_line)
        return grid_lines

    def on_change_val(self, event):
        """\
        Event handler called to notify owner that the value of the Property
        has changed
        """
        val = self.get_value()

        def has_changed():
            if len(self.val) != len(val):
                return True
            for i in range(len(val)):
                for j in range(len(val[i])):
                    if not misc.streq(val[i][j], self.val[i][j]):
                        return True
            return False

        if has_changed():
            common.app_tree.app.saved = False  # update the status of the app
            if self.setter:
                self.setter(val)
            else:
                self.owner[self.name][1](val)
            self.val = val
        event.Skip()

    def set_value(self, values):
        self.val = [[misc.wxstr(v) for v in val] for val in values]
        if not hasattr(self, 'grid'):
            return

        # values is a list of lists with the values of the cells
        rows_new = len(values)

        # add or remove rows
        if self.rows < rows_new:
            self.grid.AppendRows(rows_new - self.rows)
            self.rows = rows_new
        elif self.rows != rows_new:
            self.grid.DeleteRows(rows_new, self.rows - rows_new)

        # update content
        for i in range(len(self.val)):
            for j in range(len(self.val[i])):
                self.grid.SetCellValue(i, j, self.val[i][j])

        # update state of the remove button
        self._update_remove_button()

    def add_row(self, event):
        self.grid.AppendRows()
        self.grid.MakeCellVisible(self.rows, 0)
        self.grid.ForceRefresh()
        self.rows += 1
        self._update_remove_button()

    def remove_row(self, event):
        if not self.can_remove_last and self.rows == 1:
            self._logger.warning(
                _('You can not remove the last entry!')
                )
            return
        if self.rows > 0:
            self.grid.DeleteRows(self.cur_row)
            self.rows -= 1
        self._update_remove_button()

    def insert_row(self, event):
        self.grid.InsertRows(self.cur_row)
        self.grid.MakeCellVisible(self.cur_row, 0)
        self.grid.ForceRefresh()
        self.rows += 1
        self._update_remove_button()

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
            try:
                w = sizes[i]
            except IndexError:
                return
            if not w:
                self.grid.AutoSizeColumn(i)
                total_w += self.grid.GetColSize(i)
            elif w < 0:
                col_to_expand = i
            else:
                self.grid.SetColSize(i, w)
                total_w += w
        if col_to_expand >= 0:
            self.grid.AutoSizeColumn(col_to_expand)
            w = self.grid.GetSize()[0] - total_w
            if w >= self.grid.GetColSize(col_to_expand):
                self.grid.SetColSize(col_to_expand, w)

    def _update_remove_button(self):
        """\
        Enable or disable remove button

        The state of the remove button depends on the number of rows and
        L{self.can_remove_last}.

        @see: L{self.can_remove_last}
        @see: L{self.can_remove}
        @see: L{self.rows}
        """
        if self.can_remove and not self.can_remove_last:
            self.remove_btn.Enable(self.rows > 1)

# end of class GridProperty


class ComboBoxProperty(Property, _activator):
    """\
    Properties whose values can be changed with a combobox.
    """

    def __init__(self, owner, name, choices, parent=None, label=None,
                 can_disable=False, enabled=False, write_always=False,
                 blocked=False, omitter=None):
        Property.__init__(self, owner, name, parent, label=label)
        self.val = misc.wxstr(owner[name][0]())
        if label is None:
            label = self._mangle(name)
        self.label = label
        self.panel = None
        self.write_always = write_always
        self.choices = choices
        self.can_disable = can_disable
        _activator.__init__(self, omitter=omitter)
        if can_disable:
            self.toggle_active(enabled)
            self.toggle_blocked(blocked)
        if parent is not None:
            self.display(parent)

    def display(self, parent):
        """\
        Actually builds the check box to set the value of the property
        interactively
        """
        self.cb = wx.ComboBox(parent, wx.ID_ANY, choices=self.choices,
                              style=wx.CB_DROPDOWN | wx.CB_READONLY)
        self.cb.SetValue(self.val)
        label = wx.StaticText(parent, wx.ID_ANY, self.label)
        enabler = None
        if self.can_disable:
            enabler = wx.CheckBox(parent, wx.ID_ANY, '', size=(1, -1))
            enabler.Bind(wx.EVT_CHECKBOX,
                         lambda event: self.toggle_active(event.IsChecked()))
        self._tooltip_widgets = [label, self.cb, enabler]
        self.set_tooltip()
        self.prepare_activator(enabler, self.cb)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(label, 2, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 3)
        if getattr(self, '_enabler', None) is not None:
            sizer.Add(self._enabler, 1, wx.ALL | wx.ALIGN_CENTER, 3)
            option = 4
        else:
            option = 5
        sizer.Add(self.cb, option, wx.ALIGN_CENTER | wx.ALL, 3)
        self.panel = sizer
        self.bind_event(self.on_change_val)

    def bind_event(self, function):
        self.cb.Bind(wx.EVT_COMBOBOX, function)

    def get_value(self):
        try:
            return misc.wxstr(self.cb.GetValue())
        except AttributeError:
            return misc.wxstr(self.val)

    def set_value(self, val):
        self.val = misc.wxstr(val)
        try:
            self.cb.SetValue(self.val)
        except AttributeError:
            pass

    def write(self, outfile, tabs=0):
        if self.write_always or (self.is_active() and self.get_value()):
            if self.getter:
                value = misc.wxstr(self.getter())
            else:
                value = misc.wxstr(self.owner[self.name][0]())
            if value != 'None':
                stmt = common.format_xml_tag(self.name, value, tabs)
                outfile.write(stmt)

# end of class ComboBoxProperty
