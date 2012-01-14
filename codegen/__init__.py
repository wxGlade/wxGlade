# Common code used by all code generators
#
# License: MIT (see license.txt)
#
# THIS PROGRAM COMES WITH NO WARRANTY

import random
import time

import common
import config


class DummyPropertyHandler(object):
    """Empty handler for properties that do not need code"""

    def __init__(self):
        self.handlers = {}
        self.event_name = None
        self.curr_handler = []

    def start_elem(self, name, attrs):
        pass

    def end_elem(self, name, code_obj):
        return True

    def char_data(self, data):
        pass

# end of class DummyPropertyHandler


class EventsPropertyHandler(DummyPropertyHandler):
    """\
    Handler for event properties
    """

    def start_elem(self, name, attrs):
        if name == 'handler':
            self.event_name = attrs['event']

    def end_elem(self, name, code_obj):
        if name == 'handler':
            if self.event_name and self.curr_handler:
                self.handlers[self.event_name] = ''.join(self.curr_handler)
            self.event_name = None
            self.curr_handler = []
        elif name == 'events':
            code_obj.properties['events'] = self.handlers
            return True

    def char_data(self, data):
        data = data.strip()
        if data:
            self.curr_handler.append(data)

# end of class EventsPropertyHandler


class ExtraPropertiesPropertyHandler(DummyPropertyHandler):

    def __init__(self):
        DummyPropertyHandler.__init__(self)
        self.props = {}
        self.curr_prop = []
        self.prop_name = None

    def start_elem(self, name, attrs):
        if name == 'property':
            name = attrs['name']
            if name and name[0].islower():
                name = name[0].upper() + name[1:]
            self.prop_name = name

    def end_elem(self, name, code_obj):
        if name == 'property':
            if self.prop_name and self.curr_prop:
                self.props[self.prop_name] = ''.join(self.curr_prop)
            self.prop_name = None
            self.curr_prop = []
        elif name == 'extraproperties':
            code_obj.properties['extraproperties'] = self.props
            return True  # to remove this handler

    def char_data(self, data):
        data = data.strip()
        if data:
            self.curr_prop.append(data)

# end of class ExtraPropertiesPropertyHandler


# custom property handlers
class FontPropertyHandler(object):
    """Handler for font properties"""

    font_families = {'default': 'wxDEFAULT', 'decorative': 'wxDECORATIVE',
                     'roman': 'wxROMAN', 'swiss': 'wxSWISS',
                     'script': 'wxSCRIPT', 'modern': 'wxMODERN',
                     'teletype': 'wxTELETYPE'}
    font_styles = {'normal': 'wxNORMAL', 'slant': 'wxSLANT',
                   'italic': 'wxITALIC'}
    font_weights = {'normal': 'wxNORMAL', 'light': 'wxLIGHT',
                    'bold': 'wxBOLD'}

    def __init__(self):
        self.dicts = {'family': self.font_families, 'style': self.font_styles,
                      'weight': self.font_weights}
        self.attrs = {'size': '0', 'style': '0', 'weight': '0', 'family': '0',
                       'underlined': '0', 'face': ''}
        self.current = None
        self.curr_data = []

    def start_elem(self, name, attrs):
        self.curr_data = []
        if name != 'font' and name in self.attrs:
            self.current = name
        else:
            self.current = None

    def end_elem(self, name, code_obj):
        if name == 'font':
            code_obj.properties['font'] = self.attrs
            return True
        elif self.current is not None:
            decode = self.dicts.get(self.current)
            if decode:
                val = decode.get("".join(self.curr_data), '0')
            else:
                val = "".join(self.curr_data)
            self.attrs[self.current] = val

    def char_data(self, data):
        self.curr_data.append(data)

# end of class FontPropertyHandler


class BaseWidgetHandler(object):
    """\
    Interface the various code generators for the widgets must implement
    """

    import_modules = []
    """\
    List of modules to import (eg. ['use Wx::Grid;\n'])
    """

    def __init__(self):
        """\
        Initialise instance variables
        """
        self.import_modules = []

    def get_code(self, obj):
        """\
        Handler for normal widgets (non-toplevel): returns 3 lists of strings,
        init, properties and layout, that contain the code for the
        corresponding methods of the class to generate
        """
        return [], [], []

    def get_properties_code(self, obj):
        """\
        Handler for the code of the set_properties method of toplevel objects.
        Returns a list of strings containing the code to generate
        """
        return []

    def get_init_code(self, obj):
        """\
        Handler for the code of the constructor of toplevel objects.  Returns a
        list of strings containing the code to generate.  Usually the default
        implementation is ok (i.e. there are no extra lines to add). The
        generated lines are appended at the end of the constructor
        """
        return []

    def get_layout_code(self, obj):
        """\
        Handler for the code of the do_layout method of toplevel objects.
        Returns a list of strings containing the code to generate.
        Usually the default implementation is ok (i.e. there are no
        extra lines to add)
        """
        return []

# end of class BaseWidgetHandler


class BaseClassLines:
    """\
    Stores the lines of source code for a custom class

    @ivar dependencies:      Names of the modules this class depends on
    @ivar event_handlers:    Lines to bind events
    @ivar extra_code:        Extra code to output before this class
    @ivar done:              If True, the code for this class has already
                             been generated
    @ivar init:              Lines of code to insert in the __init__ method
                             (for children widgets)
    @ivar layout:            Lines to insert in the __do_layout method
    @ivar parents_init:      Lines of code to insert in the __init__ for
                             container widgets (panels, splitters, ...)
    @ivar props:             Lines to insert in the __set_properties method
    @ivar sizers_init :      Lines related to sizer objects declarations
    """
    def __init__(self):
        self.child_order = []
        self.dependencies = {}
        self.deps = []
        self.done = False
        self.event_handlers = []
        self.extra_code = []
        self.init = []
        self.init_lines = {}
        self.layout = []
        self.parents_init = []
        self.props = []
        self.sizers_init = []

# end of class BaseClassLines


def _do_replace(match):
    """\
    Escape double backslashed in first RE match group
    """
    if match.group(0) == '\\':
        return '\\\\'
    else:
        return match.group(0)


def string_to_colour(s):
    """\
    Convert a colour values out of a hex string to comma separated
    decimal values.

    B{Example:}

        >>> string_to_colour('#FFFFFF')
        '255, 255, 255'
        >>> string_to_colour('#ABCDEF')
        '171, 205, 239'

    @rtype:  String
    """
    return '%d, %d, %d' % (int(s[1:3], 16), int(s[3:5], 16), int(s[5:], 16))


def test_attribute(obj):
    """\
    Returns True if 'obj' should be added as an attribute of its parent's
    class, False if it should be created as a local variable of __do_layout.
    To do so, tests for the presence of the special property 'attribute'

    @rtype: Boolean
    """
    try:
        return int(obj.properties['attribute'])
    except (KeyError, ValueError):
        return True  # this is the default


def create_nonce():
    """\
    Create a random number used to be sure that the replaced tags in the
    sources are the right ones (see SourceFileContent and add_class)

    @return: A random nonce
    @rtype:  String
    """
    nonce = '%s%s' % (str(time.time()).replace('.', ''),
                      random.randrange(10 ** 6, 10 ** 7))
    return nonce


def create_generated_by():
    """\
    Create I{generated by wxGlade} string without leading comment
    characters and without tailing new lines

    @rtype:  String
    """
    if config.preferences.write_timestamp:
        msg = 'generated by wxGlade %s on %s%s' % (
            common.version,
            time.asctime(),
            common.generated_from(),
            )
    else:
        msg = 'generated by wxGlade %s%s' % (
            common.version,
            common.generated_from(),
            )
    return msg
