# common.py: global variables
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

import os

# if False, the program is invoked from the command-line in batch mode (for
# code generation only)
use_gui = True

# version identification string
version = '0.1.3'

# widgets dictionary: each key is the name of some EditWidget class; the mapped
# value is a 'factory' function which actually builds the object. Each of these
# functions accept 3 parameters: the parent of the widget, the sizer by which
# such widget is controlled, and the position inside this sizer.
widgets = {}

# widgets_from_xml dictionary: table of factory functions to build objects from
# an xml file
widgets_from_xml = {}

# property_panel wxPanel: container inside which Properties of the current
# focused widget are displayed
property_panel = None

# app_tree Tree: represents the widget hierarchy of the application; the root 
# is the application itself 
app_tree = None

# if True, the user is adding a widget to some sizer
adding_widget = False
# needed to add toplevel sizers
adding_sizer = False

# reference to the widget that is being added: this is a key in the
# 'widgets' dictionary
widget_to_add = None

# reference to the main window (the one which contains the various buttons to
# add the different widgets)
palette = None

# dictionary which maps the ids used in the event handlers to the
# corresponding widgets: used to call the appropriate builder function
# when a dropping of a widget occurs, knowing only the id of the event
refs = {}

# dictionary which maps the name of the classes used by wxGlade to the
# correspondent classes of wxWindows
class_names = {}

"""
dictionary of objects used to generate the code in a given language.
NOTE: a code writer object must implement this interface:
  - initialize(out_path, multi_files)
  - language
  - add_widget_handler(widget_name, handler[, properties_handler])
  - add_property_handler(property_name, handler[, widget_name])
  - add_object(top_obj, sub_obj)
  - add_class(obj)
  - add_sizeritem(toplevel, sizer, obj_name, option, flag, border)
  - add_app(app_attrs, top_win_class)
  - ...
"""
code_writers = {}


def load_code_writers():
    """\
    Fills the common.code_writers dictionary: to do so, loads the modules
    found in the 'codegen/' subdir
    """
    import sys
    sys.path.append('codegen')
    for module in os.listdir('codegen'):
        name, ext = os.path.splitext(module)
        if name not in sys.modules and \
               os.path.isfile(os.path.join('codegen', module)):
            try: writer = __import__(name).writer
            except (ImportError, AttributeError, ValueError):
                if use_gui:
                    print '"%s" is not a valid code generator module' % module
            else:
                code_writers[writer.language] = writer
                if use_gui:
                    print 'loaded code generator for %s' % writer.language

def load_widgets():
    """\
    Scans the 'widgets/' directory to find the installed widgets,
    and returns a list of buttons to handle them
    """
    buttons = []
    modules = open('widgets/widgets.txt')
    if use_gui: print 'loading widget modules:'
    for line in modules:
        module = line.strip()
        if not module or module.startswith('#'): continue
        module = module.split('#')[0].strip()
        try:
            b = __import__(module).initialize()
        except (ImportError, AttributeError):
            if use_gui:
                print 'ERROR loading "%s"' % module
                import traceback; traceback.print_exc()
        else:
            if use_gui: print '\t' + module
            buttons.append(b)
    modules.close()
    return buttons

def load_sizers():
    import edit_sizers
    return edit_sizers.init_all()


# function used by the various widget modules to add a button to the widgets
# toolbar
def make_object_button(widget, icon_path, toplevel=False, tip=None):
    """\
    creates a button for the widgets toolbar.
    Params:
      - widget: (name of) the widget the button will add to the app
      - icon_path: path to the icon used for the button
      - toplevel: true if the widget is a toplevel object (frame, dialog)
      - tip: tool tip to display
    Returns:
      the newly created wxBitmapButton
    """
    from wxPython import wx
    from tree import WidgetTree
    id = wx.wxNewId()
    tmp = wx.wxBitmapButton(palette, id, wx.wxBitmap(icon_path,
                                                     wx.wxBITMAP_TYPE_XPM),
                            size=(31, 31))
    if not toplevel:
        wx.EVT_BUTTON(palette, id, palette.add_object)
    else:
        wx.EVT_BUTTON(palette, id, palette.add_toplevel_object)
    refs[id] = widget
    if not tip:
        tip = 'Add a %s' % widget.replace('Edit', '')
    tmp.SetToolTip(wx.wxToolTip(tip))

    WidgetTree.images[widget] = icon_path

    return tmp


def _encode_from_xml(label, encoding='latin-1'):
    """\
    Returns a str which is the encoded version of the unicode label
    """
    return label.encode(encoding, 'replace')

def _encode_to_xml(label, encoding='latin-1'):
    """\
    returns a utf-8 encoded representation of label. This is equivalent to:
    str(label).decode(encoding).encode('utf-8')
    """
    return str(label).decode(encoding).encode('utf-8')
