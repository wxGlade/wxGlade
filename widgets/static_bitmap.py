# static_bitmap.py: wxStaticBitmap objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *
import os

_bmp_types = {
    '.bmp' : wxBITMAP_TYPE_BMP,
    '.gif' : wxBITMAP_TYPE_GIF,
    '.xpm' : wxBITMAP_TYPE_XPM,
    '.jpg' : wxBITMAP_TYPE_JPEG,
    '.jpeg': wxBITMAP_TYPE_JPEG,
    '.png' : wxBITMAP_TYPE_PNG,
    '.pcx' : wxBITMAP_TYPE_PCX
    }

_bmp_str_types = {
    '.bmp' : 'wxBITMAP_TYPE_BMP',
    '.gif' : 'wxBITMAP_TYPE_GIF',
    '.xpm' : 'wxBITMAP_TYPE_XPM',
    '.jpg' : 'wxBITMAP_TYPE_JPEG',
    '.jpeg': 'wxBITMAP_TYPE_JPEG',
    '.png' : 'wxBITMAP_TYPE_PNG',
    '.pcx' : 'wxBITMAP_TYPE_PCX'
    }

class EditStaticBitmap(ManagedBase):
    def __init__(self, name, parent, id, bmp_file, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxStaticBitmap objects
        """
        ManagedBase.__init__(self, name, 'wxStaticBitmap', parent, id, sizer,
                             pos, property_window, show=show)
        self.set_bitmap(str(bmp_file))
        # bitmap property
        self.access_functions['bitmap'] = (self.get_bitmap, self.set_bitmap)
        self.bitmap_prop = FileDialogProperty(self, 'bitmap', None, #panel,
                                              style=wxOPEN|wxFILE_MUST_EXIST,
                                              can_disable=False)
        self.properties['bitmap'] = self.bitmap_prop

    def create_widget(self):
        bmp = self.load_bitmap(self.guess_type(self.bitmap))
        self.widget = wxStaticBitmap(self.parent.widget, self.id, bmp)        

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['bitmap'].display(panel)
        szr.Add(self.properties['bitmap'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_bitmap(self):
        return self.bitmap

    def set_bitmap(self, value):
        type = self.guess_type(value)
        if type is None:
            self.bitmap = ''
        else:
            self.bitmap = value
        if self.widget:
            bmp = self.load_bitmap(type)
            self.widget.SetBitmapLabel(bmp)
            self.widget.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def load_bitmap(self, type):
        if self.bitmap:
            return wxBitmap(self.bitmap, type)
        else:
            return wxNullBitmap

    def guess_type(self, filename):
        return _bmp_types.get(os.path.splitext(str(filename))[1], None)

# end of class EditStaticBitmap
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticBitmap objects.
    """
    name = 'bitmap_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_%s' % number[0]
    bitmap = wxFileSelector("Select the image")
    static_bitmap = EditStaticBitmap(name, parent, wxNewId(), bitmap, sizer,
                                     pos, common.property_panel)
    node = Tree.Node(static_bitmap)
    static_bitmap.node = node
    static_bitmap.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStaticBitmap objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    bitmap = EditStaticBitmap(label, parent, wxNewId(), '', sizer, pos,
                              common.property_panel)
    sizer.set_item(bitmap.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border) #, size=bitmap.GetBestSize())
    node = Tree.Node(bitmap)
    bitmap.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return bitmap


def code_generator(obj):
    """\
    fuction that generates python code for wxBitmapButton objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj) 
    bmp_file = prop.get('bitmap', '')
    if not bmp_file: bmp = 'wxNullBitmap'
    else:
        type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
        if not type: bmp = 'wxNullBitmap'
        else:
            if os.sep == '\\': bmp_file = bmp_file.replace(os.sep, '/')
            bmp = 'wxBitmap("%s", %s)' % (bmp_file.replace('"', '\"'), type)
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, %s)\n' % (obj.name, obj.klass, id, bmp)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []    
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = [ 'self.%s = wxStaticBitmap(%s, %s, %s, size=%s)\n' % 
             (obj.name, parent, id, bmp, size) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditStaticBitmap'] = builder
    common.widgets_from_xml['EditStaticBitmap'] = xml_builder
    common.class_names['EditStaticBitmap'] = 'wxStaticBitmap'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxStaticBitmap', code_generator)
        
    return common.make_object_button('EditStaticBitmap',
                                     'icons/static_bitmap.xpm')
