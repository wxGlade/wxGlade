# frame.py: wxFrame and wxStatusBar objects
# $Id: frame.py,v 1.33 2004/09/30 20:35:34 agriggio Exp $
#
# Copyright (c) 2002-2004 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, math, misc
from tree import Tree
#from MenuTree import *
from widget_properties import *
from edit_windows import EditBase, TopLevelBase

class EditStatusBar(EditBase):
    def __init__(self, parent, property_window):
        EditBase.__init__(self, parent.name + '_statusbar',
                          'wxStatusBar', parent, id, property_window,
                          custom_class=False, show=False)
        # style property
        self.style_pos  = (wxST_SIZEGRIP,)
        style_labels = ('#section#Style', 'wxST_SIZEGRIP')
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)

        self.node = Tree.Node(self)
        common.app_tree.add(self.node, parent.node)

        self.fields = [ [self.name, "-1"] ] # list of 2-lists label, size
                                            # for the statusbar fields
        self.access_functions['fields'] = (self.get_fields, self.set_fields) 
        prop = self.properties['fields'] = GridProperty(
            self, 'fields', None,
            [("Text", GridProperty.STRING), ("Size", GridProperty.INT)])
        # replace the default 'write' method of 'prop' with a custom one
        def write_prop(outfile, tabs):
            from xml.sax.saxutils import escape, quoteattr
            fwrite = outfile.write
            fwrite('    ' * tabs + '<fields>\n')
            tabs += 1
            import widget_properties
            for label, width in self.fields:
                fwrite('    ' * tabs + '<field width=%s>%s</field>\n' %
                       (quoteattr(width),
                        escape(widget_properties._encode(label))))
            tabs -= 1
            fwrite('    ' * tabs + '</fields>\n')
        prop.write = write_prop

    def create_widget(self):
        self.widget = wxStatusBar(self.parent.widget, wxNewId())
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        self.set_fields(self.fields)
        if self.parent.widget: self.parent.widget.SetStatusBar(self.widget)

    def create_properties(self):
        EditBase.create_properties(self)
        page = self._common_panel 
        self.properties['style'].display(page)
        prop = self.properties['fields']
        prop.display(page)
        sizer = page.GetSizer()
        if not sizer:
            sizer = wxBoxSizer(wxVERTICAL)
            sizer.Add(self.name_prop.panel, 0, wxEXPAND)
            sizer.Add(self.klass_prop.panel, 0, wxEXPAND)
            page.SetAutoLayout(1)
            page.SetSizer(sizer)
        sizer.Add(self.properties['style'].panel, 0, wxEXPAND)
        sizer.Add(prop.panel, 1, wxALL|wxEXPAND, 3)
        sizer.Fit(page)
        page.SetSize(self.notebook.GetClientSize())
        sizer.Layout()
        self.notebook.AddPage(page, "Common")
        self.property_window.Layout()
        prop.set_col_sizes([190, 0])

    def set_fields(self, values):
        # values is a list of lists
        self.fields = []
        if self.widget: self.widget.SetFieldsCount(len(values))
        for i in range(len(values)):
            try: v = int(values[i][1])
            except: v = 0
            s = misc.wxstr(values[i][0])
            self.fields.append([s, str(v)])
            if self.widget: self.widget.SetStatusText(s, i)
        if self.widget:
            self.widget.SetStatusWidths([int(i[1]) for i in self.fields])

    def get_fields(self):
        return self.fields
    
    def __getitem__(self, key):
        return self.access_functions[key]

    def remove(self, *args):
        if self.parent.widget: self.parent.widget.SetStatusBar(None)
        try: self.parent.properties['statusbar'].set_value(0)
        except KeyError: pass
        if self.widget: self.widget.Hide()
        EditBase.remove(self)

    def popup_menu(self, *args): pass # to avoid strange segfault :)

    def get_property_handler(self, name):
        class FieldsHandler:
            """\
            custom Property handler for statusbar fields.
            """
            def __init__(self, owner):
                self.owner = owner
                self.width = -1
                self.value = []
            def start_elem(self, name, attrs):
                if name == 'fields': self.fields = []
                else: # name == 'field'
                    self.value = []
                    self.width = attrs.get('width', '-1')
            def end_elem(self, name): 
                if name == 'field':
                    self.fields.append(["".join(self.value), self.width])
                else: # name == 'fields'
                    self.owner.fields = self.fields
                    self.owner.set_fields(self.owner.fields)
                    self.owner.properties['fields'].set_value(
                        self.owner.fields)
                    return True
            def char_data(self, data):
                self.value.append(data)
                return False # tell there's no need to go further
                             # (i.e. to call add_property)

        if name == 'fields': return FieldsHandler(self)
        return None

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError: pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]

# end of class EditStatusBar


class EditFrame(TopLevelBase):
    def __init__(self, name, parent, id, title, property_window,
                 style=wxDEFAULT_FRAME_STYLE, show=True, klass='wxFrame'):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show)
        self.style = style
        self.statusbar = None
        self.icon = ''
        self.access_functions['statusbar'] = (self.get_statusbar,
                                              self.set_statusbar)
        self.menubar = None
        self.access_functions['menubar'] = (self.get_menubar, self.set_menubar)
        self.toolbar = None
        self.access_functions['toolbar'] = (self.get_toolbar, self.set_toolbar)

        self.access_functions['style'] = (self.get_style, self.set_style)

        self.access_functions['icon'] = (self.get_icon, self.set_icon)
        prop = self.properties
        style_labels = ['#section#Style', 'wxDEFAULT_FRAME_STYLE',
                        'wxICONIZE', 'wxCAPTION',
                        'wxMINIMIZE', 'wxMINIMIZE_BOX', 'wxMAXIMIZE',
                        'wxMAXIMIZE_BOX', 'wxSTAY_ON_TOP', 'wxSYSTEM_MENU',
                        'wxSIMPLE_BORDER', 'wxRESIZE_BORDER',
                        'wxFRAME_TOOL_WINDOW', 'wxFRAME_NO_TASKBAR',
                        'wxNO_BORDER',
                        'wxNO_FULL_REPAINT_ON_RESIZE']
        self.style_pos = [wxDEFAULT_FRAME_STYLE,
                          wxICONIZE, wxCAPTION, wxMINIMIZE,
                          wxMINIMIZE_BOX, wxMAXIMIZE, wxMAXIMIZE_BOX,
                          wxSTAY_ON_TOP, wxSYSTEM_MENU, wxSIMPLE_BORDER,
                          wxRESIZE_BORDER, wxFRAME_TOOL_WINDOW,
                          wxFRAME_NO_TASKBAR, wxNO_BORDER,
                          wxNO_FULL_REPAINT_ON_RESIZE]
        if misc.check_wx_version(2, 5):
            style_labels.insert(5, 'wxCLOSE_BOX')
            self.style_pos.insert(4, wxCLOSE_BOX)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        # menubar property
        prop['menubar'] = CheckBoxProperty(self, 'menubar', None,
                                           'Has MenuBar')
        # statusbar property
        prop['statusbar'] = CheckBoxProperty(self, 'statusbar', None,
                                             'Has StatusBar')
        # toolbar property
        prop['toolbar'] = CheckBoxProperty(self, 'toolbar', None,
                                           'Has ToolBar')
        # icon property
        prop['icon'] = FileDialogProperty(self, 'icon', None,
                                          style=wxOPEN|wxFILE_MUST_EXIST,
                                          can_disable=True)
        # centered property
        self.centered = False
        self.access_functions['centered'] = (self.get_centered,
                                             self.set_centered)
        prop['centered'] = CheckBoxProperty(self, 'centered', None)

    def create_widget(self):
        if self.parent: w = self.parent.widget
        else: w = common.palette
        self.widget = wxFrame(w, self.id, self.get_title())
        self.set_icon(self.icon)

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            #if self.sizer: self.sizer.fit_parent()
            #else:
            self.widget.SetSize((400, 300))
        if wxPlatform == '__WXMSW__':
            self.widget.CenterOnScreen()
        if self.menubar and self.menubar.widget:
            self.widget.SetMenuBar(self.menubar.widget)
        if self.statusbar and self.statusbar.widget:
            self.widget.SetStatusBar(self.statusbar.widget)
        if self.toolbar and self.toolbar.widget:
            self.widget.SetToolBar(self.toolbar.widget)

    def create_properties(self):
        TopLevelBase.create_properties(self)
        prop = self.properties
        panel = wxScrolledWindow(self.notebook, -1, style=wxTAB_TRAVERSAL)
        prop['title'].display(panel)
        prop['icon'].display(panel)
        prop['centered'].display(panel)
        prop['menubar'].display(panel)
        prop['toolbar'].display(panel)
        try:
            sbprop = prop['statusbar']
            sbprop.display(panel)
        except KeyError:
            sbprop = None
        prop['style'].display(panel)
        
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(prop['title'].panel, 0, wxEXPAND)
        szr.Add(prop['icon'].panel, 0, wxEXPAND)
        szr.Add(prop['centered'].panel, 0, wxEXPAND)
        szr.Add(prop['menubar'].panel, 0, wxEXPAND)
        szr.Add(prop['toolbar'].panel, 0, wxEXPAND)
        if sbprop:
            szr.Add(sbprop.panel, 0, wxEXPAND)
        szr.Add(prop['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, int(math.ceil(w/5.0)), int(math.ceil(h/5.0)))

    def get_menubar(self):
        return self.menubar is not None

    def set_menubar(self, value):
        if value:
            from menubar import EditMenuBar
            self.menubar = EditMenuBar(self.name + '_menubar', 'wxMenuBar',
                                       self, common.property_panel)
            self.menubar.node = Tree.Node(self.menubar)
            common.app_tree.add(self.menubar.node, self.node)
            
            if self.widget:
                self.menubar.show_widget(True)
                self.menubar.show_properties()
        else:
            self.menubar = self.menubar.remove()
            self.show_properties(None)

    def get_statusbar(self):
        return self.statusbar is not None

    def set_statusbar(self, value):
        if value:
            self.statusbar = EditStatusBar(self, common.property_panel)
            if self.widget:
                self.statusbar.show_widget(True)
                self.statusbar.show_properties()
        else:
            self.statusbar = self.statusbar.remove()
            self.show_properties(None)
        if self.widget:
            # this is needed at least on win32
            wxPostEvent(self.widget, wxSizeEvent(self.widget.GetSize(),
                                                 self.widget.GetId()))
        
    def get_toolbar(self):
        return self.toolbar is not None

    def set_toolbar(self, value):
        if value:
            from toolbar import EditToolBar
            self.toolbar = EditToolBar(self.name + '_toolbar', 'wxToolBar',
                                       self, common.property_panel)
            self.toolbar.node = Tree.Node(self.toolbar)
            common.app_tree.add(self.toolbar.node, self.node)
            
            if self.widget:
                self.toolbar.show_widget(True)
                self.toolbar.show_properties()
        else:
            self.toolbar = self.toolbar.remove()
            self.show_properties(None)

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            if self.style == wxDEFAULT_FRAME_STYLE: retval[0] = 1
            else:
                for i in range(len(self.style_pos)):
                    if self.style & self.style_pos[i]: retval[i] = 1
                retval[0] = 0
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        style = 0
        for v in range(len(value)):
            if value[v]:
                style |= self.style_pos[v]
        self.style = style
        if self.widget: self.widget.SetWindowStyleFlag(style)

    def remove(self, *args):
        if self.menubar:
            self.menubar = self.menubar.remove(gtk_do_nothing=True)
        if self.statusbar:
            self.statusbar = self.statusbar.remove()
        if self.toolbar:
            self.toolbar = self.toolbar.remove(do_nothing=True)
        TopLevelBase.remove(self, *args)
    
    def get_icon(self):
        # is a string that holds the filename (for example: icon.png)
        return self.icon 

    def set_icon(self, value):
        self.icon = value.strip()
        if self.widget:
            if self.icon and not (self.icon.startswith('var:') or
                                  self.icon.startswith('code:')):
                # setting icon
                bmp = wxBitmap(self.icon, wxBITMAP_TYPE_ANY)
                if not bmp.Ok():
                    self.set_icon("")
                else:
                    icon = wxEmptyIcon()
                    icon.CopyFromBitmap(bmp)
                    self.widget.SetIcon(icon) 
            else:
                # removing icon
                self.widget.SetIcon(wxNullIcon)

    def get_centered(self):
        return self.centered

    def set_centered(self, value):
        try: self.centered = bool(int(value))
        except ValueError: pass

# end of class EditFrame


class EditMDIChildFrame(EditFrame):
    _is_toplevel = False # used to avoid to appear in the "Top Window" property
                         # of the app

    def __init__(self, *args, **kwds):
        EditFrame.__init__(self, *args, **kwds)
        del self.properties['statusbar']

# end of class EditMDIChildFrame

        
def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditFrame objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select frame class')
            if not number[0]: self.klass = 'MyFrame'
            else: self.klass = 'MyFrame%s' % number[0]
            number[0] += 1
            self.base = 0
            base_prop = RadioProperty(self, 'base class', self,
                                      ['wxFrame', 'wxMDIChildFrame'])
            klass_prop = TextProperty(self, 'class', self)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(base_prop.panel, 0, wxALL|wxEXPAND, 5)
            szr.Add(klass_prop.panel, 0, wxEXPAND)
            btnbox = wxBoxSizer(wxHORIZONTAL)
            btnOK = wxButton(self, wxID_OK, 'OK')
            btnCANCEL = wxButton(self, wxID_CANCEL, 'Cancel')
            btnbox.Add(btnOK, 0, wxALL, 3)
            btnbox.Add(btnCANCEL, 0, wxALL, 3)
            btnOK.SetFocus()
            szr.Add(btnbox, 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
            
        def undo(self):
            number[0] -= 1
            
        def __getitem__(self, value):
            if value == 'class':
                def set_klass(c): self.klass = c
                return (lambda : self.klass, set_klass)
            else:
                def set_base(b): self.base = b
                return (lambda : self.base, set_base)
    # end of inner class

    dialog = Dialog()
    # Check if the user hit Cancel, if so then bail out
    if dialog.ShowModal() == wxID_CANCEL:
        # restore state
        dialog.undo()
        # clean up resources
        dialog.Destroy()
        return
    label = 'frame_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'frame_%d' % number[0]
    if dialog.base == 0: base_class = EditFrame
    else: base_class = EditMDIChildFrame
    frame = base_class(label, parent, wxNewId(), label, common.property_panel,
                       klass=dialog.klass)
    node = Tree.Node(frame)
    frame.node = node
    common.app_tree.add(node)
    frame.show_widget(True)

    # add a default vertical sizer to the frame
    import edit_sizers
    edit_sizers._builder(frame, None, 0)
    # now select the frame's node in the tree
    common.app_tree.select_item(node)
    
    dialog.Destroy()
    if wxPlatform == '__WXMSW__':
        #frame.widget.CenterOnScreen()
        frame.widget.Raise()


def _make_builder(base_class):
    def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
        from xml_parse import XmlParsingError
        try: label = attrs['name']
        except KeyError: raise XmlParsingError, "'name' attribute missing"
        frame = base_class(label, parent, wxNewId(), label,
                           common.property_panel,
                           show=False)
        node = Tree.Node(frame)
        frame.node = node
        common.app_tree.add(node)
        return frame
    return xml_builder
        
## def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
##     """\
##     factory to build EditFrame objects from an xml file
##     """
##     from xml_parse import XmlParsingError
##     try: label = attrs['name']
##     except KeyError: raise XmlParsingError, "'name' attribute missing"
##     frame = EditFrame(label, parent, wxNewId(), label, common.property_panel,
##                       show=False)
##     node = Tree.Node(frame)
##     frame.node = node
##     common.app_tree.add(node)
##     return frame

def statusbar_xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStatusBar objects from an xml file
    """
    parent.statusbar.set_fields([])
    name = attrs.get('name')
    if name:
        parent.statusbar.set_name(name)
        parent.statusbar.name_prop.set_value(name)
    return parent.statusbar


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditStatusBar'] = statusbar_xml_builder
    cwx['EditFrame'] = _make_builder(EditFrame)
    cwx['EditMDIChildFrame'] = _make_builder(EditMDIChildFrame)

    common.widgets['EditFrame'] = builder
    
    # add statusbar icon to WidgetTree
    from tree import WidgetTree
    import os.path
    WidgetTree.images['EditStatusBar'] = os.path.join(common.wxglade_path,
                                                      'icons/statusbar.xpm')
    WidgetTree.images['EditMDIChildFrame'] = os.path.join(common.wxglade_path,
                                                          'icons/frame.xpm')
       
    return common.make_object_button('EditFrame', 'icons/frame.xpm', 1)
