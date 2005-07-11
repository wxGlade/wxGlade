# dialog.py: wxDialog objects
# $Id: dialog.py,v 1.26 2005/07/11 12:12:46 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, math, misc
from tree import Tree
from widget_properties import *
from edit_windows import TopLevelBase

class EditDialog(TopLevelBase):
    def __init__(self, name, parent, id, title, property_window,
                 style=wxDEFAULT_DIALOG_STYLE, show=True, klass='wxDialog'):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show, title=title)
        self.base = 'wxDialog'
        
        self.style = style
        prop = self.properties
        # style property
        self.access_functions['style'] = (self.get_style, self.set_style)
        style_labels = ('#section#Style', 'wxDEFAULT_DIALOG_STYLE',
                        'wxDIALOG_MODAL', 'wxCAPTION',
                        'wxRESIZE_BORDER', 'wxSYSTEM_MENU')
        if misc.check_wx_version(2, 5):
            style_labels += ('wxCLOSE_BOX', 'wxMAXIMIZE_BOX', 'wxMINIMIZE_BOX')
        style_labels += ('wxTHICK_FRAME',
                         'wxSTAY_ON_TOP', 'wxNO_3D', 'wxDIALOG_NO_PARENT',
                         'wxNO_FULL_REPAINT_ON_RESIZE',
                         'wxFULL_REPAINT_ON_RESIZE',
                         'wxCLIP_CHILDREN')
        self.style_pos = (wxDEFAULT_DIALOG_STYLE,
                          wxDIALOG_MODAL, wxCAPTION, wxRESIZE_BORDER,
                          wxSYSTEM_MENU)
        if misc.check_wx_version(2, 5):
            self.style_pos += (wxCLOSE_BOX, wxMAXIMIZE_BOX, wxMINIMIZE_BOX)
        self.style_pos += (wxTHICK_FRAME, wxSTAY_ON_TOP, wxNO_3D,
                           wxDIALOG_NO_PARENT, wxNO_FULL_REPAINT_ON_RESIZE,
                           wxFULL_REPAINT_ON_RESIZE,
                           wxCLIP_CHILDREN)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        # icon property
        self.icon = ""
        self.access_functions['icon'] = (self.get_icon, self.set_icon)
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
        # we set always a default style because this is the best one for
        # editing the dialog (for example, a dialog without a caption would
        # be hard to move, etc.)
        default_style = wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER
        # change 2002-10-09: now we create a wxFrame instead of a wxDialog,
        # because the latter gives troubles I wasn't able to solve when using
        # wxPython 2.3.3.1 :-/
        self.widget = wxFrame(w, self.id, "", style=default_style)
        self.widget.SetBackgroundColour(wxSystemSettings_GetColour(
            wxSYS_COLOUR_BTNFACE))
        self.set_icon(self.icon)

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))    

    def create_properties(self):
        TopLevelBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1, style=wxTAB_TRAVERSAL)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['title'].display(panel)
        self.properties['icon'].display(panel)
        self.properties['centered'].display(panel)
        self.properties['style'].display(panel)
        szr.Add(self.properties['title'].panel, 0, wxEXPAND)
        szr.Add(self.properties['icon'].panel, 0, wxEXPAND)
        szr.Add(self.properties['centered'].panel, 0, wxEXPAND)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, int(math.ceil(w/5.0)), int(math.ceil(h/5.0)))

    def get_style(self):
        retval = [0] * len(self.style_pos)
        style = self.style
        try:
            default = 0
            if style & wxDEFAULT_DIALOG_STYLE == wxDEFAULT_DIALOG_STYLE:
                default = 1
                style = style & ~wxDEFAULT_DIALOG_STYLE
            for i in range(len(self.style_pos)):
                if style & self.style_pos[i]: retval[i] = 1
            retval[0] = default
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

    def get_icon(self):
        return self.icon 

    def set_icon(self, value):
        self.icon = value.strip()
        if self.widget:
            if self.icon and not (self.icon.startswith('var:') or
                                  self.icon.startswith('code:')):
                bmp = wxBitmap(self.icon, wxBITMAP_TYPE_ANY)
                if not bmp.Ok():
                    self.set_icon("")
                else:
                    icon = wxEmptyIcon()
                    icon.CopyFromBitmap(bmp)
                    self.widget.SetIcon(icon) 
            else:
                self.widget.SetIcon(wxNullIcon)

    def get_centered(self):
        return self.centered

    def set_centered(self, value):
        try: self.centered = bool(int(value))
        except ValueError: pass

# end of class EditDialog

        
def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditDialog objects.
    """
    try:
        import panel
        has_panel = True
    except ImportError:
        has_panel = False
        
    class Dialog(wxDialog):
        def __init__(self):
            if has_panel: title = 'Select widget type'
            else: title = 'Select dialog class'
            wxDialog.__init__(self, None, -1, title)
            if common.app_tree.app.get_language().lower() == 'xrc':
                self.klass = 'wxDialog'
            else:
                if not number[0]: self.klass = 'MyDialog'
                else: self.klass = 'MyDialog%s' % number[0]
                number[0] += 1
            self.klass_prop = TextProperty(self, 'class', None) #self)
            self.widget = 0
            szr = wxBoxSizer(wxVERTICAL)
            if has_panel:
                widget_prop = RadioProperty(self, 'widget', self,
                                            ['wxDialog', 'wxPanel'])
                szr.Add(widget_prop.panel, 0, wxALL|wxEXPAND, 5)
            self.klass_prop.display(self)
            szr.Add(self.klass_prop.panel, 0, wxALL|wxEXPAND, 5)
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
            if self.GetSize()[0] < 150: self.SetSize((150, -1))
            self.klass_modified = False

        def undo(self):
            if number[0] > 0:
                number[0] -= 1

        def set_klass(self, c):
            self.klass = c
            self.klass_modified = True
        
        def set_widget(self, c):
            self.widget = int(c)
            if not self.klass_modified:
                try: number = str(int(self.klass[-1]))
                except ValueError: number = ''
                if common.app_tree.app.get_language().lower() == 'xrc':
                    if self.widget == 0: self.klass = 'wxDialog'
                    else: self.klass = 'wxPanel'
                else:
                    if self.widget == 0: self.klass = 'MyDialog' + number
                    else: self.klass = 'MyPanel' + number                    
                self.klass_prop.set_value(self.klass)

        def __getitem__(self, value):
            if value == 'class':
                return (lambda : self.klass, self.set_klass)
            else:
                return (lambda : self.widget, self.set_widget)
    # end of inner class

    class_dialog = Dialog()
    # Check if the user hit Cancel, if so then bail out
    if class_dialog.ShowModal() == wxID_CANCEL:
        # restore state
        class_dialog.undo()
        # clean up resources
        class_dialog.Destroy()
        return
    if class_dialog.widget == 0: name = 'dialog'
    else: name = 'panel'
    label = '%s_%d' % (name, (number[0] or 1))
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (name, number[0])
    if class_dialog.widget == 0:
        is_panel = False
        dialog = EditDialog(label, parent, wxNewId(), label,
                            common.property_panel, klass=class_dialog.klass)
    else:
        is_panel = True
        import panel
        dialog = panel.EditTopLevelPanel(label, parent, wxNewId(),
                                         common.property_panel,
                                         klass=class_dialog.klass)
    node = Tree.Node(dialog)
    dialog.node = node
    dialog.show_widget(True)
    common.app_tree.add(node)
    class_dialog.Destroy()
    if wxPlatform == '__WXMSW__':
        if not is_panel: w = dialog.widget
        else: w = dialog.widget.GetParent()
        w.CenterOnScreen()
        w.Raise()

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditDialog objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    dialog = EditDialog(label, parent, wxNewId(), "", common.property_panel,
                        show=False)
    node = Tree.Node(dialog)
    dialog.node = node
    common.app_tree.add(node)
    return dialog

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditDialog'] = xml_builder

    common.widgets['EditDialog'] = builder
    
    return common.make_object_button('EditDialog', 'icons/dialog.xpm', 1,
                                     tip='Add a Dialog/Panel')
