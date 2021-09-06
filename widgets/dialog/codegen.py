"""\
Code generator functions for wxDialog objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonDialogGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        if obj.title:
            out.append( 'self.SetTitle(%s)\n' % self.codegen.quote_str(obj.title) )
        if obj.icon:
            stmt_icon = self.generate_code_bitmap(obj.icon )
            out.append( '_icon = %s\n' % self.cn('wxNullIcon') )
            out.append( '_icon.CopyFromBitmap(%s)\n' % stmt_icon )
            out.append( 'self.SetIcon(_icon)\n' )
        out.extend(self.codegen.generate_code_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = []

        # SetAffirmativeId and SetEscapeId, if defined
        if obj.check_prop_truth("affirmative"):
            buttons = obj.find_children(obj.affirmative, "wxButton")
            if buttons: ret.append( "self.SetAffirmativeId(%s.GetId())\n" % self.format_widget_access(buttons[0]) )
        if obj.check_prop_truth("escape"):
            buttons = obj.find_children(obj.escape, "wxButton")
            if buttons: ret.append( "self.SetEscapeId(%s.GetId())\n" % self.format_widget_access(buttons[0]) )
        if ret: ret.append("\n")

        ret.append('self.Layout()\n')
        if "centered" in obj.properties and obj.centered:
            ret.append('self.Centre()\n')
        return ret


class CppDialogGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_DIALOG_STYLE')]

    def get_code(self, obj):
        return [], [], [], []

    def get_properties_code(self, obj):
        """generates the code for the various wxDialog specific properties.
        Returns a list of strings containing the generated code"""
        out = []
        if obj.title:
            out.append( 'SetTitle(%s);\n' % self.codegen.quote_str(obj.title) )
        if obj.icon:
            stmt_icon = self.generate_code_bitmap(obj.icon)
            out.append( 'wxIcon _icon;\n' )
            out.append( '_icon.CopyFromBitmap(%s);\n' % stmt_icon )
            out.append( 'SetIcon(_icon);\n' )
        out.extend( self.codegen.generate_code_common_properties(obj) )
        return out

    def get_layout_code(self, obj):
        ret = []

        # SetAffirmativeId and SetEscapeId, if defined
        if obj.check_prop_truth("affirmative"):
            buttons = obj.find_children(obj.affirmative, "wxButton")
            if buttons: ret.append( "SetAffirmativeId(%s->GetId());\n" % self.format_widget_access(buttons[0]) )
        if obj.check_prop_truth("escape"):
            buttons = obj.find_children(obj.escape, "wxButton")
            if buttons: ret.append( "SetEscapeId(%s->GetId());\n" % self.format_widget_access(buttons[0]) )
        if ret: ret.append("\n")

        ret.append( 'Layout();\n' )
        if "centered" in obj.properties and obj.centered:
            ret.append('Centre();\n')
        return ret


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class DialogXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, ntabs):
            if name != 'sizehints':
                xrcgen.DefaultXrcObject.write_property(self, name, val, outfile, ntabs)
    # end of class DialogXrcObject

    return DialogXrcObject(obj)


def initialize():
    klass = 'wxDialog'
    common.class_names['EditDialog'] = klass
    common.register('python', klass, PythonDialogGenerator(klass))
    common.register('C++', klass, CppDialogGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
