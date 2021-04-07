"""\
Lisp generator functions for wxDialog objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispDialogGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        if obj.title:
            obj_name = self.codegen._format_name(obj.name)
            out.append( '(wxWindow_SetTitle (slot-%s self) %s)\n' % (obj_name, self.codegen.quote_str(obj.title)) )
        if obj.icon:
            out.append( ';;; generating code for setting icons is not implemented\n' )
        out.extend(self.codegen.generate_code_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = []
        obj_name = self.codegen._format_name(obj.name)

        # info about GetId required
        ## SetAffirmativeId and SetEscapeId, if defined
        #if obj.check_prop_truth("affirmative"):
            #buttons = obj.find_children(obj.affirmative, "wxButton")
            #fmt = "(wxWindow_SetAffirmativeId (slot-%s self) %s->GetId())\n"
            #if buttons: ret.append( fmt % (obj_name, self.format_widget_access(buttons[0])) )
        #if obj.check_prop_truth("escape"):
            #buttons = obj.find_children(obj.escape, "wxButton")
            #fmt = "(wxWindow_SetEscapeId (slot-%s self) %s->GetId())\n"
            #if buttons: ret.append( fmt % (obj_name, self.format_widget_access(buttons[0])) )
        #if ret: ret.append("\n")

        ret.append( '(wxWindow_layout (slot-%s self))\n' % obj_name )
        if obj.centered:
            ret.append( '(wxWindow_Centre (slot-%s self) wxBOTH)\n' % obj_name )
        return ret


def initialize():
    klass = 'wxDialog'
    common.class_names['EditDialog'] = klass
    common.register('lisp', klass, LispDialogGenerator(klass))
