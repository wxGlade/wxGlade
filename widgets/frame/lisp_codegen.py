"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispFrameCodeGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, obj):
        return [], [], [], []  # the frame can't be a children

    def get_properties_code(self, obj):
        out = []
        append = out.append
        if obj.title:
            append( '(wxFrame_SetTitle (slot-top-window obj) %s)\n' % self.codegen.quote_str(obj.title) )
        if obj.icon:
            append( ';;; generating code for setting icons is not implemented\n' )
        out.extend(self.codegen.generate_code_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = []
        if not obj.check_prop("size") and obj.children and obj.children[0].WX_CLASS=="wxPanel":
            panel = obj.children[0]
            if panel.children and panel.children[0].IS_SIZER:
                # add e.g. 'sizer.Fit(self)' if frame has no explicit size and the structure is frame->panel->sizer
                sizer = panel.children[0]
                tmpl = self.codegen.obj_builders[sizer.WX_CLASS].tmpl_Fit
                d = {"sizer_name":self.codegen._format_classattr(sizer),
                     "parent_widget":self.codegen.format_generic_access(obj)}
                ret.append( tmpl%d )
        ret.append( '(wxFrame_layout (slot-%s self))\n' % self.codegen._format_name(obj.name) )

        if obj.centered:
            ret.append('(wxFrame_Centre (slot-top-window obj) wxBOTH)\n')
        return ret


class LispMDIChildFrameCodeGenerator(LispFrameCodeGenerator):
    import_modules = ['Wx::MDI']


def initialize():
    klass = 'wxFrame'
    cn = common.class_names
    cn['EditFrame'] = klass
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'

    lispgen = common.code_writers.get('lisp')
    if lispgen:
        awh = lispgen.register_widget_code_generator
        awh('wxFrame', LispFrameCodeGenerator(klass))
        awh('wxMDIChildFrame', LispMDIChildFrameCodeGenerator(klass))
