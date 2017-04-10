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
        title = obj.properties.get('title')
        if title:
            append('(wxFrame_SetTitle (slot-top-window obj) %s)\n' %
                   self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            append(
                ';;; generating code for setting icons is not implemented\n'
                )
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['(wxFrame_layout (slot-%s self))\n' % obj.name]
        try:
            if int(obj.properties['centered']):
                ret.append('(wxFrame_Centre (slot-top-window obj) wxBOTH)\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class LispFrameCodeGenerator


class LispMDIChildFrameCodeGenerator(LispFrameCodeGenerator):
    import_modules = ['Wx::MDI']

# end of class LispMDIChildFrameCodeGenerator


def initialize():
    klass = 'wxFrame'
    cn = common.class_names
    cn['EditFrame'] = klass
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    lispgen = common.code_writers.get('lisp')
    if lispgen:
        awh = lispgen.add_widget_handler
        awh('wxFrame', LispFrameCodeGenerator(klass))
        awh('wxMDIChildFrame', LispMDIChildFrameCodeGenerator(klass))

        aph = lispgen.add_property_handler
        aph('menubar', lispgen.DummyPropertyHandler)
