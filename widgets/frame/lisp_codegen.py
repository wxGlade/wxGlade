"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *
from codegen import StatusFieldsHandler


class LispStatusBarCodeGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (wxFrame_CreateStatusBar ' \
           '(slot-top-window obj) %(labels_len)s %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ' '.join(map(str, widths))
        self.tmpl_dict['widths_len'] = len(widths)

        self.tmpl_props.append(
            '(wxStatusBar_SetStatusWidths %(name)s %(widths_len)s '
            '(vector %(widths)s))\n'
        )

        i = 0
        for lb in labels:
            stmt = '(wxStatusBar_SetStatusText %%(name)s %s %d)\n' % (
                self.codegen.quote_str(lb), i)
            self.tmpl_props.append(stmt)
            i = +1

# end of class LispStatusBarCodeGenerator


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
        if obj.properties.get('size', '').strip() and \
           self.codegen.for_version < (2, 8):
            ret.append(self.codegen.generate_code_size(obj))
        return ret

# end of class LispFrameCodeGenerator


class LispMDIChildFrameCodeGenerator(LispFrameCodeGenerator):
    extra_headers = ['Wx::MDI']

# end of class LispMDIChildFrameCodeGenerator


def initialize():
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    lispgen = common.code_writers.get('lisp')
    if lispgen:
        awh = lispgen.add_widget_handler
        awh('wxFrame', LispFrameCodeGenerator())
        awh('wxMDIChildFrame', LispMDIChildFrameCodeGenerator())
        awh('wxStatusBar', LispStatusBarCodeGenerator())

        aph = lispgen.add_property_handler
        aph('fields', StatusFieldsHandler)
        aph('menubar', lispgen.DummyPropertyHandler)
        aph('statusbar', lispgen.DummyPropertyHandler)
