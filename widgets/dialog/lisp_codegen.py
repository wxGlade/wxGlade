"""\
Lisp generator functions for wxDialog objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispDialogGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('(wxWindow_SetTitle (slot-%s self) %s)\n' % (
                obj.name, self.codegen.quote_str(title)))
        icon = obj.properties.get('icon')
        if icon:
            out.append(
                ';;; generating code for setting icons is not implemented\n'
                )
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['(wxWindow_layout (slot-%s self))\n' % obj.name]
        try:
            if int(obj.properties['centered']):
                ret.append('(wxWindow_Centre (slot-%s self) wxBOTH)\n' %
                           obj.name)
        except (KeyError, ValueError):
            pass
        return ret

# end of class LispDialogGenerator


def initialize():
    klass = 'wxDialog'
    common.class_names['EditDialog'] = klass
    common.toplevels['EditDialog'] = 1
    common.register('lisp', klass, LispDialogGenerator(klass))
