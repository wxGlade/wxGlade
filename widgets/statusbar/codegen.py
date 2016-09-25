"""\
Code generator functions for wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from wcodegen.taghandler import BaseCodeWriterTagHandler


class PythonStatusBarGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = self.CreateStatusBar(%(labels_len)s%(style)s)\n'
    tmpl_flags = ', %s'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels'] = ', '.join([self.codegen.quote_str(lb) for lb in labels])
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = repr(widths)
        self.tmpl_dict['widths_len'] = len(widths)
        append = self.tmpl_props.append

        append('%(name)s.SetStatusWidths(%(widths)s)\n')
        append('\n')

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            append('%(comment)s statusbar fields\n')
            append('%(obj_name)s_fields = [%(labels)s]\n')
            append('for i in range(len(%(obj_name)s_fields)):\n')
            append('%(tab)s%(name)s.SetStatusText(%(obj_name)s_fields[i], i)\n')



# property handlers for code generation
class StatusFieldsHandler(BaseCodeWriterTagHandler):
    "Handler for statusbar fields"

    def __init__(self):
        super(StatusFieldsHandler, self).__init__()
        self.labels = []
        self.widths = []

    def start_elem(self, name, attrs):
        if name == 'field':
            self.widths.append(int(attrs.get('width', -1)))

    def end_elem(self, name, code_obj):
        if name == 'fields':
            code_obj.properties['statusbar'] = (self.labels, self.widths)
            return True
        char_data = self.get_char_data()
        self.labels.append(char_data)



def xrc_statusbar_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StatusbarXrcObject(xrcgen.DefaultXrcObject):
        def write(self, outfile, tabs):
            if 'statusbar' in self.properties:
                fields, widths = self.properties['statusbar']
                self.properties['fields'] = str(len(fields))
                self.properties['widths'] = ', '.join([str(w) for w in widths])
                del self.properties['statusbar']
            xrcgen.DefaultXrcObject.write(self, outfile, tabs)

    return StatusbarXrcObject(obj)


class CppStatusBarGenerator(wcodegen.CppWidgetCodeWriter):

    tmpl = '%(name)s = CreateStatusBar(%(labels_len)s%(style)s);\n'
    prefix_style = False
    tmpl_flags = ', %s'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ', '.join(map(str, widths))
        self.tmpl_dict['widths_len'] = len(widths)
        append = self.tmpl_props.append

        append('int %(name)s_widths[] = { %(widths)s };\n')
        append('%(name)s->SetStatusWidths(%(widths_len)s, %(name)s_widths);\n')
        append('\n')

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            append('%(comment)s statusbar fields\n')
            append('const wxString %(name)s_fields[] = {\n')
            for lb in labels:
                append('%%(tab)s%s,\n' % self.codegen.quote_str(lb))
            append('};\n')

            append('for(int i = 0; i < %(name)s->GetFieldsCount(); ++i) {\n')
            append('%(tab)s%(name)s->SetStatusText(%(name)s_fields[i], i);\n')
            append('}\n')



def initialize():
    klass = 'wxStatusBar'
    cn = common.class_names
    cn['EditStatusBar'] = klass
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxStatusBar', PythonStatusBarGenerator(klass))

        aph = pygen.add_property_handler
        aph('statusbar', pygen.DummyPropertyHandler)
        aph('fields', StatusFieldsHandler)

    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxStatusBar', xrc_statusbar_code_generator)
        xrcgen.add_property_handler('fields', StatusFieldsHandler)

    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxStatusBar', CppStatusBarGenerator(klass))

        aph = cppgen.add_property_handler
        aph('fields', StatusFieldsHandler)
        aph('statusbar', cppgen.DummyPropertyHandler)
