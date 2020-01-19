"""\
Code generator functions for wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStatusBarGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = self.CreateStatusBar(%(labels_len)s%(style)s)\n'
    tmpl_flags = ', %s'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        fields = obj.fields # properties['statusbar']
        labels = [f[0] for f in fields]
        widths = [int(f[1]) for f in fields]
        self.tmpl_dict['labels'] = ', '.join([self.codegen.quote_str(lb) for lb in labels])
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = repr(widths)
        self.tmpl_dict['widths_len'] = len(widths)

        append = self.tmpl_props.append
        append( '%(name)s.SetStatusWidths(%(widths)s)\n' )

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            append( '%(comment)s statusbar fields\n' )
            append( '%(obj_name)s_fields = [%(labels)s]\n' )
            append( 'for i in range(len(%(obj_name)s_fields)):\n' )
            append( '%(tab)s%(name)s.SetStatusText(%(obj_name)s_fields[i], i)\n' )


def xrc_statusbar_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StatusbarXrcObject(xrcgen.DefaultXrcObject):
        def write(self, output, tabs):
            properties = {}
            fields = self.widget.fields # properties['statusbar']
            labels = [f[0] for f in fields]
            widths = [int(f[1]) for f in fields]

            properties['fields'] = str(len(fields))
            properties['widths'] = ', '.join([str(w) for w in widths])
            xrcgen.DefaultXrcObject.write(self, output, tabs, properties)

    return StatusbarXrcObject(obj)


class CppStatusBarGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = CreateStatusBar(%(labels_len)s%(style)s);\n'
    prefix_style = False
    tmpl_flags = ', %s'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        fields = obj.fields # properties['statusbar']
        labels = [f[0] for f in fields]
        widths = [int(f[1]) for f in fields]

        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ', '.join(map(str, widths))
        self.tmpl_dict['widths_len'] = len(widths)

        append = self.tmpl_props.append
        append( 'int %(name)s_widths[] = { %(widths)s };\n' )
        append( '%(name)s->SetStatusWidths(%(widths_len)s, %(name)s_widths);\n' )
        append( '\n' )

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            append( '%(comment)s statusbar fields\n' )
            append( 'const wxString %(name)s_fields[] = {\n' )
            for lb in labels:
                append( '%%(tab)s%s,\n' % self.codegen.quote_str(lb) )
            append( '};\n' )

            append( 'for(int i = 0; i < %(name)s->GetFieldsCount(); ++i) {\n' )
            append( '%(tab)s%(name)s->SetStatusText(%(name)s_fields[i], i);\n' )
            append( '}\n' )



def initialize():
    klass = 'wxStatusBar'
    cn = common.class_names
    cn['EditStatusBar'] = klass

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.register_widget_code_generator('wxStatusBar', PythonStatusBarGenerator(klass))

    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.register_widget_code_generator('wxStatusBar', xrc_statusbar_code_generator)

    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.register_widget_code_generator('wxStatusBar', CppStatusBarGenerator(klass))
