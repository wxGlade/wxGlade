"""\
Code generator functions for wxButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonButtonGenerator(wcodegen.PythonWidgetCodeWriter):

    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        prop = obj.properties
        stockitem = prop.get('stockitem', None)
        if stockitem:
            self.tmpl_dict['id_number'] = self.codegen.cn("wxID_" + stockitem)
            self.tmpl_dict['id'] = self.tmpl_dict['id_number']
            self.tmpl_dict['label'] = self.codegen.quote_str('')

        self.has_setdefault = prop.get('default', False)

        return

# end of class PythonButtonGenerator


class CppButtonGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        prop = obj.properties
        stockitem = prop.get('stockitem', None)
        if stockitem:
            self.tmpl_dict['label'] = self.codegen.quote_str('')
            self.tmpl_dict['id_number'] = self.codegen.cn("wxID_" + stockitem)
            self.tmpl_dict['id'] = self.tmpl_dict['id_number']

        self.has_setdefault = prop.get('default', False)

        return

# end of class CppButtonGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ButtonXrcObject(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            stockitem = self.properties.get('stockitem', 'None')
            if stockitem != 'None':
                self.name = 'wxID_' + stockitem
                del self.properties['stockitem']
                try:
                    del self.properties['label']
                except KeyError:
                    pass
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs)

        def write_property(self, name, val, outfile, tabs):
            if name == 'label':
                # translate & into _ as accelerator marker
                val2 = val.replace('&', '_')
                if val.count('&&') > 0:
                    while True:
                        index = val.find('&&')
                        if index < 0:
                            break
                        val = val2[:index] + '&&' + val2[index+2:]
                else:
                    val = val2
            xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                   outfile, tabs)
    # end of class ButtonXrcObject

    return ButtonXrcObject(obj)


def initialize():
    common.class_names['EditButton'] = 'wxButton'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxButton', PythonButtonGenerator())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxButton', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxButton', CppButtonGenerator())
