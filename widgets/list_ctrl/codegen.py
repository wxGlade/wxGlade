"""\
Code generator functions for wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen

class ListCtrlPropertyGeneratorMixin(object):
    tmpl_append_row = None
    def get_more_properties_code(self, obj):
        # only for report view: add Columns; for preview: add Rows
        styles = obj.properties["style"].value_set
        if not "wxLC_REPORT" in styles or "wxLC_VIRTUAL" in styles: return []

        out = []

        name = self.format_widget_access(obj)

        rows_number = obj.rows_number
        cols_p = obj.properties["columns"]
        columns = cols_p.value

        tmpl_append_column, tmpl_append_row = self._get_row_col_templates()

        for i, (heading,width) in enumerate(columns):
            values = {"name":name, "heading":self.codegen.quote_str(heading), "width":width, "col":i}
            out.append( tmpl_append_column % values )

        if self.codegen.preview:
            for r in range(rows_number):
                out.append( tmpl_append_row % (name, r) )

        return out


class PythonListCtrlGenerator(ListCtrlPropertyGeneratorMixin, wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

    def _get_row_col_templates(self):
        # templates for adding columns and rows (rows are for preview only)
        if self.codegen.for_version >= (3,0) and compat.IS_PHOENIX:
            tmpl_append_column = '%(name)s.AppendColumn(%(heading)s, format=wx.LIST_FORMAT_LEFT, width=%(width)d)\n'
            tmpl_append_row = '%s.InsertItem(%d, "")\n'
        else:
            tmpl_append_column ='%(name)s.InsertColumn(%(col)d, %(heading)s, format=wx.LIST_FORMAT_LEFT, width=%(width)d)\n'
            tmpl_append_row = '%s.InsertStringItem(%d, "")\n'
        return (tmpl_append_column, tmpl_append_row)


class CppListCtrlGenerator(ListCtrlPropertyGeneratorMixin, wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/listctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'
    def _get_row_col_templates(self):
        tmpl_append_column = '%(name)s->AppendColumn(%(heading)s, wxLIST_FORMAT_LEFT, %(width)d);\n'
        return (tmpl_append_column, None)


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ListXrcObject(xrcgen.DefaultXrcObject):
        unsupported = set(['columns', 'rows_number'])

        def write_property(self, name, val, output, tabs):
            if name not in self.unsupported:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
    return ListXrcObject(obj)


def initialize():
    klass = 'wxListCtrl'
    common.class_names['EditListCtrl'] = klass
    common.register('python', klass, PythonListCtrlGenerator(klass))
    common.register('C++',    klass, CppListCtrlGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
