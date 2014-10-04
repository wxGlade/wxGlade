"""\
Code generator functions for wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
import config
import os


class PythonStaticBitmapGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(bitmap)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        bmp_file = obj.properties.get('bitmap', '')
        bmp_preview_path = os.path.join(config.icons_path, "icon.xpm")
        if not bmp_file:
            stmt = self.codegen.cn('wxNullBitmap')
        elif bmp_file.startswith('var:') or bmp_file.startswith('code:'):
            if obj.preview:
                stmt = "%s('%s',%s)" % (
                    self.codegen.cn('wxBitmap'),
                    bmp_preview_path,
                    self.codegen.cn('wxBITMAP_TYPE_XPM')
                )
            elif bmp_file.startswith('var:'):
                stmt = '%s(%s, %s)' % (
                    self.codegen.cn('wxBitmap'),
                    bmp_file[4:].strip(),
                    self.codegen.cn('wxBITMAP_TYPE_ANY'),
                )
            else:
                stmt = '(%s)' % self.codegen.cn(bmp_file[5:].strip())
        else:
            if obj.preview:
                import misc
                bmp_file = misc.get_relative_path(bmp_file, True)
            stmt = '%s(%s, %s)' % (
                self.codegen.cn('wxBitmap'),
                self.codegen.quote_path(bmp_file),
                self.codegen.cn('wxBITMAP_TYPE_ANY'),
            )

        self.tmpl_dict['bitmap'] = stmt

        return

# end of class PythonStaticBitmapGenerator


class CppStaticBitmapGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(bitmap)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        bmp_file = obj.properties.get('bitmap', '')
        if not bmp_file:
            stmt = self.codegen.cn('wxNullBitmap')
        elif bmp_file.startswith('var:'):
            stmt = 'wxBitmap(%s, wxBITMAP_TYPE_ANY)' % bmp_file[4:].strip()
        elif bmp_file.startswith('code:'):
            stmt = '(%s)' % bmp_file[5:].strip()
        else:
            stmt = 'wxBitmap(wxT(%s), wxBITMAP_TYPE_ANY)' % \
                   self.codegen.quote_path(bmp_file)

        self.tmpl_dict['bitmap'] = stmt

        return

# end of class CppStaticBitmapGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try:
                del self.properties['attribute']
            except KeyError:
                pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxStaticBitmap'
    common.class_names['EditStaticBitmap'] = klass
    common.register('python', klass, PythonStaticBitmapGenerator(klass))
    common.register('C++', klass, CppStaticBitmapGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
