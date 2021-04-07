"""\
Code generator functions for wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticBitmapGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(bitmap)s%(style)s)\n'
    tmpl_SetBestSize = ''

    def _prepare_bitmaps(self, obj):
        super(PythonStaticBitmapGenerator, self)._prepare_bitmaps(obj)
        bmp_first = obj.properties.get("bitmap", '')
        if not bmp_first.value.strip() and obj.check_prop("size"):
            # overwrite code with sized EmptyBitmap instead of NullBitmap
            self.tmpl_dict["bitmap"] = self.get_inline_stmt_emptybitmap('empty:%s'%obj.size)



class CppStaticBitmapGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(bitmap)s%(style)s);\n'
    tmpl_SetBestSize = ''


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StaticBitmapXrcObject(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            properties = {"attribute":None}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

    return StaticBitmapXrcObject(obj)


def initialize():
    klass = 'wxStaticBitmap'
    common.class_names['EditStaticBitmap'] = klass
    common.register('python', klass, PythonStaticBitmapGenerator(klass))
    common.register('C++',    klass, CppStaticBitmapGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
