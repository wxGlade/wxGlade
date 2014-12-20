"""\
Code generator functions for wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import config
import os

import wcodegen


class PythonBitmapButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        self._reset_vars()
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        prop = obj.properties
        id_name, id = self.codegen.generate_code_id(obj)
        bmp_file = prop.get('bitmap', '')
        bmp_preview_path = os.path.join(config.icons_path, "icon.xpm")
        if not obj.parent.is_toplevel:
            parent = 'self.%s' % obj.parent.name
        else:
            parent = 'self'
        if not bmp_file:
            bmp = self.cn('wxNullBitmap')
        elif bmp_file.startswith('var:'):
            if obj.preview:
                bmp = "%s('%s', %s)" % (self.cn('wxBitmap'), bmp_preview_path,
                                        self.cn('wxBITMAP_TYPE_XPM'))
            else:
                bmp = (self.cn('wxBitmap') + '(%s,' +
                       self.cn('wxBITMAP_TYPE_ANY)')) % \
                       bmp_file[4:].strip()
        elif bmp_file.startswith('code:'):
            if obj.preview:
                bmp = "%s('%s', %s)" % (self.cn('wxBitmap'), bmp_preview_path,
                                        self.cn('wxBITMAP_TYPE_XPM'))
            else:
                bmp = '(%s)' % bmp_file[5:].strip()
        else:
            if obj.preview:
                import misc
                bmp_file = misc.get_relative_path(bmp_file, True)
            bmp = (self.cn('wxBitmap') + '(%s, ' +
                   self.cn('wxBITMAP_TYPE_ANY') +
                   ')') % self.codegen.quote_path(bmp_file)
        init = []
        if id_name:
            init.append(id_name)
        klass = obj.klass
        if klass == obj.base:
            klass = self.cn(klass)
        init.append('self.%s = %s(%s, %s, %s%s)\n' % 
                    (obj.name, klass, parent, id, bmp, self.tmpl_dict['style']))
        props_buf = self.codegen.generate_common_properties(obj)

        disabled_bmp = prop.get('disabled_bitmap')
        if disabled_bmp:
            if disabled_bmp.startswith('var:'):
                if not obj.preview:
                    var = disabled_bmp[4:].strip()
                    props_buf.append(
                        ('self.%s.SetBitmapDisabled(' +
                         self.cn('wxBitmap') +
                         '(%s,' + self.cn('wxBITMAP_TYPE_ANY') +
                         '))\n') % (obj.name, var))
            elif disabled_bmp.startswith('code:'):
                if not obj.preview:
                    var = disabled_bmp[5:].strip()
                    props_buf.append(
                        ('self.%s.SetBitmapDisabled(' +
                        '(%s))\n') % \
                        (obj.name, var))
            else:
                props_buf.append(('self.%s.SetBitmapDisabled(' +
                                  self.cn('wxBitmap') + '(%s, ' +
                                  self.cn('wxBITMAP_TYPE_ANY') + '))\n') % \
                                 (obj.name,
                                  self.codegen.quote_path(disabled_bmp)))
                
        if not prop.has_key('size'):
            props_buf.append('self.%s.SetSize(self.%s.GetBestSize())\n' % \
                             (obj.name, obj.name))
        if prop.get('default', False):
            props_buf.append('self.%s.SetDefault()\n' % obj.name)
        return init, props_buf, []

# end of class PythonBitmapButtonGenerator


class CppBitmapButtonGenerator(wcodegen.CppWidgetCodeWriter):
    def get_code(self, obj):
        """\
        function that generates C++ code for wxBitmapButton objects.
        """
        self._reset_vars()
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        prop = obj.properties
        id_name, id = self.codegen.generate_code_id(obj)
        if id_name: ids = [ id_name ]
        else: ids = []
        bmp_file = prop.get('bitmap', '')
        if not obj.parent.is_toplevel:
            parent = '%s' % obj.parent.name
        else:
            parent = 'this'
        
        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            bmp = 'wxBitmap(%s, wxBITMAP_TYPE_ANY)' % bmp_file[4:].strip()
        elif bmp_file.startswith('code:'):
            bmp = '(%s)' % bmp_file[5:].strip()
        else:
            bmp = 'wxBitmap(wxT(%s), wxBITMAP_TYPE_ANY)' % \
                  self.codegen.quote_path(bmp_file)
        init = ['%s = new %s(%s, %s, %s%s);\n' %
                (obj.name, obj.klass, parent, id, bmp, self.tmpl_dict['style'])]
        props_buf = self.codegen.generate_common_properties(obj)

        disabled_bmp = prop.get('disabled_bitmap')
        if disabled_bmp:
            if disabled_bmp.startswith('var:'):
                var = disabled_bmp[4:].strip()
                props_buf.append('%s->SetBitmapDisabled('
                                 'wxBitmap(%s,wxBITMAP_TYPE_ANY));\n' %
                                 (obj.name, var))
            elif disabled_bmp.startswith('code:'):
                var = disabled_bmp[5:].strip()
                props_buf.append('%s->SetBitmapDisabled('
                                 '(%s));\n' % (obj.name, var))
            else:
                props_buf.append(
                    '%s->SetBitmapDisabled('
                    'wxBitmap(%s, wxBITMAP_TYPE_ANY));\n' % \
                    (obj.name, self.codegen.quote_path(disabled_bmp)))
                
        if not prop.has_key('size'):
            props_buf.append('%s->SetSize(%s->GetBestSize());\n' % \
                             (obj.name, obj.name))
        if prop.get('default', False):
            props_buf.append('%s->SetDefault();\n' % obj.name)
        return init, ids, props_buf, []

# end of class CppBitmapButtonGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class BitmapButtonXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'disabled_bitmap':
                name = 'disabled'
            xrcgen.DefaultXrcObject.write_property(
                self, name, val, outfile, tabs)

    # end of class BitmapButtonXrcObject

    return BitmapButtonXrcObject(obj)


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('python', klass, PythonBitmapButtonGenerator(klass))
    common.register('C++', klass, CppBitmapButtonGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
