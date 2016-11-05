"""\
XRC code generator

Generates the xml code for the app in XRC format.
Calls the appropriate ``writers'' of the various objects. These functions
return an instance of XrcObject

@copyright: 2002-2007 Alberto Griggio
@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from xml.sax.saxutils import escape, quoteattr
from codegen import BaseLangCodeWriter, EventsPropertyHandler, ExtraPropertiesPropertyHandler
from collections import OrderedDict
import common, compat, errors
import wcodegen
from wcodegen.taghandler import BaseCodeWriterTagHandler


class FontPropertyHandler(BaseCodeWriterTagHandler):
    props = {'size': '', 'family': '', 'style': '', 'weight': '', 'underlined': '', 'face': ''}

    def __init__(self):
        super(FontPropertyHandler, self).__init__()
        self.current = None

    def start_elem(self, name, attrs):
        self.current = name

    def end_elem(self, name, code_obj):
        if name == 'font':
            code_obj.properties['font'] = self.props
            return True  # to remove this handler

    def char_data(self, data):
        super(FontPropertyHandler, self).char_data(data)
        self.props[self.current] = self.get_char_data()



class XrcObject(wcodegen.XrcWidgetCodeWriter):
    "Class to produce the XRC code for a given widget. This is a base class which does nothing"

    def __init__(self, klass=None):
        wcodegen.XrcWidgetCodeWriter.__init__(self, klass)
        self.properties = {}
        self.children = []  # sub-objects

    def write_child_prologue(self, child, out_file, ntabs):
        pass

    def write_child_epilogue(self, child, out_file, ntabs):
        pass

    def write_property(self, name, val, outfile, ntabs):
        pass

    def write(self, out_file, ntabs):
        pass

    def warning(self, msg):
        "Show a warning message"
        self._logger.warning(msg)



class SizerItemXrcObject(XrcObject):
    "XrcObject to handle sizer items"

    def __init__(self, obj, proportion, flag, border):
        XrcObject.__init__(self)
        self.obj = obj  # the XrcObject representing the widget
        self.proportion = proportion
        self.flag = flag
        self.border = border

    def write(self, out_file, ntabs):
        write = out_file.write
        tabs = self.tabs(ntabs)
        tabs1 = self.tabs(ntabs + 1)
        write(tabs + '<object class="sizeritem">\n')
        if self.proportion != '0':
            write(tabs1 + '<option>%s</option>\n' % self.proportion)
        if self.flag and self.flag != '0':
            write(tabs1 + '<flag>%s</flag>\n' % self.cn_f(self.flag))
        if self.border != '0':
            write(tabs1 + '<border>%s</border>\n' % self.border)
        # write the widget
        self.obj.write(out_file, ntabs + 1)
        write(tabs + '</object>\n')



class SpacerXrcObject(XrcObject):
    "XrcObject to handle widgets"

    def __init__(self, size_str, option, flag, border):
        XrcObject.__init__(self)
        self.size_str = size_str
        self.proportion = option
        self.flag = flag
        self.border = border

    def write(self, out_file, ntabs):
        write = out_file.write
        tabs = self.tabs(ntabs)
        tabs1 = self.tabs(ntabs + 1)
        write(tabs + '<object class="spacer">\n')
        write(tabs1 + '<size>%s</size>\n' % self.size_str.strip())
        if self.proportion != '0':
            write(tabs1 + '<option>%s</option>\n' % self.proportion)
        if self.flag and self.flag != '0':
            write(tabs1 + '<flag>%s</flag>\n' % self.cn_f(self.flag))
        if self.border != '0':
            write(tabs1 + '<border>%s</border>\n' % self.border)
        write(tabs + '</object>\n')



class DefaultXrcObject(XrcObject):
    "Standard XrcObject for every widget, used if no specific XrcObject is available"

    def __init__(self, code_obj):
        XrcObject.__init__(self, code_obj.klass)
        self.properties = code_obj.properties
        self.code_obj = code_obj
        self.name = code_obj.name
        self.klass = code_obj.base  # custom classes aren't allowed in XRC
        self.subclass = code_obj.klass

    def write_property(self, name, val, outfile, ntabs):
        if not val:
            return

        if name in ['icon', 'bitmap']:
            prop = self._format_bitmap_property(name, val)
        else:
            prop = common.format_xml_tag(name, val)

        if prop:
            line = '%s%s' % (self.tabs(ntabs), prop)
            outfile.write(line)

    def _format_bitmap_property(self, name, val):
        "Return formatted bitmap/icon XRC property (as string)."

        if val.startswith('art:'):
            content = val[4:]
            elements = [item.strip() for item in content.split(',')]
            art_id = elements[0]
            art_client = elements[1]

            if art_client != 'wxART_OTHER':
                prop = common.format_xml_tag( name, '', stock_id=art_id, stock_client=art_client )
            else:
                prop = common.format_xml_tag(name, u'', stock_id=art_id)

        elif val.startswith('code:') or val.startswith('empty:') or val.startswith('var:'):
            self._logger.warn( _('XRC: Unsupported bitmap statement "%s" for %s "%s"'), val, self.klass, self.name )
            prop = None

        else:
            prop = common.format_xml_tag(name, val)

        return prop

    def write(self, out_file, ntabs):
        write = out_file.write
        if self.code_obj.in_sizers:
            write(self.tabs(ntabs) + '<object class=%s>\n' % quoteattr(self.klass))
        else:
            if self.subclass and self.subclass != self.klass:
                write(self.tabs(ntabs) + '<object class=%s name=%s subclass=%s>\n' % (
                                                quoteattr(self.klass), quoteattr(self.name), quoteattr(self.subclass)) )
            else:
                write(self.tabs(ntabs) + '<object class=%s name=%s>\n' % (quoteattr(self.klass), quoteattr(self.name)))
        tab_str = self.tabs(ntabs + 1)
        # write the properties
        if 'foreground' in self.properties:
            if self.properties['foreground'].startswith('#'):
                # XRC does not support colors from system settings
                self.properties['fg'] = self.properties['foreground']
            del self.properties['foreground']
        if 'background' in self.properties:
            if self.properties['background'].startswith('#'):
                # XRC does not support colors from system settings
                self.properties['bg'] = self.properties['background']
            del self.properties['background']
        if 'font' in self.properties:
            font = self.properties['font']
            del self.properties['font']
        else:
            font = None
        style = str(self.properties.get('style', ''))
        if style:
            if style == '0':
                del self.properties['style']
            else:
                self.properties['style'] = self.cn_f(style)

        if 'id' in self.properties:
            del self.properties['id']  # id has no meaning for XRC

        if 'events' in self.properties:
            for win_id, event, handler, event_type in self.get_event_handlers(self):
                write(tab_str + '<handler event=%s>%s</handler>\n' % (quoteattr(event), escape(handler)))
            del self.properties['events']

        # 'disabled' property is actually 'enabled' for XRC
        if 'disabled' in self.properties:
            try:
                val = int(self.properties['disabled'])
            except:
                val = False
            if val:
                self.properties['enabled'] = '0'
            del self.properties['disabled']

        if 'extracode' in self.properties:
            write(self.properties['extracode'].replace('\\n', '\n'))
            del self.properties['extracode']

        # custom base classes are ignored for XRC...
        if 'custom_base' in self.properties:
            del self.properties['custom_base']

        if 'extraproperties' in self.properties:
            prop = self.properties['extraproperties']
            del self.properties['extraproperties']
            self.properties.update(prop)

        for name, val in self.properties.items():
            self.write_property(str(name), val, out_file, ntabs + 1)
        # write the font, if present
        if font:
            write(tab_str + '<font>\n')
            tab_str = self.tabs(ntabs + 2)
            for key, val in font.items():
                if val:
                    write(tab_str + '<%s>%s</%s>\n' % (escape(key), escape(val), escape(key)))
            write(self.tabs(ntabs + 1) + '</font>\n')
        # write the children
        for c in self.children:
            self.write_child_prologue(c, out_file, ntabs + 1)
            c.write(out_file, ntabs + 1)
            self.write_child_epilogue(c, out_file, ntabs + 1)
        write(self.tabs(ntabs) + '</object>\n')



class NotImplementedXrcObject(XrcObject):
    """XrcObject used when no code for the widget can be generated (for
    example, because XRC does not currently handle such widget)"""

    def __init__(self, code_obj):
        XrcObject.__init__(self)
        self.code_obj = code_obj

    def write(self, outfile, ntabs):
        msg = 'code generator for %s objects not available' % self.code_obj.base
        self.warning('%s' % msg)
        stmt = '%s%s\n' % (self.tabs(ntabs), self._format_comment(msg))
        outfile.write(stmt)



class XRCCodeWriter(BaseLangCodeWriter, wcodegen.XRCMixin):
    "Code writer class for writing XRC XML code out of the designed GUI elements"
    xrc_objects = None
    """\
    dictionary of active L{XrcObject} instances: during the code generation
    it stores all the non-sizer objects that have children (i.e. frames,
    dialogs, panels, notebooks, etc.), while at the end of the code
    generation, before L{finalize} is called, it contains only the true
    toplevel objects (frames and dialogs), and is used to write their XML
    code (see L{finalize}). The other objects are deleted when L{add_object}
    is called with their corresponding code_object as argument
    (see L{add_object})
    """

    global_property_writers = {
        'font': FontPropertyHandler,
        'events': EventsPropertyHandler,
        'extraproperties': ExtraPropertiesPropertyHandler,
        }
    "Dictionary whose items are custom handlers for widget properties"

    property_writers = {}
    """\
    Dictionary of dictionaries of property handlers specific for a widget
    the keys are the class names of the widgets

    Example: property_writers['wxRadioBox'] = {'choices', choices_handler}
    """

    obj_builders = {}
    "Dictionary of ``writers'' for the various objects"

    tmpl_encoding = '<?xml version="1.0" encoding="%s"?>\n'
    tmpl_generated_by = '<!-- %(generated_by)s -->'

    use_names_for_binding_events = False

    # inject different XRC objects
    XrcObject = XrcObject
    SizerItemXrcObject = SizerItemXrcObject
    SpacerXrcObject = SpacerXrcObject
    DefaultXrcObject = DefaultXrcObject
    NotImplementedXrcObject = NotImplementedXrcObject

    def __init__(self):
        BaseLangCodeWriter.__init__(self)
        # Inject to all classed derived from WrcObject
        if not hasattr(XrcObject, 'tabs'):
            XrcObject.tabs = self.tabs
        if not hasattr(XrcObject, '_format_comment'):
            XrcObject._format_comment = self._format_comment

    def init_lang(self, app_attrs):
        # for now we handle only single-file code generation
        if self.multiple_files:
            raise errors.WxgXRCMultipleFilesNotSupported()

        # overwrite existing sources always
        self._overwrite = True

        self.output_file_name = app_attrs['path']
        self.out_file = compat.StringIO()
        self.out_file.write('\n<resource version="2.3.0.1">\n')
        self.curr_tab = 1
        self.xrc_objects = OrderedDict()

    def finalize(self):
        # write the code for every toplevel object
        for obj in self.xrc_objects.itervalues():
            obj.write(self.out_file, 1)
        self.out_file.write('</resource>\n')
        # store the contents to file
        self.save_file( self.output_file_name, self.out_file.getvalue() )

    def add_app(self, app_attrs, top_win_class):
        "In the case of XRC output, there's no wxApp code to generate"
        pass

    def add_object(self, unused, sub_obj):
        "Adds the object sub_obj to the XRC tree. The first argument is unused."
        # what we need in XRC is not top_obj, but sub_obj's true parent
        top_obj = sub_obj.parent
        builder = self.obj_builders.get( sub_obj.base, DefaultXrcObject )
        try:
            # check whether we already created the xrc_obj
            xrc_obj = sub_obj.xrc
        except AttributeError:
            xrc_obj = builder(sub_obj)  # builder functions must return a subclass of XrcObject
            sub_obj.xrc = xrc_obj
        else:
            # if we found it, remove it from the self.xrc_objects dictionary
            # (if it was there, i.e. the object is not a sizer), because this
            # isn't a true toplevel object
            if sub_obj in self.xrc_objects:
                del self.xrc_objects[sub_obj]
        # let's see if sub_obj's parent already has an XrcObject: if so, it
        # is temporarily stored in the self.xrc_objects dict...
        if top_obj in self.xrc_objects:
            top_xrc = self.xrc_objects[top_obj]
        else:
            # ...otherwise, create it and store it in the self.xrc_objects dict
            top_xrc = self.obj_builders.get( top_obj.base, DefaultXrcObject )(top_obj)
            top_obj.xrc = top_xrc
            self.xrc_objects[top_obj] = top_xrc
        top_obj.xrc.children.append(xrc_obj)

    def add_sizeritem(self, unused, sizer, obj, option, flag, border):
        "Adds a sizeritem to the XRC tree. The first argument is unused."
        # what we need in XRC is not toplevel, but sub_obj's true parent
        toplevel = obj.parent
        top_xrc = toplevel.xrc
        obj_xrc = obj.xrc
        try:
            sizer_xrc = sizer.xrc
        except AttributeError:
            # if the sizer has not an XrcObject yet, create it now
            sizer_xrc = self.obj_builders.get( sizer.base, DefaultXrcObject )(sizer)
            sizer.xrc = sizer_xrc
        # we now have to move the children from 'toplevel' to 'sizer'
        index = top_xrc.children.index(obj_xrc)
        if obj.klass == 'spacer':
            w = obj.properties.get('width', '0')
            h = obj.properties.get('height', '0')
            obj_xrc = SpacerXrcObject( '%s, %s' % (w, h), str(option), str(flag), str(border) )
            sizer.xrc.children.append(obj_xrc)
        elif obj.klass == 'sizerslot':
            obj_xrc = SpacerXrcObject( '0, 0', '0', '0', '0' )
            sizer.xrc.children.append(obj_xrc)
        else:
            sizeritem_xrc = SizerItemXrcObject( obj_xrc, str(option), str(flag), str(border) )
            sizer.xrc.children.append(sizeritem_xrc)
        del top_xrc.children[index]

    def add_class(self, code_obj):
        """\
        Add class behaves very differently for XRC output than for other
        languages (i.e. python): since custom classes are not supported in
        XRC, this has effect only for true toplevel widgets, i.e. frames and
        dialogs. For other kinds of widgets, this is equivalent to add_object
        """
        if not code_obj in self.xrc_objects:
            builder = self.obj_builders.get( code_obj.base, DefaultXrcObject )
            xrc_obj = builder(code_obj)
            code_obj.xrc = xrc_obj
            # add the xrc_obj to the dict of the toplevel ones
            self.xrc_objects[code_obj] = xrc_obj

    def generate_code_id(self, obj, id=None):
        return '', ''

    def _format_comment(self, msg):
        return '<!-- %s -->' % escape(msg.rstrip())

    def _quote_str(self, s):
        return s


writer = XRCCodeWriter()
"The code writer is an instance of L{XRCCodeWriter}."

language = writer.language
"Language generated by this code generator"
