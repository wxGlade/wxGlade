"""\
XRC code generator

Generates the xml code for the app in XRC format.
Calls the appropriate ``writers'' of the various objects. These functions return an instance of XrcObject.
To be done: write XRC directly instead of using BaseLangCodeWriter as base.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2019-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from xml.sax.saxutils import escape, quoteattr
from codegen import BaseLangCodeWriter
from collections import OrderedDict
import common, wcodegen
import new_properties as np
import logging


class XrcObject(wcodegen.XrcWidgetCodeWriter):
    "Class to produce the XRC code for a given widget. This is a base class which does nothing"

    def __init__(self, klass=None):
        wcodegen.XrcWidgetCodeWriter.__init__(self, klass)
        self.properties = {}
        self.children = []  # sub-objects

    def write_child_prologue(self, child, output, ntabs):
        pass

    def write_child_epilogue(self, child, output, ntabs):
        pass

    def write_property(self, name, val, outfile, ntabs):
        pass

    def write(self, output, ntabs, properties=None):
        pass

    def warning(self, msg):
        "Show a warning message"
        logging.warning(msg)



class SizerItemXrcObject(XrcObject):
    "XrcObject to handle sizer items"

    def __init__(self, xrc_obj, obj):
        XrcObject.__init__(self)
        self.xrc_obj = xrc_obj
        self.obj = obj

    def write(self, output, ntabs, properties=None):
        tabs = self.tabs(ntabs)
        tabs1 = self.tabs(ntabs + 1)
        output.append(tabs + '<object class="sizeritem">\n')
        if self.obj.proportion:
            output.append(tabs1 + '<option>%s</option>\n' % self.obj.proportion)
        flag = self.obj.properties["flag"].get_string_value()
        if flag and flag!='0':
            output.append(tabs1 + '<flag>%s</flag>\n' % self.cn_f(flag))
        if self.obj.border:
            output.append(tabs1 + '<border>%s</border>\n' % self.obj.border)
        if self.obj.parent._IS_GRIDBAG:
            cellpos = self.obj.parent._get_row_col(self.obj.index)
            output.append( tabs1 + '<cellpos>%d,%d</cellpos>\n' % cellpos )
            output.append( tabs1 + '<cellspan>%d,%d</cellspan>\n' % self.obj.span )
        # write the widget
        self.xrc_obj.write(output, ntabs + 1)
        output.append(tabs + '</object>\n')



class SpacerXrcObject(XrcObject):
    "XrcObject to handle widgets"

    def __init__(self, obj):
        XrcObject.__init__(self)
        self.obj = obj

    def write(self, output, ntabs):
        obj = self.obj

        tabs = self.tabs(ntabs)
        tabs1 = self.tabs(ntabs + 1)

        if obj is None or obj.name == "spacer":
            output.append( tabs + '<object class="spacer">\n' )
        else:
            output.append( tabs + '<object class="spacer" name=%s>\n'%quoteattr(obj.name) )
        if obj is not None:
            # a real spacer
            output.append( tabs1 + '<size>%s, %s</size>\n' % (obj.width, obj.height) )
            if obj.proportion:
                output.append(tabs1 + '<option>%s</option>\n' % obj.proportion)
            if obj.flag:
                flag = obj.properties["flag"].get_string_value()
                output.append(tabs1 + '<flag>%s</flag>\n' % self.cn_f(flag))
            if obj.border:
                output.append(tabs1 + '<border>%s</border>\n' % obj.border)
        else:
            # just an empty sizer slot
            output.append( tabs1 + '<size>0, 0</size>\n' )
        output.append(tabs + '</object>\n')



class DefaultXrcObject(XrcObject):
    "Standard XrcObject for every widget, used if no specific XrcObject is available"

    def __init__(self, widget):
        #classname = widget.get_prop_value("class", widget.WX_CLASS)  # the 0.9 implementation
        if widget.IS_SIZER:
            classname = widget.WX_CLASS
        else:
            classname = widget.get_instantiation_class()
        XrcObject.__init__(self, classname)
        self.widget = widget
        self.name = widget.name
        self.klass = widget.WX_CLASS
        self.subclass = classname

    def write_property(self, name, val, output, ntabs):
        if not val:
            return

        if isinstance(val, np.BitmapProperty):
            # rename: no '..._bitmap' and some renames
            if name.endswith('_bitmap'):
                name = name[:-7]
            if   name=="pressed": name = "selected"
            elif name=="current": name = "hover"

            prop = self._format_bitmap_property(name, val.get_value(), ntabs)
        else:
            prop = common.format_xml_prop(name, val, ntabs)

        output.append(prop)

    def _format_bitmap_property(self, name, val, ntabs):
        "Return formatted bitmap/icon XRC property (as string)."

        if val.startswith('art:'):
            content = val[4:]
            elements = [item.strip() for item in content.split(',')]
            art_id = elements[0]
            art_client = elements[1]

            if art_client != 'wxART_OTHER':
                return common.format_xml_prop( name, '', ntabs, stock_id=art_id, stock_client=art_client )
            else:
                return common.format_xml_prop(name, u'', ntabs, stock_id=art_id)

        elif val.startswith('code:') or val.startswith('empty:') or val.startswith('var:'):
            logging.warn( _('XRC: Unsupported bitmap statement "%s" for %s "%s"'), val, self.klass, self.name )
            return None

        return common.format_xml_prop(name, val, ntabs)

    def write(self, output, ntabs, properties=None):
        if properties is None: properties = {}
        if "name" in properties:
            name = properties["name"]
            del properties["name"]
        else:
            name = self.name
        if self.widget.IS_SIZER:
            output.append(self.tabs(ntabs) + '<object class=%s>\n' % quoteattr(self.klass))
        else:
            if self.subclass and self.subclass != self.klass:
                output.append(self.tabs(ntabs) + '<object class=%s name=%s subclass=%s>\n' % (
                                                quoteattr(self.klass), quoteattr(name), quoteattr(self.subclass)) )
            else:
                output.append(self.tabs(ntabs) + '<object class=%s name=%s>\n' % (quoteattr(self.klass), quoteattr(name)))
        tab_str = self.tabs(ntabs + 1)
        # write the properties
        import edit_sizers
        active_properties = self.widget.get_properties(without=set(edit_sizers.SizerBase.MANAGED_PROPERTIES))

        font = None
        for prop in active_properties:
            if not prop.is_active(): continue
            if prop.value==prop.default_value: continue
            name = prop.name
            if name in properties: continue  # set already
            value = None
            if name=='foreground':
                value = prop.get_string_value()
                if not value.startswith('#'):
                    # XRC does not support colors from system settings
                    continue
                name = 'fg'
            elif name=='background':
                value = prop.get_string_value()
                if not value.startswith('#'):
                    # XRC does not support colors from system settings
                    continue
                name = "bg"
            elif name=='font':
                font = prop.value
                continue
            elif name in ("cell_font", "label_font"):
                continue
            elif name=="style":
                if hasattr(prop, "value_set"):
                    if prop.value_set==prop.default_value: continue
                value = prop.get_string_value()
                if value: value = self.cn_f(value)
            elif name=='id':
                continue  # id has no meaning for XRC
            elif name=='events':
                for win_id, event, handler, event_type in self.get_event_handlers(self.widget):
                    output.append(tab_str + '<handler event=%s>%s</handler>\n' % (quoteattr(event), escape(handler)))
                continue
            elif name=='disabled':
                # 'disabled' property is actually 'enabled' for XRC
                if prop.get():
                    properties['enabled'] = '0'
                continue
            elif name=='custom_base' in self.properties:
                # custom base classes are ignored for XRC...
                continue
            elif name=='extraproperties':
                value = prop.get()
                if value:
                    properties.update(value)
                continue
            if isinstance(prop, np.BitmapProperty):
                value = prop
            else:
                if value is None: value = prop.get_string_value()
                if value is None: continue
            properties[name] = value

        for name in sorted( properties.keys() ):
            value = properties[name]
            if value is None: continue
            self.write_property( name, value, output, ntabs + 1)
        # write the font, if present
        if font:
            output.append(tab_str + '<font>\n')
            tab_str = self.tabs(ntabs + 2)

            data = sorted( zip(['size','family','style','weight','underlined','face'], font) )
            for key, val in data:
                if isinstance(val, int): val = str(val)
                if val:
                    output.append(tab_str + '<%s>%s</%s>\n' % (escape(key), escape(val), escape(key)))
            output.append(self.tabs(ntabs + 1) + '</font>\n')
        # write the children
        for c in self.children:
            self.write_child_prologue(c, output, ntabs + 1)
            c.write(output, ntabs + 1)
            self.write_child_epilogue(c, output, ntabs + 1)
        output.append(self.tabs(ntabs) + '</object>\n')



class NotImplementedXrcObject(XrcObject):
    """XrcObject used when no code for the widget can be generated (for
    example, because XRC does not currently handle such widget)"""

    def __init__(self, code_obj):
        XrcObject.__init__(self)
        self.code_obj = code_obj

    def write(self, output, ntabs):
        msg = 'code generator for %s objects not available' % self.code_obj.WX_CLASS
        self.warning('%s' % msg)
        output.append( '%s%s\n' % (self.tabs(ntabs), self._format_comment(msg)) )



class XRCCodeWriter(BaseLangCodeWriter, wcodegen.XRCMixin):
    "Code writer class for writing XRC XML code out of the designed GUI elements"

    # dict of active XrcObject instances: during the code generation it stores all the non-sizer objects that have
    # children (i.e. frames, dialogs, panels, notebooks, etc.), while at the end of the code generation,
    # before finalize is called, it contains only the true toplevel objects (frames and dialogs), and is used to write
    # their XML code (see finalize). The other objects are deleted when add_object is called with their corresponding
    # code_object as argument (see add_object)
    xrc_objects = None

    property_writers = {}  # dict of dicts of property handlers specific for a widget; keys: class names of the widgets
    obj_builders = {}      # Dictionary of ``writers'' for the various objects

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

    def init_lang(self, app):
        # for now we handle only single-file code generation
        if self.multiple_files:
            return "XRC code cannot be split into multiple files"

        # overwrite existing sources always
        self._overwrite = True

        self.output_file_name = app.output_path
        self.out_file = []
        self.out_file.append('\n<resource version="2.3.0.1">\n')
        self.curr_tab = 1
        self.xrc_objects = OrderedDict()

    def finalize(self):
        # write the code for every toplevel object
        for obj in self.xrc_objects.values():
            obj.write(self.out_file, 1)
        self.out_file.append('</resource>\n')
        # store the contents to file
        self.save_file( self.output_file_name, self.out_file )
        self.out_file = None

    def generate_code(self, root, widget=None):
        "entry point for recursive code generation via _generate_code()"
        # root must be application.Application instance for now
        for c in root.children or []:
            if widget is not None and c is not widget: continue # for preview
            self._generate_code(None, None, None, c)

    def _generate_code(self, klass, parent, parent_builder, obj):
        # XXX old implementation from __init__.py before re-factoring 'real' code generation
        # recursively generate code, for anything except application.Application
        # for toplevel widgets or with class different from wx... a class will be added

        if obj.IS_SLOT or obj.WX_CLASS=="spacer":
            if obj.WX_CLASS:  # "slot" has no code generator, but "sizerslot" or "spacer" needs to be added
                self.add_object(obj)
                self.add_sizeritem(obj.parent_class_object, obj.parent, obj)
            return

        parent = obj.parent
        parent_class_object = obj.parent_class_object  # used for adding to this object's sizer

        obj.IS_CLASS = IS_CLASS = obj.check_prop_truth("class")

        # first the item
        if IS_CLASS:
            self.add_class(obj)
        if not obj.IS_TOPLEVEL:
            added = self.add_object(obj)  # added can be False if the widget is not supported
        else:
            added = False

        # then the children
        for child in obj.get_all_children():
            assert obj.children.count(child)<=1
            self._generate_code(None, None, None, child)  # XRCCodeWriter does not use the other args

        # check whether the object belongs to some sizer; if applicable, add it to the sizer at the top of the stack
        if added and parent.IS_SIZER:
            if obj.WX_CLASS not in ("spacer",):  # spacer and slot are adding itself to the sizer
                self.add_sizeritem(parent_class_object, parent, obj)

    def add_object(self, sub_obj):
        "Adds the object sub_obj to the XRC tree. The first argument is unused."
        # what we need in XRC is not top_obj, but sub_obj's true parent we don't need the sizer, but the window

        top_obj = sub_obj.parent_window
        builder = self.obj_builders.get( sub_obj.WX_CLASS, DefaultXrcObject )
        try:
            # check whether we already created the xrc_obj
            xrc_obj = sub_obj.xrc
        except AttributeError:
            xrc_obj = builder(sub_obj)  # builder functions must return a subclass of XrcObject
            sub_obj.xrc = xrc_obj
        else:
            # if we found it, remove it from the self.xrc_objects dictionary
            # (if it was there, i.e. the object is not a sizer), because this isn't a true toplevel object
            if sub_obj in self.xrc_objects:
                del self.xrc_objects[sub_obj]
        if not getattr(top_obj, "xrc"):
            # create XrcObject and store it in the self.xrc_objects dict
            top_xrc = self.obj_builders.get( top_obj.WX_CLASS, DefaultXrcObject )(top_obj)
            top_obj.xrc = top_xrc
            self.xrc_objects[top_obj] = top_xrc
        top_obj.xrc.children.append(xrc_obj)
        return True

    def add_sizeritem(self, unused, sizer, obj):
        "Adds a sizeritem to the XRC tree. The first argument is unused."
        # what we need in XRC is not toplevel, but sub_obj's true parent
        toplevel = obj.parent_window

        top_xrc = toplevel.xrc
        obj_xrc = obj.xrc
        try:
            sizer_xrc = sizer.xrc
        except AttributeError:
            # if the sizer has not an XrcObject yet, create it now
            sizer_xrc = self.obj_builders.get( sizer.WX_CLASS, DefaultXrcObject )(sizer)
            sizer.xrc = sizer_xrc
        # we now have to move the children from 'toplevel' to 'sizer'
        index = top_xrc.children.index(obj_xrc)
        if obj.WX_CLASS == 'spacer':
            sizer.xrc.children.append( SpacerXrcObject(obj) )
        elif obj.WX_CLASS == 'sizerslot':
            if not sizer._IS_GRIDBAG:
                sizer.xrc.children.append( SpacerXrcObject(None) )
        else:
            sizeritem_xrc = SizerItemXrcObject( obj_xrc, obj )
            sizer.xrc.children.append(sizeritem_xrc)
        del top_xrc.children[index]

    def add_class(self, code_obj):
        """Add class behaves very differently for XRC output than for other languages (i.e. python):
        since custom classes are not supported in XRC, this has effect only for true toplevel widgets, i.e. frames and
        dialogs. For other kinds of widgets, this is equivalent to add_object"""
        if not code_obj in self.xrc_objects:
            builder = self.obj_builders.get( code_obj.WX_CLASS, DefaultXrcObject )
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


writer = XRCCodeWriter()    # The code writer is an instance of XRCCodeWriter

language = writer.language  # Language generated by this code generator
