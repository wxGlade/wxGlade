"""
Parsers used to load an app and to generate the code from a XML file.

See wcodegen.taghandler for custom tag handler base classes.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
from xml.sax import SAXException, make_parser
from xml.sax.handler import ContentHandler

import time

import common, config


class XmlParsingError(SAXException):
    "Custom exception to report problems during parsing"
    locator = None

    def __init__(self, msg):
        if self.locator:
            msg = _('%s (line: %s, column: %s)') % ( msg, self.locator.getLineNumber(), self.locator.getColumnNumber() )
        SAXException.__init__(self, msg)


class XmlParser(ContentHandler):
    "'abstract' base class of the parsers used to load an app and to generate the code"

    def __init__(self):
        self._objects = Stack()      # Stack of 'alive' objects
        self._curr_prop = None       # Name of the current property
        self._curr_prop_val = []     # Value of the current property; strings, to be joined
        self._appl_started = False
        self.top = self._objects.top
        self.parser = make_parser()
        self.parser.setContentHandler(self)
        self.locator = None # Document locator
        self.index = None     # only used with ClipboardXmlWidgetBuilder

    def parse(self, source):
        ## Permanent workaround for Python bug "Sax parser crashes if given
        ## unicode file name" (http://bugs.python.org/issue11159).
        ## This bug causes a UnicodeEncodeError if the SAX XML parser wants to store an unicode filename internally.
        ## That's not a general file handling issue because the parameter source is an open file already.
        self.parser.parse(source)

    def parse_string(self, source):
        if isinstance(source, list):
            for line in source:
                self.parser.feed(line)
        else:
            self.parser.feed(source)
        self.parser.close()

    def setDocumentLocator(self, locator):
        self.locator = locator
        XmlParsingError.locator = locator

    def startElement(self, name, attrs):
        raise NotImplementedError

    def endElement(self, name, attrs):
        raise NotImplementedError

    def characters(self, data):
        raise NotImplementedError

    def _process_app_attrs(self, attrs):
        "Process attributes of the application tag; Check only existence of attributes not the logical correctness"
        res = {}

        res['encoding'] = self._get_encoding(attrs)

        for_version = attrs.get('for_version', '%s.%s' % config.for_version_min)
        for_version_tuple = tuple([int(t) for t in for_version.split('.')[:2]])
        if for_version_tuple < (2, 8):
            logging.warning( _('The loaded wxGlade designs are created for wxWidgets "%s", '
                               'but this version is not supported anymore.'),
                             for_version )
            logging.warning( _('The designs will be loaded and converted to wxWidgets "%s" partially. '
                               'Please check the designs carefully.'),
                            '%s.%s' % config.for_version_min )
            for_version = '%s.%s' % config.for_version_min
        res['for_version'] = for_version

        try:
            is_template = int(attrs['is_template'])
        except (KeyError, ValueError):
            is_template = False
        res['is_template'] = is_template

        res['class'] = attrs.get('class')

        try:
            indent_amount = int(attrs['indent_amount'])
        except (KeyError, ValueError):
            indent_amount = config.default_indent_amount
        res['indent_amount'] = indent_amount

        res['indent_symbol'] = attrs.get('indent_symbol', {" ":"space","\t":":tab"}[config.default_indent_symbol])

        if 'language' in attrs:
            res['language'] = attrs['language']
        elif hasattr(self, 'code_writer'):
            res['language'] = attrs.get('language', self.code_writer.language)
        else:
            res['language'] = config.default_language

        res['name'] = attrs.get('name')

        try:
            multiple_files = bool(int(attrs['option']))
        except (KeyError, ValueError):
            multiple_files = config.default_multiple_files
        res['option'] = multiple_files

        try:
            overwrite = int(attrs['overwrite'])
        except (KeyError, ValueError):
            overwrite = config.default_overwrite
        res['overwrite'] = bool(overwrite)

        try:
            mark_blocks = int(attrs['mark_blocks'])
        except (KeyError, ValueError):
            mark_blocks = True # config.mark_blocks
        if not overwrite and not mark_blocks:
            mark_blocks = True
        res['mark_blocks'] = bool(mark_blocks)


        res['path'] = attrs.get('path')

        res['header_extension'] = attrs.get('header_extension', config.default_header_extension)
        res['source_extension'] = attrs.get('source_extension', config.default_source_extension)

        res['top_window'] = attrs.get('top_window')

        try:
            use_gettext = int(attrs["use_gettext"])
        except (KeyError, ValueError):
            use_gettext = config.default_use_gettext
        res['use_gettext'] = bool(use_gettext)

        if attrs.get('use_new_namespace') == u'0' and attrs.get('language') == 'python':
            logging.warning( _('The loaded wxGlade designs are created to use the old Python import style '
                               '("from wxPython.wx import *)". The old import style is not supported anymore.') )
            logging.warning( _('The designs will be loaded and the import style will be converted to new style imports '
                               '("import wx"). Please check your design carefully.') )
            # no update necessary - the attribute will not be used anymore
        return res

    def _get_encoding(self, attrs):
        "Return the document encoding; attrs: Object attributes"
        encoding = attrs.get('encoding', config.default_encoding)
        if encoding:
            try:
                'a'.encode(encoding)
            except LookupError:
                logging.warning( _('Unknown encoding "%s", fallback to default encoding "%s"'),
                                      encoding, config.default_encoding)
                encoding = config.default_encoding
        return encoding



class XmlWidgetBuilder(XmlParser):
    "Parser used to build the tree of widgets from a given XML file"

    def __init__(self, filename=None, input_file_version=None):
        self.filename = filename
        self.input_file_version = input_file_version
        XmlParser.__init__(self)

    def check_input_file_version(self, version):
        # return True if file version is older
        if not self.input_file_version: return True
        return self.input_file_version[:len(version)] < version

    def startElement(self, name, attrs):
        if name == 'application':
            # get properties of the app
            self._appl_started = True
            attrs = self._process_app_attrs(attrs)
            app = common.root
            p = app.properties

            p["encoding"].set( attrs['encoding'] )
            p["output_path"].set( attrs['path'] or "" )
            p["class"].set( attrs['class'] or "MyApp", activate=bool(attrs.get("class")) )
            p["name"].set( attrs['name'] or "app", activate=bool(attrs.get("name")) )
            p["multiple_files"].set( attrs['option'] )
            p["language"].set( attrs['language'] )
            p["top_window"].set( attrs['top_window'] or "" )
            p["use_gettext"].set( attrs['use_gettext'] )
            p["is_template"].set( attrs['is_template'] )
            p["overwrite"].set( attrs['overwrite'] )
            p["mark_blocks"].set( attrs['mark_blocks'] )
            p["indent_mode"].set( attrs['indent_symbol'] )
            p["indent_amount"].set( attrs['indent_amount'] )
            p["for_version"].set( attrs['for_version'] )

            modified = ["encoding", "output_path", "class", "name", "multiple_files", "language", "top_window",
                        "use_gettext", "is_template", "overwrite", "mark_blocks",
                        "indent_mode", "indent_amount", "for_version"]

            source_extension = attrs['source_extension']
            if source_extension and source_extension[0] == '.':
                p["source_extension"].set( source_extension[1:] )
                modified.append( "source_extension" )
                
            header_extension = attrs['header_extension']
            if header_extension and header_extension[0] == '.':
                p["header_extension"].set( header_extension[1:] )
                modified.append( "header_extension" )

            app.properties_changed(modified)
            self._delayed_app_properties = {"for_version":attrs['for_version']}
            if attrs['top_window']:
                self._delayed_app_properties['top_window'] = attrs['top_window']
            return

        if not self._appl_started:
            raise XmlParsingError(_("the root of the tree must be <application>"))
        if name == 'object':
            top = self.top()
            if top: top.notify_owner()
            # create the object and push it on the appropriate stacks
            XmlWidgetObject(attrs, self)
        else:
            # handling of the various properties
            try:
                # look for a custom handler to push on the stack
                obj = self.top()
                handler = obj.obj.get_property_handler(name)
                if handler:
                    obj.prop_handlers.push(handler)
                # get the top custom handler and use it if there's one
                handler = obj.prop_handlers.top()
                if handler:
                    handler.start_elem(name, attrs)
            except AttributeError:
                pass
            self._curr_prop = name

    def endElement(self, name):
        if name == 'application':
            self._appl_started = False
            app = common.root
            for key, value in self._delayed_app_properties.items():
                app.properties[key].set( value )
            app.properties_changed( sorted(self._delayed_app_properties.keys()) )
            return
        if name == 'object':
            # remove last object from Stack
            obj = self._objects.pop()
            obj.notify_owner()
            if obj.IS_SIZERITEM or obj.IS_SLOT:
                return
            if obj.sizeritem and obj.obj.parent.IS_SIZER:
                # XXX just check whether obj.obj has these properties
                obj.obj.copy_properties( obj.sizeritem, ("option","flag","border","span") )
                obj.obj.properties["flag"]._check_value()
            obj.obj.on_load()
        else:
            # end of a property or error
            prop = self._curr_prop
            data = "".join(self._curr_prop_val)
            self._curr_prop = None
            self._curr_prop_val = []
            if prop in ("menubar", "toolbar", "statusbar"):
                # case 0: ignore properties: menubar, toolbar, statusbar
                return

            if data:
                # case 1: set _curr_prop value
                try:
                    handler = self.top().prop_handlers.top()
                    if not handler or handler.char_data(data):
                        # if char_data returned False, we don't have to call add_property
                        self.top().add_property(prop, data)
                except AttributeError:
                    pass

            # case 2: call custom end_elem handler
            try:
                # if there is a custom handler installed for this property, call its end_elem function:
                #  if this returns True, remove the handler from Stack
                obj = self.top()
                handler = obj.prop_handlers.top()
                if handler.end_elem(name):
                    obj.prop_handlers.pop()
                    obj._properties_added.append(name)
            except AttributeError:
                pass

    def characters(self, data):
        if not data or data.isspace():
            return
        if self._curr_prop is None:
            raise XmlParsingError(_("Character data can be present only inside properties"))
        self._curr_prop_val.append(data)


class ProgressXmlWidgetBuilder(XmlWidgetBuilder):
    "Adds support for a progress dialog to the widget builder parser"

    def __init__(self, filename, input_file_version, input_file):
        self.input_file = input_file
        if self.input_file:
            self.size = len(self.input_file.readlines())
            self.input_file.seek(0)
            import wx
            self.progress = wx.ProgressDialog( _("Loading..."), _("Please wait while loading the app"), 20, common.main )
            self.step = 4
            self.i = 1
            self._last_progress_update = time.time()
        else:
            self.size = 0
            self.progress = None
        XmlWidgetBuilder.__init__(self, filename, input_file_version)

    def endElement(self, name):
        if self.progress:
            if name == 'application':
                self.progress.Destroy()
                self.progress = None
            else:
                if self.locator:
                    where = self.locator.getLineNumber()
                    value = int(round(where * 20.0 / self.size))
                else:
                    # we don't have any information, so we update the progress bar "randomly"
                    value = (self.step * self.i) % 20
                    self.i += 1
                if time.time()-self._last_progress_update > 0.25:
                    self.progress.Update(value)
                    self._last_progress_update = time.time()
        XmlWidgetBuilder.endElement(self, name)

    def parse(self, *args):
        try:
            XmlWidgetBuilder.parse(self, *args)
        finally:
            if self.progress:
                self.progress.Destroy()
                self.progress = None

    def parse_string(self, *args):
        try:
            XmlWidgetBuilder.parse_string(self, *args)
        finally:
            if self.progress:
                self.progress.Destroy()
                self.progress = None


class _own_dict(dict):
    pass  # just be able to add attributes


class ClipboardXmlWidgetBuilder(XmlWidgetBuilder):
    """Parser used to cut&paste widgets.

    The differences with XmlWidgetBuilder are:
      - No <application> tag in the piece of xml to parse
      - Fake parent, sizer and sizeritem objects to push on the three stacks:
        they keep info about the destination of the hierarchy of widgets (i.e. the target of the 'paste' command)
      - The first widget built must be hidden and shown again at the end of the operation"""

    def __init__(self, parent, index, proportion, span, flag, border):
        XmlWidgetBuilder.__init__(self)
        self._renamed = {}
        self._object_counter = 0
        self.parent = parent
        self.index = index
        if not parent:
            # e.g. a frame is pasted: update with the top level names
            self.have_names = set(child.name for child in common.root.children)
        else:
            self.have_names = set(parent.toplevel_parent.names)

        class XmlClipboardObject(object):
            def __init__(self, **kwds):
                self.IS_SIZER = self.IS_WINDOW = False
                self.__dict__.update(kwds)
            def notify_owner(self):
                pass

        # fake parent window object
        fake_parent = XmlClipboardObject(obj=parent, parent=parent)
        if parent and parent.IS_SIZER:
            fake_parent.IS_SIZER = True
        else:
            fake_parent.IS_WINDOW = True

        self._objects.push(fake_parent)

        # fake sizer object
        if parent and parent.CHILDREN!=1:
            sizer = parent
            fake_sizer = XmlClipboardObject(obj=sizer, parent=parent)
            fake_sizer.IS_SIZER = True
            sizeritem = Sizeritem()
            sizeritem.properties["proportion"].set(proportion)
            sizeritem.properties["span"].set(span)
            sizeritem.properties["flag"].set(flag)
            sizeritem.properties["border"].set(border)
            sizeritem.index = index
            # fake sizer item
            fake_sizeritem = XmlClipboardObject(obj=sizeritem, parent=parent)

            self._objects.push(fake_sizer)
            self._objects.push(fake_sizeritem)

        self.depth_level = 0
        self._appl_started = True  # no application tag when parsing from the clipboard

    def _get_new_name(self, oldname):
        # when pasting, check whether the name is free and if not get a new unique one
        if not oldname in self.have_names:
            self.have_names.add(oldname)
            return oldname
        if self._renamed:
            # e.g. if notebook_1 was renamed to notebook_2, try notebook_1_panel_1 -> notebook_2_panel_1 first
            for old,new in self._renamed.items():
                if oldname.startswith(old):
                    newname = new + oldname[len(old):]
                    if not newname in self.have_names:
                        return newname

        newname = oldname
        if "_" in oldname:
            # if the old name ends with an underscore and a number, just increase the number
            try:
                template, i = oldname.rsplit("_", 1)
                i = int(i)
                template = template + '_%s'
                if self.parent is not None:
                    while newname in self.have_names:
                        newname = template%i
                        i += 1
                    self.have_names.add(newname)
                    return newname
                # top level, e.g. a new frame is pasted
                while newname in self.have_names:
                    newname = template%i
                    i += 1
                self.have_names.add(newname)
                return newname
            except:
                pass

        # add _copy or _copy_N to the old name
        if oldname.endswith('_copy'): oldname = oldname[:-5]
        i = 0
        if self.parent is not None:
            while newname in self.have_names:
                if not i:
                    newname = '%s_copy' % oldname
                else:
                    newname = '%s_copy_%s' % (oldname, i)
                i += 1
            self.have_names.add(newname)
            return newname

        # top level, e.g. a new frame is pasted
        while newname in self.have_names:
            if not i:
                newname = '%s_copy' % oldname
            else:
                newname = '%s_copy_%s' % (oldname, i)
            i += 1
        self.have_names.add(newname)
        return newname

    def startElement(self, name, attrs):
        renamed = None
        if name == 'object' and 'name' in attrs and common.widget_classes[attrs['base']].IS_NAMED:
            # generate a unique name for the copy
            oldname = str(attrs['name'])
            newname = self._get_new_name(oldname)
            attrs = _own_dict(attrs)
            attrs['name'] = newname
            if newname!=oldname:
                attrs['original_name'] = oldname  # for finding position in a virtual sizer
                self._renamed[oldname] = newname
                renamed = (oldname, newname)
        XmlWidgetBuilder.startElement(self, name, attrs)
        if renamed: self.top()._renamed = renamed
        if name == 'object':
            if not self.depth_level:
                common.app_tree.auto_expand = False
                try:
                    self.top_obj = self.top().obj
                except AttributeError:
                    logging.exception( _('Exception caused by obj: %s'), self.top_obj )
            self.depth_level += 1
            self._object_counter += 1

    def endElement(self, name):
        if name == 'label':
            # if e.g. a button_1 is copied to button_2, also change the label if it was "button_1"
            obj = self.top()
            renamed = getattr(obj, "_renamed", None)
            if renamed:
                oldname, newname = renamed
                if obj.obj.name==newname and self._curr_prop_val and self._curr_prop_val[0]==oldname:
                    self._curr_prop_val[0] = newname

        XmlWidgetBuilder.endElement(self, name)

        if name!="object": return
        self.depth_level -= 1

        if not self.depth_level:
            if self.parent:
                self.parent.on_load(child=self.top_obj)  # e.g. a GridBagSizer needs to check overlapped slots
            common.app_tree.auto_expand = True
            try:
                if self.parent and self.parent.widget:
                    self.top_obj.create()
            except AttributeError:
                logging.exception( _('Exception caused by obj: %s'), self.top_obj )


class XMLAttrs(dict):
    # raises XmlParsingError instead of KeyError
    def __getitem__(self, key):
        try:
            return dict.__getitem__(self, key)
        except KeyError:
            if 'name' in self:
                raise XmlParsingError( _("attribute %s missing from object '%s'")%(key, self['name']) )
            raise XmlParsingError( _("attribute %s missing from object")%key )


class XmlWidgetObject(object):
    "A class to encapsulate widget attributes read from a XML file, to store them until the widget can be created"

    def __init__(self, attrs, parser):
        attrs = XMLAttrs(attrs)

        self.prop_handlers = Stack()  # a stack of custom handler functions to set properties of this object

        self._properties_added = []
        base = attrs.get('base', None)
        klass = attrs['class']

        # find sizeritem, sizer, parent window
        sizeritem = sizer = parent = None
        stack = parser._objects[:]
        top = stack.pop(-1) if stack else None

        if top and isinstance(top.obj, Sizeritem):
            sizeritem = top.obj
            top = stack.pop(-1)

        if top and top.IS_SIZER:
            sizer = top.obj
            top = stack.pop(-1)
        if top and top.IS_WINDOW:
            parent = top.obj

        while parent is None and stack:
            top = stack.pop()
            if top.IS_WINDOW: parent = top.obj

        if parent is None:
            parent = common.root

        self.IS_SIZER = self.IS_WINDOW = self.IS_SLOT = self.IS_SIZERITEM = False
        if base is not None:
            # if base is not None, the object is a widget (or sizer), and not a sizeritem
            self.sizeritem = sizeritem  # the properties will be copied later in endElement

            index = getattr(sizeritem, 'index', None)
            if index is None and hasattr(parent, "get_itempos"):
                # splitters and notebooks don't use sizeritems around their items in XML; pos is found from the name
                index = parent.get_itempos(attrs)
            elif not sizeritem and not stack:
                index = parser.index

            # build the widget
            builder = common.widgets_from_xml.get(base, None)
            if builder is None: raise XmlParsingError("Widget '%s' not supported."%base)

            self.obj = builder(parser, base, attrs["name"], sizer or parent, index)
            self.set_class_attributes(parser, attrs) # set 'class' and 'instance_class' properties, if applicable

            self.IS_SIZER = self.obj.IS_SIZER
            self.IS_WINDOW = self.obj.IS_WINDOW

        elif klass == 'sizeritem':
            self.obj = Sizeritem()
            self.parent = parent
            self.IS_SIZERITEM = True

        elif klass == 'sizerslot':
            assert sizer is not None, _("malformed wxg file: sizer slots can only be inside sizers!")
            self.obj = None
            self.IS_SLOT = True
            sizer._add_slot(loading=True)

        elif klass == 'slot':
            # for a slot in a panel
            self.obj = None
            self.IS_SLOT = True
            assert parent.CHILDREN == -1
            parent._add_slot()

        # push the object on the _objects stack
        parser._objects.push(self)

    def set_class_attributes(self, parser, attrs):
        # old files: no 'instance_class' attribute, 'class' is 'multi purpose'
        CLASS = self.obj.__class__
        class_p = self.obj.properties.get("class")

        class_v = attrs.get("class") or None
        IS_BASE = class_v==CLASS.WX_CLASS or (CLASS.WX_CLASSES and class_v in CLASS.WX_CLASSES)

        instance_class = attrs.get("instance_class")

        if parser.check_input_file_version( (0,9,9) ):
            # handle backwards compatibility
            if class_p:
                if class_p.deactivated is not None:  # i.e. 'class' property is not mandatory / ClassPropertyD
                    if IS_BASE:
                        class_v = None
            elif class_v:
                if not IS_BASE or CLASS.WX_CLASS=="CustomWidget":
                    instance_class = class_v
                class_v = None
            if attrs.get("no_custom_class", None) in ('1',1):
                if class_v:
                    instance_class = class_v
                    class_v = None
        else:
            # current file format: 'class' is always written
            # if the object does not have a 'class' property at all, the value is the instance class
            # if it has a 'class' property, the 'instance_class' is written if required
            if class_p:
                if class_p.deactivated is not None and IS_BASE:
                    class_v = None
            else:
                if not IS_BASE and instance_class is None:
                    instance_class = class_v
                class_v = None

        # update self.obj properties
        modified = []
        if class_v:
            class_p.set( class_v, activate=True )
            modified.append("class")
        if instance_class:
            self.obj.properties["instance_class"].set( instance_class, activate=True )
            modified.append("instance_class")
        if modified:
            self.obj.properties_changed(modified)

    def add_property(self, name, val):
        """adds a property to this widget. This method is not called if there
        was a custom handler for this property, and its char_data method returned False"""
        if name=="no_custom_class": return  # this was also stored as attribute
        if name == 'pos':  # sanity check, this shouldn't happen...
            logging.debug('add_property(name=pos)')
            return
        try:
            prop = self.obj.properties[name]
        except KeyError:
            # unknown property for this object; issue a warning and ignore the property
            if config.debugging: raise
            logging.error( _("WARNING: Property '%s' not supported by this object ('%s') "), name, self.obj )
            return
        prop.load(val, activate=True)
        self._properties_added.append(name)

    def notify_owner(self):
        # notify owner about the added properties
        if not self._properties_added: return
        self.obj.properties_changed(self._properties_added)
        del self._properties_added[:]



class Stack(list):
    "Simple stack implementation"
    def push(self, elem):
        self.append(elem)
    def top(self):
        return self and self[-1] or None
    def count(self):
        return len(self)



import new_properties as np

class Sizeritem(np.PropertyOwner):
    "temporarily represents a child of a sizer"
    SIZER_PROPERTIES = ["proportion","span","border","flag"]
    def __init__(self):
        np.PropertyOwner.__init__(self)
        self.proportion = np.LayoutProportionProperty(0)
        self.span = np.LayoutSpanProperty((1,1))
        self.border = np.SpinProperty(0)
        self.flag = np.ManagedFlags(None, name="sizeritem_flags")

    def on_load(self, child=None):
        pass