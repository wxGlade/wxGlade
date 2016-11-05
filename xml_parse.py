"""
Parsers used to load an app and to generate the code from a XML file.

See L{wcodegen.taghandler} for custom tag handler base classes.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
from xml.sax import SAXException, make_parser
from xml.sax.handler import ContentHandler

import common, config, compat
import edit_sizers

if config.use_gui:
    import wx


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
        self._logger = logging.getLogger(self.__class__.__name__)
        self._objects = Stack()      # Stack of 'alive' objects
        self._windows = Stack()      # Stack of window objects (derived by wxWindow)
        self._sizers = Stack()       # Stack of sizer objects
        self._sizer_item = Stack()   # Stack of sizer items
        self._curr_prop = None       # Name of the current property
        self._curr_prop_val = []     # Value of the current property; strings, to be joined
        self._appl_started = False
        self.top = self._objects.top
        self.parser = make_parser()
        self.parser.setContentHandler(self)
        self.locator = None # Document locator

    def parse(self, source):
        # Permanent workaround for Python bug "Sax parser crashes if given
        # unicode file name" (http://bugs.python.org/issue11159).
        #
        # This bug causes a UnicodeEncodeError if the SAX XML parser wants to store an unicode filename internally.
        #
        # That's not a general file handling issue because the parameter source is an open file already.
        source = compat.StringIO(source.read())
        self.parser.parse(source)
        source.close()

    def parse_string(self, source):
        #source = compat.StringIO(source)
        source = compat.BytesIO(source)
        self.parser.parse(source)
        source.close()

    def setDocumentLocator(self, locator):
        self.locator = locator
        XmlParsingError.locator = locator

    def startElement(self, name, attrs):
        raise NotImplementedError

    def endElement(self, name, attrs):
        raise NotImplementedError

    def characters(self, data):
        raise NotImplementedError

    def pop(self):
        try:
            return self._objects.pop().pop()
        except AttributeError:
            return None

    def _process_app_attrs(self, attrs):
        "Process attributes of the application tag; Check only existence of attributes not the logical correctness"
        res = {}

        res['encoding'] = self._get_encoding(attrs)

        for_version = attrs.get('for_version', '%s.%s' % config.for_version)
        for_version_tuple = tuple([int(t) for t in for_version.split('.')[:2]])
        if for_version_tuple < (2, 8):
            logging.warning( _('The loaded wxGlade designs are created for wxWidgets "%s", '
                               'but this version is not supported anymore.'),
                             for_version )
            logging.warning( _('The designs will be loaded and converted to wxWidgets "%s" partially. '
                               'Please check the designs carefully.'),
                            '%s.%s' % config.for_version )
            for_version = '%s.%s' % config.for_version
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

        res['indent_symbol'] = attrs.get('indent_symbol', config.default_indent_symbol)

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
            logging.warning(
                _('The loaded wxGlade designs are created to use the '
                  'old Python import style ("from wxPython.wx '
                  'import *)". The old import style is not supported '
                  'anymore.')
            )
            logging.warning(
                _('The designs will be loaded and the import style will '
                  'be converted to new style imports ("import wx"). '
                  'Please check your design carefully.')
            )
            # no update necessary - the attribute will not be used anymore

        return res

    def _get_encoding(self, attrs):
        "Return the document encoding; attrs: Object attributes"
        encoding = attrs.get('encoding', config.default_encoding)
        if encoding:
            try:
                'a'.encode(encoding)
            except LookupError:
                self._logger.warning( _('Unknown encoding "%s", fallback to default encoding "%s"'),
                                      encoding, config.default_encoding)
                encoding = config.default_encoding
        return encoding



class XmlWidgetBuilder(XmlParser):
    "Parser used to build the tree of widgets from a given XML file"

    def __init__(self):
        XmlParser.__init__(self)

    def startElement(self, name, attrs):
        if name == 'application':
            # get properties of the app
            self._appl_started = True
            attrs = self._process_app_attrs(attrs)
            app = common.app_tree.app
            p = app.properties

            p["encoding"].set( attrs['encoding'] )
            p["output_path"].set( attrs['path'] )
            p["class"].set( attrs['class'], activate=bool(attrs.get("class")) )
            p["name"].set( attrs['name'], activate=bool(attrs.get("name")) )
            p["multiple_files"].set( attrs['option'] )
            p["language"].set( attrs['language'] )
            p["top_window"].set( attrs['top_window'] )
            p["use_gettext"].set( attrs['use_gettext'] )
            p["is_template"].set( attrs['is_template'] )
            p["overwrite"].set( attrs['overwrite'] )
            p["indent_mode"].set( attrs['indent_symbol'] )
            p["indent_amount"].set( attrs['indent_amount'] )
            p["for_version"].set( attrs['for_version'] )

            modified = ["encoding", "output_path", "class", "name", "multiple_files", "language", "top_window",
                        "use_gettext", "use_gettext", "is_template", "overwrite", "indent_mode", "indent_amount",
                        "for_version"]

            source_extension = attrs['source_extension']
            if source_extension and source_extension[0] == '.':
                p["source_extension"].set( source_extension[1:] )
                modified.append( "source_extension" )
                
            header_extension = attrs['header_extension']
            if header_extension and header_extension[0] == '.':
                p["header_extension"].set( header_extension[1:] )
                modified.append( "header_extension" )

            app.properties_changed(modified)
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
            return
        if name == 'object':
            # remove last object from Stack
            obj = self.pop()
            obj.notify_owner()
            if obj.klass in ('sizeritem', 'sizerslot'):
                return
            si = self._sizer_item.top()
            if si is not None and si.parent == obj.parent:
                obj.obj.copy_properties( si.obj, ("option","flag","border") )
        else:
            # end of a property or error
            # case 1: set _curr_prop value
            data = "".join(self._curr_prop_val)
            if data:
                try:
                    handler = self.top().prop_handlers.top()
                    if not handler or handler.char_data(data):
                        # if char_data returned False, we don't have to call add_property
                        self.top().add_property(self._curr_prop, data)
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
            except AttributeError:
                pass
            self._curr_prop = None
            self._curr_prop_val = []

    def characters(self, data):
        if not data or data.isspace():
            return
        if self._curr_prop is None:
            raise XmlParsingError(_("Character data can be present only inside properties"))
        self._curr_prop_val.append(data)



class ProgressXmlWidgetBuilder(XmlWidgetBuilder):
    "Adds support for a progress dialog to the widget builder parser"

    def __init__(self, *args, **kwds):
        self.input_file = kwds.get('input_file')
        if self.input_file:
            del kwds['input_file']
            self.size = len(self.input_file.readlines())
            self.input_file.seek(0)
            self.progress = wx.ProgressDialog( _("Loading..."), _("Please wait while loading the app"), 20 )
            self.step = 4
            self.i = 1
        else:
            self.size = 0
            self.progress = None
        XmlWidgetBuilder.__init__(self)

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
                self.progress.Update(value)
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



class ClipboardXmlWidgetBuilder(XmlWidgetBuilder):
    """\
    Parser used to cut&paste widgets.

    The differences with XmlWidgetBuilder are:
      - No <application> tag in the piece of xml to parse
      - Fake parent, sizer and sizeritem objects to push on the three stacks:
        they keep info about the destination of the hierarchy of widgets (i.e.
        the target of the 'paste' command)
      - The first widget built must be hidden and shown again at the end of
        the operation
    """

    def __init__(self, parent, sizer, pos, proportion, flag, border):
        XmlWidgetBuilder.__init__(self)
        if parent is not None:
            self.parent_node = parent.node
        else:
            # e.g. a frame is pasted
            self.parent_node = None
            self.have_names = set()
            # update with the top level names
            for node in common.app_tree.names.keys():
                self.have_names.add( node.widget.name )

        class XmlClipboardObject(object):
            def __init__(self, **kwds):
                self.__dict__.update(kwds)
            def notify_owner(self):
                pass

        # fake parent window object
        fake_parent = XmlClipboardObject(obj=parent, parent=parent)

        # fake sizer object
        if sizer:
            fake_sizer = XmlClipboardObject(obj=sizer, parent=parent)
            sizeritem = Sizeritem()
            sizeritem.properties["proportion"].set(proportion)
            sizeritem.properties["flag"].set(flag)
            sizeritem.properties["border"].set(border)
            sizeritem.properties["pos"].set(pos)
            # fake sizer item
            fake_sizeritem = XmlClipboardObject(obj=sizeritem, parent=parent)

        # push the fake objects on the stacks
        self._objects.push(fake_parent)
        self._windows.push(fake_parent)
        if sizer:
            self._objects.push(fake_sizer)
            self._sizers.push(fake_sizer)
            self._objects.push(fake_sizeritem)
            self._sizer_item.push(fake_sizeritem)

        self.depth_level = 0
        self._appl_started = True  # no application tag when parsing from the clipboard

    def _get_new_name(self, oldname):
        newname = oldname
        if "_" in oldname:
            # if the old name ends with an underscore and a number, just increase the number
            try:
                template, i = oldname.rsplit("_", 1)
                #i = int(i) + 1
                i = int(i)
                template = template + '_%s'
                if self.parent_node is not None:
                    while common.app_tree.has_name(newname, node=self.parent_node):
                        newname = template%i
                        i += 1
                    return newname
                # top level, e.g. a new frame is pasted
                while newname in self.have_names:
                    newname = template%i
                    i += 1
                self.have_names.add(newname)
                return newname
            except:
                pass


        # add _copy to the old name
        i = 0
        if self.parent_node is not None:
            while common.app_tree.has_name(newname, node=self.parent_node):
                if not i:
                    newname = '%s_copy' % oldname
                else:
                    newname = '%s_copy_%s' % (oldname, i)
                i += 1
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
        if name == 'object' and 'name' in attrs:
            # generate a unique name for the copy
            oldname = str(attrs['name'])
            newname = self._get_new_name(oldname)
            attrs = dict(attrs)
            attrs['name'] = newname
        XmlWidgetBuilder.startElement(self, name, attrs)
        if name == 'object':
            if not self.depth_level:
                common.app_tree.auto_expand = False
                try:
                    self.top_obj = self.top().obj
                except AttributeError:
                    self._logger.exception( _('Exception caused by obj: %s'), self.top_obj )
            self.depth_level += 1

    def endElement(self, name):
        if name == 'object':
            obj = self.top()
            self.depth_level -= 1
            if not self.depth_level:
                common.app_tree.auto_expand = True
                import misc
                try:
                    # show the first object and update its layout
                    #if self.top_obj.node.parent.widget.is_visible():
                    #    common.app_tree.show_widget(self.top_obj.node)
                    if self.top_obj.node.parent.widget.widget:
                        self.top_obj.create()
                except AttributeError:
                    self._logger.exception( _('Exception caused by obj: %s'), self.top_obj )
                misc.set_focused_widget(self.top_obj)

        XmlWidgetBuilder.endElement(self, name)



class XmlWidgetObject(object):
    "A class to encapsulate widget attributes read from a XML file, to store them until the widget can be created"

    def __init__(self, attrs, parser):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        self.prop_handlers = Stack()  # a stack of custom handler functions to set properties of this object
        self.parser = parser
        self.in_sizers = False   # if True, the widget is     a sizer, opposite of 'in_windows'
        self.in_windows = False  # if True, the widget is not a sizer, opposite of 'in_sizers'
        self._properties_added = []
        try:
            base = attrs.get('base', None)
            self.klass = attrs['class']
        except KeyError:
            raise XmlParsingError(_("'object' items must have a 'class' attribute"))

        if base is not None:
            # if base is not None, the object is a widget (or sizer), and not a sizeritem
            sizer  = self.parser._sizers.top()
            parent = self.parser._windows.top()
            if parent is not None:
                parent = self.parent = parent.obj
            else:
                self.parent = None
            sizeritem = self.parser._sizer_item.top()
            if sizeritem is not None:
                sizeritem = sizeritem.obj
            if sizer is not None:
                # we must check if the sizer on the top of the stack is really the one we are looking for: to check this
                if sizer.parent != parent:
                    sizer = None
                else:
                    sizer = sizer.obj

            pos = getattr(sizeritem, 'pos', None)

            if parent and hasattr(parent, 'virtual_sizer') and parent.virtual_sizer:
                sizer = parent.virtual_sizer
                sizer.node = parent.node
                sizeritem = Sizeritem()
                if pos is None:
                    pos = sizer.get_itempos(attrs)

            # build the widget
            if pos is not None: pos = int(pos)
            self.obj = common.widgets_from_xml[base](attrs, parent, sizer, sizeritem, pos)
            p = self.obj.properties.get("class")
            if p: p.set(self.klass)

            # push the object on the appropriate stack
            if isinstance(self.obj, edit_sizers.SizerBase):
                self.parser._sizers.push(self)
                self.in_sizers = True
            else:
                self.parser._windows.push(self)
                self.in_windows = True

        elif self.klass == 'sizeritem':
            self.obj = Sizeritem()
            self.parent = self.parser._windows.top().obj
            self.parser._sizer_item.push(self)

        elif self.klass == 'sizerslot':
            sizer = self.parser._sizers.top().obj
            assert sizer is not None, _("malformed wxg file: slots can only be inside sizers!")
            sizer._add_slot()
            sizer.layout()
            self.parser._sizer_item.push(self)

        # push the object on the _objects stack
        self.parser._objects.push(self)

    def pop(self):
        if self.in_windows:
            return self.parser._windows.pop()
        elif self.in_sizers:
            return self.parser._sizers.pop()
        else:
            return self.parser._sizer_item.pop()

    def add_property(self, name, val):
        """adds a property to this widget. This method is not called if there
        was a custom handler for this property, and its char_data method returned False"""
        if name == 'pos':  # sanity check, this shouldn't happen...
            self._logger.debug('add_property(name=pos)')
            return
        prop = self.obj.properties[name]
        try:
            prop.load(val, activate=True)
        except KeyError:
            # unknown property for this object; issue a warning and ignore the property
            import os
            if 'WINGDB_ACTIVE' in os.environ: raise
            self._logger.error( _("WARNING: Property '%s' not supported by this object ('%s') "), name, self.obj )
        #self.obj.properties_changed([name])
        self._properties_added.append(name)
    def notify_owner(self):
        # notify owner about the added properties
        if not self._properties_added: return
        self.obj.properties_changed(self._properties_added)
        del self._properties_added[:]



class CodeWriter(XmlParser):
    "Parser used to produce the source from a given XML file"

    def __init__(self, writer, input, from_string=False, out_path=None, preview=False, class_names=None):
        """
        writer: Language specific code writer; stored in L{self.code_writer}; wxglade.codegen.BaseLangCodeWriter
        input: Name of file to load or XML document as a String
        from_string: True if L{input} is a XML document as String
        out_path: Override the output path specified in the XML document; stored in L{self.out_path}, string or None

        preview: True to generate code for the preview"""
        self._logger = logging.getLogger(self.__class__.__name__)

        XmlParser.__init__(self)
        self._toplevels = Stack() # Toplevel objects, i.e. instances of a custom class
        self.app_attrs = {}       # Attributes of the app (name, class, top_window)
        self.top_window = ''      # Class name of the top window of the app (if any)
        self.out_path = out_path  # Override the output path specified in the XML document
        self.code_writer = writer # Language specific code writer; wxglade.codegen.BaseLangCodeWriter instance
        self.preview = preview    # True to generate code for the preview

        # used in the CustomWidget preview code, to generate better previews (see widgets/custom_widget/codegen.py)
        self.class_names = class_names
        if self.class_names is None:
            self.class_names = set()

        if from_string:
            self.parse_string(input)
        else:
            inputfile = None
            try:
                inputfile = open(input)
                self.parse(inputfile)
            finally:
                if inputfile:
                    inputfile.close()

    def startElement(self, name, attrs):
        # check only existence of attributes not the logical correctness
        if name == 'application':
            # get properties of the app
            self._appl_started = True
            attrs = self._process_app_attrs(attrs)
            self.app_attrs = attrs

            attrs['for_version'] = attrs['for_version']
            attrs['option'] = attrs['option']

            if self.out_path is None:
                out_path = attrs['path']
                if out_path:
                    self.out_path = attrs['path']
                else:
                    raise XmlParsingError(_("'path' attribute missing: could not generate code"))
            else:
                attrs['path'] = self.out_path

            # Prevent empty output path
            if not self.out_path:
                raise XmlParsingError( _("'path' attribute empty: could not generate code") )

            # Initialize the writer, thereby a logical check will be performed
            self.code_writer.new_project(**attrs)

            return

        if not self._appl_started:
            raise XmlParsingError( _("the root of the tree must be <application>") )
        if name == 'object':
            # create the object and push it on the appropriate stacks
            CodeObject(attrs, self, preview=self.preview)
            if 'name' in attrs and attrs['name'] == self.app_attrs.get('top_window', ''):
                self.top_window = attrs['class']
        else:
            # handling of the various properties
            try:
                # look for a custom handler to push on the stack
                obj = self.top()
                handler = self.code_writer.get_property_handler(name, obj.base)
                if handler:
                    obj.prop_handlers.push(handler)
                # get the top custom handler and use it if there's one
                handler = obj.prop_handlers.top()
                if handler:
                    handler.start_elem(name, attrs)
            except AttributeError:
                self._logger.exception(_('Internal error'))
            self._curr_prop = name

    def endElement(self, name):
        if name == 'application':
            self._appl_started = False
            if self.app_attrs:
                self.code_writer.add_app(self.app_attrs, self.top_window)
            # call the finalization function of the code writer
            self.code_writer.finalize()
            return
        if name == 'object':
            # remove last object from Stack
            obj = self.pop()
            if obj.klass=='sizeritem':
                # nothing to do; the object inside was added already
                return

            if obj.klass=='sizerslot':  # no sizeritem required
                topl = self._toplevels.top()
                self.code_writer.add_object(topl, obj)
                szr = self._sizers.top()
                self.code_writer.add_sizeritem(topl, szr, obj, 0, "", 0)  # XXX maybe, grow and expand would be better
                return

            # at the end of the object, we have all the information to add it to its toplevel parent,
            # or to generate the code for the custom class
            if obj.is_toplevel and not obj.in_sizers:
                self.code_writer.add_class(obj)
            topl = self._toplevels.top()
            if topl:
                self.code_writer.add_object(topl, obj)
                # if the object is not a sizeritem, check whether it belongs to some sizer
                #  (in this case, self._sizer_item.top() doesn't return None):
                # if so, write the code to add it to the sizer at the top of the stack
                si = self._sizer_item.top()
                if si is not None and si.parent == obj.parent:
                    szr = self._sizers.top()
                    if not szr:
                        return
                    flag = si.obj.properties["flag"].get_string_value()  # as string, joined with "|"
                    self.code_writer.add_sizeritem(topl, szr, obj, si.obj.proportion, flag, si.obj.border)
        else:
            # end of a property or error
            # case 1: set _curr_prop value
            data = "".join(self._curr_prop_val)
            if data:
                try:
                    handler = self.top().prop_handlers.top()
                    if not handler or handler.char_data(data):
                        # if char_data returned False, we don't have to call add_property
                        self.top().add_property(self._curr_prop, data)
                except AttributeError:
                    raise

            # case 2: call custom end_elem handler
            try:
                # if there is a custom handler installed for this property, call its end_elem function:
                # if this returns True, remove the handler from Stack
                obj = self.top()
                handler = obj.prop_handlers.top()
                if handler.end_elem(name, obj):
                    obj.prop_handlers.pop()
            except AttributeError:
                pass
            self._curr_prop = None
            self._curr_prop_val = []

    def characters(self, data):
        if not data or data.isspace():
            return
        if self._curr_prop is None:
            raise XmlParsingError(_("Character data can be present only inside properties"))
        self._curr_prop_val.append(data)



class CodeObject(object):
    "A class to store information needed to generate the code for a given object"

    def __init__(self, attrs, parser, preview=False):
        self._logger = logging.getLogger(self.__class__.__name__) # initialise instance logger

        self.parser = parser
        self.in_sizers  = False       # If True, the widget is a sizer,     opposite of 'in_windows'
        self.in_windows = False       # If True, the widget is not a sizer, opposite of 'in_sizers'
        self.is_toplevel = False      # if True, the widget is a toplevel sizer or window (instance of a custom class)
        self.is_container = False     # If True, the widget is a container (frame, dialog, panel, ...)
        self.properties = {}          # Properties of the widget sizer
        self.prop_handlers = Stack()  # Is a stack of custom handler functions to set properties of this object
        self.preview = preview
        try:
            base = attrs.get('base', None)
            self.klass = attrs['class']
        except KeyError:
            raise XmlParsingError(_("'object' items must have a 'class' attribute"))
        self.parser._objects.push(self)
        self.parent = self.parser._windows.top()
        if self.parent is not None:
            self.parent.is_container = True
        self.base = None
        if base is not None:  # this is a ``real'' object, not a sizeritem
            self.name = attrs['name']
            self.base = common.class_names[base]
            can_be_toplevel = base in common.toplevels
            if (self.parent is None or self.klass != self.base) and can_be_toplevel:
                self.is_toplevel = True
                # for panel objects, if the user sets a custom class but (s)he doesn't want the code to be generated...
                if int(attrs.get('no_custom_class', False)) and not self.preview:
                    self.is_toplevel = False
                else:
                    self.parser._toplevels.push(self)
            elif self.preview and not can_be_toplevel and self.base != 'CustomWidget':
                # if this is a custom class, but not a toplevel one, for the preview we have to use the "real" class
                # CustomWidgets handle this in a special way (see widgets/custom_widget/codegen.py)
                self.klass = self.base

            # temporary hack: to detect a sizer, check whether the name of its class contains the string 'Sizer'
            # TODO: find a better way!!
            if base.find('Sizer') != -1:
                self.in_sizers = True
                if not self.parser._sizers.count():
                    self.is_toplevel = True
                else:
                    # the sizer is a toplevel one if its parent has not a sizer yet
                    sz = self.parser._sizers.top()
                    if sz.parent != self.parent:
                        self.is_toplevel = True
                self.parser._sizers.push(self)
            else:
                self.parser._windows.push(self)
                self.in_windows = True
        else:  # the object is a sizeritem or a slot
            self.obj = Sizeritem()
            if self.klass == "sizerslot":
                self.base = "sizerslot"
                self.name = "sizerslot"
                self.is_slot = True
            self.obj.flag_s = '0'
            self.parser._sizer_item.push(self)

    def __str__(self):
        return "<xml_code_object: %s, %s, %s>" % (self.name, self.base, self.klass)

    def add_property(self, name, value):
        if hasattr(self, 'obj'):  # self is a sizeritem
            try:
                self.obj.properties[name].set( value )
            except:
                raise XmlParsingError(_("property '%s' not supported by '%s' objects") % (name, self.klass))
        self.properties[name] = value

    def pop(self):
        if self.is_toplevel and not self.in_sizers:
            self.parser._toplevels.pop()
        if self.in_windows:
            return self.parser._windows.pop()
        elif self.in_sizers:
            return self.parser._sizers.pop()
        else:
            return self.parser._sizer_item.pop()



class Stack(object):
    "Simple stack implementation"

    def __init__(self):
        self._repr = []

    def push(self, elem):
        self._repr.append(elem)

    def pop(self):
        try:
            return self._repr.pop()
        except IndexError:
            return None

    def top(self):
        try:
            return self._repr[-1]
        except IndexError:
            return None

    def count(self):
        return len(self._repr)



import new_properties as np

class Sizeritem(np.PropertyOwner):
    "temporarily represents a child of a sizer"
    SIZER_PROPERTIES = ["pos","proportion","border","flag"]
    def __init__(self):
        np.PropertyOwner.__init__(self)
        self.proportion = np.SpinProperty(0, name="option")
        self.border = np.SpinProperty(0)
        self.flag = np.ManagedFlags(None)
        self.pos = np.SpinProperty(None)
