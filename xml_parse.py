"""
Parsers used to load an app and to generate the code from a XML file.

See L{wcodegen.taghandler} for custom tag handler base classes.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import StringIO
from xml.sax import SAXException, make_parser
from xml.sax.handler import ContentHandler

import common
import config
import edit_sizers

if config.use_gui:
    import wx


class XmlParsingError(SAXException):
    """\
    Custom exception to report problems during parsing
    """
    locator = None

    def __init__(self, msg):
        if self.locator:
            msg = _('%s (line: %s, column: %s)') % (
                msg,
                self.locator.getLineNumber(),
                self.locator.getColumnNumber(),
            )
        SAXException.__init__(self, msg)

# end of class XmlParsingError


class XmlParser(ContentHandler):
    """\
    'abstract' base class of the parsers used to load an app and to generate
    the code

    @ivar _curr_prop:  Name of the current property

    @ivar _curr_prop_val: Value of the current property (list into which the
                          various pieces of char data collected are inserted)

    @ivar _objects:    Stack of 'alive' objects

    @ivar _sizer_item: Stack of sizer items

    @ivar _sizers:     Stack of sizer objects

    @ivar _windows:    Stack of window objects (derived by wxWindow)

    @ivar locator:     Document locator

    @ivar _logger:     Instance specific logger
    """

    def __init__(self):
        self._logger = logging.getLogger(self.__class__.__name__)
        self._objects = Stack()
        self._windows = Stack()
        self._sizers = Stack()
        self._sizer_item = Stack()
        self._curr_prop = None
        self._curr_prop_val = []
        self._appl_started = False
        self.top = self._objects.top
        self.parser = make_parser()
        self.parser.setContentHandler(self)
        self.locator = None

    def parse(self, source):
        # Permanent workaround for Python bug "Sax parser crashes if given
        # unicode file name" (http://bugs.python.org/issue11159).
        #
        # This bug causes a UnicodeEncodeError if the SAX XML parser wants to
        # store an unicode filename internally.
        #
        # That's not a general file handling issue because the parameter
        # source is an open file already.
        source = StringIO.StringIO(source.read())
        self.parser.parse(source)
        source.close()

    def parse_string(self, source):
        source = StringIO.StringIO(source)
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
        """\
        Process attributes of the application tag

        Check only existence of attributes not the logical correctness

        @param attrs: Object attributes

        @rtype: dict
        """
        res = {}

        res['encoding'] = self._get_encoding(attrs)

        for_version = attrs.get('for_version', '%s.%s' % config.for_version)
        for_version_tuple = tuple([int(t) for t in for_version.split('.')[:2]])
        if for_version_tuple < (2, 8):
            logging.warning(
                _('The loaded wxGlade designs are created for wxWidgets '
                  '"%s", but this version is not supported anymore.'),
                for_version,
            )
            logging.warning(
                _('The designs will be loaded and converted to '
                  'wxWidgets "%s" partially. Please check the designs '
                  'carefully.'),
                '%s.%s' % config.for_version
            )
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

        res['indent_symbol'] = attrs.get('indent_symbol',
                                         config.default_indent_symbol)

        if 'language' in attrs:
            res['language'] = attrs['language']
        elif hasattr(self, 'code_writer'):
            res['language'] = attrs.get('language',
                                        self.code_writer.language)
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

        res['header_extension'] = attrs.get('header_extension',
                                            config.default_header_extension)
        res['source_extension'] = attrs.get('source_extension',
                                            config.default_source_extension)

        res['top_window'] = attrs.get('top_window')

        try:
            use_gettext = int(attrs["use_gettext"])
        except (KeyError, ValueError):
            use_gettext = config.default_use_gettext
        res['use_gettext'] = bool(use_gettext)

        if attrs.get('use_new_namespace') == u'0' and \
                        attrs.get('language') == 'python':
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
            # no update necessary - the attribute will not be used
            # anymore

        return res

    def _get_encoding(self, attrs):
        """\
        Return the document encoding

        @param attrs: Object attributes

        @rtype: str
        """
        encoding = attrs.get('encoding', config.default_encoding)
        if encoding:
            try:
                unicode('a', encoding)
            except LookupError:
                self._logger.warning(
                    _('Unknown encoding "%s", fallback to default encoding '
                      '"%s"'), encoding, config.default_encoding
                )
                encoding = config.default_encoding
        return encoding

# end of class XmlParser


class XmlWidgetBuilder(XmlParser):
    """\
    Parser used to build the tree of widgets from a given XML file
    """

    def __init__(self):
        XmlParser.__init__(self)
        self.top_window = ''

    def startElement(self, name, attrs):
        if name == 'application':
            # get properties of the app
            self._appl_started = True
            attrs = self._process_app_attrs(attrs)
            app = common.app_tree.app

            encoding = attrs['encoding']
            app.encoding = encoding
            app.encoding_prop.set_value(encoding)

            path = attrs['path']
            if path:
                app.output_path = path
                app.outpath_prop.set_value(path)

            klass = attrs['class']
            if klass:
                app.klass = klass
                app.klass_prop.toggle_active(True)
                app.klass_prop.set_value(klass)

            name = attrs['name']
            if name:
                app.name = name
                app.name_prop.toggle_active(True)
                app.name_prop.set_value(name)

            app.multiple_files = attrs['option']
            app.multiple_files_prop.set_value(attrs['option'])

            app.set_language(attrs['language'])

            top_window = attrs['top_window']
            if top_window:
                self.top_window = top_window

            app.use_gettext = attrs['use_gettext']
            app.use_gettext_prop.set_value(attrs['use_gettext'])

            app.is_template = attrs['is_template']

            app.overwrite = attrs['overwrite']
            app.overwrite_prop.set_value(attrs['overwrite'])

            indent_symbol = attrs['indent_symbol']
            if indent_symbol == 'space':
                app.indent_mode = 1
            elif indent_symbol == 'tab':
                app.indent_mode = 0
            app.indent_mode_prop.set_value(app.indent_mode)

            indent_amount = attrs['indent_amount']
            app.indent_amount = indent_amount
            app.indent_amount_prop.set_value(indent_amount)

            source_extension = attrs['source_extension']
            if source_extension and source_extension[0] == '.':
                app.source_ext = source_extension[1:]
                app.source_ext_prop.set_value(source_extension[1:])
            header_extension = attrs['header_extension']
            if header_extension and header_extension[0] == '.':
                app.header_ext = header_extension[1:]
                app.header_ext_prop.set_value(header_extension[1:])

            for_version = attrs['for_version']
            app.for_version = for_version
            app.for_version_prop.set_str_value(for_version)
            app.set_for_version(for_version)

            return

        if not self._appl_started:
            raise XmlParsingError(
                _("the root of the tree must be <application>")
                )
        if name == 'object':
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
            if hasattr(self, 'top_window'):
                common.app_tree.app.top_window = self.top_window
                common.app_tree.app.top_win_prop.SetStringSelection(
                    self.top_window)
            return
        if name == 'object':
            # remove last object from Stack
            obj = self.pop()
            if obj.klass in ('sizeritem', 'sizerslot'):
                return
            si = self._sizer_item.top()
            if si is not None and si.parent == obj.parent:
                sprop = obj.obj.sizer_properties
                # update the values
                sprop['option'].set_value(si.obj.option)
                sprop['flag'].set_value(si.obj.flag_str())
                sprop['border'].set_value(si.obj.border)
                # call the setter functions
                obj.obj['option'][1](si.obj.option)
                obj.obj['flag'][1](si.obj.flag_str())
                obj.obj['border'][1](si.obj.border)
        else:
            # end of a property or error
            # 1: set _curr_prop value
            data = "".join(self._curr_prop_val)
            if data:
                try:
                    handler = self.top().prop_handlers.top()
                    if not handler or handler.char_data(data):
                        # if char_data returned False,
                        # we don't have to call add_property
                        self.top().add_property(self._curr_prop, data)
                except AttributeError:
                    pass

            # 2: call custom end_elem handler
            try:
                # if there is a custom handler installed for this property,
                # call its end_elem function: if this returns True, remove
                # the handler from Stack
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
            raise XmlParsingError(
                _("Character data can be present only inside properties"))
        self._curr_prop_val.append(data)

# end of class XmlWidgetBuilder


class ProgressXmlWidgetBuilder(XmlWidgetBuilder):
    """\
    Adds support for a progress dialog to the widget builder parser
    """

    def __init__(self, *args, **kwds):
        self.input_file = kwds.get('input_file')
        if self.input_file:
            del kwds['input_file']
            self.size = len(self.input_file.readlines())
            self.input_file.seek(0)
            self.progress = wx.ProgressDialog(
                _("Loading..."),
                _("Please wait while loading the app"),
                20
                )
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
            else:
                if self.locator:
                    where = self.locator.getLineNumber()
                    value = int(round(where * 20.0 / self.size))
                else:
                    # we don't have any information, so we update the progress
                    # bar ``randomly''
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

    def parse_string(self, *args):
        try:
            XmlWidgetBuilder.parse_string(self, *args)
        finally:
            if self.progress:
                self.progress.Destroy()

# end of class ProgressXmlWidgetBuilder


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

    def __init__(self, parent, sizer, pos, option, flag, border):
        XmlWidgetBuilder.__init__(self)
        self.parent_node = parent.node

        class XmlClipboardObject(object):
            def __init__(self, **kwds):
                self.__dict__.update(kwds)

        # fake parent window object
        fake_parent = XmlClipboardObject(obj=parent, parent=parent)

        # fake sizer object
        if sizer:
            fake_sizer = XmlClipboardObject(obj=sizer, parent=parent)
            sizeritem = Sizeritem()
            sizeritem.option = option
            sizeritem.set_flag(flag)
            sizeritem.border = border
            sizeritem.pos = pos
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
        self._appl_started = True  # no application tag when parsing from the
                                   # clipboard

    def startElement(self, name, attrs):
        if name == 'object' and 'name' in attrs:
            # generate a unique name for the copy
            oldname = str(attrs['name'])
            newname = oldname
            i = 0
            while common.app_tree.has_name(newname, node=self.parent_node):
                if not i:
                    newname = '%s_copy' % oldname
                else:
                    newname = '%s_copy_%s' % (oldname, i)
                i += 1
            attrs = dict(attrs)
            attrs['name'] = newname
        XmlWidgetBuilder.startElement(self, name, attrs)
        if name == 'object':
            if not self.depth_level:
                common.app_tree.auto_expand = False
                try:
                    self.top_obj = self.top().obj
                except AttributeError:
                    self._logger.exception(
                        _('Exception caused by obj: %s'),
                        self.top_obj
                        )
            self.depth_level += 1

    def endElement(self, name):
        if name == 'object':
            obj = self.top()
            self.depth_level -= 1
            if not self.depth_level:
                common.app_tree.auto_expand = True
                try:
                    # show the first object and update its layout
                    common.app_tree.show_widget(self.top_obj.node)
                    self.top_obj.show_properties()
                    common.app_tree.select_item(self.top_obj.node)
                except AttributeError:
                    self._logger.exception(
                        _('Exception caused by obj: %s'),
                        self.top_obj
                        )
        XmlWidgetBuilder.endElement(self, name)

# end of class ClipboardXmlWidgetBuilder


class XmlWidgetObject(object):
    """\
    A class to encapsulate a widget read from a XML file

    Its purpose is to store various widget attributes until the widget can
    be created.

    @ivar in_sizers: If True, the widget is a sizer, opposite of L{in_windows}
    @type in_sizers: bool

    @ivar in_windows: If True, the widget is not a sizer, opposite of
                      L{in_sizers}
    @type in_windows: bool

    @ivar prop_handlers: Is a stack of custom handler functions to set
                         properties of this object

    @ivar _logger: Class specific logging instance
    """

    def __init__(self, attrs, parser):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        self.prop_handlers = Stack()
        self.parser = parser
        self.in_windows = self.in_sizers = False
        try:
            base = attrs.get('base', None)
            self.klass = attrs['class']
        except KeyError:
            raise XmlParsingError(_("'object' items must have a 'class' "
                                  "attribute"))

        if base is not None:
            # if base is not None, the object is a widget (or sizer), and
            # not a sizeritem
            sizer = self.parser._sizers.top()
            parent = self.parser._windows.top()
            if parent is not None:
                parent = self.parent = parent.obj
            else:
                self.parent = None
            sizeritem = self.parser._sizer_item.top()
            if sizeritem is not None:
                sizeritem = sizeritem.obj
            if sizer is not None:
                # we must check if the sizer on the top of the stack is
                # really the one we are looking for: to check this
                if sizer.parent != parent:
                    sizer = None
                else:
                    sizer = sizer.obj
            if hasattr(sizeritem, 'pos'):
                pos = sizeritem.pos
            else:
                pos = None

            if parent and hasattr(parent, 'virtual_sizer') and \
               parent.virtual_sizer:
                sizer = parent.virtual_sizer
                sizer.node = parent.node
                sizeritem = Sizeritem()
                if pos is None:
                    pos = sizer.get_itempos(attrs)

            # build the widget
            if pos is not None:
                pos = int(pos)
            self.obj = common.widgets_from_xml[base](attrs, parent, sizer,
                                                     sizeritem, pos)
            try:
                #self.obj.klass = self.klass
                self.obj.set_klass(self.klass)
                self.obj.klass_prop.set_value(self.klass)
            except AttributeError:
                pass

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
            assert sizer is not None, \
                _("malformed wxg file: slots can only be inside sizers!")
            sizer.add_slot()
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
        """\
        adds a property to this widget. This method is not called if there
        was a custom handler for this property, and its char_data method
        returned False
        """
        if name == 'pos':  # sanity check, this shouldn't happen...
            self._logger.debug('add_property(name=pos)')
            return
        try:
            # call the setter for this property
            setter = self.obj[name][1]
            setter(val)
            try:
                prop = self.obj.properties[name]
                prop.set_value(val)
                prop.toggle_active(True)
            except AttributeError:
                pass
        except KeyError:
            # unknown property for this object
            # issue a warning and ignore the property
            self._logger.error(
                _("WARNING: Property '%s' not supported by this "
                  "object ('%s') "),
                name,
                self.obj,
                )

#end of class XmlWidgetObject


class CodeWriter(XmlParser):
    """\
    Parser used to produce the source from a given XML file

    @ivar _toplevels: Toplevel objects, i.e. instances of a custom class

    @ivar app_attrs: Attributes of the app (name, class, top_window)
    @type app_attrs: dict

    @ivar code_writer: Language specific code writer
    @type code_writer: wxglade.codegen.BaseLangCodeWriter

    @ivar top_window: Class name of the top window of the app (if any)
    @type top_window: str

    @ivar out_path: Override the output path specified in the XML document
    @type out_path: str

    @ivar preview: True to generate code for the preview
    @type preview: bool

    @ivar _logger: Instance specific logger
    """

    def __init__(self, writer, input, from_string=False, out_path=None,
                 preview=False, class_names=None):
        """\
        @param writer: Language specific code writer; stored in
                       L{self.code_writer}
        @type writer:  wxglade.codegen.BaseLangCodeWriter

        @param input: Name of file to load or XML document as a String
        @type input:  str

        @param from_string: True if L{input} is a XML document as String
        @type from_string:  bool

        @param out_path: Override the output path specified in the XML
                         document; stored in L{self.out_path}
        @type out_path:  str | None

        @param preview: True to generate code for the preview
        @type preview:  bool
        """
        self._logger = logging.getLogger(self.__class__.__name__)
        # writer: object that actually writes the code
        XmlParser.__init__(self)
        self._toplevels = Stack()
        self.app_attrs = {}
        self.top_window = ''
        self.out_path = out_path
        self.code_writer = writer
        self.preview = preview
        # used in the CustomWidget preview code, to generate better previews
        # (see widgets/custom_widget/codegen.py)
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
                    raise XmlParsingError(_("'path' attribute missing: could "
                                            "not generate code"))
            else:
                attrs['path'] = self.out_path

            # Prevent empty output path
            if not self.out_path:
                raise XmlParsingError(
                    _("'path' attribute empty: could not generate code")
                    )

            # Initialize the writer, thereby a logical check will be performed
            self.code_writer.initialize(attrs)

            return

        if not self._appl_started:
            raise XmlParsingError(
                _("the root of the tree must be <application>")
                )
        if name == 'object':
            # create the object and push it on the appropriate stacks
            CodeObject(attrs, self, preview=self.preview)
            if 'name' in attrs and \
               attrs['name'] == self.app_attrs.get('top_window', ''):
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
            if obj.klass in ('sizeritem', 'sizerslot'):
                return
            # at the end of the object, we have all the information to add it
            # to its toplevel parent, or to generate the code for the custom
            # class
            if obj.is_toplevel and not obj.in_sizers:
                self.code_writer.add_class(obj)
            topl = self._toplevels.top()
            if topl:
                self.code_writer.add_object(topl, obj)
                # if the object is not a sizeritem, check whether it
                # belongs to some sizer (in this case,
                # self._sizer_item.top() doesn't return None): if so,
                # write the code to add it to the sizer at the top of
                # the stack
                si = self._sizer_item.top()
                if si is not None and si.parent == obj.parent:
                    szr = self._sizers.top()
                    if not szr:
                        return
                    self.code_writer.add_sizeritem(topl, szr, obj,
                                                   si.obj.option,
                                                   si.obj.flag_str(),
                                                   si.obj.border)
        else:
            # end of a property or error
            # 1: set _curr_prop value
            data = "".join(self._curr_prop_val)
            if data:
                try:
                    handler = self.top().prop_handlers.top()
                    if not handler or handler.char_data(data):
                        # if char_data returned False,
                        # we don't have to call add_property
                        self.top().add_property(self._curr_prop, data)
                except AttributeError:
                    raise

            # 2: call custom end_elem handler
            try:
                # if there is a custom handler installed for this property,
                # call its end_elem function: if this returns True, remove
                # the handler from Stack
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
            raise XmlParsingError(
                _("Character data can be present only inside properties"))
        self._curr_prop_val.append(data)

# end of class CodeWriter


class CodeObject(object):
    """\
    A class to store information needed to generate the code for a given
    object.

    @ivar in_sizers: If True, the widget is a sizer, opposite of L{in_windows}
    @type in_sizers: bool

    @ivar in_windows: If True, the widget is not a sizer, opposite of
                      L{in_sizers}
    @type in_windows: bool

    @ivar is_container: If True, the widget is a container (frame, dialog,
                        panel, ...)
    @type is_container: bool

    @ivar is_toplevel: If True, the object is a toplevel one: for window
                       objects, this means that they are instances of a
                       custom class, for sizers, that they are at the top
                       of the hierarchy.
    @type is_toplevel: bool

    @ivar properties: Properties of the widget sizer
    @type properties: dict

    @ivar prop_handlers: Is a stack of custom handler functions to set
                         properties of this object

    @ivar _logger: Class specific logging instance
    """

    def __init__(self, attrs, parser, preview=False):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        self.parser = parser
        self.in_windows = self.in_sizers = False
        self.is_toplevel = False
        self.is_container = False
        self.properties = {}
        self.prop_handlers = Stack()
        self.preview = preview
        try:
            base = attrs.get('base', None)
            self.klass = attrs['class']
        except KeyError:
            raise XmlParsingError(_("'object' items must have a 'class' "
                                  "attribute"))
        self.parser._objects.push(self)
        self.parent = self.parser._windows.top()
        if self.parent is not None:
            self.parent.is_container = True
        self.base = None
        if base is not None:  # this is a ``real'' object, not a sizeritem
            self.name = attrs['name']
            self.base = common.class_names[base]
            can_be_toplevel = base in common.toplevels
            if (self.parent is None or self.klass != self.base) and \
               can_be_toplevel:
                self.is_toplevel = True
                # for panel objects, if the user sets a custom class but (s)he
                # doesn't want the code to be generated...
                if int(attrs.get('no_custom_class', False)) and \
                   not self.preview:
                    self.is_toplevel = False
                    #self._logger.debug('OK:', str(self))
                    #self.in_windows = True
                    #self.parser._windows.push(self)
                else:
                    self.parser._toplevels.push(self)
            elif self.preview and not can_be_toplevel and \
                 self.base != 'CustomWidget':
                # if this is a custom class, but not a toplevel one,
                # for the preview we have to use the "real" class
                #
                # CustomWidgets handle this in a special way
                # (see widgets/custom_widget/codegen.py)
                self.klass = self.base

            # temporary hack: to detect a sizer, check whether the name
            # of its class contains the string 'Sizer': TODO: find a
            # better way!!
            if base.find('Sizer') != -1:
                self.in_sizers = True
                if not self.parser._sizers.count():
                    self.is_toplevel = True
                else:
                    # the sizer is a toplevel one if its parent has not a
                    # sizer yet
                    sz = self.parser._sizers.top()
                    if sz.parent != self.parent:
                        self.is_toplevel = True
                self.parser._sizers.push(self)
            else:
                self.parser._windows.push(self)
                self.in_windows = True
        else:  # the object is a sizeritem
            self.obj = Sizeritem()
            self.obj.flag_s = '0'
            self.parser._sizer_item.push(self)

    def __str__(self):
        return "<xml_code_object: %s, %s, %s>" % (self.name, self.base,
                                                  self.klass)

    def add_property(self, name, value):
        if hasattr(self, 'obj'):  # self is a sizeritem
            try:
                if name == 'flag':
                    self.obj.flag_s = value.strip()
                else:
                    setattr(self.obj, name, int(value))
            except:
                raise XmlParsingError(_("property '%s' not supported by "
                                        "'%s' objects") % (name, self.klass))
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

# end of class CodeObject


class Stack(object):
    """\
    Simple stack implementation
    """

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

# end of class Stack


class Sizeritem(object):
    """\
    Represents a child of a sizer.

    @ivar _logger: Class specific logging instance
    """
    if config.use_gui:
        flags = {
            'wxALL': wx.ALL,
            'wxEXPAND': wx.EXPAND,
            'wxALIGN_RIGHT': wx.ALIGN_RIGHT,
            'wxALIGN_BOTTOM': wx.ALIGN_BOTTOM,
            'wxALIGN_CENTER': wx.ALIGN_CENTER,
            'wxALIGN_CENTER_HORIZONTAL': wx.ALIGN_CENTER_HORIZONTAL,
            'wxALIGN_CENTER_VERTICAL': wx.ALIGN_CENTER_VERTICAL,
            'wxLEFT': wx.LEFT,
            'wxRIGHT': wx.RIGHT,
            'wxTOP': wx.TOP,
            'wxBOTTOM': wx.BOTTOM,
            'wxSHAPED': wx.SHAPED,
            'wxADJUST_MINSIZE': wx.ADJUST_MINSIZE,
            'wxFIXED_MINSIZE': wx.FIXED_MINSIZE,
        }

    all_border_flags = set(['wxLEFT', 'wxRIGHT', 'wxTOP', 'wxBOTTOM'])

    def __init__(self):
        self.option = 0
        self.border = 0
        self.flag = 0
        self.flag_set = set()

        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

    def __getitem__(self, name):
        def set_value(value):
            setattr(self, name, value)

        if name != 'flag':
            return None, set_value
        return None, self.set_flag

    def set_flag(self, value):
        """\
        Set flags

        @param value: Flags concatenated with '|'
        @type value: str | Unicode
        """
        self.flag_set = set(value.split("|"))

        # convert flags to integers
        value = 0
        for flag in self.flag_set:
            try:
                value += self.flags[flag]
            except KeyError:
                pass
        self.flag = value

    def flag_str(self):
        """\
        Returns the flag attribute as a string of tokens separated by a '|'
        (used during the code generation)

        @rtype: str
        """
        if hasattr(self, 'flag_s'):
            return self.flag_s

        try:
            if self.all_border_flags <= self.flag_set:
                self.flag_set -= self.all_border_flags
                self.flag_set.add('wxALL')

            if 'wxALL' in self.flag_set:
                self.flag_set -= self.all_border_flags

            tmp = '|'.join(sorted(self.flag_set))
        except:
            self._logger.exception('self.flags = %s, self.flag_set = %s',
                                   self.flags, repr(self.flag_set))
            raise

        if tmp:
            return tmp
        else:
            return '0'

# end of class Sizeritem
