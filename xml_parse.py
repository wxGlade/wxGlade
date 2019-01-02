"""
Parsers used to load an app and to generate the code from a XML file.

See L{wcodegen.taghandler} for custom tag handler base classes.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
from xml.sax import SAXException, make_parser
from xml.sax.handler import ContentHandler

import time

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
        self._curr_prop = None       # Name of the current property
        self._curr_prop_val = []     # Value of the current property; strings, to be joined
        self._appl_started = False
        self.top = self._objects.top
        self.parser = make_parser()
        self.parser.setContentHandler(self)
        self.locator = None # Document locator

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
                self._logger.warning( _('Unknown encoding "%s", fallback to default encoding "%s"'),
                                      encoding, config.default_encoding)
                encoding = config.default_encoding
        return encoding



class XmlWidgetBuilder(XmlParser):
    "Parser used to build the tree of widgets from a given XML file"

    def __init__(self, filename=None, input_file_version=None):
        self.filename = filename
        self.input_file_version = input_file_version
        XmlParser.__init__(self)

    def startElement(self, name, attrs):
        if name == 'application':
            # get properties of the app
            self._appl_started = True
            attrs = self._process_app_attrs(attrs)
            app = common.app_tree.app
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
            app = common.app_tree.app
            for key, value in self._delayed_app_properties.items():
                app.properties[key].set( value )
            app.properties_changed( sorted(self._delayed_app_properties.keys()) )
            return
        if name == 'object':
            # remove last object from Stack
            obj = self._objects.pop()
            obj.notify_owner()
            if obj.klass in ('sizeritem', 'sizerslot'):
                return
            if obj.sizeritem:
                obj.obj.copy_properties( obj.sizeritem, ("option","flag","border","span") )
            obj.obj.on_load()
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
            self._last_progress_update = time.time()
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

    def __init__(self, parent, sizer, pos, proportion, span, flag, border):
        XmlWidgetBuilder.__init__(self)
        self._renamed = {}
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
        if parent and parent.is_sizer:
            fake_parent.in_sizers = True
            fake_parent.in_windows = False
        else:
            fake_parent.in_sizers = False
            fake_parent.in_windows = True

        self._objects.push(fake_parent)

        # fake sizer object
        if sizer:
            fake_sizer = XmlClipboardObject(obj=sizer, parent=parent)
            fake_sizer.in_windows = False
            fake_sizer.in_sizers = True
            sizeritem = Sizeritem()
            sizeritem.properties["proportion"].set(proportion)
            sizeritem.properties["span"].set(span)
            sizeritem.properties["flag"].set(flag)
            sizeritem.properties["border"].set(border)
            sizeritem.properties["pos"].set(pos)
            # fake sizer item
            fake_sizeritem = XmlClipboardObject(obj=sizeritem, parent=parent)
            fake_sizeritem.in_windows = False
            fake_sizeritem.in_sizers = False

            self._objects.push(fake_sizer)
            self._objects.push(fake_sizeritem)


        self.depth_level = 0
        self._appl_started = True  # no application tag when parsing from the clipboard

    def _get_new_name(self, oldname):
        if self._renamed:
            # e.g. if notebook_1 was renamed to notebook_2, try notebook_1_panel_1 -> notebook_2_panel_1 first
            for old,new in self._renamed.items():
                if oldname.startswith(old):
                    newname = new + oldname[len(old):]
                    if not common.app_tree.has_name(newname, node=self.parent_node):
                        return newname

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
        renamed = None
        if name == 'object' and 'name' in attrs:
            # generate a unique name for the copy
            oldname = str(attrs['name'])
            if not common.app_tree.has_name(oldname, node=self.parent_node):
                newname = oldname
            else:
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
                        self.top_obj.create_widgets()
                except AttributeError:
                    self._logger.exception( _('Exception caused by obj: %s'), self.top_obj )
                misc.set_focused_widget(self.top_obj)

        if name == 'label':
            # if e.g. a button_1 is copied to button_2, also change the label if it was "button_1"
            obj = self.top()
            renamed = getattr(obj, "_renamed", None)
            if renamed:
                oldname, newname = renamed
                if obj.obj.name==newname and self._curr_prop_val and self._curr_prop_val[0]==oldname:
                    self._curr_prop_val[0] = newname

        XmlWidgetBuilder.endElement(self, name)


class XmlWidgetObject(object):
    "A class to encapsulate widget attributes read from a XML file, to store them until the widget can be created"

    def __init__(self, attrs, parser):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        attrs.input_file_version = parser.input_file_version  # for handling backwards compatibility on loading

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

        # find sizeritem, sizer, parent window
        sizeritem = sizer = parent = None
        stack = self.parser._objects[:]
        top = stack.pop(-1) if stack else None

        if top and isinstance(top.obj, Sizeritem):
            sizeritem = top.obj
            top = stack.pop(-1)
        if top and top.in_sizers:
            sizer = top.obj
            top = stack.pop(-1)
        if top and top.in_windows:
            parent = top.obj
        while parent is None and stack:
            top = stack.pop()
            if top.in_windows: parent = top.obj

        if base is not None:
            # if base is not None, the object is a widget (or sizer), and not a sizeritem
            self.sizeritem = sizeritem

            pos = getattr(sizeritem, 'pos', None)
            # XXX change this: don't use pos here at all; either we're just appending to the end or filling virtual sizers acc. to pagenames
            #if pos is None and parent and hasattr(parent, 'virtual_sizer') and parent.virtual_sizer:
            if pos is None and parent and hasattr(parent, 'virtual_sizer') and parent.virtual_sizer:
                # virtual sizers don't use sizeritem objects around their items in XML; the index is found from the name
                sizer = parent.virtual_sizer
                sizer.node = parent.node
                sizeritem = Sizeritem()
                pos_ = sizer.get_itempos(attrs)
                # XXXinsert empty slots before, if required
                if pos_: pos = pos_  # when loading from a file, the names are set already and pos_ is not None

            # build the widget
            if pos is not None: pos = int(pos)
            builder = common.widgets_from_xml.get(base, None)
            if builder is None: raise XmlParsingError("Widget '%s' not supported."%base)
            self.obj = builder(attrs, parent, sizer, sizeritem, pos)
            p = self.obj.properties.get("class")
            if p:
                p.set(self.klass)
                common.app_tree.refresh(self.obj.node)

            if isinstance(self.obj, edit_sizers.SizerBase):
                self.in_sizers = True
            else:
                self.in_windows = True

        elif self.klass == 'sizeritem':
            self.obj = Sizeritem()
            self.parent = parent

        elif self.klass == 'sizerslot':
            assert sizer is not None, _("malformed wxg file: slots can only be inside sizers!")
            sizer._add_slot(loading=True)
            sizer.layout()

        # push the object on the _objects stack
        self.parser._objects.push(self)

    def add_property(self, name, val):
        """adds a property to this widget. This method is not called if there
        was a custom handler for this property, and its char_data method returned False"""
        if name == 'pos':  # sanity check, this shouldn't happen...
            self._logger.debug('add_property(name=pos)')
            return
        try:
            prop = self.obj.properties[name]
        except KeyError:
            # unknown property for this object; issue a warning and ignore the property
            if config.debugging: raise
            self._logger.error( _("WARNING: Property '%s' not supported by this object ('%s') "), name, self.obj )
            return
        prop.load(val, activate=True)
        #self.obj.properties_changed([name])
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
    SIZER_PROPERTIES = ["pos","proportion","span","border","flag"]
    def __init__(self):
        np.PropertyOwner.__init__(self)
        self.proportion = np.LayoutProportionProperty(0)
        self.span = np.LayoutSpanProperty((1,1))
        self.border = np.SpinProperty(0)
        self.flag = np.ManagedFlags(None)
        self.pos = np.SpinProperty(None)
