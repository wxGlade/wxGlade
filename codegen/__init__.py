"""Common code used by all code generators

@copyright: 2011-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy, logging, os, os.path, random, re, sys, time

import common, config, compat, errors, misc
import new_properties as np
import wcodegen


def _replace_tag(lst, tag, content):
    ret = False
    add_line = False
    if not tag in lst and not tag.endswith("\n"):
        tag = tag + "\n"
        add_line = True
    while True:
        try:
            idx = lst.index(tag)
        except ValueError:
            return ret
        if isinstance(content, list):
            if add_line:
                lst[idx:idx+1] = content + ["\n"]
            else:
                lst[idx:idx+1] = content
        elif isinstance(content, compat.basestring):
            if add_line:
                lst[idx] = content + "\n"
            else:
                lst[idx] = content
        else:
            raise ValueError("Internal error")
        ret = True


class BaseSourceFileContent(object):
    """Keeps info about an existing file that has to be updated, to replace only
    the lines inside a wxGlade block, an to keep the rest of the file as it was

    Attributers to be defined in derived classes:
     rec_block_start:   Regexp to match the begin of a wxglade block
     rec_block_end:     Regexp to match the end of a wxGlade block
     rec_class_decl:    Regexp to match class declarations
     rec_event_handler: Regexp to match event handlers"""

    def __init__(self, name, code_writer):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        self.classes = {}                  # Classes declared in the file
        self.class_name = None             # Name of the current processed class
        self.content = []                  # Content of the source file, if it existed before this session of code gen.
        self.event_handlers = {}           # List of event handlers for each class
        self.name = name                   # Name of the file
        self.new_classes = []              # New classes to add to the file (they are inserted BEFORE the old ones)
        self.new_classes_inserted = False  # Flag if the placeholder for new classes has been inserted in file already
        self.code_writer = code_writer     # Reference to the parent code writer object (BaseLangCodeWriter instance)
        self.spaces = {}                   # Indentation level for each class

        self.nonce = code_writer.nonce
        self.out_dir = code_writer.out_dir
        self.multiple_files = code_writer.multiple_files
        if not self.content:
            self.build_untouched_content()

    def replace(self, tag, content):
        return _replace_tag(self.content, tag, content)

    def build_untouched_content(self):
        """Builds a string with the contents of the file that must be left as is, and replaces the wxGlade blocks
        with tags that in turn will be replaced by the new wxGlade blocks"""
        self.class_name = None
        self.new_classes_inserted = False

    def format_classname(self, class_name):
        "Format class name read from existing source file; may be overwritten in derived class"
        return class_name

    def is_end_of_class(self, line):
        "True if the line is the marker for class end"
        return line.strip().startswith('# end of class ')

    def is_import_line(self, line):
        "True if the line imports wx; may be overwritten in derived class"
        return False

    def _load_file(self, filename):
        "Load a file and return the content. The read source file will be decoded to unicode automatically."
        # Separated for debugging purposes
        lines = common._read_file(filename)

        encoding = self.code_writer.app_encoding
        if encoding:
            try:
                lines = [line.decode(encoding) for line in lines]
            except UnicodeDecodeError:
                raise errors.WxgReadSourceFileUnicodeError(filename)

        return lines

    def __getstate__(self):
        state = self.__dict__.copy()
        del state['_logger']
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)

        # re-initialise logger instance deleted from __getstate__
        self._logger = logging.getLogger(self.__class__.__name__)



class BaseWidgetHandler(object):
    "Interface the various code generators for the widgets must implement"

    import_modules = []  # List of modules to import (eg. ['use Wx::Grid;\n'])

    def __init__(self):
        "Initialise instance variables"
        self.import_modules = []

    def get_code(self, obj):
        """Handler for normal widgets (non-toplevel): returns 3 lists of strings, init, properties and layout,
        that contain the code for the corresponding methods of the class to generate"""
        return [], [], []

    def get_properties_code(self, obj):
        """Handler for the code of the set_properties method of toplevel objects.
        Returns a list of strings containing the code to generate"""
        return []

    def get_init_code(self, obj):
        """Handler for the code of the constructor of toplevel objects.
        Returns a list of strings containing the code to generate.
        Usually the default implementation is ok (i.e. there are no extra lines to add).
        The generated lines are appended at the end of the constructor."""
        return []

    def get_layout_code(self, obj):
        """Handler for the code of the do_layout method of toplevel objects.
        Returns a list of strings containing the code to generate.
        Usually the default implementation is ok (i.e. there are no extra lines to add)."""
        return []


class ClassLines(object):
    "Stores the lines of source code for a custom class"
    def __init__(self):
        self.child_order = []
        self.dependencies = {}    # Names of the modules this class depends on
        self.deps = []
        self.event_handlers = []  # Lines to bind events (see L{wcodegen.BaseWidgetWriter.get_event_handlers()})
        self.extra_code = []      # Extra code to output before this class
        self.done = False         # If True, the code for this class has already been generated
        self.init = []            # Lines of code to insert in the __init__ method (for children widgets)
        self.init_lines = {}
        self.layout = []          # Lines to insert in the __do_layout method
        self.parents_init = []    # Lines to insert in the __init__ for container widgets (panels, splitters, ...)
        self.props = []           # Lines to insert in the __set_properties method
        self.sizers_init = []     # Lines related to sizer objects declarations


class BaseLangCodeWriter(wcodegen.BaseCodeWriter):
    """Dictionary of objects used to generate the code in a given language.

    A code writer object *must* implement those interface and set those variables:
      - init_lang(), init_files(), finalize()
      - wcodegen.BaseLanguageMixin.language
      - add_app(), add_class(), add_object()
      - add_property_handler()
      - add_sizeritem(}
      - add_widget_handler()
      - generate_code_background(), generate_code_font(), generate_code_foreground()
      - generate_code_id()
      - generate_code_size()
      - format_generic_access()
      - _code_statements

    A code writer object *could* implement those interfaces and set those variables:
      - setup()
      - quote_str(), quote_path()
      - wcodegen.BaseLanguageMixin.cn()
      - wcodegen.BaseLanguageMixin.cn_f()


     app_name:                Application name
     app_encoding:            Encoding of the application; will be initialised with config.default_encoding
     app_filename:            File name to store the application start code within multi file projects

     app_mapping:             Default mapping of template variables for substituting in templates
                               (see lang_mapping, add_app())
     lang_mapping:            Language specific mapping of template variables for substituting in templates
                               (see app_mapping, add_app())

     for_version:             wx version we are generating code for (e.g. (2, 8) )
     blacklisted_widgets:     Don't add those widgets to sizers because they are not supported for the requested
                               wx version or there is no code generator available.

     classes:                 Dictionary that maps the lines of code of a class to the name of such class:
                               the lines are divided in 3 categories: '__init__', '__set_properties' and '__do_layout'
     dependencies:            Module dependencies of all classes

     header_lines:            Lines common to all the generated files (import of wxCL, ...)

     curr_tab:                Current indentation level
     indent_amount:           An indentation level is  indent_symbol*indent_amount;
                               will be initialised with config.default_indent_amount
     indent_symbol:           Character to use for indentation; will be initialised with L{config.default_indent_symbol}

     nonce:                   Random number used to be sure that the replaced tags in the sources are the right ones
                              (see BaseSourceFileContent, add_class and create_nonce)

     obj_builders:           "writers" for the various objects
     obj_properties:         "property writer" functions, used to set the properties of a toplevel object

     out_dir:                If not None, it is the directory inside which the output files are saved
     output_file:            Output string buffer for the code
     output_file_name:       Name of the output file
     multiple_files: If True, generate a file for each custom class

     previous_source:        If not None, it is an instance of BaseSourceFileContent that keeps info about the
                              previous version of the source to generate
     _app_added:             True after wxApp instance has been generated
     _current_extra_code:    Set of lines for extra code to add to the current file
     _current_extra_modules: Set of lines of extra modules to add to the current file

     _overwrite:             If True, overwrite any previous version of the source file instead of updating only
                              the wxGlade blocks;  will be initialised with config.default_overwrite

     _property_writers:      Dictionary of dictionaries of property handlers specific for a widget;
                              keys are the class names of the widgets
                              (E.g. _property_writers['wxRadioBox'] = {'choices', choices_handler})

     _textdomain:            gettext textdomain (see _use_gettext)
     _use_gettext:           If True, enable gettext support; will be initialised with config.default_use_gettext

     _widget_extra_modules:  Map of widget class names to a list of extra modules needed for the widget
                              (e.g. C{'wxGrid': 'from wxLisp.grid import *\\n'})."""

    _code_statements = {}
    """Language specific code templates for for small statements.

    The statements are stored in a dictionary. The property names are the keys.
    
    The keys may have one of two different extensions: 
     - "C{_<major version>X}"               e.g. "C{tooltip_3X}" to generate tooltips source code for wxWidgets 3.x
     - "C{_<major version><minor version>}" e.g. "C{wxcolour_28}" to generate source code for wxWidgets 2.8 only

    The extensions will be evaluated from most specific to generic.

    Example::
        >>> _code_statements = {
        ... 'disabled':    "<code sequence for all wxWidget versions>",
        ... 'wxcolour_28': "<code sequence for wxWidgets 2.8 only>",
        ... 'tooltip_3':   "<code sequence for wxWidgets 3.X only>" }

    The function _get_code_statement() handles the extensions properly and returns the requested template.

    see: _get_code_statement(), _generic_code(), generate_code_extraproperties()"""
    ClassLines = ClassLines

    classattr_always = [] # List of classes to store always as class attributes; see store_as_attr()
    class_separator = '' # Separator between class and attribute or between different name space elements;
    # E.g "." for Python or "->" for Perl.

    global_property_writers = {} # Custom handlers for widget properties
    indent_level_func_body = 1 # Indentation level for bodies of class functions.
    language_note = "" # Language specific notice written into every file header; newline sequence at the end; see save_file
    name_ctor = '' # Name of the constructor. E.g. "__init__" in Python or "new" in Perl.
    shebang = None # Shebang line, the first line of the generated main files; newline sequence at the end; see save_file
    SourceFileContent = None # Just a reference to the language specific instance of SourceFileContent

    ####################################################################################################################
    # code generation templates

    tmpl_encoding = None  # Template of the encoding notices; file encoding will be added to the output in save_file()
    tmpl_block_begin = '%(tab)s%(comment_sign)s begin wxGlade: %(klass)s%(class_separator)s%(function)s\n'

    tmpl_cfunc_end = ''        # Statement to add at the end of a class function. e.g. 'return $self;' for Perl.
    tmpl_class_end = ''        # Statement to add at the end of a class, with 'end of class' marker
    tmpl_class_end_nomarker='' # same without marker
    tmpl_ctor_call_layout = '' # Code add to the contructor to call '__do_layout()' and '__set_properties()'.


    # these statements differ between the various code generators:
    tmpl_func_empty = ''           # Statement for an empty function e.g. "pass" for Python or "return;" for perl
    tmpl_empty_string = '""'       # Template for an empty string.

    tmpl_name_do_layout = ''       # Name of the function __do_layout() in wxGlade begin tag
    tmpl_func_do_layout = ''       # Statement for the    __do_layout() function;
                                   #  -> see generate_code_do_layout():

    tmpl_func_event_stub = ''      # Statement for a event handler stub -> see generate_code_event_handler()_

    tmpl_name_set_properties = ''  # Name of the function __set_properties() in wxGlade begin tag
    tmpl_func_set_properties = ''  # Statement for the    __set_properties() function
                                   #  -> see generate_code_set_properties():


    # Template of the "generated by ..." message ;see create_generated_by(); save_file()
    tmpl_generated_by = "%(comment_sign)s %(generated_by)s\n%(comment_sign)s\n"

    # Template of the overwrite message in all standalone app files; see add_app
    tmpl_overwrite = "%(comment_sign)s This is an automatically generated file.\n" \
                     "%(comment_sign)s Manual changes will be overwritten without warning!\n\n"

    tmpl_sizeritem = ''           # Template for adding a widget to a sizer; see add_sizeritem()
    tmpl_spacersize = '(%s, %s)'  # Python and Lisp need the braces
    tmpl_style = ''               # Template for setting style in constructor; see _format_style()
    tmpl_toplevel_style = ''      # same for a toplevel object
    tmpl_style0 = ''              # same for style == 0
    tmpl_toplevel_style0 = ''     # same for style == 0

    # templates used by add_app():
    tmpl_appfile = None           # file header for standalone files with application start code
    tmpl_detailed = None          # detailed application start code without gettext support
    tmpl_gettext_detailed = None  # detailed application start code with gettext support
    tmpl_simple = None            # simplified application start code without gettext support
    tmpl_gettext_simple = None    # simplified application start code with gettext support

    _show_warnings = True  # Enable or disable printing of warning messages; see self.warning()

    def __init__(self):
        "Initialise only instance variables using there defaults"
        wcodegen.BaseCodeWriter.__init__(self)
        self.obj_builders = {}
        self.obj_properties = {}
        self._property_writers = {}
        self._init_vars()

    def _init_vars(self):
        """Set instance variables (back) to default values during class instantiation (__init__) and before
        loading new data (new_project())."""
        self.app_encoding = config.default_encoding
        self.app_filename = None
        self.app_mapping = {}
        self.app_name = None
        self.classes = {}
        self.curr_tab = 0
        self.dependencies = {}
        self.for_version = config.for_version
        self.header_lines = []
        self.indent_symbol = config.default_indent_symbol
        self.indent_amount = config.default_indent_amount
        self.is_template = 0
        self.blacklisted_widgets = {}
        self.lang_mapping = {}
        self.multiple_files = False

        # this is to be more sure to replace the right tags
        self.nonce = self.create_nonce()

        self.out_dir = None
        self.output_file_name = None
        self.output_file = None
        self.previous_source = None
        self._app_added = False
        self._current_extra_code = []
        self._current_extra_modules = {}
        self._overwrite = config.default_overwrite
        self._mark_blocks = True # YYY config.mark_blocks
        self._textdomain = 'app'
        self._use_gettext = config.default_use_gettext
        self._widget_extra_modules = {}

    def new_project(self, app, out_path=None, preview=False):
        "Initialise generic and language independent code generator settings; see init_lang(), init_files()"

        # set (most of) instance variables back to default values
        self._init_vars()

        # application name
        self.app_name = app.name
        if self.app_name:
            self.app_filename = '%s.%s' % ( self.app_name, self.default_extensions[0] )
            self._textdomain = self.app_name

        # file encoding
        self.app_encoding = app.encoding.upper() or config.default_encoding
        # wx doesn't like latin-1
        if self.app_encoding == 'latin-1':
            self.app_encoding = 'ISO-8859-1'

        # Indentation symbol and level based on the project options
        self.indent_symbol = app.properties["indent_mode"].get_string_value()
        if self.indent_symbol == 'tab':
            self.indent_symbol = '\t'
        elif self.indent_symbol == 'space':
            self.indent_symbol = ' '
        else:
            self.indent_symbol = config.default_indent_symbol
        self.indent_amount = app.indent_amount

        if preview:
            self.multiple_files = False
            self._overwrite = True
            self._mark_blocks = False
            self._use_gettext = False
        else:
            self.multiple_files = app.multiple_files
            self._overwrite = app.overwrite
            self._mark_blocks = True if self._overwrite else app.mark_blocks
            self._use_gettext = app.use_gettext

        if not preview:
            self.for_version = tuple([int(t) for t in app.for_version.split('.')[:2]])
        else:
            self.for_version = compat.version
        self.is_template = app.is_template

        if self.multiple_files:
            self.out_dir = out_path or config.default_output_path
        else:
            self.out_dir = out_path or config.default_output_file
        self.out_dir = os.path.normpath( os.path.expanduser(self.out_dir.strip()) )
        self.preview = preview

        self.init_lang(app)      # call initialisation of language specific settings
        self.check_values()            # check the validity of the set values
        self.init_files(self.out_dir)  # call initialisation of the file handling

    def init_lang(self, app_attrs):
        "Initialise language specific settings; overwrite this function in the derived class"
        raise NotImplementedError

    def init_files(self, out_path):
        "Initialise the file handling; outp_path: file or path, depending on self.multiple_files"
        # You may overwrite this function in the derived class
        if self.multiple_files:
            self.previous_source = None
            if not os.path.isdir(out_path):
                raise errors.WxgOutputPathIsNotDirectory(out_path)
            self.out_dir = out_path
        else:
            if os.path.isdir(out_path):
                raise errors.WxgOutputPathIsDirectory(out_path)
            if not self._overwrite and self._file_exists(out_path):
                # the file exists, we must keep all the lines not inside a
                # wxGlade block. NOTE: this may cause troubles if out_path is
                # not a valid source file, so be careful!
                self.previous_source = self.SourceFileContent(out_path, self)
            else:
                # if the file doesn't exist, create it and write the ``intro''
                self.previous_source = None
                self.output_file = []
                self.output_file_name = out_path
                self.output_file.extend( self.header_lines )
                self.output_file.append('<%swxGlade extra_modules>\n' % self.nonce)
                self.output_file.append('\n')
                self.output_file.append('<%swxGlade replace dependencies>\n' % self.nonce)
                self.output_file.append('<%swxGlade replace extracode>\n' % self.nonce)

    def output_file_replace(self, tag, content):
        _replace_tag(self.output_file, tag, content)

    def check_values(self):
        "Check the validity of the set application values. Raises exceptions for errors; see module errors"
        # XXX implement without exceptions
        # Check if the values of use_multiple_files and out_path agree
        if self.multiple_files:
            if not os.path.isdir(self.out_dir):
                raise errors.WxgOutputDirectoryNotExist(self.out_dir)
            if not os.access(self.out_dir, os.W_OK):
                raise errors.WxgOutputDirectoryNotWritable(self.out_dir)
        else:
            if os.path.isdir(self.out_dir):
                raise errors.WxgOutputPathIsDirectory(self.out_dir)
            directory = os.path.dirname(self.out_dir)
            if directory:
                if not os.path.isdir(directory):
                    raise errors.WxgOutputDirectoryNotExist(directory)
                if not os.access(directory, os.W_OK):
                    raise errors.WxgOutputDirectoryNotWritable(directory)

        # It's not possible to generate code from a template directly
        if self.is_template:
            raise errors.WxgTemplateCodegenNotPossible

    def _generate_code(self, node):
        # for anything except application.Application
        import tree
        obj = node.widget
        topl = self.toplevel

        if isinstance(node, tree.SlotNode):
            # empty sizer slot
            szr = self.sizers[-1]
            if not szr._IS_GRIDBAG:
                self.add_spacer(topl, szr, None)
            return
        elif node.widget.classname=="spacer":
            # add a spacer
            szr = self.sizers[-1]
            self.add_spacer(topl, szr, obj, obj.proportion, obj.properties["flag"].get_string_value(), obj.border )
            return

        obj.is_toplevel = node.is_toplevel  # this is from the position in the data structure

        if obj.is_sizer:
            self.sizers.append(obj)

        can_be_toplevel = obj.__class__.__name__ in common.toplevels

        old_class = old_base = None
        # XXX check for alternatives
        # classname is used only for 'EditTopLevelScrolledWindow' vs. 'EditTopLevelPanel'
        classname = getattr(obj, '_classname', obj.__class__.__name__)
        base = common.class_names[classname]
        if base!=obj.base:
            old_base = obj.base
            obj.base = base
        
        if (not self.toplevels or obj.klass != obj.base) and can_be_toplevel:
            obj.is_toplevel = True
            # for panel objects, if the user sets a custom class but (s)he doesn't want the code to be generated...
            if obj.check_prop("no_custom_class") and obj.no_custom_class and not self.preview:
                obj.is_toplevel = False
        elif self.preview and not can_be_toplevel and obj.base != 'CustomWidget':
            # if this is a custom class, but not a toplevel one, for the preview we have to use the "real" class
            # CustomWidgets handle this in a special way (see widgets/custom_widget/codegen.py)
            old_class = obj.properties["klass"].get()
            obj.properties["klass"].set(obj.base) # XXX handle this in a different way

        if obj.is_toplevel:  # XXX as long as generate_code is called with 
            self.toplevels.append(obj)

        # children first
        for c in node.children or []:
            self._generate_code(c)

        # clean up stacks
        if obj.is_toplevel: del self.toplevels[-1]
        if obj.is_sizer:    del self.sizers[-1]

        # then the item
        if obj.is_toplevel and not obj.is_sizer:  # XXX as long as generate_code is called with 
            self.add_class(obj)
        if self.toplevels:
            self.add_object(obj)

        # check whether the object belongs to some sizer; if applicable, add it to the sizer at the top of the stack
        parent = node.parent.widget
        if parent.is_sizer:
            flag = obj.properties["flag"].get_string_value()  # as string, joined with "|"
            self.add_sizeritem(topl, parent, obj, obj.proportion, flag, obj.border)

        # XXX handle this in a different way
        if old_class is not None:
            obj.properties["klass"].set(old_class)
        if old_base is not None:
            obj.base = old_base

    def generate_code(self, root, widget=None):
        # root must be application.Application instance for now
        self.class_names = {}
        self.toplevels = []
        self.sizers = []
        for c in root.children or []:
            if widget is not None and c is not widget.node: continue # for preview
            self._generate_code(c)
        #if isinstance(root, application.Application):
        topwin = [c.widget for c in root.children if c.widget.name==root.widget.top_window]
        if topwin:
            topwin = topwin[0]
        elif root.children:
            topwin = root.children[0].widget
        else:
            topwin = None
        self.add_app(root.widget, topwin)

    @property
    def toplevel(self):
        return self.toplevels and self.toplevels[-1] or None

    def finalize(self):
        "Code generator finalization function"
        if self.previous_source:
            # insert all the new custom classes inside the old file
            if self.previous_source.new_classes:
                code = "".join(self.previous_source.new_classes)
            else:
                code = ""
            self.previous_source.replace( '<%swxGlade insert new_classes>' % self.nonce, code )
            code = "".join(sorted(self._current_extra_modules.keys()))
            self.previous_source.replace( '<%swxGlade extra_modules>\n' % self.nonce, code )

            # module dependencies of all classes
            dep_list = sorted( self.dependencies.keys() )
            code = self._tagcontent('dependencies', dep_list)
            self.previous_source.replace( '<%swxGlade replace dependencies>' % self.nonce, code )

            # extra code (see the 'extracode' property of top-level widgets)
            code = self._tagcontent( 'extracode', self._current_extra_code )
            self.previous_source.replace( '<%swxGlade replace extracode>' % self.nonce, code )

            # now remove all the remaining <123415wxGlade ...> tags from the source:
            # this may happen if we're not generating multiple files, and one of the container class names is changed
            self._content_notfound( self.previous_source )

            self._remove_tag_re(self.previous_source, r'<%swxGlade event_handlers \w+>')

            # write the new file contents to disk
            content = "".join(self.previous_source.content)
            self.save_file( self.previous_source.name, content, content_only=True )

        elif not self.multiple_files:
            code = "".join(sorted(self._current_extra_modules.keys()))
            self.output_file_replace( '<%swxGlade extra_modules>\n' % self.nonce, code )

            # module dependencies of all classes
            dep_list = sorted( self.dependencies.keys() )
            code = self._tagcontent('dependencies', dep_list)
            self.output_file_replace( '<%swxGlade replace dependencies>' % self.nonce, code )

            # extra code (see the 'extracode' property of top-level widgets)
            code = self._tagcontent('extracode', self._current_extra_code)
            self.output_file_replace( '<%swxGlade replace extracode>' % self.nonce, code )

            self.save_file(self.output_file_name, self.output_file, self._app_added)
            self.output_file = None

    def clean_up(self, root):
        pass

    def add_app(self, app, top_win):
        """Generates the code for a wxApp instance.
        If the file to write into already exists, this function does nothing.

        If gettext support is requested and there is not template with gettext support but there is a template without
        gettext support, template without gettext support will be used.

        This fallback mechanism works bidirectional.

        L{app_mapping} will be reset to default values and updated with L{lang_mapping}.

        see: tmpl_appfile, tmpl_detailed, tmpl_gettext_detailed, tmpl_simple, tmpl_gettext_simple, app_mapping,
             lang_mapping"""

        if not self.multiple_files:
            prev_src = self.previous_source
        else:
            # overwrite apps file always
            prev_src = None

        # do nothing if the file exists
        if prev_src:
            return

        klass = app.klass

        # top window and application name are mandatory
        if not app.top_window:
            return
        
        # get and fill language dependent template
        tmpl = self._get_app_template(app, top_win)
        if not tmpl: return

        self._app_added = True

        # map to substitute template variables
        self.app_mapping = {'comment_sign': self.comment_sign,
                            'header_lines': ''.join(self.header_lines),
                            'klass': self.cn_class(klass), 'name': self.app_name,
                            'overwrite': self.tmpl_overwrite % {'comment_sign': self.comment_sign},
                            'tab': self.tabs(1),
                            'textdomain': self._textdomain,
                            'top_win_class': self.cn_class(top_win and top_win.klass or None),
                            'top_win_module': top_win and top_win.klass.replace('::', '_'),
                            'top_win': app.top_window }

        # extend default mapping with language specific mapping
        if self.lang_mapping:
            self.app_mapping.update(self.lang_mapping)

        code = tmpl % self.app_mapping

        if self.multiple_files:
            filename = os.path.join(self.out_dir, self.app_filename)
            code = "%s%s" % ( self.tmpl_appfile % self.app_mapping, code )
            # write the wxApp code
            self.save_file(filename, [code], True)
        else:
            self.output_file.append(code)

    def add_class(self, code_obj):
        """Add class behaves very differently for XRC output than for other languages (i.e. python):
        since custom classes are not supported in XRC, this has effect only for true toplevel widgets,
        i.e. frames and dialogs. For other kinds of widgets, this is equivalent to add_object"""
        # shortcuts
        base = code_obj.base
        klass = code_obj.klass

        if klass in self.classes and self.classes[klass].done:
            return  # the code has already been generated

        if self.multiple_files:
            # let's see if the file to generate exists, and in this case create a SourceFileContent instance
            filename = self._get_class_filename(klass)
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = self.SourceFileContent(filename, self)
            self._current_extra_modules = {}
        else:
            # in this case, previous_source is the SourceFileContent instance
            # that keeps info about the single file to generate
            prev_src = self.previous_source

        try:
            builder = self.obj_builders[base]
            mycn = getattr(builder, 'cn', self.cn)
            mycn_f = getattr(builder, 'cn_f', self.cn_f)
        except KeyError:
            self._logger.error('%s', code_obj)
            # this is an error, let the exception be raised the details are logged by the global exception handler
            raise

        if prev_src and klass in prev_src.classes:
            is_new = False
            indentation = prev_src.spaces[klass]
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True
            indentation = self.tabs(self.indent_level_func_body)
            mods = getattr(builder, 'extra_modules', [])
            if mods:
                for m in mods:
                    self._current_extra_modules[m] = 1

        obuffer = []

        if not klass in self.classes:
            # if the class body was empty, create an empty ClassLines
            self.classes[klass] = self.ClassLines()

        # collect all event handlers
        event_handlers = self.classes[klass].event_handlers
        for win_id, evt, handler, evt_type in builder.get_event_handlers(code_obj):
            event_handlers.append((win_id, mycn(evt), handler, evt_type))

        # try to see if there's some extra code to add to this class
        if not self.preview:
            extra_code = getattr(builder, 'extracode', getattr(code_obj, 'extracode', "") or "")
            if extra_code and not extra_code in self.classes[klass].extra_code:
                self.classes[klass].extra_code.append(extra_code)
            # Don't add extra_code to self._current_extra_code here, that is  handled later.
            # Otherwise we'll emit duplicate extra code for frames.

        tab = indentation

        # generate code for first constructor stage
        code_lines = self.generate_code_ctor(code_obj, is_new, tab)
        obuffer.extend(code_lines)

        # now check if there are extra lines to add to the constructor
        if hasattr(builder, 'get_init_code'):
            for l in builder.get_init_code(code_obj):
                obuffer.append(tab+l)

        obuffer.append( self.tmpl_ctor_call_layout % {'tab':tab} )

        # generate code for binding events
        code_lines = self.generate_code_event_bind( code_obj, tab, event_handlers )
        obuffer.extend(code_lines)

        if self._mark_blocks:
            # end tag
            obuffer.append( '%s%s end wxGlade\n' % (tab, self.comment_sign) )

            # write class function end statement
            if self.tmpl_cfunc_end and is_new:
                obuffer.append( self.tmpl_cfunc_end%{'tab':tab} )

        # end of ctor generation

        # replace code inside existing constructor block
        if prev_src and not is_new:
            # replace the lines inside the ctor wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, self.name_ctor)
            if not prev_src.replace(tag, obuffer):
                # no __init__ tag found, issue a warning and do nothing
                self.warning( "wxGlade %(ctor)s block not found for %(name)s, %(ctor)s code "
                              "NOT generated" % { 'name': code_obj.name, 'ctor': self.name_ctor } )
            obuffer = []

        # generate code for __set_properties()
        code_lines = self.generate_code_set_properties( builder, code_obj, is_new, tab )
        obuffer.extend(code_lines)

        # replace code inside existing __set_properties() function
        if prev_src and not is_new:
            # replace the lines inside the __set_properties wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, '__set_properties')
            if not prev_src.replace(tag, obuffer):
                # no __set_properties tag found, issue a warning and do nothing
                self.warning( "wxGlade __set_properties block not found for %s, "
                              "__set_properties code NOT generated" % code_obj.name )
            obuffer = []

        # generate code for __do_layout()
        code_lines = self.generate_code_do_layout( builder, code_obj, is_new, tab )
        obuffer.extend(code_lines)

        # replace code inside existing __do_layout() function
        if prev_src and not is_new:
            # replace the lines inside the __do_layout wxGlade block with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, '__do_layout')
            if not prev_src.replace(tag, obuffer):
                # no __do_layout tag found, issue a warning and do nothing
                self.warning("wxGlade __do_layout block not found for %s, __do_layout code NOT generated"%code_obj.name)

        # generate code for event handler stubs
        code_lines = self.generate_code_event_handler( code_obj, is_new, tab, prev_src, event_handlers )

        # replace code inside existing event handlers
        if prev_src and not is_new:
            tag = '<%swxGlade event_handlers %s>' % (self.nonce, klass)
            if not prev_src.replace( tag, code_lines ):
                # no event_handlers tag found, issue a warning and do nothing
                self.warning( "wxGlade event_handlers block not found for %s, "
                              "event_handlers code NOT generated" % code_obj.name )
        else:
            obuffer.extend(code_lines)

        # the code has been generated
        self.classes[klass].done = True

        # write "end of class" statement
        if self.tmpl_class_end and self._mark_blocks:
            obuffer.append( self.tmpl_class_end % { 'klass': self.cn_class(klass), 'comment': self.comment_sign } )
        elif self.tmpl_class_end_nomarker:
            obuffer.append( self.tmpl_class_end_nomarker )

        if self.multiple_files:
            if prev_src:
                prev_src.replace('<%swxGlade insert new_classes>' % self.nonce, "")

                # insert the extra modules
                prev_src.replace( '<%swxGlade extra_modules>\n' % self.nonce, list(self._current_extra_modules.keys()) )

                # insert the module dependencies of this class
                dep_list = list( self.classes[klass].dependencies.keys() )
                dep_list.extend(self.dependencies.keys())
                dep_list.sort()
                code = self._tagcontent('dependencies', dep_list)
                prev_src.replace('<%swxGlade replace dependencies>' % self.nonce, code)
                prev_src.replace('<%swxGlade replace %s dependencies>' % (self.nonce, klass), code)  # for Perl

                # insert the extra code of this class
                extra_code = self.classes[klass].extra_code[::-1]
                code = self._tagcontent('extracode', extra_code)
                prev_src.replace('<%swxGlade replace extracode>' % self.nonce, code)

                # store the new file contents to disk
                self.save_file(filename, prev_src.content, content_only=True)
                return

            # create the new source file
            filename = self._get_class_filename(klass)
            out = []

            # write the common lines
            out.extend( self.header_lines )

            # write the module dependencies for this class
            dep_list = list( self.classes[klass].dependencies.keys() )
            dep_list.extend(self.dependencies.keys())
            dep_list.sort()
            code = self._tagcontent('dependencies', dep_list, True)
            out.append(code)

            # insert the extra code of this class
            code = self._tagcontent( 'extracode', self.classes[klass].extra_code[::-1], True )
            out.append(code)

            # write the class body
            out.extend( obuffer )
            # store the contents to filename
            self.save_file(filename, out)
        else:  # not self.multiple_files
            if prev_src:
                # if this is a new class, add its code to the new_classes list of the SourceFileContent instance
                if is_new:
                    prev_src.new_classes.append("".join(obuffer))
                elif self.classes[klass].extra_code:
                    self._current_extra_code.extend(self.classes[klass].extra_code[::-1])
                return
            else:
                # write the class body onto the single source file
                for dep in self.classes[klass].dependencies:
                    self._current_extra_modules[dep] = 1
                if self.classes[klass].extra_code:
                    self._current_extra_code.extend(self.classes[klass].extra_code[::-1])
                for line in obuffer:
                    self.output_file.append(line)

    def add_object(self, sub_obj):
        """Adds the code to build 'sub_obj' to the class body of 'top_obj';
        see _add_object_init(), add_object_format_name()"""
        #sub_obj.name = self._format_name(sub_obj.name)
        #sub_obj.parent.name = self._format_name(sub_obj.parent.name)

        # get top level source code object and the widget builder instance
        klass, builder = self._add_object_init(sub_obj)
        if not klass or not builder:
            return

        try:
            init, props, layout = builder.get_code(sub_obj)
        except:
            self._logger.error('%s', sub_obj)
            # this is an error, let the exception be raised the details are logged by the global exception handler
            raise

        if not sub_obj.is_sizer:  # the object is a wxWindow instance
            if sub_obj.node.children and not sub_obj.is_toplevel:
                init.reverse()  # parents_init will be reversed in the end
                klass.parents_init.extend(init)
            else:
                klass.init.extend(init)

            if not self.preview:
                if "extracode_pre" in sub_obj.properties and sub_obj.extracode_pre:
                    init = sub_obj.properties["extracode_pre"].get_lines() + init
                if "extracode_post" in sub_obj.properties and sub_obj.extracode_post:
                    init += sub_obj.properties["extracode_post"].get_lines()

            # Add a dependency of the current object on its parent
            klass.deps.append((sub_obj, sub_obj.parent))
            klass.child_order.append(sub_obj)
            klass.init_lines[sub_obj] = init

            mycn = getattr(builder, 'cn', self.cn)
            for win_id, evt, handler, evt_type in builder.get_event_handlers(sub_obj):
                klass.event_handlers.append( (win_id, mycn(evt), handler, evt_type) )

            # try to see if there's some extra code to add to this class
            if not self.preview:
                extra_code = getattr(builder, 'extracode', getattr(sub_obj, 'extracode', "") or "" )
                if extra_code and not extra_code in klass.extra_code:
                    klass.extra_code.append(extra_code)
        else:  # the object is a sizer
            klass.sizers_init.extend(init)

        klass.props.extend(props)
        klass.layout.extend(layout)
        if self.multiple_files and (sub_obj.is_toplevel and sub_obj.base != sub_obj.klass):
            key = self._format_import(sub_obj.klass)
            klass.dependencies[key] = 1
        for dep in getattr(self.obj_builders.get(sub_obj.base), 'import_modules', []):
            klass.dependencies[dep] = 1

    def _add_object_init(self, sub_obj):
        """Perform some initial actions for L{add_object()}
        
        Widgets without code generator or widget that are not supporting the
        requested wx version are blacklisted at L{blacklisted_widgets}.

        @return: Top level source code object and the widget builder instance or C{None, None} in case of errors."""
        # initialise internal variables first
        klass = None
        builder = None

        # Check for proper source code instance
        top_obj = self.toplevel
        if top_obj.klass in self.classes:
            klass = self.classes[top_obj.klass]
        else:
            klass = self.classes[top_obj.klass] = self.ClassLines()

        # Check for widget builder object
        try:
            builder = self.obj_builders[sub_obj.base]
        except KeyError:
            # no code generator found: write a comment about it
            name = getattr(sub_obj, "name", "None")
            name = self._format_name(name)
            msg = _('Code for instance "%s" of "%s" not generated: no suitable writer found') % (name, sub_obj.klass )
            self._source_warning(klass, msg, sub_obj)
            self.warning(msg)
            # ignore widget later too
            self.blacklisted_widgets[sub_obj] = 1
            return None, None

        # check for supported versions
        if hasattr(builder, 'is_widget_supported'):
            is_supported = builder.is_widget_supported(*self.for_version)
        else:
            is_supported = True
        if not is_supported:
            supported_versions = [misc.format_supported_by(version) for version in builder.config['supported_by']]
            msg = _('Code for instance "%(name)s" of "%(klass)s" was\n'
                    'not created, because the widget is not available for wx version %(requested_version)s.\n'
                    'It is available for wx versions %(supported_versions)s only.') % {
                        'name':  self._format_name(sub_obj.name), 'klass': sub_obj.klass,
                        'requested_version':  str(misc.format_for_version(self.for_version)),
                        'supported_versions': ', '.join(supported_versions) }
            self._source_warning(klass, msg, sub_obj)
            self.warning(msg)
            # ignore widget later too
            self.blacklisted_widgets[sub_obj] = 1
            return None, None

        return klass, builder

    def add_property_handler(self, property_name, handler, widget_name=None):
        """Sets a function to parse a portion of XML to get the value of the property property_name.
        If widget_name is not None, the function is called only if the property in inside a widget whose class is
        widget_name."""
        if not widget_name:
            self.global_property_writers[property_name] = handler
        else:
            try:
                self._property_writers[widget_name][property_name] = handler
            except KeyError:
                self._property_writers[widget_name] = {property_name: handler}

    def add_sizeritem(self, toplevel, sizer, obj, option, flag, border):
        """Writes the code to add the object 'obj' to the sizer 'sizer' in the 'toplevel' object.
        All widgets in L{blacklisted_widgets} are ignored; template is self.tmpl_sizeritem"""

        # don't process widgets listed in blacklisted_widgets
        if obj in self.blacklisted_widgets:
            return

        # the name attribute of a spacer is already formatted "<width>, <height>".
        # This string can simply inserted in Add() call.
        obj_name = self._format_classattr(obj)

        if toplevel.klass in self.classes:
            klass = self.classes[toplevel.klass]
        else:
            klass = self.classes[toplevel.klass] = self.ClassLines()

        # check if sizer has to store as a class attribute
        sizer_name = self._format_classattr(sizer)

        flag = self.cn_f(flag) or '0'

        if sizer.klass!="wxGridBagSizer":
            stmt = self.tmpl_sizeritem % ( sizer_name, obj_name, option, flag, border )
        else:
            pos = sizer._get_row_col(obj.pos)
            stmt = self.tmpl_gridbagsizeritem % ( sizer_name, obj_name, pos, obj.span, flag, border )

        klass.layout.append(stmt)

    def add_spacer(self, toplevel, sizer, obj=None, proportion=0, flag='0', border=0):
        # add spacer
        if toplevel.klass in self.classes:
            klass = self.classes[toplevel.klass]
        else:
            klass = self.classes[toplevel.klass] = self.ClassLines()
        sizer_name = self._format_classattr(sizer)
        if obj is not None:
            size = (obj.width, obj.height)
        else:
            size = (0, 0)
        flag = self.cn_f(flag) or '0'
        if sizer.klass!="wxGridBagSizer":
            size = self.tmpl_spacersize%size
            stmt = self.tmpl_sizeritem % ( sizer_name, size, proportion,flag, border )
        else:
            # GridBagSizer
            pos = sizer._get_row_col(obj.pos)
            stmt = self.tmpl_gridbagsizerspacer % ( sizer_name, size[0], size[1], pos, obj.span, flag, border )
        klass.layout.append( stmt )

    def add_widget_handler(self, widget_name, handler, *args, **kwds):
        self.obj_builders[widget_name] = handler

    def create_generated_by(self):
        "Create I{generated by wxGlade} string without leading comment characters and without tailing new lines"
        generated_from = ''
        if config.preferences.write_generated_from and common.app_tree and common.app_tree.app.filename:
            generated_from = ' from "%s"' % common.app_tree.app.filename

        if config.preferences.write_timestamp:
            msg = 'generated by wxGlade %s on %s%s' % ( config.version, time.asctime(), generated_from )
        else:
            msg = 'generated by wxGlade%s' % generated_from
        return msg

    def create_nonce(self):
        """Create a random number used to be sure that the replaced tags in the sources are the right ones
        (see SourceFileContent and add_class)"""

        nonce = '%s%s' % ( str(time.time()).replace('.', ''), random.randrange(10 ** 6, 10 ** 7) )
        return nonce

    def get_property_handler(self, property_name, widget_name):
        """Return the widget specific property handler;
        @see: L{add_property_handler}, L{global_property_writers}, L{_property_writers}"""
        try:
            cls = self._property_writers[widget_name][property_name]
        except KeyError:
            cls = self.global_property_writers.get(property_name, None)
        if cls:
            return cls()
        return None

    def generate_code_background(self, obj):
        "Returns the code fragment that sets the background colour of the given object; @see: L{_get_colour()}"
        # check if there is an code template for this property
        tmpl = self._get_code_statement('backgroundcolour')
        if not tmpl:
            msg = "%s WARNING: no code template for property '%s' registered!\n"%(self.comment_sign, 'backgroundcolour')
            self.warning(msg)
            return msg

        objname = self.format_generic_access(obj)
        color = self._get_colour(obj.background)
        stmt = tmpl % { 'objname':objname, 'value':color }
        return stmt

    def generate_code_ctor(self, code_obj, is_new, tab):
        "Generate constructor code for top-level object (CodeObject instance); is_new: create a new file?"
        return []

    #def generate_code_disabled(self, obj):
        #"Returns the code fragment that disables the given object."
        #return self._generic_code(obj, 'disabled')

    def generate_code_do_layout(self, builder, code_obj, is_new, tab):
        """Generate code for the function C{__do_layout()}.

        If C{is_new} is set, this function returns list of source code for the whole function.
        Otherwise it returns just the function body framed by "begin wxGlade" and "end wxGlade".

        @param builder: Widget specific builder
        @param code_obj: Object to generate code for (CodeObject)
        @param is_new: Indicates if previous source code exists

        see tmpl_name_do_layout, tmpl_func_do_layout, tmpl_func_empty; _generate_function()"""
        code_lines = []
        write = code_lines.append

        # generate content of function body first
        layout_lines = self.classes[code_obj.klass].layout
        sizers_init_lines = self.classes[code_obj.klass].sizers_init

        # check if there are extra layout lines to add
        if hasattr(builder, 'get_layout_code'):
            extra_layout_lines = builder.get_layout_code(code_obj)
        else:
            extra_layout_lines = []

        if layout_lines or sizers_init_lines or extra_layout_lines:
            sizers_init_lines.reverse()
            for l in sizers_init_lines:
                write(l)
            for l in layout_lines:
                write(l)
            for l in extra_layout_lines:
                write(l)

        code_lines = self._generate_function( code_obj, is_new, tab, self.tmpl_name_do_layout,
                                              self.tmpl_func_do_layout, code_lines )

        return code_lines

    def generate_code_event_bind(self, code_obj, tab, event_handlers):
        "Generate code to bind events for 'code_obj'; event_handlers is a list of event handlers (str,str,str)"
        return []

    def generate_code_event_handler(self, code_obj, is_new, tab, prev_src, event_handlers):
        """Generate the event handler stubs for 'code_obj'

         is_new:         Indicates if previous source code exists;
         prev_src:       Previous source code event_handlers; SourceFileContent instance
         event_handlers: List of event handlers (str,str,str)"""
        code_lines = []
        write = code_lines.append

        if prev_src and not is_new:
            already_there = prev_src.event_handlers.get(code_obj.klass, {})
        else:
            already_there = {}

        for win_id, event, handler, unused in event_handlers:
            # don't create handler twice
            if handler in already_there:
                continue

            # add an empty line for;  TODO: Remove later
            if self.language in ('python', 'lisp') and not (prev_src and not is_new):
                write('\n')
            write(self.tmpl_func_event_stub % {'tab': tab, 'klass': self.cn_class(code_obj.klass), 'handler': handler })
            already_there[handler] = 1

        return code_lines

    def generate_code_extraproperties(self, obj):
        "Returns a list of code fragments that set extra properties for the given object"
        tmpl = self._get_code_statement('extraproperties')
        if not tmpl:
            return []
        objname = self.format_generic_access(obj)

        ret = []
        for name, value in sorted(obj.extraproperties):
            name_cap = name[0].upper() + name[1:]
            stmt = tmpl % { 'klass': self.cn_class(obj.klass), 'objname': objname, 'propname': name,
                            'propname_cap': name_cap, 'value': value }
            ret.append(stmt)
        return ret

    #def generate_code_focused(self, obj):
        #"Returns the code fragment that get the focus to the given object"
        #return self._generic_code(obj, 'focused')

    def generate_code_font(self, obj):
        "Returns the code fragment that sets the font of the given object"
        stmt = None

        # check if there is an code template for this property
        tmpl = self._get_code_statement('setfont' )
        if not tmpl:
            msg = " %s WARNING: no code template for property '%s' registered!\n" % (self.comment_sign, 'setfont')
            self.warning(msg)
            return msg

        objname = self.format_generic_access(obj)
        cnfont = self.cn('wxFont')
        
        font_p = obj.properties["font"]
        size, family, style, weight, underlined, face = font_p.get()
        family  = font_p.font_families[family]  # e.g. 'roman' -> 'wxROMAN'
        style   = font_p.font_styles[style]
        weight  = font_p.font_weights[weight]
        if self.for_version[0]<3:
            # use old constants
            family = family.replace('wxFONTFAMILY_', 'wx')
            style  = style.replace( 'wxFONTSTYLE_',  'wx')
            weight = weight.replace('wxFONTWEIGHT_', 'wx')

        face = '"%s"' % face.replace('"', r'\"')
        stmt = tmpl % { 'objname':objname, 'cnfont':cnfont, 'face':face, 'family':self.cn(family), 'size':size,
                        'style':self.cn(style), 'underlined': underlined, 'weight':self.cn(weight) }
        return stmt

    def generate_code_foreground(self, obj):
        "Returns the code fragment that sets the foreground colour of the given object; @see: L{_get_colour()}"
        # check if there is an code template for this property
        tmpl = self._get_code_statement('foregroundcolour' )
        if not tmpl:
            msg =" %s WARNING: no code template for property '%s' registered!\n"%(self.comment_sign, 'foregroundcolour')
            self.warning(msg)
            return msg

        objname = self.format_generic_access(obj)
        color = self._get_colour(obj.foreground)
        stmt = tmpl % { 'objname':objname, 'value':color }
        return stmt

    #def generate_code_hidden(self, obj):
        #"Returns the code fragment that hides the given object"
        #return self._generic_code(obj, 'hidden')

    def generate_code_id(self, obj, id=None):
        """Generate the code for the widget ID.

        The parameter C{id} is evaluated first. An empty string for C{id} returns C{'', 'wxID_ANY'}.

        Returns a tuple of two string. The two strings are:
         1. A line to the declare the variable. It's empty if the object id is a constant
         2. The value of the id

        @param obj: An instance of L{xml_parse.CodeObject}
        @param id:  Widget ID definition as String."""
        raise NotImplementedError

    def generate_code_set_properties(self, builder, code_obj, is_new, tab):
        """Generate code for the function C{__set_properties()}.

        If C{is_new} is set, this function returns a list of source code for the whole function.
        Otherwise it returns just the function body framed by "begin wxGlade" and "end wxGlade".

        @param builder: Widget specific builder
        @param code_obj: Object to generate code for (CodeObject instance)
        @param is_new: Indicates if previous source code exists

        see tmpl_name_set_properties, tmpl_func_set_properties, tmpl_func_empty; _generate_function()"""
        # check if there are property lines to add
        _get_properties = getattr( builder, 'get_properties_code', self.generate_common_properties )
        property_lines = _get_properties(code_obj)
        property_lines.extend(self.classes[code_obj.klass].props)

        code_lines = self._generate_function( code_obj, is_new, tab,
                                              self.tmpl_name_set_properties, self.tmpl_func_set_properties,
                                              property_lines )

        return code_lines

    def generate_code_size(self, obj):
        "Returns the code fragment that sets the size of the given object."
        raise NotImplementedError

    #def generate_code_tooltip(self, obj):
        #"Returns the code fragment that sets the tooltip of the given object."
        #return self._generic_code(obj, 'tooltip')

    def generate_common_properties(self, widget):
        """generates the code for various properties common to all widgets (background and foreground colours, font,...)
        returns a list of strings containing the generated code"""
        out = []
        props = [p.name for p in widget.get_properties() if p.is_active()]
        if widget.check_prop('size') and widget.base!='wxFrame':
            out.append(self.generate_code_size(widget))
        if widget.check_prop('background'): out.append(self.generate_code_background(widget))
        if widget.check_prop('foreground'): out.append(self.generate_code_foreground(widget))
        if widget.check_prop('font'):       out.append(self.generate_code_font(widget))
        # tooltip
        if widget.check_prop('tooltip')  and widget.tooltip:   out.append( self._generic_code(widget, 'tooltip') )
        # trivial boolean properties
        if widget.check_prop('disabled') and widget.disabled: out.append( self._generic_code(widget, 'disabled') )
        if widget.check_prop('focused' ) and widget.focused:  out.append( self._generic_code(widget, 'focused') )
        if widget.check_prop('hidden'  ) and widget.hidden:   out.append( self._generic_code(widget, 'hidden') )

        if widget.check_prop('extraproperties') and not self.preview:
            out.extend(self.generate_code_extraproperties(widget))
        out = [l for l in out if l is not None]
        return out

    def quote_str(self, s):
        """Returns a quoted / escaped version of 's', suitable to insert in a source file as a string object.
        Takes care also of gettext support.

        Escaped are: quotations marks, backslash, characters with special meaning

        Please use quote_path() to quote / escape filenames or paths.
        Please check the test case tests.test_codegen.TestCodeGen.test_quote_str() for additional infos.
        The language specific implementations are in _quote_str()."""
        if not s:
            return self.tmpl_empty_string
        # no longer with direct generation:
        ## this is called from the Codewriter which is fed with XML
        ## here newlines are escaped already as '\n' and '\n' two-character sequences as '\\n'
        ## so we start by unescaping these
        #s = np.TextProperty._unescape(s)
        # then escape as required
        s = s.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t").replace('"', '\\"')
        return self._quote_str(s)

    def _quote_str(self, s):
        "Language specific implementation for escaping or quoting."
        raise NotImplementedError

    def quote_path(self, s):
        "Escapes all quotation marks and backslashes, thus making a path suitable to insert in a list source file"
        # You may overwrite this function in the derived class
        s = s.replace('\\', '\\\\')
        s = s.replace('"', r'\"')
        s = s.replace('$', r'\$')  # sigh
        s = s.replace('@', r'\@')
        return '"%s"' % s

    def save_file(self, filename, content, mainfile=False, content_only=False):
        """Store the content in a file.

        A shebang is added in top of all mainfiles. The permissions of mainfiles will be set to 0755 too.
        common.save_file() is used for storing content, i.e. the file will only be written in case of changes.

        filename:     File name
        content:      File content as list of strings
        mainfile:     Mainfiles gets a shebang and 0755 permissions.
        content_only: Write only content to the file"""

        tmp = []

        # write additional information to file header
        if not content_only:
            # add shebang to main file
            if self.shebang and mainfile or self.language == 'C++':
                tmp.append( self.shebang )

            # add file encoding notice
            if self.tmpl_encoding and self.app_encoding:
                tmp.append( self.tmpl_encoding % self.app_encoding )

            # add created by notice
            if self.tmpl_generated_by:
                tmp.append( self.tmpl_generated_by % {'comment_sign': self.comment_sign,
                                                      'generated_by': self.create_generated_by() } )

            # add language specific note
            if self.language_note:
                tmp.append( "%s" % self.language_note )

            # add a empty line
            tmp.append( "\n" )

        # add original file content
        tmp += content

        # encode unicode to binary; filter out empty line
        def encode(line):
            if isinstance(line, compat.unicode):
                return line.encode(self.app_encoding)
            return line
        try:
            tmp = [encode(line) for line in tmp if line]
        except UnicodeEncodeError as inst:
            raise errors.WxgOutputUnicodeError( self.app_encoding,
                                                inst.object[inst.start:inst.end].encode('unicode-escape'),
                                                inst.start, inst.end )

        # check for necessary sub directories e.g. for Perl or Python modules
        dirname = os.path.dirname(filename)
        if dirname and not os.path.isdir(dirname):
            try:
                os.makedirs(dirname)
            except:
                self._logger.exception( _('Can not create output directory "%s"'), dirname )

        # save the file now
        try:
            common.save_file(filename, tmp, 'codegen')
        except (errors.WxgBaseException, EnvironmentError):
            # EnvironmentError's will be caught at a higher level
            raise
        except:
            self._logger.exception(_('Internal Error'))
        if mainfile and sys.platform in ['linux2', 'darwin']:
            try:
                # make the file executable
                os.chmod(filename, 0o755)
            except OSError as e:
                # this isn't necessarily a bad error
                self.warning(_('Changing permission of file "%s" failed: %s') % (filename, str(e)))

    def store_as_attr(self, obj):
        """Returns True if 'obj' should be added as an attribute of its parent's class,
        False if it should be created as a local variable of C{__do_layout}.
        
        The function returns True of the object klass is listed in L{classattr_always}.

        The function returns True for all widgets except sizers, if
         - the property exists and is an integer greater equal 1
         - the property does not exists
         - the property contains a non-integer value

        The function returns True for sizers, if
         - the property exists and is an integer greater equal 1

        @see: L{classattr_always}
        """
        if obj.klass in self.classattr_always:
            return True
        if 'attribute' in obj.properties:
            return obj.attribute

        if obj.is_sizer:
            return False
        if obj.classname in ("wxStaticText","wxHyperlinkCtrl","wxStaticBitmap","wxStaticLine"):
            return False
        return True  # this is the default

    def tabs(self, number):
        "Return a proper formatted string for indenting lines"
        return self.indent_symbol * self.indent_amount * number

    def warning(self, msg):
        "Show a warning message"
        if self._show_warnings:
            self._logger.warning(msg)

    def _remove_tag_re(self, source, re_string):
        "Remove all tags that match the regular expression"
        tags = re.compile( re_string%self.nonce )
        remove = [i for i,line in enumerate(source.content) if tags.match(line)]
        for i in reversed(remove):
            del source.content[i]

    def _content_notfound(self, source):
        """Remove all the remaining <123415wxGlade ...> tags from the source and add a warning instead.

        This may happen if we're not generating multiple files, and one of the container class names is changed.

        The indentation of the string depends values detected during the initial parsing of the source file.
        Those values are stored in L{BaseSourceFileContent.spaces}.

        @param source: Source content string with tags to replace"""

        tags = re.compile( r'(<%swxGlade replace ([a-zA-Z_]\w*) +[.\w]+>)' % self.nonce)
        for i,line in enumerate(source.content):
            match = tags.match(line)
            if not match: continue
            # re.findall() returned a list of tuples (caused by grouping)
            # first element in tuple:  the whole match
            # second element in tuple: the class / block name
            tag = match.groups()
            indent = self.previous_source.spaces.get(tag[1], "")
            comment = '%(indent)s%(comment_sign)s Content of this block not found. Did you rename this class?\n'
            tmpl = self._get_code_statement('contentnotfound' )
            if tmpl:
                comment += '%(indent)s%(command)s\n'
                command = tmpl
            else:
                command = ""
            comment = comment % {'command':command, 'comment_sign':self.comment_sign, 'indent':indent }
            #source = source.replace(tag[0], comment)
            source.content[i] = comment

    def _do_replace_backslashes(self, match):
        "Escape double backslashes in first RE match group; see quote_str()"
        return 2 * match.group(0)

    def _do_replace_doublequotes(self, match):
        "Escape double quotes"
        # " -> \"
        # \" -> \\"
        if match.group(0).startswith('\\'):
            return '\\\"'
        else:
            return '\\"'

    def _file_exists(self, filename):
        "Check if the file exists; separated for debugging purposes"
        return os.path.isfile(filename)

    def add_object_format_name(self, name):
        "Format a widget name to use in add_object("
        return name

    def _format_classattr(self, obj):
        "Format the object name to store as a class attribute; obj: Instance of L{xml_parse.CodeObject}"
        if not obj:
            return ''
        elif not getattr(obj, 'name', None):
            return ''
        return obj.name

    def _format_comment(self, msg):
        """Return message formatted to add as a comment string in generating source code.
        Trailing spaces will be removed. Leading spaces e.g. indentation won't be added."""
        return "%s %s" % (self.comment_sign, msg.rstrip())

    def _format_import(self, klass):
        "Return formatted import statement for the given class name"
        return klass

    def _format_name(self, name):
        "Format a class or a widget name by replacing forbidden characters."
        return name

    def _format_style(self, style, code_obj):
        """Return the formatted styles to insert into constructor code.
        The function just returned L{tmpl_style}. Write a derived version implementation if more logic is needed."""
        if code_obj.is_toplevel:
            if style:
                return self.tmpl_toplevel_style
            return self.tmpl_toplevel_style0
        if style:
            return self.tmpl_style
        return self.tmpl_style0

    def _generic_code(self, obj, prop_name):
        """Create a code statement for calling a method e.g. to hide a widget.

        @param obj:       Instance of L{xml_parse.CodeObject}
        @param prop_name: Name of the property to set

        @return: Code statement or None

        @see: L{_code_statements}"""

        value = obj.properties[prop_name].get_value()
        if isinstance(value, compat.basestring): value = self.quote_str(obj.tooltip)
        tmpl = self._get_code_statement(prop_name)
        objname = self.format_generic_access(obj)
        stmt = tmpl % { 'objname': objname, 'tooltip': value }
        return stmt

    def _get_code_statement(self, prop_name):
        """Return non-formatted code statement related to prop_name.
        This function handled the properties extensions described in L{_code_statements}.

        @param prop_name: Name of the property to set
        @return: Code statement or None

        @see: L{_code_statements}"""

        prop_name_major = '%s_%d' % ( prop_name, self.for_version[0] )
        prop_name_detailed = '%s_%d%d' % ( prop_name, self.for_version[0], self.for_version[1] )

        # check if there is an code template for this prop_name
        # most specific to generic
        if prop_name_detailed in self._code_statements:
            prop_name_use = prop_name_detailed
        elif prop_name_major in self._code_statements:
            prop_name_use = prop_name_major
        elif prop_name in self._code_statements:
            prop_name_use = prop_name
        else:
            return None

        return self._code_statements[prop_name_use]

    def format_generic_access(self, obj):
        """Format a generic and language specific access to the given object.
        For example: C{self}, C{$self} or C{$self->}."""
        raise NotImplementedError

    def _get_colour(self, colourvalue):
        "Returns the language specific colour statement"
        # check if there is an code template for this properties
        if colourvalue == 'wxNullColour':
            return self.cn(self._get_code_statement('wxnullcolour'))
        tmpl_wxcolour = self._get_code_statement('wxcolour' )
        if not tmpl_wxcolour:
            msg = " %s WARNING: no code template for property '%s' registered!\n" % (self.comment_sign, 'wxcolour')
            self.warning(msg)
            return msg
        tmpl_wxsystemcolour = self._get_code_statement('wxsystemcolour' )
        if not tmpl_wxsystemcolour:
            msg = " %s WARNING: no code template for property '%s' registered!\n"%(self.comment_sign, 'wxsystemcolour')
            self.warning(msg)
            return msg
        try:
            value = self._string_to_colour(colourvalue)
            tmpl = self.cn(tmpl_wxcolour)
        except (IndexError, ValueError):  # the color is from system settings
            value = self.cn(colourvalue)
            tmpl = self.cn(tmpl_wxsystemcolour)
        stmt = tmpl % {'value': value}
        return stmt

    def _get_class_filename(self, klass):
        "Returns the filename to store a single class in multi file projects; 'class': class name"
        return ''

    def _generate_function(self, code_obj, is_new, tab, fname, ftmpl, body):
        """Generic function to generate a complete function from given parts.

        @param code_obj: Object to generate code for (CodeObject instance)
        @param is_new: Indicates if previous source code exists
        @param fname: Name of the function
        @param ftmpl: Template string of the function
        @param body: Content of the function
        @type body:  list[str]
        
        @rtype: list[str]"""
        code_lines = []
        write = code_lines.append

        if self._mark_blocks:
            # begin tag
            write( self.tmpl_block_begin % { 'class_separator': self.class_separator, 'comment_sign': self.comment_sign,
                                             'function': fname, 'klass': self.cn_class(code_obj.klass), 'tab': tab} )

        if body:
            for l in body:
                write(tab + l)
        else:
            write(self.tmpl_func_empty % {'tab': tab})

        if self._mark_blocks:
            # end tag
            write('%s%s end wxGlade\n' % (tab, self.comment_sign))

        # embed the content into function template
        if is_new:
            stmt = ftmpl % {'tab': tab, 'klass': self.cn_class(code_obj.klass), 'content': ''.join(code_lines) }
            code_lines = ["%s\n" % line.rstrip() for line in stmt.split('\n')]

            # remove newline at last line
            code_lines[-1] = code_lines[-1].rstrip()

        return code_lines

    def _recode_x80_xff(self, s):
        """Re-code characters in range 0x80-0xFF (Latin-1 Supplement - also
        called C1 Controls and Latin-1 Supplement) from \\xXX to \\u00XX"""
        assert isinstance(s, bytes)

        def repl(matchobj):
            dec = ord(matchobj.group(0))
            if dec > 127:
                return b'\u00' + ('%x'% dec).encode()
            return matchobj.group(0)

        s = re.sub(b'[\\x80-\\xFF]{1}', repl, s)

        return s

    def _source_warning(self, klass, msg, sub_obj):
        """Format and add a warning message to the source code.
        
        The message msg will be split into single lines and every line will be properly formatted.
        
        @param klass: Instance of L{ClassLines} to add the code in
        @param msg:   Multiline message
        
        @param sub_obj: Object to generate code for (CodeObject instance)
        
        @see: L{_format_comment()}"""
        code_lines = []

        # add leading empty line
        code_lines.append('\n')

        # add a leading "WARNING:" to the message
        if not msg.upper().startswith(_('WARNING:')):
            msg = "%s %s" % (_('WARNING:'), msg)

        # add message text
        for line in msg.split('\n'):
            code_lines.append( "%s\n" % self._format_comment(line.rstrip()) )

        # add tailing empty line
        code_lines.append('\n')

        # Add warning message to source code
        # TODO: Remove next three lines after C++ code gen uses dependencies like Python, Perl and Lisp
        if self.language == 'C++':
            klass.init.extend(code_lines)
        else:
            klass.deps.append((sub_obj, sub_obj.parent))
            klass.child_order.append(sub_obj)
            klass.init_lines[sub_obj] = code_lines

    def _string_to_colour(self, s):
        """Convert a colour values out of a hex string to comma separated decimal values.

        Example::
            >>> self._string_to_colour('#FFFFFF')
            '255, 255, 255'
            >>> self._string_to_colour('#ABCDEF')
            '171, 205, 239' """
        return '%d, %d, %d' % ( int(s[1:3], 16), int(s[3:5], 16), int(s[5:], 16) )

    def _tagcontent(self, tag, content, newline=False):
        """Returns content embedded between C{begin wxGlade} and C{end wxGlade} sequence.

        @return: Embedded content string
        @param tag: used in C{begin wxGlade} statement to indicate different blocks
        @param content: Content to enter as string or list of strings
        @param newline: Add a tailing empty line?"""
        code_list = []
        if self._mark_blocks:
            code_list.append( '%s begin wxGlade: %s' % (self.comment_sign, tag) )
        if isinstance(content, list):
            for entry in content:
                code_list.append(entry.rstrip())
        elif isinstance(content, compat.basestring):
            # don't append empty content
            _content = content.rstrip()
            if _content:
                code_list.append(_content)
        else:
            raise AssertionError('Unknown content type: %s' % type(content))
        if self._mark_blocks:
            code_list.append( '%s end wxGlade' % self.comment_sign )
        # newline for "end wxGlade" line
        code_list.append('')
        if newline:
            code_list.append('')
        return "\n".join(code_list)

    def copy(self):
        """Return a deep copy of the current instance.
        The instance will be reinitialised with defaults automatically in L{__setstate__()}."""
        new_codegen = copy.deepcopy(self)
        return new_codegen

    def __getstate__(self):
        """Return the state dict of this instance except the L{_logger} and the L{classes} attributes.
        Both attributes caused copy errors due to file locking resp. weak references in XML SAX module."""
        state = self.__dict__.copy()
        del state['_logger']
        del state['classes']
        return state

    def __setstate__(self, state):
        """Update the instance using values from dict 'state'.
        The code generator will be reinitialised after the state has been updated; see L{new_project()}"""

        self.__dict__.update(state)

        # re-initialise logger instance deleted from __getstate__ and instance variables
        self._logger = logging.getLogger(self.__class__.__name__)
        #self.new_project()

