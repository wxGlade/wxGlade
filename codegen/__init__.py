# -*- coding: LATIN1 -*-
"""Common code used by all code generators

@copyright: 2011-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy, logging, os, os.path, random, re, sys, time

import common, config, compat, misc
import wcodegen
from collections import OrderedDict


def _replace_tag(lst, tag, content):
    # used before writing generated code to file: replace placeholder tag with content
    # an example tag: '<15535320686269365730972wxGlade extra_modules>\n'
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
        self.OK = False

        # initialise instance
        self.classes = set()               # Classes declared in the file
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

        try:
            self.build_untouched_content()
            self.OK = True
        except UnicodeDecodeError:
            # file could not be read and OK will remain False
            # this will be checked in writer.init_files and returned via writer.new_project to application.generate_code
            pass

    def replace(self, tag, content):
        return _replace_tag(self.content, tag, content)

    def build_untouched_content(self):
        """Builds a string with the contents of the file that must be left as is, and replaces the wxGlade blocks
        with tags that in turn will be replaced by the new wxGlade blocks"""
        # the derived classes must implement the language specific parsing
        self.class_name = None
        self.new_classes_inserted = False

    def _remove_method(self, out_lines, i0, i1):
        # helper for derived classes at end of _build_untouched to remove empty methods
        del out_lines[i0:i1+1]
        # remove trailing empty lines
        while len(out_lines)>i0 and not out_lines[i0].strip():
            del out_lines[i0]

    def format_classname(self, class_name):
        "Format class name read from existing source file; may be overwritten in derived class"
        return class_name

    def is_end_of_class(self, line):
        "True if the line is the marker for class end"
        return line.strip().startswith('# end of class ')

    def _load_file(self, filename):
        "Load a file and return the content. The read source file will be decoded to unicode automatically."
        # Separated for debugging purposes
        lines = common._read_file(filename)

        encoding = self.code_writer.app_encoding
        if encoding:
            # UnicodeDecodeError will be handled in application.generate_code
            lines = [line.decode(encoding) for line in lines]
        return lines


class ClassLines(object):
    "Stores the lines of source code for a custom class"
    def __init__(self):
        self.child_order = []
        self.dependencies = set() # Names of the modules this class depends on
        self.deps = []
        self.event_handlers = []  # Lines to bind events (see wcodegen.BaseWidgetWriter.get_event_handlers())
        self.extra_code = []      # Extra code to output before this class
        self.done = False         # If True, the code for this class has already been generated
        # XXX refactor init and final into init_code, final_code?
        self.init = []            # Lines of code to insert in the __init__ method (for children widgets)
        self.final = []  # to be inserted after children, e.g. Add or AddPage for sizers / notebooks


class BaseLangCodeWriter(wcodegen.BaseCodeWriter):
    """Dictionary of objects used to generate the code in a given language.

    XXX the following is outdated:
    A code writer object *must* implement those interface and set those variables:
      - init_lang(), init_files(), finalize()
      - wcodegen.BaseLanguageMixin.language
      - add_app(), add_class(), add_object(), finalize_class()
      - register_widget_code_generator()
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

     classes:                 Dictionary that maps the lines of code of a class to the name of such class

     dependencies:            Module dependencies of all classes

     header_lines:            Lines common to all the generated files (import of wxCL, ...)

     curr_tab:                Current indentation level
     indent_amount:           An indentation level is  indent_symbol*indent_amount;
                               will be initialised with config.default_indent_amount
     indent_symbol:           Character to use for indentation; will be initialised with config.default_indent_symbol

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

     _overwrite:             If True, overwrite any previous version of the source file instead of updating only
                              the wxGlade blocks;  will be initialised with config.default_overwrite

     _property_writers:      Dictionary of dictionaries of property handlers specific for a widget;
                              keys are the class names of the widgets
                              (E.g. _property_writers['wxRadioBox'] = {'choices', choices_handler})

     _textdomain:            gettext textdomain (see _use_gettext)
     _use_gettext:           If True, enable gettext support; will be initialised with config.default_use_gettext"""

    _code_statements = {}
    """Language specific code templates for for small statements.

    The statements are stored in a dictionary. The property names are the keys.
    
    The keys may have one of two different extensions: 
     - "_<major version>X"               e.g. "tooltip_3X" to generate tooltips source code for wxWidgets 3.x
     - "_<major version><minor version>" e.g. "wxcolour_28" to generate source code for wxWidgets 2.8 only

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

    tmpl_func_event_stub = ''      # Statement for a event handler stub -> see generate_code_event_handler()_

    # Template of the "generated by ..." message ;see create_generated_by(); save_file()
    tmpl_generated_by = "%(comment_sign)s %(generated_by)s\n%(comment_sign)s\n"

    # Template of the overwrite message in all standalone app files; see add_app
    tmpl_overwrite = "%(comment_sign)s This is an automatically generated file.\n" \
                     "%(comment_sign)s Manual changes will be overwritten without warning!\n\n"

    tmpl_sizeritem = ''           # Template for adding a widget to a sizer
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
        self._init_cache()  # the cache is mainly used to inject values for CustomWidget

    def _init_vars(self):
        """Set instance variables (back) to default values during class instantiation (__init__) and before
        loading new data (new_project())."""
        self.app_encoding = config.default_encoding
        self.app_filename = None
        self.app_mapping = {}
        self.app_name = None
        self.classes = OrderedDict()
        self.curr_tab = 0
        self.dependencies = set()
        self.for_version = config.for_version_min
        self.header_lines = []
        self.indent_symbol = config.default_indent_symbol
        self.indent_amount = config.default_indent_amount
        self.is_template = 0
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
        self._overwrite = config.default_overwrite
        self._mark_blocks = True # YYY config.mark_blocks
        self._textdomain = 'app'
        self._use_gettext = config.default_use_gettext

    ####################################################################################################################
    # the cache is mainly used to inject values for CustomWidget
    def _init_cache(self):
        self._cache = {}

    def cache(self, widget, key, value):
        # e.g. code_gen.cache(custom_widget, "attribute_access", "self.widget")
        # supported keys for now: "attribute_access"
        self._cache.setdefault(widget, {})[key] = value
        return value

    def get_cached(self, widget, key, default=None):
        return self._cache.get("", {}).get(key, default)

    ####################################################################################################################
    def new_project(self, app, out_path=None, preview=False):
        "Initialise generic and language independent code generator settings; see init_lang(), init_files()"

        # set (most of) instance variables back to default values
        self._init_vars()
        self._init_cache()

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
            self._mark_blocks = True if not self._overwrite else app.mark_blocks
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
        self.have_extracode = False  # set to True if (extra) code for custom widget is added

        # any of the following could return an error as string
        return self.init_lang(app) or self.check_values() or self.init_files(self.out_dir)

    def init_lang(self, app_attrs):
        "Initialise language specific settings; overwrite this function in the derived class"
        raise NotImplementedError

    def init_files(self, out_path):
        "Initialise the file handling; outp_path: file or path, depending on self.multiple_files"
        # You may overwrite this function in the derived class
        if self.multiple_files:
            self.previous_source = None
            if not os.path.isdir(out_path):
                return _("Output path is not a directory")
            self.out_dir = out_path
        else:
            if os.path.isdir(out_path):
                return _("Output path is a directory")
            if not self._overwrite and self._file_exists(out_path):
                # the file exists, we must keep all the lines not inside a
                # wxGlade block. NOTE: this may cause troubles if out_path is
                # not a valid source file, so be careful!
                self.previous_source = self.SourceFileContent(out_path, self)
                if not self.previous_source.OK:
                    return _("Encoding error in existing file (UnicodeDecodeError) '%s'"%out_path)
            else:
                # if the file doesn't exist, create it and write the ``intro''
                self.previous_source = None
                self.output_file = []
                self.output_file_name = out_path
                self.output_file.extend( self.header_lines )
                self.output_file.append('\n')
                self.output_file.append('<%swxGlade replace dependencies>\n' % self.nonce)
                self.output_file.append('<%swxGlade replace extracode>\n' % self.nonce)
        return None

    def output_file_replace(self, tag, content):
        _replace_tag(self.output_file, tag, content)

    def check_values(self):
        "Check the validity of output directory/file name"
        out_dir = self.out_dir
        if not self.multiple_files:
            if os.path.isdir(out_dir):
                return "Output path is directory, not file"
            out_dir = os.path.dirname(out_dir)

        if out_dir:
            if not os.path.isdir(out_dir):
                return "Output directory does not exist"
            if not os.access(out_dir, os.W_OK):
                return "Output directory is not writable"

        # It's not possible to generate code from a template directly
        if self.is_template:
            return 'Code generation from a template is not possible'

        return None

    def _generate_code(self, parent_klass, parent, parent_builder, obj):
        # recursively generate code, for anything except application.Application
        # for toplevel widgets or with class different from wx... a class will be added

        if obj.IS_SLOT or obj.WX_CLASS=="spacer":
            if obj.WX_CLASS:  # "slot" has no code generator, but "sizerslot" or "spacer" needs to be added
                self.add_object(parent_klass, parent, parent_builder, obj)
            return

        # XXX check: set obj.IS_CLASS again?
        obj.IS_CLASS = IS_CLASS = obj.IS_TOPLEVEL or (not self.preview and obj.check_prop_truth("class"))
        # first the item
        klass = IS_CLASS and self.add_class(obj) or None
        if not obj.IS_TOPLEVEL:
            builder = self.add_object(parent_klass, parent, parent_builder, obj)
        else:
            builder = None

        # then the children
        for child in obj.get_all_children():
            self._generate_code(klass or parent_klass, obj, builder, child)

        if IS_CLASS:
            self.finalize_class(obj)

    def generate_code(self, root, widget=None):
        "entry point for recursive code generation via _generate_code()"
        # root must be application.Application instance for now
        for c in root.children or []:
            if widget is not None and c is not widget: continue # for preview
            self._generate_code(None, None, None, c)
        if not root.IS_ROOT or self.preview: return
        topwin = [c for c in root.children if c.name==root.top_window]
        topwin = topwin and topwin[0] or root.children and root.children[0] or None
        self.add_app(root, topwin)

    def finalize(self):
        "Code generator finalization function"
        if self.previous_source:
            # insert all the new custom classes inside the old file
            if self.previous_source.new_classes:
                code = "".join(self.previous_source.new_classes)
            else:
                code = ""
            self.previous_source.replace( '<%swxGlade insert new_classes>' % self.nonce, code )

            # module dependencies of all classes
            code = self._tagcontent( 'dependencies', sorted(self.dependencies) )
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
            # module dependencies of all classes
            code = self._tagcontent( 'dependencies', sorted(self.dependencies) )
            self.output_file_replace( '<%swxGlade replace dependencies>' % self.nonce, code )

            # extra code (see the 'extracode' property of top-level widgets)
            code = self._tagcontent('extracode', self._current_extra_code)
            self.output_file_replace( '<%swxGlade replace extracode>' % self.nonce, code )

            self.save_file(self.output_file_name, self.output_file, self._app_added)
            self.output_file = None

    def clean_up(self, obj):
        if hasattr(obj, "xrc"):
            del obj.xrc
        obj.restore_properties()
        for c in obj.children or []:
            self.clean_up(c)

    def add_app(self, app, top_win):
        """Generates the code for a wxApp instance.
        If the file to write into already exists, this function does nothing.

        If gettext support is requested and there is not template with gettext support but there is a template without
        gettext support, template without gettext support will be used.

        This fallback mechanism works bidirectional.

        app_mapping will be reset to default values and updated with lang_mapping.

        see: tmpl_appfile, tmpl_detailed, tmpl_gettext_detailed, tmpl_simple, tmpl_gettext_simple, app_mapping,
             lang_mapping"""

        if not self.multiple_files:
            prev_src = self.previous_source
        else:
            # overwrite apps file always
            prev_src = None

        # do nothing if the file exists; this should be changed
        if prev_src: return

        klass = app.klass

        # top window and application name are mandatory
        if not top_win: return

        # get and fill language dependent template
        tmpl = self._get_app_template(app, top_win)
        if not tmpl: return

        self._app_added = True

        # map to substitute template variables
        cls = top_win.get_instantiation_class(formatter=None, cls_formatter=self.cn_class, preview=self.preview)
        self.app_mapping = {'comment_sign': self.comment_sign,
                            'header_lines': ''.join(self.header_lines),
                            'klass': self.cn_class(klass), 'name': self.app_name,
                            'overwrite': self.tmpl_overwrite % {'comment_sign': self.comment_sign},
                            'tab': self.tabs(1),
                            'textdomain': self._textdomain,
                            'top_win_class': cls,
                            'top_win_module': top_win and top_win.klass.replace('::', '_'),
                            'top_win': top_win.name }

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
        """Add an object for that a Python/C++/... class will be generated.
        This is done for each toplevel element as well as for elements with a custom class property.
        See the _is_class(code_obj) method above."""
        klass = code_obj.klass

        assert code_obj not in self.classes

        try:
            builder = self.obj_builders[code_obj.WX_CLASS]
        except KeyError:
            logging.error('%s', code_obj)
            # this is an error, let the exception be raised the details are logged by the global exception handler
            raise

        ret = self.classes[code_obj] = self.ClassLines()
        return ret

    def finalize_class(self, code_obj):
        "Finalize and write the code for the class that was started with add_class(code_obj)."
        klass = code_obj.klass

        if self.multiple_files:
            # let's see if the file to generate exists, and in this case create a SourceFileContent instance
            filename = self._get_class_filename(klass)
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = self.SourceFileContent(filename, self)
        else:
            # previous_source is the SourceFileContent instance that keeps info about the single file to generate
            prev_src = self.previous_source

        builder = self.obj_builders[code_obj.WX_CLASS]
        mycn = getattr(builder, 'cn', self.cn)

        # collect all event handlers
        event_handlers = self.classes[code_obj].event_handlers
        for obj, evt, handler, evt_type in builder.get_event_handlers(code_obj):
            event_handlers.append((obj, mycn(evt), handler, evt_type))

        # try to see if there's some extra code to add to this class
        if not self.preview:
            extra_code = getattr(builder, 'extracode', getattr(code_obj, 'extracode', "") or "")
            if extra_code and not extra_code in self.classes[code_obj].extra_code:
                self.classes[code_obj].extra_code.append(extra_code)
            # Don't add extra_code to self._current_extra_code here, that is  handled later.
            # Otherwise we'll emit duplicate extra code for frames.

        if prev_src and klass in prev_src.classes:
            is_new = False
            indentation = prev_src.spaces[klass]
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True
            indentation = self.tabs(self.indent_level_func_body)

        # the code lines
        obuffer = []
        tab = indentation

        # generate code for first constructor stage
        code_lines = self.generate_code_ctor(code_obj, is_new, tab)
        obuffer.extend(code_lines)

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
            # replace the lines inside the ctor wxGlade block with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, self.name_ctor)
            if not prev_src.replace(tag, obuffer):
                # no __init__ tag found, issue a warning and do nothing
                tmpl = "wxGlade %(ctor)s block not found for %(name)s, %(ctor)s code NOT generated"
                self.warning( tmpl % { 'name': code_obj.name, 'ctor': self.name_ctor } )
            obuffer = []

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

        # write "end of class" statement
        if self.tmpl_class_end and self._mark_blocks:
            obuffer.append( self.tmpl_class_end % { 'klass': self.cn_class(klass), 'comment': self.comment_sign } )
        elif self.tmpl_class_end_nomarker:
            obuffer.append( self.tmpl_class_end_nomarker )

        if self.multiple_files:
            dep_list = sorted( self.classes[code_obj].dependencies | self.dependencies )  # module dependencies of class
            if prev_src:
                prev_src.replace('<%swxGlade insert new_classes>' % self.nonce, "")

                # insert the module dependencies of this class
                code = self._tagcontent('dependencies', dep_list)
                prev_src.replace('<%swxGlade replace dependencies>' % self.nonce, code)
                prev_src.replace('<%swxGlade replace %s dependencies>' % (self.nonce, klass), code)  # for Perl

                # insert the extra code of this class
                extra_code = self.classes[code_obj].extra_code[::-1]
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
            code = self._tagcontent('dependencies', dep_list, True)
            out.append(code)

            # insert the extra code of this class
            code = self._tagcontent( 'extracode', reversed(self.classes[code_obj].extra_code), True )
            out.append(code)

            # write the class body
            out.extend( obuffer )
            # store the contents to filename
            self.save_file(filename, out)
        else:  # not self.multiple_files
            self.dependencies.update( self.classes[code_obj].dependencies )
            extra_code = [l for l in reversed(self.classes[code_obj].extra_code) if not l in self._current_extra_code]
            if prev_src:
                # if this is a new class, add its code to the new_classes list of the SourceFileContent instance
                if is_new:
                    prev_src.new_classes.append("".join(obuffer))
                else:
                    self._current_extra_code.extend(extra_code)
            else:
                # write the class body onto the single source file
                self._current_extra_code.extend(extra_code)
                self.output_file.extend(obuffer)

    def _check_code_prop(self, obj, property_name):
        # check whether user code from named property should be added: for preview only if 'show_preview' is selected
        if self.preview:
            if not config.preferences.allow_custom_widgets: return False
            if not obj.check_prop_truth("show_preview"): return False
        if obj.check_prop(property_name):
            self.have_extracode = True
            return True
        return False

    def add_object(self, parent_klass, parent, parent_builder, obj):
        """Adds the code to build 'obj' to the class body in parent_klass. (Not called for toplevel elements.)"""

        builder = self._get_object_builder(parent_klass, obj)
        if not builder: return None

        try:
            init, final = builder.get_code(obj)
        except:
            logging.error('%s', obj)
            # this is an error, let the exception be raised the details are logged by the global exception handler
            # XXX create handler
            raise

        if not obj.IS_SIZER and not obj.IS_CLASS:  # the object is a wxWindow instance
            if self._check_code_prop(obj, "extracode_pre"):
                init = obj.properties["extracode_pre"].get_lines() + init
            if self._check_code_prop(obj, "extracode_post"):
                init += obj.properties["extracode_post"].get_lines()
            if self._check_code_prop(obj, "extraproperties"):  # insert these only after extracode_post
                init += self.generate_code_extraproperties(obj)

            mycn = getattr(builder, 'cn', self.cn)
            for obj_, evt, handler, evt_type in builder.get_event_handlers(obj):
                parent_klass.event_handlers.append( (obj_, mycn(evt), handler, evt_type) )

            # try to see if there's some extra code to add to this class
            if not self.preview or obj.check_prop_truth("show_preview"):
                extra_code = getattr(builder, 'extracode', getattr(obj, 'extracode', "") or "" )
                extra_code = extra_code.rstrip()
                if extra_code and not extra_code in parent_klass.extra_code:
                    parent_klass.extra_code.append(extra_code)
                    self.have_extracode = True

        if init:
            parent_klass.init.append("\n")
            parent_klass.init.extend(init)

        if obj.check_prop_truth("max_size"):
            parent_klass.init.append( self.generate_code_size(obj, obj.max_size, "SetMaxSize") )

        if parent_builder:  # add to sizer or notebook
            parent_klass.init.extend( parent_builder.get_code_per_child(parent, obj) )

        if final:
            parent_klass.final.insert(0, "\n")
            parent_klass.final[:0] = final
        if self.multiple_files and obj.IS_CLASS:
            key = self._format_import(obj.klass)
            parent_klass.dependencies.add( key )
        import_modules = getattr(self.obj_builders.get(obj.WX_CLASS), 'import_modules', [])
        parent_klass.dependencies.update( import_modules )
        return builder

    def _get_object_builder(self, parent_klass, obj):
        "Perform some checks and return the code builder"

        # Check for widget builder object
        try:
            builder = self.obj_builders[obj.WX_CLASS]
        except KeyError:
            # no code generator found: write a comment about it
            name = getattr(obj, "name", "None")
            name = self._format_name(name)
            msg = _('Code for instance "%s" of "%s" not generated: no suitable writer found') % (name, obj.WX_CLASS )
            self._source_warning(parent_klass, msg, obj)
            self.warning(msg)
            return None

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
                        'name':  self._format_name(obj.name), 'klass': obj.klass,
                        'requested_version':  str(misc.format_for_version(self.for_version)),
                        'supported_versions': ', '.join(supported_versions) }
            self._source_warning(parent_klass, msg, obj)
            self.warning(msg)
            return None

        return builder

    def register_widget_code_generator(self, widget_name, handler, *args, **kwds):
        self.obj_builders[widget_name] = handler

    def create_generated_by(self):
        "Create I{generated by wxGlade} string without leading comment characters and without tailing new lines"
        generated_from = ''
        if config.preferences.write_generated_from and common.app_tree and common.root.filename:
            generated_from = ' from "%s"' % common.root.filename

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

    def generate_code_background(self, obj):
        "Returns the code fragment that sets the background colour of the given object; see: _get_colour()"
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
            already_there = prev_src.event_handlers.get(code_obj.klass, set())
        else:
            already_there = set()

        for obj, event, handler, unused in event_handlers:
            # don't create handler twice
            if handler in already_there:
                continue

            # add an empty line for;  TODO: Remove later
            if self.language in ('python', 'lisp') and not (prev_src and not is_new):
                write('\n')
            write(self.tmpl_func_event_stub % {'tab': tab, 'klass': self.cn_class(code_obj.klass), 'handler': handler })
            already_there.add( handler )

        return code_lines

    def generate_code_extraproperties(self, obj):
        "Returns a list of code fragments that set extra properties for the given object"
        tmpl = self._get_code_statement('extraproperties')
        if not tmpl: return []
        objname = self.format_generic_access(obj)
        klass = self.cn_class( obj.get_prop_value("class", obj.WX_CLASS) )

        ret = []
        for name, value in obj.extraproperties:
            name_cap = name[0].upper() + name[1:]
            stmt = tmpl % { 'klass':klass, 'objname':objname, 'propname':name, 'propname_cap':name_cap, 'value':value }
            ret.append(stmt)
        return ret

    def generate_code_font(self, obj, prop_name="font", method=None):
        "Returns the code fragment that sets the font of the given object"
        stmt = None

        # check if there is an code template for this property
        tmpl = self._get_code_statement('setfont' )
        if not tmpl:
            msg = " %s WARNING: no code template for property '%s' registered!\n" % (self.comment_sign, 'setfont')
            self.warning(msg)
            return msg
        if method is not None:
            tmpl = tmpl.replace("SetFont", method)

        objname = self.format_generic_access(obj)
        cnfont = self.cn('wxFont')
        
        font_p = obj.properties[prop_name]
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
        "Returns the code fragment that sets the foreground colour of the given object; see: _get_colour()"
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

    def generate_code_id(self, obj, id=None):
        """Generate the code for the widget ID.

        The parameter id is evaluated first. An empty string for id returns ('', 'wxID_ANY').

        Returns a tuple of two string. The two strings are:
         1. A line to the declare the variable. It's empty if the object id is a constant
         2. The value of the id

        obj: An instance of xml_parse.CodeObject
        id:  Widget ID definition as String."""
        raise NotImplementedError

    def generate_code_size(self, obj):
        "Returns the code fragment that sets the size of the given object."
        raise NotImplementedError

    def generate_code_common_properties(self, widget):
        """generates the code for various properties common to all widgets (background and foreground colours, font,...)
        returns a list of strings containing the generated code"""
        out = []
        if widget.check_prop('size') and not widget.IS_CLASS:
            out.append(self.generate_code_size(widget))
        if widget.check_prop_truth('background'): out.append(self.generate_code_background(widget))
        if widget.check_prop_truth('foreground'): out.append(self.generate_code_foreground(widget))
        if widget.check_prop('font'):             out.append(self.generate_code_font(widget))
        # tooltip
        if widget.check_prop('tooltip')  and widget.tooltip:  out.append( self._generic_code(widget, 'tooltip') )
        # trivial boolean properties
        if widget.check_prop('disabled') and widget.disabled: out.append( self._generic_code(widget, 'disabled') )
        if widget.check_prop('focused' ) and widget.focused:  out.append( self._generic_code(widget, 'focused') )
        if widget.check_prop('hidden'  ) and widget.hidden:   out.append( self._generic_code(widget, 'hidden') )

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

        # UnicodeEncodeError will be handled in application.generate_code
        tmp = [encode(line) for line in tmp if line]

        # check for necessary sub directories e.g. for Perl or Python modules
        dirname = os.path.dirname(filename)
        if dirname and not os.path.isdir(dirname):
            try:
                os.makedirs(dirname)
            except:
                logging.exception( _('Can not create output directory "%s"'), dirname )

        # save the file now
        try:
            common.save_file(filename, tmp, 'codegen')
        except (UnicodeEncodeError, EnvironmentError):
            # these will be handled inside application.generate_code
            raise
        except:
            if config.debugging: raise
            logging.exception(_('Internal Error'))
        if mainfile and sys.platform in ['linux2', 'darwin']:
            try:
                # make the file executable
                os.chmod(filename, 0o755)
            except OSError as e:
                # this isn't necessarily a bad error
                self.warning(_('Changing permission of file "%s" failed: %s') % (filename, str(e)))

        logging.info('Generated %s', filename)

    def store_as_attr(self, obj):
        """Returns True if 'obj' should be added as an attribute of its parent's class,
        False if it should be created as a local variable.
        
        The function returns True of the object klass is listed in classattr_always.

        The function returns True for all widgets except sizers, if
         - the property exists and is an integer greater equal 1
         - the property does not exists
         - the property contains a non-integer value

        The function returns True for sizers, if
         - the property exists and is an integer greater equal 1

        see: classattr_always
        """
        if not obj.IS_NAMED: return False
        if obj.WX_CLASS in self.classattr_always:
            return True
        if 'attribute' in obj.properties:
            return obj.attribute

        if obj.WX_CLASS in ("wxStaticText","wxHyperlinkCtrl","wxStaticBitmap","wxStaticLine"):
            return False
        return True  # this is the default

    def tabs(self, number):
        "Return a proper formatted string for indenting lines"
        return self.indent_symbol * self.indent_amount * number

    def warning(self, msg):
        "Show a warning message"
        if self._show_warnings:
            logging.warning(msg)

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
        Those values are stored in BaseSourceFileContent.spaces.

        source: Source content string with tags to replace"""

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
        return '\\"'

    def _file_exists(self, filename):
        "Check if the file exists; separated for debugging purposes"
        return os.path.isfile(filename)

    def add_object_format_name(self, name):
        "Format a widget name to use in add_object("
        return name

    def _format_classattr(self, obj):
        "Format the object name to store as a class attribute; obj: Instance of xml_parse.CodeObject"
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
        The function just returned tmpl_style. Write a derived version implementation if more logic is needed."""
        if code_obj.IS_TOPLEVEL or (not self.preview and code_obj.check_prop_truth("class")):
            if style:
                return self.tmpl_toplevel_style
            return self.tmpl_toplevel_style0
        if style:
            return self.tmpl_style
        return self.tmpl_style0

    def _generic_code(self, obj, prop_name):
        """Create a code statement for calling a method e.g. to hide a widget.

        obj:       Instance of xml_parse.CodeObject
        prop_name: Name of the property to set

        returns Code statement or None

        see: _code_statements"""

        value = obj.properties[prop_name].get_value()
        if isinstance(value, compat.basestring): value = self.quote_str(obj.tooltip)
        tmpl = self._get_code_statement(prop_name)
        objname = self.format_generic_access(obj)
        stmt = tmpl % { 'objname': objname, 'tooltip': value }
        return stmt

    def _get_code_statement(self, prop_name):
        """Return non-formatted code statement related to prop_name.
        This function handled the properties extensions described in _code_statements.

        prop_name: Name of the property to set
        returns Code statement or None

        see: _code_statements"""

        prop_name_major = '%s_%d' % ( prop_name, self.for_version[0] )
        prop_name_detailed = '%s_%d%d' % ( prop_name, self.for_version[0], self.for_version[1] )

        # check if there is an code template for this prop_name most specific to generic
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
        For example: 'self'}, '$self' or '$self->'."""
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
                return b'\\u00' + ('%x'% dec).encode()
            return matchobj.group(0)

        s = re.sub(b'[\\x80-\\xFF]{1}', repl, s)

        return s

    def _source_warning(self, klass, msg, sub_obj):
        """Format and add a warning message to the source code.
        
        The message msg will be split into single lines and every line will be properly formatted.
        
        klass: Instance of ClassLines to add the code in
        msg:   Multiline message
        
        sub_obj: Object to generate code for (CodeObject instance)
        
        see: _format_comment()"""
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
        klass.init.extend(code_lines)

    def _string_to_colour(self, s):
        """Convert a colour values out of a hex string to comma separated decimal values.

        Example::
            >>> self._string_to_colour('#FFFFFF')
            '255, 255, 255'
            >>> self._string_to_colour('#ABCDEF')
            '171, 205, 239' """
        return '%d, %d, %d' % ( int(s[1:3], 16), int(s[3:5], 16), int(s[5:], 16) )

    def _tagcontent(self, tag, content, newline=False):
        """Returns content embedded between 'begin wxGlade' and 'end wxGlade' sequence.

        returns: Embedded content string
        tag:     used in 'begin wxGlade' statement to indicate different blocks
        content: Content to enter as string or list of strings
        newline: Add a tailing empty line?"""
        code_list = []
        if self._mark_blocks:
            code_list.append( '%s begin wxGlade: %s' % (self.comment_sign, tag) )
        if isinstance(content, compat.basestring):
            # don't append empty content
            _content = content.rstrip()
            if _content:
                code_list.append(_content)
        else:  # list or iterator
            for entry in content:
                code_list.append(entry.rstrip())

        if self._mark_blocks:
            code_list.append( '%s end wxGlade' % self.comment_sign )
        # newline for "end wxGlade" line
        code_list.append('')
        if newline:
            code_list.append('')
        return "\n".join(code_list)

    def copy(self):
        """Return a deep copy of the current instance.
        The instance will be reinitialised with defaults automatically in __setstate__()."""
        return copy.deepcopy(self)
