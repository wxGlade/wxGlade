"""\
C++ code generator

How the code is generated: every time the end of an object is reached during
the parsing of the xml tree, either the function 'add_object' or the function
'add_class' is called: the latter when the object is a toplevel one, the former
when it is not. In the last case, 'add_object' calls the appropriate ``writer''
function for the specific object, found in the 'obj_builders' dict. Such
function accepts one argument, the CodeObject representing the object for
which the code has to be written, and returns 3 lists of strings, representing
the lines to add to the '__init__', '__set_properties' and '__do_layout'
methods of the parent object.

@copyright: 2002-2007 Alberto Griggio
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2017-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import os.path, re

from codegen import BaseLangCodeWriter, BaseSourceFileContent, BaseWidgetHandler, _replace_tag
from codegen import ClassLines as BaseClassLines
import config, compat, misc
import wcodegen


class SourceFileContent(BaseSourceFileContent):
    """Keeps info about an existing file that has to be updated, to replace only the lines inside a wxGlade block,
    and to keep the rest of the file as it was.

    @ivar event_handlers: dictionary of event handlers for each class
    @ivar header_content: Content of the header file
    @ivar source_content: Content of the source file"""

    rec_block_start = re.compile(
        r'^(?P<spaces>\s*)'                     # leading spaces
        r'//\s*'                                # comment sign
        r'begin\s+wxGlade:\s*'                  # "begin wxGlade:" statement and tailing spaces
        r'(?P<classname>\w*)'                   # class or function name
        r'::'                                   # separator between class and function / block (non-greedy)
        r'(?P<block>\w+)'                       # function / block name
        r'\s*$'                                 # tailing spaces
        )

    rec_block_end = re.compile(
        r'^\s*'                                 # leading spaces
        r'//\s*'                                # comment sign
        r'end\s+wxGlade'                        # "end exGlade" statement
        r'\s*$'                                 # tailing spaces
        )

    rec_class_end = re.compile(
        r'^\s*};\s*'                            # closing curly brackets
        r'//\s*'                                # comment sign
        r'wxGlade:\s+end\s+class'               # "wxGlade: end class" statement
        r'\s*$'                                 # tailing spaces
        )
    "Regexp to match last line of a class statement"

    rec_class_decl = re.compile(
        r'^\s*'                                  # leading spaces
        r'class\s+([a-zA-Z_]\w*)'                # "class <name>" statement
        r'\s*'                                   # tailing spaces
        )
    """Regexp to match class declarations

    This isn't very accurate - doesn't match template classes, nor virtual
    inheritance, but should be enough for most cases"""

    rec_decl_event_table = re.compile(
        r'^\s*'                                       # leading spaces
        r'DECLARE_EVENT_TABLE\s*\(\s*\)\s*;?'         # declaration of the event table
        r'\s*$'                                       # tailing spaces
        )
    "Regexp to match declaration of event table"

    rec_def_event_table = re.compile(
        r'^\s*'                                       # leading spaces
        r'BEGIN_EVENT_TABLE\s*\(\s*(\w+)\s*,\s*(\w+)\s*\)'
        r'\s*$'                                       # tailing spaces
        )
    "Regexp to match event table"

    rec_event_handler = re.compile(
        r'^\s*'                                       # leading spaces
        r'(?:virtual\s+)?'
        r'void\s+(?P<handler>[A-Za-z_]+\w*)'          # event handler name
        r'\s*'                                        # optional spaces
        r'\([A-Za-z_:0-9]+\s*&\s*\w*\)\s*;'
        r'\s*'                                        # optional spaces
        r'//\s*wxGlade:\s*<event_handler>'            # wxGlade event handler statement
        r'\s*$'                                       # tailing spaces
        )

    rec_event_handlers_marker = re.compile(
        r'^\s*'                                       # leading spaces
        r'//\s*wxGlade:\s*add\s+'
        r'((?:\w|:)+)\s+event handlers'
        r'\s*$'                                       # tailing spaces
        )
    "Regexp to match wxGlade comment of event handlers"

    def __init__(self, name, code_writer):

        # initialise new variables first
        self.header_content = None
        #self.source_content = None
        self.content = None
        self.event_table_decl = {}
        self.event_table_def  = {}
        self.header_extension = code_writer.header_extension
        self.source_extension = code_writer.source_extension

        # call inherited constructor
        BaseSourceFileContent.__init__(self, name, code_writer)

    def replace_header(self, tag, content):
        return _replace_tag(self.header_content, tag, content)

    def build_untouched_content(self):
        BaseSourceFileContent.build_untouched_content(self)
        self._build_untouched(self.name + "." + self.header_extension, True)
        BaseSourceFileContent.build_untouched_content(self)
        self._build_untouched(self.name + "." + self.source_extension, False)

    def _build_untouched(self, filename, is_header):
        prev_was_handler = False
        events_tag_added = False

        inside_block = False
        inside_comment = False
        tmp_in = self._load_file(filename)
        out_lines = []
        for line in tmp_in:
            comment_index = line.find('/*')
            if not inside_comment and comment_index != -1 and comment_index > line.find('//'):
                inside_comment = True
            if inside_comment:
                end_index = line.find('*/')
                if end_index > comment_index:
                    inside_comment = False
            if not is_header:
                result = None
            else:
                result = self.rec_class_decl.match(line)
            if not inside_comment and not inside_block and result:
                if not self.class_name:
                    # this is the first class declared in the file: insert the new ones before this
                    out_lines.append( '<%swxGlade insert new_classes>' % self.nonce )
                    self.new_classes_inserted = True
                self.class_name = result.group(1)
                self.class_name = self.format_classname(self.class_name)
                self.classes[self.class_name] = 1  # add the found class to the list of classes of this module
                out_lines.append(line)
            elif not inside_block:
                result = self.rec_block_start.match(line)
                if not inside_comment and result:
                    # replace the lines inside a wxGlade block with a tag that will be used later by add_class
                    spaces = result.group('spaces')
                    which_class = result.group('classname')
                    which_block = result.group('block')
                    if not which_class:
                        which_class = self.class_name
                    else:
                        which_class = self.format_classname(which_class)
                    self.spaces[which_class] = spaces
                    inside_block = True
                    out_lines.append( '<%swxGlade replace %s %s>' %
                                      (self.nonce, result.group('classname'), result.group('block') ) )
                else:
                    dont_append = False

                    # ALB 2004-12-08 event handling support...
                    if is_header and not inside_comment:
                        result = self.rec_event_handler.match(line)
                        if result:
                            prev_was_handler = True
                            which_handler = result.group('handler')
                            which_class = self.class_name
                            self.event_handlers.setdefault(which_class, {})[which_handler] = 1
                        else:
                            if prev_was_handler:
                                # add extra event handlers here...
                                out_lines.append('<%swxGlade event_handlers %s>' % (self.nonce, self.class_name) )
                                prev_was_handler = False
                                events_tag_added = True
                            elif not events_tag_added and \
                                     self.is_end_of_class(line):
                                out_lines.append( '<%swxGlade event_handlers %s>' % (self.nonce, self.class_name) )
                            # now try to see if we already have a DECLARE_EVENT_TABLE
                            result = self.rec_decl_event_table.match(line)
                            if result:
                                self.event_table_decl[self.class_name] = True
                    elif not inside_comment:
                        result = self.rec_event_handlers_marker.match(line)
                        if result:
                            out_lines.append( '<%swxGlade add %s event handlers>' % (self.nonce, result.group(1)) )
                            dont_append = True
                        result = self.rec_def_event_table.match(line)
                        if result:
                            which_class = result.group(1)
                            self.event_table_def[which_class] = True
                    # ----------------------------------------

                    if not dont_append:
                        out_lines.append(line)
            else:
                # ignore all the lines inside a wxGlade block
                if self.rec_block_end.match(line):
                    inside_block = False
        if is_header and not self.new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not contain any class, so we must add the
            # new_classes tag at the end of the file
            out_lines.append('<%swxGlade insert new_classes>' % self.nonce)
        # set the ``persistent'' content of the file
        if is_header:
            self.header_content = out_lines
        else:
            self.content = out_lines

    def is_end_of_class(self, line):
        """Returns True if the line is the last line of a class
        Not really, but for wxglade-generated code it should work..."""
        return self.rec_class_end.match(line)



class WidgetHandler(BaseWidgetHandler):
    "Interface the various code generators for the widgets must implement"

    constructor    = []  # 'signature' of the widget's constructor
    import_modules = []  # If not None, list of extra header file, in the form <header.h> or "header.h"

    def __init__(self):
        BaseWidgetHandler.__init__(self)
        self.constructor = []

    def get_ids_code(self, obj):
        """Handler for the code of the ids enum of toplevel objects.
        Returns a list of strings containing the code to generate.
        Usually the default implementation is ok (i.e. there are no extra lines to add)"""
        return []

class ClassLines(BaseClassLines):
    """Stores the lines of C++ code for a custom class"""
    def __init__(self):
        BaseClassLines.__init__(self)
        self.ids = [] # Ids declared in the source (for Evt. handling): grouped in a public enum in the custom class
        self.sub_objs = [] # List of 2-tuples (type, name) of the sub-objects; attributes of the toplevel object
        self.extra_code_h   = [] # Extra header code to output
        self.extra_code_cpp = [] # Extra source code to output
        self.dependencies = []     # List not dictionary


class CPPCodeWriter(BaseLangCodeWriter, wcodegen.CppMixin):
    """Code writer class for writing C++ code out of the designed GUI elements

    source_extension: Extension of the source file
    header_extension: Extension of the header file
    last_generated_id: Last generated Id number (wxNewId() is not used yet)
    tmpl_init_gettext: Template for inclusion of i18n headers and defining APP_CATALOG constant or None

    @see: L{BaseLangCodeWriter}"""
    ClassLines = ClassLines
    _code_statements = {
        'backgroundcolour': "%(objname)sSetBackgroundColour(%(value)s);\n",
        'disabled':         "%(objname)sEnable(0);\n",
        'extraproperties':  "%(objname)sSet%(propname_cap)s(%(value)s);\n",
        'focused':          "%(objname)sSetFocus();\n",
        'foregroundcolour': "%(objname)sSetForegroundColour(%(value)s);\n",
        'hidden':           "%(objname)sHide();\n",
        'setfont':          "%(objname)sSetFont(wxFont(%(size)s, %(family)s, "
                            "%(style)s, %(weight)s, %(underlined)s, wxT(%(face)s)));\n",
        'tooltip':          "%(objname)sSetToolTip(%(tooltip)s);\n",
        'wxcolour':         "wxColour(%(value)s)",
        'wxnullcolour':     "wxNullColour",
        'wxsystemcolour':   "wxSystemSettings::GetColour(%(value)s)",
        }

    class_separator = '::'

    language_note = \
        '// Example for compiling a single file project under Linux using g++:\n' \
        '//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp\n' \
        '//\n' \
        '// Example for compiling a multi file project under Linux using g++:\n' \
        '//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp\n' \
        '//\n'

    output_name   = None  # If not None, name (without extension) of the file to write into
    output_header = None  # Temporary storage of header file for writing into (list)
    output_file   = None  # Temporary storage of source file for writing into (list)

    shebang = '// -*- C++ -*-\n//\n'
    tmpl_cfunc_end = '}\n\n'
    tmpl_name_do_layout = 'do_layout'
    tmpl_name_set_properties = 'set_properties'
    tmpl_sizeritem = '%s->Add(%s, %s, %s, %s);\n'
    tmpl_gridbagsizeritem = '%s->Add(%s, wxGBPosition%s, wxGBSpan%s, %s, %s);\n'
    tmpl_gridbagsizerspacer = '%s->Add(%s, %s, wxGBPosition%s, wxGBSpan%s, %s, %s);\n'
    tmpl_spacersize = '%s, %s'

    tmpl_ctor_call_layout = '\n' \
                            '%(tab)sset_properties();\n' \
                            '%(tab)sdo_layout();\n'

    tmpl_func_do_layout = '\n' \
                          'void %(klass)s::do_layout()\n{\n' \
                          '%(content)s' \
                          '}\n\n'

    tmpl_func_set_properties = '\n' \
                          'void %(klass)s::set_properties()\n{\n' \
                          '%(content)s' \
                          '}\n\n'

    tmpl_appfile = """\
%(overwrite)s\
%(header_lines)s\
#include "%(filename_top_win_class)s"

"""

    tmpl_init_gettext = """\
#include <wx/intl.h>

#ifndef APP_CATALOG
#define APP_CATALOG "%(textdomain)s"  // replace with the appropriate catalog name
#endif

"""

    def _get_app_template(self, app, top_win):
        'build template string for application'
        if not self.app_name: return None

        # XXX use Show() for frames/panels and ShowModal()/Destroy for dialogs
        klass = app.klass
        
        if self._use_gettext:
            gettext1 = ["protected:", "%(tab)swxLocale m_locale;  // locale we'll be using"]
            gettext2 = ['%(tab)sm_locale.Init();',
                        '#ifdef APP_LOCALE_DIR',
                        '%(tab)sm_locale.AddCatalogLookupPathPrefix(wxT(APP_LOCALE_DIR));',
                        '#endif',
                        '%(tab)sm_locale.AddCatalog(wxT(APP_CATALOG));\n']
        else:
            gettext1 = gettext2 = []
        
        if klass:
            klass1 = 'class %(klass)s: public wxApp {'
            klass2 = ['IMPLEMENT_APP(%(klass)s)\n',
                      'bool %(klass)s::OnInit()']
        else:
            klass1 = 'class MyApp: public wxApp {'
            klass2 = ['IMPLEMENT_APP(MyApp)\n',
                      'bool MyApp::OnInit()',]

        ret = ['', klass1,
               'public:', '%(tab)sbool OnInit();'
               ] + gettext1 + ['};\n'] + klass2 + ['{'] + gettext2 + [
                '%(tab)swxInitAllImageHandlers();',
                '%(tab)s%(top_win_class)s* %(top_win)s = new %(top_win_class)s(NULL, wxID_ANY, wxEmptyString);',
                '%(tab)sSetTopWindow(%(top_win)s);',
                '%(tab)s%(top_win)s->Show();',
                '%(tab)sreturn true;',
                '}', '']
        return '\n'.join(ret)

    tmpl_empty_string = 'wxEmptyString'

    def init_lang(self, app=None):
        self.last_generated_id = 1000
        self.generated_ids = {}
        

        # Extensions and main filename based on Project options when set
        if app is not None:
            self.source_extension = app.source_extension or config.default_source_extension
            self.header_extension = app.header_extension or config.default_header_extension
        else:
            self.source_extension = config.default_source_extension
            self.header_extension = config.default_header_extension

        if hasattr(app, "app_filename"):  # only for testing
            base = os.path.splitext(app.app_filename)[0]
        else:
            base = os.path.splitext(config.default_cpp_app_name)[0]  # 
        self.app_filename = '%s.%s' % (base, self.source_extension)


        self.header_lines = [ '#include <wx/wx.h>\n',
                              '#include <wx/image.h>\n' ]

        # include i18n / gettext
        if self._use_gettext and self._textdomain:
            self.header_lines.append( self.tmpl_init_gettext % {'textdomain': self._textdomain} )

        # extra lines to generate (see the 'extracode' property of top-level widgets)
        self._current_extra_code_h = []
        self._current_extra_code_cpp = []

    def init_files(self, out_path):
        if self.multiple_files:
            self.previous_source = None
            self.out_dir = out_path
        else:
            name = os.path.splitext(out_path)[0]
            self.output_name = name
            if not self._overwrite and self._file_exists(name + "." + self.header_extension):
                # the file exists, we must keep all the lines not inside a wxGlade block.
                # NOTE: this may cause troubles if out_path is not a valid source file, so be careful!
                self.previous_source = SourceFileContent(name, self)
            else:
                # if the file doesn't exist, create it and write the ``intro''
                self.previous_source = None
                self.output_header = []
                self.output_file   = []

                # isolation directives
                oh = os.path.basename(name + "." + self.header_extension).upper().replace( '.', '_' )
                self.output_header.append('#ifndef %s\n#define %s\n' % (oh, oh))
                self.output_header.append('\n')

                for line in self.header_lines:
                    self.output_header.append(line)
                self.output_header.append('\n')

                # now, write the tags to store dependencies and extra code
                self.output_header.append('<%swxGlade replace  dependencies>' % self.nonce)
                self.output_header.append('\n')
                self.output_header.append('<%swxGlade replace  extracode>' % self.nonce)

                self.output_header.append('\n')

                self.output_file.append('#include "%s.%s"\n\n' % (os.path.basename(name), self.header_extension))
                self.output_file.append('<%swxGlade replace  extracode>\n' % self.nonce)
                self.output_file.append('\n')

    def output_header_replace(self, tag, content):
        _replace_tag(self.output_header, tag, content)

    def finalize(self):
        if self.previous_source:
            # insert all the new custom classes inside the old file
            tag = '<%swxGlade insert new_classes>' % self.nonce
            if self.previous_source.new_classes:
                code = "".join([c[0] for c in self.previous_source.new_classes])
            else:
                code = ""
            self.previous_source.replace_header(tag, code)
            extra_source = "".join([c[1] for c in self.previous_source.new_classes])

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace  extracode>' % self.nonce
            code = self._tagcontent( '::extracode', self._current_extra_code_h )
            self.previous_source.replace_header(tag, code)
            code = self._tagcontent( '::extracode', self._current_extra_code_cpp )
            self.previous_source.replace(tag, code)
            # --------------------------------------------------------------

            # now remove all the remaining <123415wxGlade ...> tags from the source:
            # this may happen if we're not generating multiple files, and one of the container class names is changed
            tags = re.compile( r'(<%swxGlade replace ([a-zA-Z_]*\w*) (\w+)>)' % self.nonce )
            for i,line in enumerate(self.previous_source.header_content):
                match = tags.match(line)
                if not match: continue
                tag = match.groups()
                if tag[2] == 'dependencies':
                    #self._logger.debug('writing dependencies')
                    deps = []
                    for code in self.classes.values():
                        deps.extend(code.dependencies)
                    lines = self._format_dependencies( deps )
                elif tag[2] == 'methods':
                    lines = '%svoid set_properties();\n%svoid do_layout();\n' % (self.tabs(1), self.tabs(1))
                else:
                    lines = '// content of this block (%s) not found: did you rename this class?\n' % tag[2]
                self.previous_source.replace_header(tag[0], lines)

            # remove all the remaining <123415wxGlade ...> tags in source file    XXX make more efficient
            self._content_notfound( self.previous_source )
            tag_start = r'<%swxGlade add ' % self.nonce
            tag_end = r' event_handlers>'
            for i, line in enumerate(self.previous_source.content):
                if line.startswith(tag_start) and line.endswith(tag_end):
                    source_content.content[i] = ""

            # write the new file contents to disk
            header_content = "".join( self.previous_source.header_content )
            self.save_file( self.previous_source.name + "." + self.header_extension, header_content, content_only=True )
            if extra_source:
                extra_source = '\n\n' + extra_source
            source_content = "".join( self.previous_source.content )
            self.save_file( self.previous_source.name + "." + self.source_extension, source_content + extra_source,
                            content_only=True )

        elif not self.multiple_files:
            oh = os.path.basename(self.output_name).upper() + '_H'
            self.output_header.append('\n#endif // %s\n' % oh)

            # write the list of include files
            deps = []
            for code in self.classes.values():
                deps.extend(code.dependencies)
            code = self._format_dependencies( deps )
            self.output_header_replace( '<%swxGlade replace  dependencies>' % self.nonce, code )

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace  extracode>' % self.nonce
            code = self._tagcontent('::extracode', self._current_extra_code_h)
            self.output_header_replace( tag, code )
            code = self._tagcontent('::extracode', self._current_extra_code_cpp)
            self.output_file_replace( tag, code )

            self.save_file( self.output_name + "." + self.header_extension, self.output_header, self._app_added )
            self.save_file( self.output_name + "." + self.source_extension, self.output_file, self._app_added )
            self.output_file = self.output_header = None

    def add_app(self, app_attrs, top_win):
        # add language specific mappings
        self.lang_mapping['filename_top_win_class'] = '%s.%s' % (top_win.klass, self.header_extension)
        BaseLangCodeWriter.add_app(self, app_attrs, top_win)

    def add_class(self, code_obj):
        # shortcuts
        base = code_obj.base
        klass = code_obj.klass
        fmt_klass = self.cn_class(klass)

        if klass in self.classes and self.classes[klass].done:
            return  # the code has already been generated

        if self.multiple_files:
            # let's see if the file to generate exists, and in this case create a SourceFileContent instance
            filename = os.path.join(self.out_dir, klass.replace('::', '_') + "." + self.header_extension)
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = SourceFileContent( os.path.join(self.out_dir, klass), self )
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
            # this is an error, let the exception be raised; the details are logged by the global exception handler
            raise

        if prev_src and klass in prev_src.classes:
            is_new = False
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True

        header_buffer = []
        source_buffer = []
        hwrite = header_buffer.append
        swrite = source_buffer.append

        if not klass in self.classes:
            # if the class body was empty, create an empty ClassLines
            self.classes[klass] = self.ClassLines()

        # collect all event handlers
        event_handlers = self.classes[klass].event_handlers
        for win_id, evt, handler, evt_type in builder.get_event_handlers(code_obj):
            event_handlers.append((win_id, mycn(evt), handler, evt_type))

        # try to see if there's some extra code to add to this class
        extra_code = getattr(builder, 'extracode', getattr(code_obj, 'extracode', "") or "")
        if extra_code:
            extra_code = re.sub(r'\\n', '\n', extra_code)
            extra_code = re.split(re.compile(r'^###\s*$', re.M), extra_code, 1)
            self.classes[klass].extra_code_h.append(extra_code[0])
            if len(extra_code) > 1:
                self.classes[klass].extra_code_cpp.append(extra_code[1])
            if not is_new:
                self.warning( '%s has extra code, but you are not overwriting existing sources:'
                              ' please check that the resulting code is correct!' % code_obj.name )

        if not self.multiple_files:
            if self.classes[klass].extra_code_h:
                self._current_extra_code_h.append( "".join( self.classes[klass].extra_code_h[::-1] ) )
            if self.classes[klass].extra_code_cpp:
                self._current_extra_code_cpp.append( "".join( self.classes[klass].extra_code_cpp[::-1] ) )

        default_sign = [('wxWindow*', 'parent'), ('wxWindowID', 'id')]
        sign = getattr(builder, 'constructor', default_sign)

        defaults = []
        for t in sign:
            if len(t) == 3:
                defaults.append(t[2])
            else:
                defaults.append(None)
        tmp_sign = [t[0] + ' ' + t[1] for t in sign]
        sign_decl2 = ', '.join(tmp_sign)
        for i in range(len(tmp_sign)):
            if defaults[i]:
                tmp_sign[i] += '=%s' % defaults[i]
        sign_decl1 = ', '.join(tmp_sign)
        sign_inst = ', '.join([t[1] for t in sign])

        # custom base classes support
        custom_base = getattr(code_obj, 'custom_base', code_obj.properties.get('custom_base', None))
        if custom_base and not custom_base.strip():
            custom_base = None

        # generate constructor code
        if is_new:
            pass
        elif custom_base:
            # custom base classes set, but "overwrite existing sources" not
            # set. Issue a warning about this
            self.warning( '%s has custom base classes, but you are not overwriting existing sources: '
                          'please check that the resulting code is correct!' % code_obj.name )

        if is_new:
            # header file
            if custom_base:
                base = ", public ".join([b.strip() for b in custom_base.split(',')])
            hwrite('\nclass %s: public %s {\n' % (fmt_klass, base))
            hwrite('public:\n')
            # the first thing to add it the enum of the various ids
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::ids\n' % fmt_klass)
            ids = self.classes[klass].ids

            # let's try to see if there are extra ids to add to the enum
            if hasattr(builder, 'get_ids_code'):
                ids.extend(builder.get_ids_code(code_obj))

            if ids:
                hwrite(self.tabs(1) + 'enum {\n')
                for id_name in ids:
                    hwrite('%s%s,\n' % (self.tabs(2), id_name))
                hwrite(self.tabs(1) + '};\n')
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// end wxGlade\n\n')
            # constructor prototype
            hwrite(self.tabs(1) + '%s(%s);\n' % (fmt_klass, sign_decl1))
            hwrite('\nprivate:\n')
            # set_properties and do_layout prototypes
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::methods\n' % fmt_klass)
            hwrite(self.tabs(1) + 'void set_properties();\n')
            hwrite(self.tabs(1) + 'void do_layout();\n')
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// end wxGlade\n')
            # declarations of the attributes
            hwrite('\n')
            hwrite('protected:\n')
            hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % fmt_klass)
            for o_type, o_name in self.classes[klass].sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            hwrite(self.tabs(1) + '// end wxGlade\n')

            if event_handlers:
                t = self.tabs(1)
                hwrite('\n' + t + 'DECLARE_EVENT_TABLE();\n')
                hwrite('\npublic:\n')
                already_there = {}
                for win_id, evt, handler, evt_type in event_handlers:
                    if handler not in already_there:
                        hwrite('%svoid %s(%s &event); // wxGlade: <event_handler>\n' % (t, handler, evt_type))
                        already_there[handler] = 1

            hwrite('}; // wxGlade: end class\n\n')

        elif prev_src:
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::ids\n' % fmt_klass)
            ids = self.classes[klass].ids

            # let's try to see if there are extra ids to add to the enum
            if hasattr(builder, 'get_ids_code'):
                ids.extend(builder.get_ids_code(code_obj))

            if ids:
                hwrite(self.tabs(1) + 'enum {\n')
                for id_name in ids:
                    hwrite('%s%s,\n' % (self.tabs(2), id_name))
                hwrite(self.tabs(1) + '};\n')
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// end wxGlade\n')
            tag = '<%swxGlade replace %s ids>' % (self.nonce, klass)
            if not prev_src.replace_header( tag, "".join(header_buffer) ):
                # no ids tag found, issue a warning and do nothing
                self.warning("wxGlade ids block not found for %s, ids declarations code NOT generated" % code_obj.name)
            header_buffer = []
            if self._mark_blocks:
                header_buffer.append( self.tabs(1) + '// begin wxGlade: %s::methods\n' % fmt_klass )
            header_buffer.append( self.tabs(1) + 'void set_properties();\n' )
            header_buffer.append( self.tabs(1) + 'void do_layout();\n' )
            if self._mark_blocks:
                header_buffer.append( self.tabs(1) + '// end wxGlade\n' )
            tag = '<%swxGlade replace %s methods>' % (self.nonce, klass)
            if not prev_src.replace_header(tag, "".join(header_buffer)):
                # no methods tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade methods block not found for %s, methods declarations code NOT generated" % code_obj.name )
            header_buffer = []
            hwrite = header_buffer.append
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % fmt_klass)
            for o_type, o_name in self.classes[klass].sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// end wxGlade\n')
            tag = '<%swxGlade replace %s attributes>' % (self.nonce, klass)
            if not prev_src.replace_header(tag, "".join(header_buffer)):
                # no attributes tag found, issue a warning and do nothing
                self.warning( "wxGlade attributes block not found for %s, attributes declarations code NOT generated" %
                              code_obj.name )

            header_buffer = []
            hwrite = header_buffer.append
            if event_handlers:
                already_there = prev_src.event_handlers.get(klass, {})
                t = self.tabs(1)
                for win_id, evt, handler, evt_type in event_handlers:
                    if handler not in already_there:
                        hwrite('%svoid %s(%s &event); // wxGlade: <event_handler>\n' % (t, handler, evt_type))
                        already_there[handler] = 1
                if klass not in prev_src.event_table_def:
                    hwrite('\nprotected:\n')
                    hwrite(self.tabs(1) + 'DECLARE_EVENT_TABLE()\n')
            tag = '<%swxGlade event_handlers %s>' % (self.nonce, klass)
            if not prev_src.replace_header( tag, "".join(header_buffer) ):
                # no attributes tag found, issue a warning and do nothing
                self.warning( "wxGlade events block not found for %s, event table code NOT generated" % code_obj.name )

        # source file
        # set the window's style
        style_p = code_obj.properties.get("style")
        if style_p and style_p.value_set != style_p.default_value:
            style = mycn_f(style_p.get_string_value())
            if style:
                sign_inst = sign_inst.replace('style', '%s' % style)

        # constructor
        if is_new:
            base = "%s(%s)" % (base, sign_inst)
            if custom_base:
                bases = [b.strip() for b in custom_base.split(',')]
                if bases:
                    base = "%s(%s)" % (bases[0], sign_inst)
                    rest = ", ".join([b + "()" for b in bases[1:]])
                    if rest:
                        base += ", " + rest

            swrite('\n%s::%s(%s):\n%s%s\n{\n' % (fmt_klass, fmt_klass, sign_decl2, self.tabs(1), base) )
        if self._mark_blocks:
            swrite(self.tabs(1) + '// begin wxGlade: %s::%s\n' % (fmt_klass, fmt_klass))

        # set size here to avoid problems with splitter windows
        if 'size' in code_obj.properties and code_obj.properties["size"].is_active():
            swrite( self.tabs(1) + self.generate_code_size(code_obj) )

        tab = self.tabs(1)
        init_lines = self.classes[klass].init
        parents_init = self.classes[klass].parents_init
        parents_init.reverse()
        for l in parents_init:
            swrite(tab + l)
        for l in init_lines:
            swrite(tab + l)

        # now check if there are extra lines to add to the constructor
        if hasattr(builder, 'get_init_code'):
            for l in builder.get_init_code(code_obj):
                swrite(tab + l)

        swrite( self.tmpl_ctor_call_layout % {'tab':tab} )

        if self._mark_blocks:
            # end tag
            swrite('%s%s end wxGlade\n' % (tab, self.comment_sign))

        # write class function end statement
        if self.tmpl_cfunc_end and is_new:
            swrite( self.tmpl_cfunc_end % {'tab':tab} )

        # replace code inside existing constructor block
        if prev_src and not is_new:
            # replace the lines inside the ctor wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, klass)
            if not prev_src.replace( tag, "".join(source_buffer) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s::%s block not found, relative code NOT generated" % (fmt_klass, fmt_klass) )
            source_buffer = []
            swrite = source_buffer.append

        # generate code for __set_properties()
        code_lines = self.generate_code_set_properties( builder, code_obj, is_new, tab )
        source_buffer.extend(code_lines)

        # replace code inside existing __set_properties() function
        if prev_src and not is_new:
            # replace the lines inside the set_properties wxGlade block with the new ones
            tag = '<%swxGlade replace %s set_properties>' % (self.nonce, klass)
            if not prev_src.replace( tag, "".join(source_buffer) ):
                # no set_properties tag found, issue a warning and do nothing
                self.warning( "wxGlade %s::set_properties block not found, relative code NOT generated" % fmt_klass )
            source_buffer = []
            swrite = source_buffer.append

        # generate code for __do_layout()
        code_lines = self.generate_code_do_layout( builder, code_obj, is_new, tab )
        source_buffer.extend(code_lines)

        # replace code inside existing do_layout() function
        if prev_src and not is_new:
            # replace the lines inside the do_layout wxGlade block with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, klass, 'do_layout')
            if not prev_src.replace(tag, "".join(source_buffer)):
                # no do_layout tag found, issue a warning and do nothing
                self.warning( "wxGlade do_layout block not found for %s, do_layout code NOT generated" % code_obj.name )
            source_buffer = []
            swrite = source_buffer.append

        # generate code for event table
        code_lines = self.generate_code_event_table( code_obj, is_new, tab, prev_src, event_handlers )

        if prev_src and not is_new:
            tag = '<%swxGlade replace %s event_table>' % (self.nonce, klass)
            if not prev_src.replace( tag, "".join(code_lines) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s::event_table block not found, relative code NOT generated" % fmt_klass )
        else:
            source_buffer.extend(code_lines)

        # generate code for event handler stubs
        code_lines = self.generate_code_event_handler( code_obj, is_new, tab, prev_src, event_handlers )

        # replace code inside existing event handlers
        if prev_src and not is_new:
            tag = '<%swxGlade add %s event handlers>' % (self.nonce, klass)
            if not prev_src.replace( tag, "".join(code_lines) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s event handlers marker not found, relative code NOT generated" % fmt_klass )
        else:
            source_buffer.extend(code_lines)

        # the code has been generated
        self.classes[klass].done = True

        if not self.multiple_files and prev_src:
            # if this is a new class, add its code to the new_classes list of the SourceFileContent instance
            if is_new:
                prev_src.new_classes.append( ("".join(header_buffer), "".join(source_buffer)) )
            return

        if self.multiple_files:
            if base in self.obj_builders:
                self.classes[klass].dependencies.extend( getattr(self.obj_builders[base], 'import_modules', []) )

            if prev_src:
                tag = '<%swxGlade insert new_classes>' % self.nonce
                prev_src.replace_header(tag, "")

                # insert the module dependencies of this class
                extra_modules = self.classes[klass].dependencies
                # WARNING: there's a double space '  ' between 'replace' and 'dependencies' in the tag below,
                # because there is no class name (see SourceFileContent, line ~147)
                tag = '<%swxGlade replace  dependencies>' % self.nonce
                code = self._format_dependencies(extra_modules)
                prev_src.replace_header(tag, code)

                # insert the extra code of this class
                extra_code_h = "".join(self.classes[klass].extra_code_h[::-1])
                extra_code_cpp = "".join(self.classes[klass].extra_code_cpp[::-1])
                # if there's extra code but we are not overwriting existing sources, warn the user
                if extra_code_h or extra_code_cpp:
                    self.warning( '%s (or one of its children) has extra code classes, but you are not overwriting '
                                  'existing sources: please check that the resulting code is correct!' % code_obj.name )

                extra_code_h   = self._tagcontent("::extracode", extra_code_h)
                extra_code_cpp = self._tagcontent("::extracode", extra_code_cpp)
                tag = '<%swxGlade replace  extracode>' % self.nonce
                prev_src.replace_header(tag, extra_code_h)
                prev_src.replace(tag, extra_code_cpp)

                # store the new file contents to disk
                name = os.path.join(self.out_dir, klass)
                self.save_file( name +"."+ self.header_extension, "".join(prev_src.header_content), content_only=True )
                self.save_file( name +"."+ self.source_extension, "".join(prev_src.content), content_only=True )

                return

            # create the new source file
            header_file = os.path.join(self.out_dir, klass + "." + self.header_extension)
            source_file = os.path.join(self.out_dir, klass + "." + self.source_extension)
            hout = []
            sout = []

            # header file ----------------------------------------------------------------------------------------------

            # isolation directives
            hn = os.path.basename(header_file).upper().replace('.', '_')
            hout.append('#ifndef %s\n#define %s\n' % (hn, hn))
            hout.append('\n')

            # write the common lines
            hout.extend( self.header_lines )
            hout.append('\n')

            # write the module dependencies for this class
            extra_modules = self.classes[klass].dependencies
            code = self._format_dependencies(extra_modules)
            hout.append(code)
            hout.append('\n')

            # insert the extra code of this class
            extra_code_h = "".join(self.classes[klass].extra_code_h[::-1])
            extra_code_h = self._tagcontent('::extracode', extra_code_h)
            hout.append(extra_code_h)
            hout.append('\n')

            # write the class body
            for line in header_buffer:
                hout.append(line)
            hout.append('\n#endif // %s\n' % hn)

            # source file ----------------------------------------------------------------------------------------------
            # write the common lines
            sout.append(self.header_lines[0])
            sout.append('#include "%s"\n\n' % os.path.basename(header_file))

            # insert the extra code of this class
            extra_code_cpp = "".join(self.classes[klass].extra_code_cpp[::-1])
            extra_code_cpp = self._tagcontent('::extracode', extra_code_cpp)
            sout.append(extra_code_cpp)
            sout.append('\n')

            # write the class implementation
            sout.extend(source_buffer)

            # store source to disk
            self.save_file(header_file, hout)
            self.save_file(source_file, sout)

        else:  # not self.multiple_files
            # write the class body onto the single source file
            self.output_header.extend(header_buffer)
            self.output_file.extend(source_buffer)

    def add_object(self, sub_obj):
        # get top level source code object and the widget builder instance
        klass, builder = self._add_object_init(sub_obj)
        if not klass or not builder:
            return

        try:
            init, ids, props, layout = builder.get_code(sub_obj)
        except:
            print(sub_obj)
            raise  # this shouldn't happen

        if not sub_obj.is_sizer:  # the object is a wxWindow instance
            if "extracode_pre" in sub_obj.properties and sub_obj.extracode_pre:
                init = sub_obj.properties["extracode_pre"].get_lines() + init
            if "extracode_post" in sub_obj.properties and sub_obj.extracode_post:
                init += sub_obj.properties["extracode_post"].get_lines()

            if sub_obj.node.children and not sub_obj.is_toplevel:
                init.reverse()
                klass.parents_init.extend(init)
            else:
                klass.init.extend(init)

            mycn = getattr(builder, 'cn', self.cn)
            for win_id, evt, handler, evt_type in builder.get_event_handlers(sub_obj):
                klass.event_handlers.append( (win_id, mycn(evt), handler, evt_type) )

            # try to see if there's some extra code to add to this class
            extra_code = getattr(builder, 'extracode', getattr(sub_obj, 'extracode', "") or "" )
            if extra_code:
                extra_code = re.sub(r'\\n', '\n', extra_code)
                extra_code = re.split(re.compile(r'^###\s*$', re.M), extra_code, 1)
                klass.extra_code_h.append(extra_code[0])
                if len(extra_code) > 1:
                    klass.extra_code_cpp.append(extra_code[1])
                # if we are not overwriting existing source, warn the user about the presence of extra code
                if not self.multiple_files and self.previous_source:
                    self.warning( '%s has extra code, but you are not overwriting existing sources: please check '
                                  'that the resulting code is correct!' % sub_obj.name )

            klass.ids.extend(ids)
            if sub_obj.klass != 'spacer':
                # attribute is a special property which control whether sub_obj must be accessible as an attribute of
                # top_obj, or as a local variable in the do_layout method
                if self.store_as_attr(sub_obj):
                    klass.sub_objs.append((sub_obj.klass, sub_obj.name))
        elif sub_obj.klass != "sizerslot":
            # the object is a sizer
            if self.store_as_attr(sub_obj):
                klass.sub_objs.append((sub_obj.klass, sub_obj.name))
            klass.sizers_init.extend(init)

        klass.props.extend(props)
        klass.layout.extend(layout)
        if self.multiple_files and (sub_obj.is_toplevel and sub_obj.base != sub_obj.klass):
            klass.dependencies.append(sub_obj.klass)
        else:
            if sub_obj.base in self.obj_builders:
                headers = getattr(self.obj_builders[sub_obj.base], 'import_modules', [])
                klass.dependencies.extend(headers)

    def generate_code_event_handler(self, code_obj, is_new, tab, prev_src, event_handlers):
        """Generate the event handler stubs
        Parameters:
            code_obj: Object to generate code for (CodeObject)
            is_new: Indicates if previous source code exists (bool)
            tab: Indentation of function body (str)
            prev_src: Previous source code (SourceFileContent)
            event_handlers: List of event handlers
        @see: L{tmpl_func_event_stub}"""
        code_lines = []
        swrite = code_lines.append

        if not event_handlers:
            return []

        tmpl_handler = """
void %(klass)s::%(handler)s(%(evt_type)s &event)  // wxGlade: %(klass)s.<event_handler>
{
%(tab)sevent.Skip();
%(tab)s// notify the user that he hasn't implemented the event handler yet
%(tab)swxLogDebug(wxT("Event handler (%(klass)s::%(handler)s) not implemented yet"));
}
"""

        if prev_src:
            already_there = prev_src.event_handlers.get(code_obj.klass, {})
        else:
            already_there = {}

        for win_id, event, handler, evt_type in event_handlers:
            if handler not in already_there:
                swrite( tmpl_handler % {'evt_type': evt_type, 'handler': handler, 'klass': code_obj.klass, 'tab': tab} )
                already_there[handler] = 1
        if is_new or not prev_src:
            swrite('\n\n')
        swrite('// wxGlade: add %s event handlers\n' % code_obj.klass)
        if is_new or not prev_src:
            swrite('\n')

        return code_lines

    def generate_code_event_table(self, code_obj, is_new, tab, prev_src, event_handlers):
        """Generate code for event table declaration.
        
        code_obj: Object to generate code for (CodeObject)
        is_new: Indicates if previous source code exists (bool)
        tab: Indentation of function body (str)
        prev_src: Previous source code (SourceFileContent)
        event_handlers: List of event handlers (strings)"""
        code_lines = []
        write = code_lines.append

        if not event_handlers:
            return code_lines

        if prev_src and code_obj.klass in prev_src.event_table_decl:
            has_event_table = True
        else:
            has_event_table = False

        if is_new or not has_event_table:
            write('\nBEGIN_EVENT_TABLE(%s, %s)\n' % (code_obj.klass, code_obj.base))
        write(tab + '// begin wxGlade: %s::event_table\n' % code_obj.klass)

        for win_id, event, handler, evt_type in event_handlers:
            if win_id is None: continue
            if 'EVT_NAVIGATION_KEY' in event:
                tmpl = '%(tab)s%(event)s(%(klass)s::%(handler)s)\n'
            else:
                tmpl = '%(tab)s%(event)s(%(win_id)s, %(klass)s::%(handler)s)\n'
            details = { 'tab': tab, 'event': event, 'win_id': win_id, 'klass': code_obj.klass, 'handler': handler }
            write(tmpl % details)

        write(tab + '// end wxGlade\n')
        if is_new or not has_event_table:
            write('END_EVENT_TABLE();\n\n')

        return code_lines

    def generate_code_id(self, obj, id=None):
        if id is None:
            id = obj.window_id
        if not id:
            if obj is not None and "stockitem" in obj.properties and obj.stockitem:
                return '', "wxID_" + obj.stockitem
            return '', 'wxID_ANY'
        id = str(id)
        tokens = id.split('=', 1)
        if len(tokens) == 2:
            name, val = tokens
        else:
            return '', tokens[0]   # we assume name is declared elsewhere
        if not name:
            return '', val
        name = name.strip()
        val = val.strip()
        if val == '?':
            val = self.generated_ids.get(name)
            if val is None:
                val = 'wxID_HIGHEST + %d' % self.last_generated_id
                self.last_generated_id += 1
                self.generated_ids[name] = val
        else:
            val = val
        return '%s = %s' % (name, val), name

    def generate_code_size(self, obj):
        objname = self.format_generic_access(obj)
        if obj.is_toplevel:
            name2 = 'this'
        else:
            name2 = obj.name
        size = obj.properties["size"].get_string_value()
        use_dialog_units = (size[-1] == 'd')
        if not obj.parent:
            method = 'SetSize'
        else:
            method = 'SetMinSize'
        if use_dialog_units:
            return '%s%s(wxDLG_UNIT(%s, wxSize(%s)));\n' % (objname, method, name2, size[:-1])
        else:
            return '%s%s(wxSize(%s));\n' % (objname, method, size)

    def quote_path(self, s):
        path = super(CPPCodeWriter, self).quote_path(s)
        # path starts and ends with double quotes already
        return 'wxT(%s)' % path

    def _quote_str(self, s):
        if self._use_gettext:
            return '_("%s")' % s
        else:
            return 'wxT("%s")' % s

    def format_generic_access(self, obj):
        if obj.is_toplevel:
            return ''
        else:
            return '%s->' % obj.name

    def _format_dependencies(self, dependencies):
        "Format a list of header files for the dependencies output"
        dep_list = []
        for dependency in sorted( set(dependencies) ):  # unique and sorted
            if dependency and ('"' != dependency[0] != '<'):
                dep_list.append('#include "%s.h"\n' % dependency)
            else:
                dep_list.append('#include %s\n' % dependency)
        code = self._tagcontent( '::dependencies', dep_list )
        return code

writer = CPPCodeWriter()  # The code writer is an instance of CPPCodeWriter

language = writer.language  # Language generated by this code generator
