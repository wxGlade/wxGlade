"""\
C++ code generator

@copyright: 2002-2007 Alberto Griggio
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import os.path, re, logging

from codegen import BaseLangCodeWriter, BaseSourceFileContent, _replace_tag
from codegen import ClassLines as BaseClassLines
import config, wcodegen


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
        check_old_methods = []  # list of indices with set_properties or do_layout
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
                self.classes.add( self.class_name )  # add the found class to the list of classes of this module
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
                    if which_block in ("do_layout","set_properties"):
                        # probably to be removed
                        check_old_methods.append( len(out_lines) )

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
                            self.event_handlers.setdefault( which_class, set() ).add( which_handler )
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

        # when moving from 0.9 to 1.0: remove empty methods "do_layout" and "set_properties"
        while check_old_methods:
            i = check_old_methods.pop(-1)
            if out_lines[i+1].strip()=='}':  # just end of block -> remove incl. trailing empty lines
                self._remove_method(out_lines, i-2, i+1)

        # set the ``persistent'' content of the file
        if is_header:
            self.header_content = out_lines
        else:
            self.content = out_lines

    def is_end_of_class(self, line):
        """Returns True if the line is the last line of a class
        Not really, but for wxglade-generated code it should work..."""
        return self.rec_class_end.match(line)



class ClassLines(BaseClassLines):
    """Stores the lines of C++ code for a custom class"""
    def __init__(self):
        BaseClassLines.__init__(self)
        self.ids = [] # Ids declared in the source (for Evt. handling): grouped in a public enum in the custom class
        self.sub_objs = [] # List of 2-tuples (type, name) of the sub-objects; attributes of the toplevel object
        self.extra_code_h   = [] # Extra header code to output
        self.extra_code_cpp = [] # Extra source code to output
        self.dependencies = set()


class CPPCodeWriter(BaseLangCodeWriter, wcodegen.CppMixin):
    """Code writer class for writing C++ code out of the designed GUI elements

    source_extension: Extension of the source file
    header_extension: Extension of the header file
    last_generated_id: Last generated Id number (wxNewId() is not used yet)
    tmpl_init_gettext: Template for inclusion of i18n headers and defining APP_CATALOG constant or None

    see: BaseLangCodeWriter"""
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

    tmpl_sizeritem = '%s->Add(%s, %s, %s, %s);\n'
    tmpl_sizeritem_button = '%s->AddButton(%s)\n'
    tmpl_gridbagsizeritem = '%s->Add(%s, wxGBPosition%s, wxGBSpan%s, %s, %s);\n'
    tmpl_gridbagsizerspacer = '%s->Add(%s, %s, wxGBPosition%s, wxGBSpan%s, %s, %s);\n'
    tmpl_spacersize = '%s, %s'

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

            if not self._overwrite:
                header_exists = self._file_exists(name + "." + self.header_extension)
                source_exists = self._file_exists(name + "." + self.source_extension)
                if (header_exists and not source_exists) or (source_exists and not header_exists):
                    ret = _("To keep existing user code, both header and source file must exist.\n"
                            "(files '%s...'")
                    return ret%name

            if not self._overwrite and header_exists:
                # keep all the lines not inside a wxGlade block.
                self.previous_source = SourceFileContent(name, self)
            else:
                # if the file doesn't exist, create it and write the intro
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
                    deps = set()
                    for code in self.classes.values():
                        deps.update(code.dependencies)
                    lines = self._format_dependencies( deps )
                elif tag[2] == 'methods':
                    lines = ''
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
            deps = set()
            for code in self.classes.values():
                deps.update(code.dependencies)
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
        assert code_obj not in self.classes
        try:
            builder = self.obj_builders[code_obj.WX_CLASS]
        except KeyError:
            logging.error('%s', code_obj)
            # this is an error, let the exception be raised; the details are logged by the global exception handler
            raise
        ret = self.classes[code_obj] = self.ClassLines()  # ClassLines will collect the code lines incl. children
        return ret

    def finalize_class(self, code_obj):
        # write the collected code for the class and its children
        base = code_obj.WX_CLASS
        klass = self.classes[code_obj]
        classname = code_obj.klass
        fmt_klass = self.cn_class(classname)

        if self.multiple_files:
            # let's see if the file to generate exists, and in this case create a SourceFileContent instance
            filename = os.path.join(self.out_dir, classname.replace('::', '_') + "." + self.header_extension)
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = SourceFileContent( os.path.join(self.out_dir, classname), self )
        else:
            # in this case, previous_source is the SourceFileContent instance
            # that keeps info about the single file to generate
            prev_src = self.previous_source

        if prev_src and classname in prev_src.classes:
            is_new = False
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True

        builder = self.obj_builders[base]
        mycn = getattr(builder, 'cn', self.cn)
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        # collect all event handlers
        event_handlers = klass.event_handlers
        for win_id, evt, handler, evt_type in builder.get_event_handlers(code_obj):
            event_handlers.append((win_id, mycn(evt), handler, evt_type))

        # try to see if there's some extra code to add to this class
        extra_code = getattr(builder, 'extracode', getattr(code_obj, 'extracode', "") or "")
        if extra_code:
            extra_code = re.sub(r'\\n', '\n', extra_code)
            extra_code = re.split(re.compile(r'^###\s*$', re.M), extra_code, 1)
            klass.extra_code_h.append(extra_code[0].rstrip())
            if len(extra_code) > 1:
                klass.extra_code_cpp.append(extra_code[1].rstrip())
            if not is_new:
                self.warning( '%s has extra code, but you are not overwriting existing sources:'
                              ' please check that the resulting code is correct!' % code_obj.name )

        if not self.multiple_files:
            if klass.extra_code_h:
                self._current_extra_code_h.append( "\n".join( klass.extra_code_h[::-1] ) )
            if klass.extra_code_cpp:
                self._current_extra_code_cpp.append( "\n".join( klass.extra_code_cpp[::-1] ) )

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
        custom_base = code_obj.check_prop_nodefault('custom_base') and code_obj.custom_base.strip() or None

        # the header and code lines
        header_buffer = []
        source_buffer = []
        hwrite = header_buffer.append
        swrite = source_buffer.append

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
            ids = klass.ids

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
            # declarations of the attributes
            hwrite('\n')
            hwrite('protected:\n')
            hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % fmt_klass)
            for o_type, o_name in klass.sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            hwrite(self.tabs(1) + '// end wxGlade\n')

            if event_handlers:
                t = self.tabs(1)
                hwrite('\n' + t + 'DECLARE_EVENT_TABLE();\n')
                hwrite('\npublic:\n')
                already_there = set()
                for win_id, evt, handler, evt_type in event_handlers:
                    if handler not in already_there:
                        hwrite('%svirtual void %s(%s &event); // wxGlade: <event_handler>\n' % (t, handler, evt_type))
                        already_there.add( handler )

            hwrite('}; // wxGlade: end class\n\n')

        elif prev_src:
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::ids\n' % fmt_klass)
            ids = klass.ids

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
            tag = '<%swxGlade replace %s ids>' % (self.nonce, classname)
            if not prev_src.replace_header( tag, "".join(header_buffer) ):
                # no ids tag found, issue a warning and do nothing
                self.warning("wxGlade ids block not found for %s, ids declarations code NOT generated" % code_obj.name)

            # remove methods block if in old file
            tag = '<%swxGlade replace %s methods>' % (self.nonce, classname)
            prev_src.replace_header(tag, [])

            header_buffer = []
            hwrite = header_buffer.append
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % fmt_klass)
            for o_type, o_name in klass.sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            if self._mark_blocks:
                hwrite(self.tabs(1) + '// end wxGlade\n')
            tag = '<%swxGlade replace %s attributes>' % (self.nonce, classname)
            if not prev_src.replace_header(tag, "".join(header_buffer)):
                # no attributes tag found, issue a warning and do nothing
                self.warning( "wxGlade attributes block not found for %s, attributes declarations code NOT generated" %
                              code_obj.name )

            header_buffer = []
            hwrite = header_buffer.append
            if event_handlers:
                already_there = prev_src.event_handlers.get(classname, set())
                t = self.tabs(1)
                for win_id, evt, handler, evt_type in event_handlers:
                    if handler not in already_there:
                        hwrite('%svirtual void %s(%s &event); // wxGlade: <event_handler>\n' % (t, handler, evt_type))
                        already_there.add( handler )
                if classname not in prev_src.event_table_def:
                    hwrite('\nprotected:\n')
                    hwrite(self.tabs(1) + 'DECLARE_EVENT_TABLE()\n')
            tag = '<%swxGlade event_handlers %s>' % (self.nonce, classname)
            if not prev_src.replace_header( tag, "".join(header_buffer) ):
                # no attributes tag found, issue a warning and do nothing
                self.warning( "wxGlade events block not found for %s, event table code NOT generated" % code_obj.name )

        # source file
        tab = self.tabs(1)
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

            swrite('\n%s::%s(%s):\n%s%s\n{\n' % (fmt_klass, fmt_klass, sign_decl2, tab, base) )

        if self._mark_blocks:
            swrite(tab + '// begin wxGlade: %s::%s\n' % (fmt_klass, fmt_klass))

        # the optional initial code from the code properties
        if not self.preview and code_obj.check_prop("extracode_pre"):
            for l in code_obj.properties["extracode_pre"].get_lines():
                swrite(tab + l)

        # set size here to avoid problems with splitter windows
        if code_obj.check_prop('size'):
            swrite( tab + self.generate_code_size(code_obj) )
        if code_obj.check_prop('min_size'):
            swrite( tab + self.generate_code_size(code_obj, code_obj.min_size, "SetMinSize") )

        for l in builder.get_properties_code(code_obj):
            swrite(tab + l)

        for l in klass.init:
            swrite(tab + l)

        if klass.final:
            swrite(tab + "\n")
            for l in klass.final:
                swrite(tab + l)

        for l in builder.get_layout_code(code_obj):
            swrite(tab + l)

        # the optional final code from the code properties
        if not self.preview and code_obj.check_prop("extracode_post"):
            for l in code_obj.properties["extracode_post"].get_lines():
                swrite(tab + l)

        # now check if there are extra lines to add to the constructor
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
            tag = '<%swxGlade replace %s %s>' % (self.nonce, classname, classname)
            if not prev_src.replace( tag, "".join(source_buffer) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s::%s block not found, relative code NOT generated" % (fmt_klass, fmt_klass) )
            source_buffer = []
            swrite = source_buffer.append

        # generate code for event table
        code_lines = self.generate_code_event_table( code_obj, is_new, tab, prev_src, event_handlers )

        if prev_src and not is_new:
            tag = '<%swxGlade replace %s event_table>' % (self.nonce, classname)
            if not prev_src.replace( tag, "".join(code_lines) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s::event_table block not found, relative code NOT generated" % fmt_klass )
        else:
            source_buffer.extend(code_lines)

        # generate code for event handler stubs
        code_lines = self.generate_code_event_handler( code_obj, is_new, tab, prev_src, event_handlers )

        # replace code inside existing event handlers
        if prev_src and not is_new:
            tag = '<%swxGlade add %s event handlers>' % (self.nonce, classname)
            if not prev_src.replace( tag, "".join(code_lines) ):
                # no constructor tag found, issue a warning and do nothing
                self.warning( "wxGlade %s event handlers marker not found, relative code NOT generated" % fmt_klass )
        else:
            source_buffer.extend(code_lines)

        if not self.multiple_files and prev_src:
            # if this is a new class, add its code to the new_classes list of the SourceFileContent instance
            if is_new:
                prev_src.new_classes.append( ("".join(header_buffer), "".join(source_buffer)) )
            return

        if self.multiple_files:
            if base in self.obj_builders:
                klass.dependencies.update( getattr(self.obj_builders[base], 'import_modules', []) )

            if prev_src:
                tag = '<%swxGlade insert new_classes>' % self.nonce
                prev_src.replace_header(tag, "")

                # insert the module dependencies of this class
                # WARNING: there's a double space '  ' between 'replace' and 'dependencies' in the tag below,
                # because there is no class name (see SourceFileContent, line ~147)
                tag = '<%swxGlade replace  dependencies>' % self.nonce
                code = self._format_dependencies(klass.dependencies)
                prev_src.replace_header(tag, code)

                # insert the extra code of this class
                extra_code_h = "".join(klass.extra_code_h[::-1])
                extra_code_cpp = "".join(klass.extra_code_cpp[::-1])
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
                name = os.path.join(self.out_dir, classname)
                self.save_file( name +"."+ self.header_extension, "".join(prev_src.header_content), content_only=True )
                self.save_file( name +"."+ self.source_extension, "".join(prev_src.content), content_only=True )

                return

            # create the new source file
            header_file = os.path.join(self.out_dir, classname + "." + self.header_extension)
            source_file = os.path.join(self.out_dir, classname + "." + self.source_extension)
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
            code = self._format_dependencies(klass.dependencies)
            hout.append(code)
            hout.append('\n')

            # insert the extra code of this class
            extra_code_h = "".join(klass.extra_code_h[::-1])
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
            extra_code_cpp = "".join(klass.extra_code_cpp[::-1])
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

    def add_object(self, parent_klass, parent, parent_builder, obj):
        # get the widget builder instance
        builder = self._get_object_builder(parent_klass, obj)
        if not builder: return None

        try:
            init, ids, final = builder.get_code(obj)
        except:
            print(obj)
            raise  # this shouldn't happen

        if not obj.IS_SIZER:  # the object is a wxWindow instance
            if obj.check_prop_truth("extracode_pre"):
                init = obj.properties["extracode_pre"].get_lines() + init
            if obj.check_prop_truth("extracode_post"):
                init += obj.properties["extracode_post"].get_lines()
            if obj.check_prop_truth('extraproperties'):  # insert these only after extracode_post
                init += self.generate_code_extraproperties(obj)

            mycn = getattr(builder, 'cn', self.cn)
            for win_id, evt, handler, evt_type in builder.get_event_handlers(obj):
                parent_klass.event_handlers.append( (win_id, mycn(evt), handler, evt_type) )

            # try to see if there's some extra code to add to this class
            extra_code = getattr(builder, 'extracode', getattr(obj, 'extracode', "") or "" )
            if extra_code:
                extra_code = re.sub(r'\\n', '\n', extra_code)
                extra_code = re.split(re.compile(r'^###\s*$', re.M), extra_code, 1)
                parent_klass.extra_code_h.append(extra_code[0].rstrip())
                if len(extra_code) > 1:
                    parent_klass.extra_code_cpp.append(extra_code[1].rstrip())
                # if we are not overwriting existing source, warn the user about the presence of extra code
                if not self.multiple_files and self.previous_source:
                    self.warning( '%s has extra code, but you are not overwriting existing sources: please check '
                                  'that the resulting code is correct!' % obj.name )

            parent_klass.ids.extend(ids)

        if self.store_as_attr(obj):
            if obj.check_prop("instance_class"):
                klassname = obj.instance_class
            else:
                klassname = obj.get_prop_value("class", obj.WX_CLASS)
            parent_klass.sub_objs.append( (klassname, obj.name) )

        parent_klass.init.extend(init)

        if obj.check_prop_truth("max_size"):
            parent_klass.init.append( self.generate_code_size(obj, obj.max_size, "SetMaxSize") )

        if parent_builder:  # add to sizer or notebook
            parent_klass.init.extend( parent_builder.get_code_per_child(parent, obj) )


        parent_klass.final[:0] = final
        if self.multiple_files and obj.IS_CLASS:
            parent_klass.dependencies.append(obj.klass)
        else:
            if obj.WX_CLASS in self.obj_builders:
                headers = getattr(self.obj_builders[obj.WX_CLASS], 'import_modules', [])
                parent_klass.dependencies.update(headers)
        return builder

    def generate_code_event_handler(self, code_obj, is_new, tab, prev_src, event_handlers):
        """Generate the event handler stubs
        Parameters:
            code_obj: Object to generate code for (CodeObject)
            is_new: Indicates if previous source code exists (bool)
            tab: Indentation of function body (str)
            prev_src: Previous source code (SourceFileContent)
            event_handlers: List of event handlers
        see: tmpl_func_event_stub"""
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
            already_there = prev_src.event_handlers.get(code_obj.klass, set())
        else:
            already_there = set()

        for win_id, event, handler, evt_type in event_handlers:
            if handler not in already_there:
                swrite( tmpl_handler % {'evt_type': evt_type, 'handler': handler, 'klass': code_obj.klass, 'tab': tab} )
                already_there.add( handler )
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
            write('\nBEGIN_EVENT_TABLE(%s, %s)\n' % (code_obj.klass, code_obj.WX_CLASS))
        write(tab + '// begin wxGlade: %s::event_table\n' % code_obj.klass)

        for obj, event, handler, evt_type in event_handlers:
            if obj is None: continue
            if isinstance(obj, str):
                win_id = obj
            else:
                win_id = self.generate_code_id(obj)[1]
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
            if obj is not None and obj.check_prop_truth("stockitem"):
                return '', "wxID_" + obj.stockitem
            return '', 'wxID_ANY'
        id = str(id)
        tokens = id.split('=', 1)
        if len(tokens) != 2:
            return '', tokens[0]   # we assume name is declared elsewhere
        name, val = tokens
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

    def generate_code_size(self, obj, size=None, method=None):
        objname = self.format_generic_access(obj)
        if obj.IS_CLASS:
            name2 = 'this'
        else:
            name2 = obj.name
        if size is None:
            size = obj.properties["size"].get_string_value()
        use_dialog_units = (size[-1] == 'd')
        if method is None:
            method = 'SetMinSize'  if obj.parent_window else  'SetSize'

        if use_dialog_units:
            return '%s%s(wxDLG_UNIT(%s, wxSize(%s)));\n' % (objname, method, name2, size[:-1])
        return '%s%s(wxSize(%s));\n' % (objname, method, size)

    def quote_path(self, s):
        return 'wxT(%s)' % super(CPPCodeWriter, self).quote_path(s)

    def _quote_str(self, s):
        if self._use_gettext:
            return '_("%s")' % s
        return 'wxT("%s")' % s

    def format_generic_access(self, obj):
        if obj.IS_CLASS:
            return ''
        return '%s->' % obj.name

    def _format_dependencies(self, dependencies):
        "Format a list of header files for the dependencies output"
        dep_list = []
        for dependency in sorted(dependencies):  # unique and sorted
            if dependency and ('"' != dependency[0] != '<'):
                dep_list.append('#include "%s.h"\n' % dependency)
            else:
                dep_list.append('#include %s\n' % dependency)
        return self._tagcontent( '::dependencies', dep_list )

writer = CPPCodeWriter()  # The code writer is an instance of CPPCodeWriter

language = writer.language  # Language generated by this code generator
