# cpp_codegen.py: C++ code generator
#
# Copyright (c) 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

"""\
How the code is generated: every time the end of an object is reached during
the parsing of the xml tree, either the function 'add_object' or the function
'add_class' is called: the latter when the object is a toplevel one, the former
when it is not. In the last case, 'add_object' calls the appropriate ``writer''
function for the specific object, found in the 'obj_builders' dict. Such
function accepts one argument, the CodeObject representing the object for
which the code has to be written, and returns 3 lists of strings, representing
the lines to add to the '__init__', '__set_properties' and '__do_layout'
methods of the parent object.
"""

import cStringIO
import os
import os.path
import re

from codegen import BaseCodeWriter,  \
                    BaseSourceFileContent, \
                    BaseWidgetHandler


class SourceFileContent(BaseSourceFileContent):
    """\
    Keeps info about an existing file that has to be updated, to replace only
    the lines inside a wxGlade block, an to keep the rest of the file as it was

    @ivar event_handlers: List of event handlers for each class
    @ivar header_content: Content of the header file
    @ivar source_content: Content of the source file
    """

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
    """\
    Regexp to match last line of a class statement
    """

    rec_class_decl = re.compile(
        r'^\s*'                                  # leading spaces
        r'class\s+([a-zA-Z_]\w*)'                # "class <name>" statement
        r'\s*'                                   # tailing spaces
        )
    """\
    Regexp to match class declarations

    This isn't very accurate - doesn't match template classes, nor virtual
    inheritance, but should be enough for most cases
    """

    rec_decl_event_table = re.compile(
        r'^\s*'                                       # leading spaces
        r'DECLARE_EVENT_TABLE\s*\(\s*\)\s*;?'         # declaration of the event table
        r'\s*$'                                       # tailing spaces
        )
    """\
    Regexp to match declaration of event table
    """

    rec_def_event_table = re.compile(
        r'^\s*'                                       # leading spaces
        r'BEGIN_EVENT_TABLE\s*\(\s*(\w+)\s*,\s*(\w+)\s*\)'
        r'\s*$'                                       # tailing spaces
        )
    """\
    Regexp to match event table
    """

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
    """\
    Regexp to match wxGlade comment of event handlers
    """

    def __init__(self, name=None, content=None, classes=None, nonce=None,
                 out_dir=None, multiple_files=None,
                 header_extension=None, source_extension=None):

        # initialise new variables first
        self.header_content = None
        self.source_content = None
        self.event_table_decl = {}
        self.event_table_def = {}
        self.header_extension = header_extension
        self.source_extension = source_extension
     
        # call inherited constructor
        BaseSourceFileContent.__init__(
            self, name, content, classes, nonce, out_dir, multiple_files
            )

    def build_untouched_content(self):
        BaseSourceFileContent.build_untouched_content(self)
        self._build_untouched(self.name + self.header_extension, True)
        BaseSourceFileContent.build_untouched_content(self)
        self._build_untouched(self.name + self.source_extension, False)

    def _build_untouched(self, filename, is_header):
        prev_was_handler = False
        events_tag_added = False

        inside_block = False
        inside_comment = False
        tmp_in = self._load_file(filename)
        out_lines = []
        for line in tmp_in:
            comment_index = line.find('/*')
            if not inside_comment and comment_index != -1 \
                   and comment_index > line.find('//'):
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
##                print ">> class %r" % result.group(1)
                if not self.class_name:
                    # this is the first class declared in the file: insert the
                    # new ones before this
                    out_lines.append('<%swxGlade insert new_classes>' %
                                     self.nonce)
                    self.new_classes_inserted = True
                self.class_name = result.group(1)
                self.class_name = self.format_classname(self.class_name)
                self.classes[self.class_name] = 1  # add the found class to the list
                                              # of classes of this module
                out_lines.append(line)
            elif not inside_block:
                result = self.rec_block_start.match(line)
                if not inside_comment and result:
##                     print ">> block %r %r %r" % (
##                         result.group('spaces'), result.group('classname'), result.group('block'))
                    # replace the lines inside a wxGlade block with a tag that
                    # will be used later by add_class
                    spaces = result.group('spaces')
                    which_class = result.group('classname')
                    which_block = result.group('block')
                    if not which_class:
                        which_class = self.class_name
                    else:
                        which_class = self.format_classname(which_class)
                    self.spaces[which_class] = spaces
                    inside_block = True
                    out_lines.append('<%swxGlade replace %s %s>' % (
                        self.nonce,
                        result.group('classname'),
                        result.group('block')
                        ))
                else:
                    dont_append = False

                    # ALB 2004-12-08 event handling support...
                    if is_header and not inside_comment:
                        result = self.rec_event_handler.match(line)
                        if result:
                            prev_was_handler = True
                            which_handler = result.group('handler')
                            which_class = self.class_name
                            self.event_handlers.setdefault(
                                which_class, {})[which_handler] = 1
                        else:
                            if prev_was_handler:
                                # add extra event handlers here...
                                out_lines.append('<%swxGlade event_handlers %s>'
                                                 % (self.nonce, self.class_name)
                                        )
                                prev_was_handler = False
                                events_tag_added = True
                            elif not events_tag_added and \
                                     self.is_end_of_class(line):
                                out_lines.append(
                                    '<%swxGlade event_handlers %s>' % \
                                        (self.nonce, self.class_name)
                                        )
                            # now try to see if we already have a
                            # DECLARE_EVENT_TABLE
                            result = self.rec_decl_event_table.match(line)
                            if result:
                                self.event_table_decl[self.class_name] = True
                    elif not inside_comment:
                        result = self.rec_event_handlers_marker.match(line)
                        if result:
                            out_lines.append('<%swxGlade add %s event '
                                             'handlers>' % \
                                             (self.nonce, result.group(1)))
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
            # if we are here, the previous ``version'' of the file did not
            # contain any class, so we must add the new_classes tag at the
            # end of the file
            out_lines.append('<%swxGlade insert new_classes>' % self.nonce)
        # set the ``persistent'' content of the file
        if is_header:
            self.header_content = "".join(out_lines)
        else:
            self.source_content = "".join(out_lines)

    def is_end_of_class(self, line):
        """\
        Return True if the line is the last line of a class

        Not really, but for wxglade-generated code it should work...
        """
        return self.rec_class_end.match(line)

# end of class SourceFileContent


class WidgetHandler(BaseWidgetHandler):
    """\
    Interface the various code generators for the widgets must implement
    """

    constructor = []
    """\
    ``signature'' of the widget's constructor
    """

    extra_headers = []
    """\
    If not None, list of extra header file, in the form
    <header.h> or "header.h"
    """

    def __init__(self):
        BaseWidgetHandler.__init__(self)
        self.constructor = []
        self.extra_headers = []

    def get_ids_code(self, obj):
        """\
        Handler for the code of the ids enum of toplevel objects.
        Returns a list of strings containing the code to generate.
        Usually the default implementation is ok (i.e. there are no
        extra lines to add)
        """
        return []

# end of class WidgetHandler


class CPPCodeWriter(BaseCodeWriter):
    """\
    Code writer class for writing C++ code out of the designed GUI elements

    @ivar source_extension: Extension of the source file
    @type source_extension: String

    @ivar header_extension: Extension of the header file
    @type header_extension: String

    @ivar last_generated_id: Last generated Id number (wxNewId() is not
                             used yet)
    @type last_generated_id: Integer

    @see: L{BaseCodeWriter}
    """

    default_extensions = ['h', 'hh', 'hpp', 'H', 'hxx',
                          'cpp', 'cc', 'C', 'cxx', 'c++']
    language = "C++"

    code_statements = {
        'backgroundcolour': "%(objname)sSetBackgroundColour(%(value)s);\n",
        'disabled':         "%(objname)sEnable(0);\n",
        'extraproperties':  "%(objname)sSet%(propname)s(%(value)s);\n",
        'focused':          "%(objname)sSetFocus();\n",
        'foregroundcolour': "%(objname)sSetForegroundColour(%(value)s);\n",
        'hidden':           "%(objname)sHide();\n",
        'setfont':          "%(objname)sSetFont(wxFont(%(size)s, %(family)s, "
                            "%(style)s, %(weight)s, %(underlined)s, wxT(%(face)s)));\n",
        'tooltip':          "%(objname)sSetToolTip(%(tooltip)s);\n",
        'wxcolour':         "wxColour(%(value)s)",
        'wxsystemcolour':   "wxSystemSettings::GetColour(%(value)s)",
        }

    comment_sign = '//'

    global_property_writers = {
        'font':            BaseCodeWriter.FontPropertyHandler,
        'events':          BaseCodeWriter.EventsPropertyHandler,
        'extraproperties': BaseCodeWriter.ExtraPropertiesPropertyHandler,
        }

    language_note = \
        '// Example for compiling a single file project under Linux using g++:\n' \
        '//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp\n' \
        '//\n' \
        '// Example for compiling a multi file project under Linux using g++:\n' \
        '//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp\n'

    last_generated_id = 1000
    
    output_name = None
    """\
    If not None, name (without extension) of the file to write into
    """
    
    output_header, output_source = None, None
    """\
    Temporary storage (StringIO) of header and source file for writing into
    """
    
    shebang = '// -*- C++ -*-\n'
    
    _quote_str_pattern = re.compile(r'\\[natbv"]?')

    class ClassLines(BaseCodeWriter.ClassLines):
        """\
        Stores the lines of C++ code for a custom class
    
        @ivar ids:             Ids declared in the source (to use for Event
                               handling): these are grouped together into a
                               public enum in the custom class
        @ivar sub_objs:        List of 2-tuples (type, name) of the sub-objects
                               which are attributes of the toplevel object
        @ivar extra_code_h:    Extra header code to output
        @ivar extra_code_cpp:  Extra source code to output
        """
        def __init__(self):
            BaseCodeWriter.ClassLines.__init__(self)
            self.ids = []
            self.sub_objs = []
            self.extra_code_h = []
            self.extra_code_cpp = []
            self.dependencies = []     # List not dictionary
    
    # end of class ClassLines

    def initialize(self, app_attrs):
        """\
        Writer initialization function.

        @keyword path: Output path for the generated code (a file if multi_files is
                       False, a dir otherwise)
        @keyword option: If True, generate a separate file for each custom class
        """
        # initialise parent class
        BaseCodeWriter.initialize(self, app_attrs)
        out_path = app_attrs['path']

        self.last_generated_id = 1000

        # Extensions based on Project options when set
        self.source_extension = app_attrs.get('source_extension', '.cpp')
        self.header_extension = app_attrs.get('header_extension', '.h')

        self.header_lines = [
            '#include <wx/wx.h>\n',
            '#include <wx/image.h>\n',
            ]

        # extra lines to generate (see the 'extracode' property of top-level
        # widgets)
        self._current_extra_code_h = []
        self._current_extra_code_cpp = []

        if self.multiple_files:
            self.previous_source = None
            if not os.path.isdir(out_path):
                raise IOError("'path' must be a directory when generating"\
                                      " multiple output files")
            self.out_dir = out_path
        else:
            name = os.path.splitext(out_path)[0]
            self.output_name = name
            if not self._overwrite and self._file_exists(name + self.header_extension):
                # the file exists, we must keep all the lines not inside a wxGlade
                # block. NOTE: this may cause troubles if out_path is not a valid
                # source file, so be careful!
                self.previous_source = SourceFileContent(
                    name,
                    nonce=self.nonce,
                    out_dir=self.out_dir,
                    multiple_files=self.multiple_files,
                    header_extension=self.header_extension,
                    source_extension=self.source_extension,
                    )
            else:
                # if the file doesn't exist, create it and write the ``intro''
                self.previous_source = None
                self.output_header = cStringIO.StringIO()
                self.output_source = cStringIO.StringIO()
                for line in self.header_lines:
                    self.output_header.write(line)
                    #self.output_source.write(line)
                # isolation directives
                oh = os.path.basename(name + self.header_extension).upper().replace(
                    '.', '_')
                # now, write the tag to store dependencies
                self.output_header.write('<%swxGlade replace  dependencies>\n' % self.nonce)

                self.output_header.write('\n#ifndef %s\n#define %s\n' % (oh, oh))
                self.output_header.write('\n')
                # write the tag to store extra code
                self.output_header.write('\n<%swxGlade replace  extracode>\n' % self.nonce)

                self.output_source.write('#include "%s%s"\n\n' % \
                                    (os.path.basename(name), self.header_extension))
                self.output_source.write('<%swxGlade replace  extracode>\n\n' % self.nonce)

    def finalize(self):
        if self.previous_source:
            # insert all the new custom classes inside the old file
            tag = '<%swxGlade insert new_classes>' % self.nonce
            if self.previous_source.new_classes:
                code = "".join([c[0] for c in self.previous_source.new_classes])
            else:
                code = ""
            header_content = self.previous_source.header_content.replace(tag, code)
            extra_source = "".join([c[1] for c in self.previous_source.new_classes])
            source_content = self.previous_source.source_content

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace  extracode>' % self.nonce
            code = self._tagcontent(
                '::extracode',
                self._current_extra_code_h
                )
            header_content = header_content.replace(tag, code)
            code = self._tagcontent(
                '::extracode',
                self._current_extra_code_cpp
                )
            source_content = source_content.replace(tag, code)
            # --------------------------------------------------------------

            # now remove all the remaining <123415wxGlade ...> tags from the
            # source: this may happen if we're not generating multiple files,
            # and one of the container class names is changed
            tags = re.findall(
                '(<%swxGlade replace ([a-zA-Z_]*\w*) (\w+)>)' % self.nonce,
                header_content
                )
            for tag in tags:
                if tag[2] == 'dependencies':
                    #print 'writing dependencies'
                    deps = []
                    for code in self.classes.itervalues():
                        deps.extend(code.dependencies)
                    lines = self._format_dependencies(deps)
                elif tag[2] == 'methods':
                    lines = '%svoid set_properties();\n%svoid do_layout();\n' \
                            % (self.tabs(1), self.tabs(1))
                else:
                    lines = '// content of this block (%s) not found: ' \
                            'did you rename this class?\n' % tag[2]
                header_content = header_content.replace(tag[0], lines)

            # remove all the remaining <123415wxGlade ...> tags in source file
            source_content = self._content_notfound(
                source_content,
                )

            # ALB 2004-12-08
            tags = re.findall('<%swxGlade event_handlers \w+>' % self.nonce,
                              header_content)
            for tag in tags:
                header_content = header_content.replace(tag, "")
            tags = re.findall('<%swxGlade add \w+ event_handlers>' % self.nonce,
                              source_content)
            for tag in tags:
                source_content = source_content.replace(tag, "")

            # write the new file contents to disk
            self.save_file(
                self.previous_source.name + self.header_extension,
                header_content,
                content_only=True
                )
            if extra_source:
                extra_source = '\n\n' + extra_source
            self.save_file(
                self.previous_source.name + self.source_extension,
                source_content + extra_source,
                content_only=True
                )

        elif not self.multiple_files:
            oh = os.path.basename(self.output_name).upper() + '_H'
            self.output_header.write('\n#endif // %s\n' % oh)
            # write the list of include files
            header_content = self.output_header.getvalue()
            source_content = self.output_source.getvalue()
            tags = re.findall('<%swxGlade replace  dependencies>' %
                              self.nonce, header_content)
            deps = []
            for code in self.classes.itervalues():
                deps.extend(code.dependencies)
            code = self._format_dependencies(deps)
            header_content = header_content.replace(
                '<%swxGlade replace  dependencies>' % self.nonce, code)

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace  extracode>' % self.nonce
            code = self._tagcontent('::extracode', self._current_extra_code_h)
            header_content = header_content.replace(tag, code)
            code = self._tagcontent('::extracode', self._current_extra_code_cpp)
            source_content = source_content.replace(tag, code)
            # --------------------------------------------------------------

            self.save_file(
                self.output_name + self.header_extension,
                header_content,
                self._app_added
                )
            self.save_file(
                self.output_name + self.source_extension,
                source_content,
                self._app_added
                )

    def add_app(self, app_attrs, top_win_class):
        self._app_added = True

        name = app_attrs.get('name')
        if not name:
            name = 'app'

        if not self.multiple_files:
            prev_src = self.previous_source
        else:
            # overwrite apps file always
            prev_src = None

        # do nothing if the file exists
        if prev_src:
            return

        klass = app_attrs.get('class')
        top_win = app_attrs.get('top_window')
        if not klass or not top_win:
            return  # do nothing in these cases

        lines = []
        append = lines.append
        tab = self.tabs(1)
        append('\nclass %s: public wxApp {\n' % klass)
        append('public:\n')
        append(tab + 'bool OnInit();\n')
        append('};\n\n')
        append('IMPLEMENT_APP(%s)\n\n' % klass)
        append('bool %s::OnInit()\n{\n' % klass)
        append(tab + 'wxInitAllImageHandlers();\n')  # we add this to avoid troubles
        append(tab + '%s* %s = new %s(NULL, wxID_ANY, wxEmptyString);\n' % \
               (top_win_class, top_win, top_win_class))
        append(tab + 'SetTopWindow(%s);\n' % top_win)
        append(tab + '%s->Show();\n' % top_win)
        append(tab + 'return true;\n}\n')

        if self.multiple_files:
            filename = os.path.join(self.out_dir, 'main.cpp')
            out = cStringIO.StringIO()
            write = out.write
            # write overwrite warning to standalone app file
            write(self.tmpl_overwrite % {'comment_sign': self.comment_sign,})
            # write the common lines
            for line in self.header_lines:
                write(line)
            # import the top window module
            write('#include "%s.h"\n\n' % top_win_class)
            # write the wxApp code
            for line in lines:
                write(line)
            self.save_file(filename, out.getvalue(), True)
            out.close()
        else:
            write = self.output_source.write
            for line in lines:
                write(line)

    def add_class(self, code_obj):
        if self.classes.has_key(code_obj.klass) and self.classes[code_obj.klass].done:
            return  # the code has already been generated

        if not self.multiple_files:
            # in this case, self.previous_source is the SourceFileContent instance
            # that keeps info about the single file to generate
            prev_src = self.previous_source
        else:
            # let's see if the file to generate exists, and in this case
            # create a SourceFileContent instance
            filename = os.path.join(self.out_dir,
                                    code_obj.klass.replace('::', '_') +
                                    self.header_extension)
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = SourceFileContent(
                    os.path.join(self.out_dir, code_obj.klass),
                    nonce=self.nonce,
                    out_dir=self.out_dir,
                    multiple_files=self.multiple_files,
                    header_extension=self.header_extension,
                    source_extension=self.source_extension,
                    )

        try:
            builder = self.obj_builders[code_obj.base]
            mycn = getattr(builder, 'cn', self.cn)
            mycn_f = getattr(builder, 'cn_f', self.cn_f)
        except KeyError:
            print code_obj
            raise  # this is an error, let the exception be raised

        if prev_src and prev_src.classes.has_key(code_obj.klass):
            is_new = False
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True

        header_buffer = []
        source_buffer = []
        hwrite = header_buffer.append
        swrite = source_buffer.append

        if not self.classes.has_key(code_obj.klass):
            # if the class body was empty, create an empty ClassLines
            self.classes[code_obj.klass] = self.ClassLines()

        # try to see if there's some extra code to add to this class
        extra_code = getattr(builder, 'extracode',
                             code_obj.properties.get('extracode', ""))
        if extra_code:
            extra_code = re.sub(r'\\n', '\n', extra_code)
            extra_code = re.split(re.compile(r'^###\s*$', re.M), extra_code, 1)
            self.classes[code_obj.klass].extra_code_h.append(extra_code[0])
            if len(extra_code) > 1:
                self.classes[code_obj.klass].extra_code_cpp.append(extra_code[1])
            if not is_new:
                self.warning(
                    '%s has extra code, but you are not overwriting '
                    'existing sources: please check that the resulting '
                    'code is correct!' % code_obj.name
                    )

        if not self.multiple_files and extra_code:
            if self.classes[code_obj.klass].extra_code_h:
                self._current_extra_code_h.append("".join(
                    self.classes[code_obj.klass].extra_code_h[::-1]))
            if self.classes[code_obj.klass].extra_code_cpp:
                self._current_extra_code_cpp.append("".join(
                    self.classes[code_obj.klass].extra_code_cpp[::-1]))

        default_sign = [('wxWindow*', 'parent'), ('int', 'id')]
##         sign = obj_constructors.get(code_obj.base, default_sign)
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

        # ALB 2004-12-08 event handling
        event_handlers = self.classes[code_obj.klass].event_handlers
        if hasattr(builder, 'get_events'):
            event_handlers.extend(builder.get_events(code_obj))

        # ALB 2007-08-31 custom base classes support
        custom_base = getattr(code_obj, 'custom_base',
                              code_obj.properties.get('custom_base', None))
        if custom_base and not custom_base.strip():
            custom_base = None

        if not is_new and custom_base:
            # custom base classes set, but "overwrite existing sources" not
            # set. Issue a warning about this
            self.warning(
                '%s has custom base classes, but you are not overwriting '
                'existing sources: please check that the resulting code is '
                'correct!' % code_obj.name
                )

        if is_new:
            # header file
            base = code_obj.base
            if custom_base:
                base = ", public ".join([b.strip() for b in custom_base.split(',')])
            hwrite('\nclass %s: public %s {\n' % (code_obj.klass, base))
            hwrite('public:\n')
            # the first thing to add it the enum of the various ids
            hwrite(self.tabs(1) + '// begin wxGlade: %s::ids\n' % code_obj.klass)
            ids = self.classes[code_obj.klass].ids

            # let's try to see if there are extra ids to add to the enum
            if hasattr(builder, 'get_ids_code'):
                ids.extend(builder.get_ids_code(code_obj))

            if ids:
                hwrite(self.tabs(1) + 'enum {\n')
                ids = (',\n' + self.tabs(2)).join(ids)
                hwrite(self.tabs(2) + ids)
                hwrite('\n' + self.tabs(1) + '};\n')
            hwrite(self.tabs(1) + '// end wxGlade\n\n')
            # constructor prototype
            hwrite(self.tabs(1) + '%s(%s);\n' % (code_obj.klass, sign_decl1))
            hwrite('\nprivate:\n')
            # set_properties and do_layout prototypes
            hwrite(self.tabs(1) + '// begin wxGlade: %s::methods\n' % code_obj.klass)
            hwrite(self.tabs(1) + 'void set_properties();\n')
            hwrite(self.tabs(1) + 'void do_layout();\n')
            hwrite(self.tabs(1) + '// end wxGlade\n')
            # declarations of the attributes
            hwrite('\n')
            hwrite('protected:\n')
            hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % code_obj.klass)
            for o_type, o_name in self.classes[code_obj.klass].sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            hwrite(self.tabs(1) + '// end wxGlade\n')

            # ALB 2004-12-08 event handling
            if event_handlers:
                t = self.tabs(1)
                hwrite('\n' + t + 'DECLARE_EVENT_TABLE();\n')
                hwrite('\npublic:\n')
                already_there = {}
                for tpl in event_handlers:
                    if len(tpl) == 4:
                        win_id, event, handler, evt_type = tpl
                    else:
                        win_id, event, handler = tpl
                        evt_type = 'wxCommandEvent'
                    if handler not in already_there:
                        # Sebastien JEFFROY & Steve MULLER contribution
                        # Adding virtual attribute permits to derivate from the
                        # class generated by wxGlade
                        hwrite(t + 'virtual void %s(%s &event); '
                               '// wxGlade: <event_handler>\n' %
                               (handler, evt_type))
                        already_there[handler] = 1

            hwrite('}; // wxGlade: end class\n\n')

        elif prev_src:
            hwrite(self.tabs(1) + '// begin wxGlade: %s::ids\n' % code_obj.klass)
            ids = self.classes[code_obj.klass].ids

            # let's try to see if there are extra ids to add to the enum
            if hasattr(builder, 'get_ids_code'):
                ids.extend(builder.get_ids_code(code_obj))

            if ids:
                hwrite(self.tabs(1) + 'enum {\n')
                ids = (',\n' + self.tabs(2)).join(ids)
                hwrite(self.tabs(2) + ids)
                hwrite('\n' + self.tabs(1) + '};\n')
            hwrite(self.tabs(1) + '// end wxGlade\n')
            tag = '<%swxGlade replace %s ids>' % (self.nonce, code_obj.klass)
            if prev_src.header_content.find(tag) < 0:
                # no ids tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade ids block not found for %s, ids declarations "
                    "code NOT generated" % code_obj.name
                    )
            else:
                prev_src.header_content = prev_src.header_content.\
                                          replace(tag, "".join(header_buffer))
            header_buffer = [
                self.tabs(1) + '// begin wxGlade: %s::methods\n' % \
                code_obj.klass,
                self.tabs(1) + 'void set_properties();\n',
                self.tabs(1) + 'void do_layout();\n',
                self.tabs(1) + '// end wxGlade\n',
                ]
            tag = '<%swxGlade replace %s methods>' % (self.nonce, code_obj.klass)
            if prev_src.header_content.find(tag) < 0:
                # no methods tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade methods block not found for %s, methods "
                    "declarations code NOT generated" % code_obj.name
                    )
            else:
                prev_src.header_content = prev_src.header_content.\
                                          replace(tag, "".join(header_buffer))
            header_buffer = []
            hwrite = header_buffer.append
            hwrite(self.tabs(1) + '// begin wxGlade: %s::attributes\n' % code_obj.klass)
            for o_type, o_name in self.classes[code_obj.klass].sub_objs:
                hwrite(self.tabs(1) + '%s* %s;\n' % (o_type, o_name))
            hwrite(self.tabs(1) + '// end wxGlade\n')
            tag = '<%swxGlade replace %s attributes>' % (self.nonce, code_obj.klass)
            if prev_src.header_content.find(tag) < 0:
                # no attributes tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade attributes block not found for %s, attributes "
                    "declarations code NOT generated" % code_obj.name
                    )
            else:
                prev_src.header_content = prev_src.header_content.\
                                          replace(tag, "".join(header_buffer))

            header_buffer = []
            hwrite = header_buffer.append
            # ALB 2004-12-08 event handling
            if event_handlers:
                already_there = prev_src.event_handlers.get(code_obj.klass, {})
                t = self.tabs(1)
                for tpl in event_handlers:
                    if len(tpl) == 4:
                        win_id, event, handler, evt_type = tpl
                    else:
                        win_id, event, handler = tpl
                        evt_type = 'wxCommandEvent'
                    if handler not in already_there:
                        # Sebastien JEFFROY & Steve MULLER contribution :
                        # Adding virtual attribute permits to derivate from the
                        # class generated by wxGlade
                        hwrite(t + 'virtual void %s(%s &event); // wxGlade: '
                               '<event_handler>\n' % (handler, evt_type))
                        already_there[handler] = 1
                if code_obj.klass not in prev_src.event_table_def:
                    hwrite('\nprotected:\n')
                    hwrite(self.tabs(1) + 'DECLARE_EVENT_TABLE()\n')
            tag = '<%swxGlade event_handlers %s>' % (self.nonce, code_obj.klass)
            if prev_src.header_content.find(tag) < 0:
                # no attributes tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade events block not found for %s, event table code "
                    "NOT generated" % code_obj.name
                    )
            else:
                prev_src.header_content = prev_src.header_content.\
                                          replace(tag, "".join(header_buffer))

        # source file
        # set the window's style
        prop = code_obj.properties
        style = prop.get("style", None)
        if style:
            sign_inst = sign_inst.replace('style', '%s' % style)

        # constructor
        if is_new:
            base = "%s(%s)" % (code_obj.base, sign_inst)
            if custom_base:
                bases = [b.strip() for b in custom_base.split(',')]
                if bases:
                    base = "%s(%s)" % (bases[0], sign_inst)
                    rest = ", ".join([b + "()" for b in bases[1:]])
                    if rest:
                        base += ", " + rest

            swrite('\n%s::%s(%s):\n%s%s\n{\n' % (code_obj.klass,
                                                 code_obj.klass,
                                                 sign_decl2, self.tabs(1), base))
##                                                      code_obj.base, sign_inst))
        swrite(self.tabs(1) + '// begin wxGlade: %s::%s\n' % (code_obj.klass,
                                                         code_obj.klass))
        tab = self.tabs(1)
        init_lines = self.classes[code_obj.klass].init
        # --- patch 2002-08-26 ------------------------------------------------
        parents_init = self.classes[code_obj.klass].parents_init
        parents_init.reverse()
        for l in parents_init:
            swrite(tab + l)
        # ---------------------------------------------------------------------
        for l in init_lines:
            swrite(tab + l)

        # now see if there are extra init lines to add
        if hasattr(builder, 'get_init_code'):
            for l in builder.get_init_code(code_obj):
                swrite(tab + l)

        swrite('\n' + tab + 'set_properties();\n')
        swrite(tab + 'do_layout();\n')
        # end tag
        swrite(tab + '// end wxGlade\n')
        if is_new:
            swrite('}\n\n')

        if prev_src and not is_new:
            # replace the lines inside the constructor wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, code_obj.klass,
                                                 code_obj.klass)
            if prev_src.source_content.find(tag) < 0:
                # no constructor tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade %s::%s block not found, relative code NOT "
                    "generated" % (code_obj.klass, code_obj.klass)
                    )
            else:
                prev_src.source_content = prev_src.source_content.\
                                          replace(tag, "".join(source_buffer))
            source_buffer = []
            swrite = source_buffer.append

        # ALB 2004-12-08 event handling code
        if event_handlers:
            # 1) event table declaration/definition...
            if prev_src and code_obj.klass in prev_src.event_table_decl:
                has_event_table = True
            else:
                has_event_table = False
            if is_new or not has_event_table:
                swrite('\nBEGIN_EVENT_TABLE(%s, %s)\n' % \
                       (code_obj.klass, code_obj.base))

            swrite(tab + '// begin wxGlade: %s::event_table\n' % code_obj.klass)
            for tpl in event_handlers:
                win_id, event, handler = tpl[:3]
                swrite(tab + '%s(%s, %s::%s)\n' % \
                       (event, win_id, code_obj.klass, handler))
            swrite(tab + '// end wxGlade\n')

            if is_new or not has_event_table:
                swrite('END_EVENT_TABLE();\n')

            if prev_src and not is_new:
                tag = '<%swxGlade replace %s event_table>' % (self.nonce, code_obj.klass)
                if prev_src.source_content.find(tag) < 0:
                    # no constructor tag found, issue a warning and do nothing
                    self.warning(
                        "wxGlade %s::event_table block not found, relative "
                        "code NOT generated" % code_obj.klass
                        )
                else:
                    prev_src.source_content = prev_src.source_content.\
                                              replace(tag, "".join(source_buffer))
                source_buffer = []
                swrite = source_buffer.append

            # 2) event handler stubs...
            if prev_src:
                already_there = prev_src.event_handlers.get(code_obj.klass, {})
            else:
                already_there = {}
            for tpl in event_handlers:
                if len(tpl) == 4:
                    win_id, event, handler, evt_type = tpl
                else:
                    win_id, event, handler = tpl
                    evt_type = 'wxCommandEvent'
                if handler not in already_there:
                    swrite('\n\nvoid %s::%s(%s &event)\n{\n' % \
                           (code_obj.klass, handler, evt_type))
                    swrite(tab + 'event.Skip();\n')
                    swrite(tab + "// notify the user that he hasn't "
                                 "implemented the event handler yet\n")
                    swrite(tab + 'wxLogDebug(wxT("Event handler (%s::%s) not '
                                 'implemented yet"));\n' % \
                           (code_obj.klass, handler))
                    swrite('}\n')
                    already_there[handler] = 1
            if is_new or not prev_src:
                swrite('\n\n')
            swrite('// wxGlade: add %s event handlers\n' % code_obj.klass)
            if is_new or not prev_src:
                swrite('\n')

            if prev_src and not is_new:
                tag = '<%swxGlade add %s event handlers>' % \
                      (self.nonce, code_obj.klass)
                if prev_src.source_content.find(tag) < 0:
                    # no constructor tag found, issue a warning and do nothing
                    self.warning(
                        "wxGlade %s event handlers marker not found, relative "
                        "code NOT generated" % code_obj.klass
                        )
                else:
                    prev_src.source_content = prev_src.source_content.\
                                              replace(tag, "".join(source_buffer))
                source_buffer = []
                swrite = source_buffer.append

        # set_properties
        if hasattr(builder, 'get_properties_code'):
            obj_p = builder.get_properties_code(code_obj)
        else:
            obj_p = self.generate_common_properties(code_obj)

        # set_properties
        if is_new:
            swrite('\nvoid %s::set_properties()\n{\n' % code_obj.klass)
        swrite(tab + '// begin wxGlade: %s::set_properties\n' % code_obj.klass)
        for l in obj_p:
            swrite(tab + l)
        for l in self.classes[code_obj.klass].props:
            swrite(tab + l)
        swrite(tab + '// end wxGlade\n')
        if is_new:
            swrite('}\n\n')

        if prev_src and not is_new:
            # replace the lines inside the set_properties wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s set_properties>' % (self.nonce, code_obj.klass)
            if prev_src.source_content.find(tag) < 0:
                # no set_properties tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade %s::set_properties block not found, relative "
                    "code NOT generated" % code_obj.klass
                    )
            else:
                prev_src.source_content = prev_src.source_content.\
                                          replace(tag, "".join(source_buffer))
            source_buffer = []
            swrite = source_buffer.append

        # do_layout
        if is_new:
            swrite('\nvoid %s::do_layout()\n{\n' % code_obj.klass)
        layout_lines = self.classes[code_obj.klass].layout
        sizers_init_lines = self.classes[code_obj.klass].sizers_init
        swrite(tab + '// begin wxGlade: %s::do_layout\n' % code_obj.klass)
        sizers_init_lines.reverse()
        for l in sizers_init_lines:
            swrite(tab + l)
        for l in layout_lines:
            swrite(tab + l)

        #if sizers_init_lines or layout_lines: swrite(tab + 'Layout();\n')

        # now, check if there are extra layout lines to add
        if hasattr(builder, 'get_layout_code'):
            for l in builder.get_layout_code(code_obj):
                swrite(tab + l)

        swrite(tab + '// end wxGlade\n')
        if is_new:
            swrite('}\n\n')

        if prev_src and not is_new:
            # replace the lines inside the constructor wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s do_layout>' % \
                (self.nonce, code_obj.klass)
            if prev_src.source_content.find(tag) < 0:
                # no do_layout tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade %s::do_layout block not found, relative code "
                    "NOT generated" % code_obj.klass
                    )
            else:
                prev_src.source_content = prev_src.source_content.\
                                          replace(tag, "".join(source_buffer))
            source_buffer = []
            swrite = source_buffer.append

        # the code has been generated
        self.classes[code_obj.klass].done = True

        if not self.multiple_files and prev_src:
            # if this is a new class, add its code to the new_classes list of the
            # SourceFileContent instance
            if is_new:
                prev_src.new_classes.append(
                    ("".join(header_buffer), "".join(source_buffer))
                    )
            return

        if self.multiple_files:
            if code_obj.base in self.obj_builders:
                self.classes[code_obj.klass].dependencies.extend(
                    getattr(self.obj_builders[code_obj.base], 'extra_headers', []))
            if prev_src:
                tag = '<%swxGlade insert new_classes>' % self.nonce
                prev_src.header_content = prev_src.header_content.replace(tag, "")

                # insert the module dependencies of this class
                extra_modules = self.classes[code_obj.klass].dependencies
                #print 'extra_modules:', extra_modules, code_obj.base
                # WARNING: there's a double space '  ' between 'replace' and
                # 'dependencies' in the tag below, because there is no class name
                # (see SourceFileContent, line ~147)
                tag = '<%swxGlade replace  dependencies>' % self.nonce
                code = self._format_dependencies(extra_modules)
                prev_src.header_content = prev_src.header_content.\
                                          replace(tag, code)

                # insert the extra code of this class
                extra_code_h = "".join(self.classes[code_obj.klass].extra_code_h[::-1])
                extra_code_cpp = \
                               "".join(self.classes[code_obj.klass].extra_code_cpp[::-1])
                # if there's extra code but we are not overwriting existing
                # sources, warn the user
                if extra_code_h or extra_code_cpp:
                    self.warning(
                        '%s (or one of its chilren) has extra code classes, '
                        'but you are not overwriting existing sources: please '
                        'check that the resulting code is correct!' % \
                        code_obj.name
                        )

                extra_code_h   = self._tagcontent("::extracode", extra_code_h)
                extra_code_cpp = self._tagcontent("::extracode", extra_code_cpp)
                tag = '<%swxGlade replace  extracode>' % self.nonce
                prev_src.header_content = prev_src.header_content.replace(
                    tag, extra_code_h)
                prev_src.source_content = prev_src.source_content.replace(
                    tag, extra_code_cpp)

                # store the new file contents to disk
                name = os.path.join(self.out_dir, code_obj.klass)
                self.save_file(
                    name + self.header_extension,
                    prev_src.header_content,
                    content_only=True
                    )
                self.save_file(
                    name + self.source_extension,
                    prev_src.source_content,
                    content_only=True
                    )

                return

            # create the new source file
            header_file = os.path.join(self.out_dir, code_obj.klass + self.header_extension)
            source_file = os.path.join(self.out_dir, code_obj.klass + self.source_extension)
            hout = cStringIO.StringIO()
            sout = cStringIO.StringIO()
            # header file
            hwrite = hout.write
            # write the common lines
            for line in self.header_lines:
                hwrite(line)
            # isolation directives
            hn = os.path.basename(header_file).upper().replace('.', '_')
            hwrite('\n#ifndef %s\n#define %s\n' % (hn, hn))

            # write the module dependecies for this class
            #extra_headers = classes[code_obj.klass].dependencies
            extra_modules = self.classes[code_obj.klass].dependencies
            code = self._format_dependencies(extra_modules)
            hwrite(code)
            hwrite('\n')

            # insert the extra code of this class
            extra_code_h = "".join(self.classes[code_obj.klass].extra_code_h[::-1])
            extra_code_h = self._tagcontent('::extracode', extra_code_h)
            hwrite(extra_code_h)
            hwrite('\n')

            # write the class body
            for line in header_buffer:
                hwrite(line)
            hwrite('\n#endif // %s\n' % hn)

            # source file
            swrite = sout.write
            # write the common lines
            #for line in self.header_lines:
            #    swrite(line)
            swrite(self.header_lines[0])
            swrite('#include "%s"\n\n' % os.path.basename(header_file))

            # insert the extra code of this class
            extra_code_cpp = "".join(self.classes[code_obj.klass].extra_code_cpp[::-1])
            extra_code_cpp = self._tagcontent('::extracode', extra_code_cpp)
            swrite(extra_code_cpp)
            swrite('\n')

            # write the class implementation
            for line in source_buffer:
                swrite(line)

            # store source to disk
            self.save_file(header_file, hout.getvalue())
            self.save_file(source_file, sout.getvalue())

            hout.close()
            sout.close()

        else:  # not self.multiple_files
            # write the class body onto the single source file
            hwrite = self.output_header.write
            for line in header_buffer:
                hwrite(line)
            swrite = self.output_source.write
            for line in source_buffer:
                swrite(line)

    def add_object(self, top_obj, sub_obj):
        if top_obj.klass in self.classes:
            klass = self.classes[top_obj.klass]
        else:
            klass = self.classes[top_obj.klass] = self.ClassLines()
        try:
            builder = self.obj_builders[sub_obj.base]
        except KeyError:
            # no code generator found: write a comment about it
            klass.init.extend([
                '\n',
                '%s code for %s (type %s) not generated: no suitable writer ' \
                'found' % (self.comment_sign, sub_obj.name, sub_obj.klass),
                '\n'])
            self.warning(
                'code for %s (type %s) not generated: no suitable writer '
                'found' % (sub_obj.name, sub_obj.klass)
                )
        else:
            try:
                init, ids, props, layout = builder.get_code(sub_obj)
            except:
                print sub_obj
                raise  # this shouldn't happen
            if sub_obj.in_windows:  # the object is a wxWindow instance
                if sub_obj.is_container and not sub_obj.is_toplevel:
                    init.reverse()
                    klass.parents_init.extend(init)
                else:
                    klass.init.extend(init)
                if hasattr(builder, 'get_events'):
                    klass.event_handlers.extend(builder.get_events(sub_obj))
                elif 'events' in sub_obj.properties:
                    id_name, id = self.generate_code_id(sub_obj)
                    for event, handler in sub_obj.properties['events'].iteritems():
                        klass.event_handlers.append((id, event, handler))
                # try to see if there's some extra code to add to this class
                extra_code = getattr(builder, 'extracode',
                                     sub_obj.properties.get('extracode', ""))
                if extra_code:
                    extra_code = re.sub(r'\\n', '\n', extra_code)
                    extra_code = re.split(re.compile(r'^###\s*$', re.M),
                                          extra_code, 1)
                    klass.extra_code_h.append(extra_code[0])
                    if len(extra_code) > 1:
                        klass.extra_code_cpp.append(extra_code[1])
                    # if we are not overwriting existing source, warn the user
                    # about the presence of extra code
                    if not self.multiple_files and self.previous_source:
                        self.warning(
                            '%s has extra code, but you are not '
                            'overwriting existing sources: please check '
                            'that the resulting code is correct!' % \
                            sub_obj.name
                            )

                klass.ids.extend(ids)
                if sub_obj.klass != 'spacer':
                    # attribute is a special property which control whether
                    # sub_obj must be accessible as an attribute of top_obj,
                    # or as a local variable in the do_layout method
                    if self.test_attribute(sub_obj):
                        klass.sub_objs.append((sub_obj.klass, sub_obj.name))
            else:  # the object is a sizer
                # ALB 2004-09-17: workaround (hack) for static box sizers...
                if sub_obj.base == 'wxStaticBoxSizer':
                    klass.sub_objs.insert(0, ('wxStaticBox',
                                              '%s_staticbox' % sub_obj.name))
                    klass.parents_init.insert(1, init.pop(0))
                klass.sizers_init.extend(init)

            klass.props.extend(props)
            klass.layout.extend(layout)
            if self.multiple_files and \
                   (sub_obj.is_toplevel and sub_obj.base != sub_obj.klass):
                #print top_obj.name, sub_obj.name
                klass.dependencies.append(sub_obj.klass)
            else:
                if sub_obj.base in self.obj_builders:
                    headers = getattr(self.obj_builders[sub_obj.base],
                                      'extra_headers', [])
                    klass.dependencies.extend(headers)

    def add_sizeritem(self, toplevel, sizer, obj, option, flag, border):
        if toplevel.klass in self.classes:
            klass = self.classes[toplevel.klass]
        else:
            klass = self.classes[toplevel.klass] = self.ClassLines()
        buffer = '%s->Add(%s, %s, %s, %s);\n' % \
                 (sizer.name, obj.name, option, flag, border)
        klass.layout.append(buffer)

    def generate_code_id(self, obj, id=None):
        if not id:
            id = obj.properties.get('id')
        if not id:
            return '', 'wxID_ANY'
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
            val = 'wxID_HIGHEST + %d' % self.last_generated_id
            self.last_generated_id += 1
        else:
            val = val
        return '%s = %s' % (name, val), name

    def generate_code_size(self, obj):
        objname = self._get_code_name(obj)
        if obj.is_toplevel:
            name2 = 'this'
        else:
            name2 = obj.name
        size = obj.properties.get('size', '').strip()
        use_dialog_units = (size[-1] == 'd')
        if not obj.parent:
            method = 'SetSize'
        else:
            method = 'SetMinSize'
        if use_dialog_units:
            return '%s%s(wxDLG_UNIT(%s, wxSize(%s)));\n' % \
                   (objname, method, name2, size[:-1])
        else:
            return '%s%s(wxSize(%s));\n' % (objname, method, size)

    def get_events_with_type(self, obj, evt_type):
        """\
        Returns the list of event handlers defined for `obj', setting the type
        of the argument of the handlers (i.e. the event parameter) to
        `evt_type'
        """
        ret = []
        if 'events' not in obj.properties:
            return ret
        id_name, id = self.generate_code_id(obj)
        for event, handler in obj.properties['events'].iteritems():
            ret.append((id, event, handler, evt_type))
        return ret

    def quote_str(self, s, translate=True, escape_chars=True):
        if not s:
            return 'wxEmptyString'
        s = s.replace('"', r'\"')
        if escape_chars:
            s = self._quote_str_pattern.sub(self._do_replace, s)
        else:
            s = s.replace('\\', r'\\')  # just quote the backslashes
        if self._use_gettext and translate:
            return '_("%s")' % s
        else:
            return 'wxT("%s")' % s

    def _get_code_name(self, obj):
        if obj.is_toplevel:
            return ''
        else:
            return '%s->' % obj.name

    def _unique(self, sequence):
        """\
        Strips all duplicates from sequence. Works only if items of sequence
        are hashable
        """
        tmp = {}
        for item in sequence:
            tmp[item] = 1
        return tmp.keys()

    def _format_dependencies(self, dependencies):
        """\
        Format the dependecies output

        @param dependencies: List if header files
        @type dependencies:  List of strings

        @return: Changed content
        @rtype:  String

        @see: L{_tagcontent()}
        """
        dep_list = []
        for dependency in self._unique(dependencies):
            if dependency and ('"' != dependency[0] != '<'):
                dep_list.append('#include "%s.h"\n' % dependency)
            else:
                dep_list.append('#include %s\n' % dependency)
        code = self._tagcontent(
            '::dependencies',
            dep_list,
            )
        return code

# end of class CPPCodeWriter

writer = CPPCodeWriter()
"""\
The code writer is an instance of L{CPPCodeWriter}.
"""

language = writer.language
"""\
Language generated by this code generator
"""
