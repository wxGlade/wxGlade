"""\
Perl code generator

How the code is generated: every time the end of an object is reached during
the parsing of the xml tree, either the function 'add_object' or the function
'add_class' is called: the latter when the object is a toplevel one, the former
when it is not. In the last case, 'add_object' calls the appropriate ``writer''
function for the specific object, found in the 'obj_builders' dict. Such
function accepts one argument, the CodeObject representing the object for
which the code has to be written, and returns 3 lists of strings, representing
the lines to add to the '__init__', '__set_properties' and '__do_layout'
methods of the parent object.

Like all other perl parts, based on the pre-existing python generators

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import cStringIO
import os
import os.path
import re

from codegen import BaseCodeWriter, \
                    BaseSourceFileContent, \
                    BaseWidgetHandler


class SourceFileContent(BaseSourceFileContent):

    rec_block_start = re.compile(
        r'^(?P<spaces>\s*)'                     # leading spaces
        r'#\s*'                                 # comment sign
        r'begin\s+wxGlade:\s*'                  # "begin wxGlade:" statement and tailing spaces
        r'(?P<classname>[a-zA-Z_]+[\w:]*?)??'   # class or function name (non-greedy)
        r'(?::{2}|\s*)'                         # separator between class and function / block (non-greedy)
        r'(?P<block>\w+)'                       # function / block name
        r'\s*$'                                 # tailing spaces
        )

    rec_block_end = re.compile(
        r'^\s*'                                 # leading spaces
        r'#\s*'                                 # comment sign
        r'end\s+wxGlade'                        # "end exGlade" statement
        r'\s*$'                                 # tailing spaces
        )

    # Less precise regex, but working :-P
    # Should match: package Foo; or package Foo::bar::baz   ;
    rec_class_decl = re.compile(
        r'^\s*'                                 # leading spaces
        r'package\s+([a-zA-Z_][\w:]*)\s*;'      # "package <name>" statement
        r'.*$'                                  # any character till eol
        )

    rec_event_handler = re.compile(
        r'^\s*'                                 # leading spaces
        r'#\s*wxGlade:\s*(?P<class>[\w:]+)::(?P<handler>\w+) <event_handler>' # wxGlade event handler
                                                                              # statement with class and
                                                                              # event handler name
        r'\s*$'                                 # tailing spaces
        )

    rec_pod = re.compile(
        r'^\s*'                                 # leading spaces
        r'=[A-Za-z_]+\w*'                       # match POD statement
        r'.*$'                                  # any character till eol
        )
    """\
    Regexp to match Perl's Plain Old Documentation format

    @see: manpage perlpod
    """

    def build_untouched_content(self):
        """\
        Builds a string with the contents of the file that must be left as is,
        and replaces the wxGlade blocks with tags that in turn will be replaced
        by the new wxGlade blocks

        WARNING: NOT YET COMPLETE -- crazyinsomniac

        alb - almost done :)
        WARNING: There is *NO* support for here documents: if you put wxGlade
        blocks inside a here document, you're likely going into troubles...
        """
        BaseSourceFileContent.build_untouched_content(self)
        inside_block = False
        inside_pod = False
        tmp_in = self._load_file(self.name)
        out_lines = []
        for line in tmp_in:
            result = self.rec_pod.match(line)
            if result:
                inside_pod = True
            if inside_pod:
                out_lines.append(line)
                if line.startswith('=cut'):
                    inside_pod = False
                continue

            result = self.rec_class_decl.match(line)
            if result:
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
                if result:
##                    print ">> block %r %r %r" % (
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
                    if not self.class_name:
                        out_lines.append('<%swxGlade replace %s>' % \
                                         (self.nonce, which_block))
                    else:
                        out_lines.append('<%swxGlade replace %s %s>' % \
                                         (self.nonce, which_class, which_block))
                else:
                    result = self.rec_event_handler.match(line)
                    if result:
                        which_handler = result.group('handler')
                        which_class = self.format_classname(result.group('class'))
                        self.event_handlers.setdefault(
                            which_class, {})[which_handler] = 1
                    if self.class_name and self.is_end_of_class(line):
                        # add extra event handlers here...
                        out_lines.append('<%swxGlade event_handlers %s>'
                                         % (self.nonce, self.class_name))
                    out_lines.append(line)
                    if self.is_import_line(line):
                        # add a tag to allow extra modules
                        out_lines.append('<%swxGlade extra_modules>\n'
                                         % self.nonce)
            else:
                # ignore all the lines inside a wxGlade block
                if self.rec_block_end.match(line):
##                     print 'end block'
                    inside_block = False
        if not self.new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not
            # contain any class, so we must add the new_classes tag at the
            # end of the file
            out_lines.append('<%swxGlade insert new_classes>' % self.nonce)
        # set the ``persistent'' content of the file
        self.content = "".join(out_lines)

    def is_import_line(self, line):
        return line.lstrip().startswith('use Wx')

# end of class SourceFileContent


class WidgetHandler(BaseWidgetHandler):
    """\
    Interface the various code generators for the widgets must implement
    """

    new_signature = []
    """\
    Constructor signature ($self->SUPER::new(@stuff))

    @see: L{PerlCodeWriter.new_defaults}
    """

# end of class WidgetHandler


class PerlCodeWriter(BaseCodeWriter):
    """\
    Code writer class for writing Perl code out of the designed GUI elements

    @cvar _perl_constant_list: Incomplete list of wx constants used in wxPerl
                               Constants don't follow the Wx::ObjectName name
                               schema. There is a need to handle constants
                               separately. See also L{cn}.
    @type _perl_constant_list: List of strings

    @see: L{BaseCodeWriter}
    """

    default_extensions = ['pl', 'pm']
    language = "perl"

    code_statements = {
        'backgroundcolour': "%(objname)s->SetBackgroundColour(%(value)s);\n",
        'disabled':         "%(objname)s->Enable(0);\n",
        'extraproperties':  "%(objname)s->Set%(propname)s(%(value)s);\n",
        'focused':          "%(objname)s->SetFocus();\n",
        'foregroundcolour': "%(objname)s->SetForegroundColour(%(value)s);\n",
        'hidden':           "%(objname)s->Show(0);\n",
        'setfont':          "%(objname)s->SetFont(Wx::Font->new(%(size)s, %(family)s, "
                            "%(style)s, %(weight)s, %(underlined)s, %(face)s));\n",
        'tooltip':          "%(objname)s->SetToolTipString(%(tooltip)s);\n",
        'wxcolour':         "Wx::Colour->new(%(value)s)",
        'wxsystemcolour':   "Wx::SystemSettings::GetColour(%(value)s)",
        }

    comment_sign = '#'

    global_property_writers = {
        'font':            BaseCodeWriter.FontPropertyHandler,
        'events':          BaseCodeWriter.EventsPropertyHandler,
        'extraproperties': BaseCodeWriter.ExtraPropertiesPropertyHandler,
        }

    indent_amount = 1
    indent_symbol = '\t'

    language_note = '# To get wxPerl visit http://wxPerl.sourceforge.net/\n',

    new_defaults = []
    """\
    Default class members, will be initialised during L{initialize()}
    """

    shebang = '#!/usr/bin/perl -w -- \n'

    tmpl_appfile = """%(overwrite)s%(header_lines)s"""

    tmpl_detailed = """\
package %(klass)s;

use base qw(Wx::App);
use strict;
%(pl_import)s
sub OnInit {
%(tab)smy( $self ) = shift;

%(tab)sWx::InitAllImageHandlers();

%(tab)smy $%(top_win)s = %(top_win_class)s->new();

%(tab)s$self->SetTopWindow($%(top_win)s);
%(tab)s$%(top_win)s->Show(1);

%(tab)sreturn 1;
}
# end of class %(klass)s

package main;

unless(caller){
%(tab)smy $%(name)s = %(klass)s->new();
%(tab)s$%(name)s->MainLoop();
}
"""

    tmpl_gettext_detailed = """\
package %(klass)s;

use base qw(Wx::App);
use strict;
%(pl_import)s
sub OnInit {
%(tab)smy( $self ) = shift;

%(tab)sWx::InitAllImageHandlers();

%(tab)smy $%(top_win)s = %(top_win_class)s->new();

%(tab)s$self->SetTopWindow($%(top_win)s);
%(tab)s$%(top_win)s->Show(1);

%(tab)sreturn 1;
}
# end of class %(klass)s

package main;

unless(caller){
%(tab)smy $local = Wx::Locale->new("English", "en", "en"); # replace with ??
%(tab)s$local->AddCatalog("%(name)s"); # replace with the appropriate catalog name

%(tab)smy $%(name)s = %(klass)s->new();
%(tab)s$%(name)s->MainLoop();
}
"""

    tmpl_simple = """\
1;

package main;
%(pl_import)s
unless(caller){
%(tab)slocal *Wx::App::OnInit = sub{1};
%(tab)smy $%(name)s = Wx::App->new();
%(tab)sWx::InitAllImageHandlers();

%(tab)smy $%(top_win)s = %(top_win_class)s->new();

%(tab)s$%(name)s->SetTopWindow($%(top_win)s);
%(tab)s$%(top_win)s->Show(1);
%(tab)s$%(name)s->MainLoop();
}
"""

    tmpl_gettext_simple = """\
1;

package main;
%(pl_import)s
unless(caller){
%(tab)smy $local = Wx::Locale->new("English", "en", "en"); # replace with ??
%(tab)s$local->AddCatalog("%(name)s"); # replace with the appropriate catalog name

%(tab)slocal *Wx::App::OnInit = sub{1};
%(tab)smy $%(name)s = Wx::App->new();
%(tab)sWx::InitAllImageHandlers();

%(tab)smy $%(top_win)s = %(top_win_class)s->new();

%(tab)s$%(name)s->SetTopWindow($%(top_win)s);
%(tab)s$%(top_win)s->Show(1);
%(tab)s$%(name)s->MainLoop();
}
"""

    _perl_constant_list = [
         "wxALL", "wxTOP", "wxBOTTOM", "wxLEFT", "wxRIGHT",
         "wxNORTH", "wxSOUTH", "wxWEST", "wxEAST",
         "wxEXPAND", "wxGROW", "wxSHAPED", "wxFIXED_MINSIZE",
         "wxCAPTION", "wxMINIMIZE_BOX", "wxMAXIMIZE_BOX", "wxRESIZE_BORDER",
         "wxYES_NO", "wxYES", "wxNO", "wxCANCEL", "wxOK",
         "wxICON_EXCLAMATION", "wxICON_HAND", "wxICON_ERROR", "wxICON_QUESTION",
         "wxICON_INFORMATION",
         "wxBLACK", "wxWHITE", "wxRED", "wxBLUE", "wxGREEN", "wxCYAN",
         "wxLIGHT_GREY",
         'wxDEFAULT', 'wxDECORATIVE', 'wxROMAN', 'wxSWISS', 'wxSCRIPT',
         'wxMODERN', 'wxTELETYPE',
         'wxNORMAL', 'wxSLANT', 'wxITALIC', 'wxNORMAL', 'wxLIGHT',
         'wxBOLD',
        ]

    _quote_str_pattern = re.compile(r'\\(?![nrt])')

    def cn(self, name):
        """\
        Return the name properly formatted.

        @see: L{self._perl_constant_list}
        """
        # handles constants like event or language identifiers
        if name.startswith('wxID_')       or \
           name.startswith('wxK_')        or \
           name.startswith('wxMOD_')      or \
           name.startswith('wxLANGUAGE_') or \
           name.startswith('wxALIGN_')    or \
           name in self._perl_constant_list:
            return name

        # don't process already formatted items again
        if name.startswith('Wx::'):
            return name

        # use default for remaining names
        if name[:2] == 'wx':
            return 'Wx::' + name[2:]
        elif name[:4] == 'EVT_':
            return 'Wx::' + name
        return name

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

        # initial new defaults late to use the proper indent characters
        tab = self.tabs(1)
        self.new_defaults = {
            '$parent' : '%s$parent = undef              unless defined $parent;\n' % tab,
            '$id'     : '%s$id     = -1                 unless defined $id;\n' % tab,
            '$title'  : '%s$title  = ""                 unless defined $title;\n' % tab,
            '$pos'    : '%s$pos    = wxDefaultPosition  unless defined $pos;\n' % tab,
            '$size'   : '%s$size   = wxDefaultSize      unless defined $size;\n' % tab,
            '$name'   : '%s$name   = ""                 unless defined $name;\n\n' % tab,
            #'$style' is a special case
            }

        self.header_lines = [
            'use Wx 0.15 qw[:allclasses];\n',
            'use strict;\n'
        ]

        if self.multiple_files:
            self.previous_source = None
            if not os.path.isdir(out_path):
                raise IOError("'path' must be a directory when generating"\
                                      " multiple output files")
            self.out_dir = out_path
        else:
            if not self._overwrite and self._file_exists(out_path):
                # the file exists, we must keep all the lines not inside a wxGlade
                # block. NOTE: this may cause troubles if out_path is not a valid
                # source file, so be careful!
                self.previous_source = SourceFileContent(
                    out_path,
                    nonce=self.nonce,
                    out_dir=self.out_dir,
                    multiple_files=self.multiple_files,
                    )
            else:
                # if the file doesn't exist, create it and write the ``intro''
                self.previous_source = None
                self.output_file = cStringIO.StringIO()
                self.output_file_name = out_path
                for line in self.header_lines:
                    self.output_file.write(line)
                self.output_file.write('<%swxGlade extra_modules>\n' % self.nonce)
                self.output_file.write('\n')
                self.output_file.write('<%swxGlade replace dependencies>\n' % self.nonce)
                self.output_file.write('<%swxGlade replace extracode>\n' % self.nonce)

    def setup(self):
        """\
        Load language specific code generators and sizer code generators

        @see: L{_setup()}
        """
        # load perl_codegen's ...
        self._setup()
        # ... then, the sizers
        import edit_sizers.perl_sizers_codegen
        edit_sizers.perl_sizers_codegen.initialize()

    def add_app(self, app_attrs, top_win_class):
        # add language specific mappings
        if self.multiple_files:
            self.lang_mapping['pl_import'] = "\nuse %s;\n" % top_win_class
        else:
            self.lang_mapping['pl_import'] = ''
        BaseCodeWriter.add_app(self, app_attrs, top_win_class)

    def add_class(self, code_obj):
        if self.multiple_files:
            # let's see if the file to generate exists, and in this case
            # create a SourceFileContent instance
            filename = os.path.join(self.out_dir,
                                    code_obj.klass + '.pm')  # MODULE!!
            if self._overwrite or not self._file_exists(filename):
                prev_src = None
            else:
                prev_src = SourceFileContent(
                    filename,
                    nonce=self.nonce,
                    out_dir=self.out_dir,
                    multiple_files=self.multiple_files,
                    )
            self._current_extra_modules = {}
        else:
            # in this case, previous_source is the SourceFileContent instance
            # that keeps info about the single file to generate
            prev_src = self.previous_source

        if self.classes.has_key(code_obj.klass) and \
           self.classes[code_obj.klass].done:
            return  # the code has already been generated

        try:
            builder = self.obj_builders[code_obj.base]
            mycn = getattr(builder, 'cn', self.cn)
            mycn_f = getattr(builder, 'cn_f', self.cn_f)
        except KeyError:
            print code_obj
            raise  # this is an error, let the exception be raised

        if prev_src and prev_src.classes.has_key(code_obj.klass):
            is_new = False
            indentation = prev_src.spaces[code_obj.klass]
        else:
            # this class wasn't in the previous version of the source (if any)
            is_new = True
            indentation = self.tabs(2)
            mods = getattr(builder, 'extra_modules', [])
            if mods:
                for m in mods:
                    self._current_extra_modules[m] = 1

        buffer = []
        write = buffer.append
        tab = self.tabs(1)

        if not self.classes.has_key(code_obj.klass):
            # if the class body was empty, create an empty ClassLines
            self.classes[code_obj.klass] = self.ClassLines()

        # try to see if there's some extra code to add to this class
        extra_code = getattr(builder, 'extracode',
                             code_obj.properties.get('extracode', ""))
        if extra_code:
            extra_code = re.sub(r'\\n', '\n', extra_code)
            self.classes[code_obj.klass].extra_code.append(extra_code)
            if not is_new:
                self.warning(
                    '%s has extra code, but you are not overwriting '
                    'existing sources: please check that the resulting '
                    'code is correct!' % code_obj.name
                    )

        # Don't add extra_code to self._current_extra_code here, that is handled
        # later.  Otherwise we'll emit duplicate extra code for frames.

        new_signature = getattr(builder, 'new_signature', [] )

        if is_new:
            write('package %s;\n\n' % code_obj.klass)
            write('use Wx qw[:everything];\nuse base qw(%s);\nuse strict;\n\n'
                    % code_obj.base.replace('wx', 'Wx::', 1))

            if self._use_gettext:
                if self.multiple_files:
                    self.classes[code_obj.klass].dependencies[
                        "use Wx::Locale gettext => '_T';\n"] = 1
                else:
                    write("use Wx::Locale gettext => '_T';\n")

            if self.multiple_files:
                # write the module dependecies for this class (package)
                dep_list = self.classes[code_obj.klass].dependencies.keys()
                dep_list.sort()
                code = self._tagcontent('dependencies', dep_list, True)
                write('\n')

            write('sub new {\n')

            write(tab + "my( $self, %s ) = @_;\n" % ", ".join(new_signature))

            if new_signature:
                for k in new_signature:
                    if self.new_defaults.has_key(k):
                        write(self.new_defaults[k])
            else:
                new_signature = ['@_[1 .. $#_]']  # shift(@_)->SUPER::new(@_);
                print code_obj.klass + " did not declare self.new_defaults "

        # we don't need gettext in main context currently
        #else:
        #    if self._use_gettext:
        #        self.classes[code_obj.klass].dependencies[
        #            "use Wx::Locale gettext => '_T';\n"] = 1

        # constructor (new) begin tag
        write(tab + '# begin wxGlade: %s::new\n\n' % code_obj.klass)
        prop = code_obj.properties
        style = prop.get("style", None)
        if style:
            write(tab + '$style = %s \n' % mycn_f(style))
            write(tab + tab + 'unless defined $style;\n\n')

        # constructor (new)
        write(tab + '$self = $self->SUPER::new( %s );\n' % ", ".join(new_signature))

        init_lines = self.classes[code_obj.klass].init

        parents_init = self.classes[code_obj.klass].parents_init
        parents_init.reverse()

        for l in parents_init:
            write(tab + l)
        for l in init_lines:
            write(tab + l)

        # now check if there are extra lines to add to the init method
        if hasattr(builder, 'get_init_code'):
            for l in builder.get_init_code(code_obj):
                write(tab + l)

        write('\n')
        write(tab + '$self->__set_properties();\n')
        write(tab + '$self->__do_layout();\n\n')

        event_handlers = self.classes[code_obj.klass].event_handlers
        if hasattr(builder, 'get_events'):
            for id, event, handler in builder.get_events(code_obj):
                event_handlers.append((id, mycn(event), handler))
        for win_id, event, handler in event_handlers:
            if win_id.startswith('#'):
                win_id = '$self->{' + win_id[8:] + '}->GetId'
            write(tab + 'Wx::Event::%s($self, %s, \\&%s);\n' % \
                  (event, win_id, handler))
        if event_handlers:
            write('\n')

        # end tag
        write(tab + '# end wxGlade\n')

        if is_new:
            write(tab + 'return $self;\n\n')
            write('}\n\n')

        if prev_src and not is_new:
            # replace the lines inside the ctor wxGlade block with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, code_obj.klass, 'new')
            if prev_src.content.find(tag) < 0:
                # no __init__ tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade ::new block not found for %s, new code NOT "
                    "generated" % code_obj.name
                    )
            else:
                prev_src.content = prev_src.content.replace(tag, "".join(buffer))
            buffer = []
            write = buffer.append

        # __set_properties
        obj_p = getattr(builder, 'get_properties_code',
                        self.generate_common_properties)(code_obj)
        obj_p.extend(self.classes[code_obj.klass].props)
        write_body = len(obj_p)

        if is_new:
            write('\n')
            write('sub __set_properties {\n')
            write(tab + 'my $self = shift;\n\n')

        # begin tag
        write(tab + '# begin wxGlade: %s::__set_properties\n\n' % code_obj.klass)

        if not write_body:
            write(tab + 'return;\n')
        else:
            for l in obj_p:
                write(tab + l)
        # end tag
        write('\n')
        write(tab + '# end wxGlade\n')

        if is_new:
            write('}\n')

        if prev_src and not is_new:
            # replace the lines inside the __set_properties wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, code_obj.klass,
                                                 '__set_properties')
            if prev_src.content.find(tag) < 0:
                # no __set_properties tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade __set_properties block not found for %s, "
                    "__set_properties code NOT generated" % code_obj.name
                    )
            else:
                prev_src.content = prev_src.content.replace(tag, "".join(buffer))
            buffer = []
            write = buffer.append

        # __do_layout
        if is_new:
            write('\n')
            write('sub __do_layout {\n')
            write(tab + 'my $self = shift;\n\n')
        layout_lines = self.classes[code_obj.klass].layout
        sizers_init_lines = self.classes[code_obj.klass].sizers_init

        # check if there are extra layout lines to add
        if hasattr(builder, 'get_layout_code'):
            extra_layout_lines = builder.get_layout_code(code_obj)
        else:
            extra_layout_lines = []

        # begin tag
        write(tab + '# begin wxGlade: %s::__do_layout\n\n' % code_obj.klass)

        if layout_lines or sizers_init_lines or extra_layout_lines:
            sizers_init_lines.reverse()
            for l in sizers_init_lines:
                write(tab + l)
            for l in layout_lines:
                write(tab + l)
            for l in extra_layout_lines:
                write(tab + l)
        else:
            write(tab + 'return;\n')

        # end tag
        write('\n')
        write(tab + '# end wxGlade\n')

        if is_new:
            write('}\n')

        if prev_src and not is_new:
            # replace the lines inside the __do_layout wxGlade block
            # with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, code_obj.klass,
                                                 '__do_layout')
            if prev_src.content.find(tag) < 0:
                # no __do_layout tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade __do_layout block not found for %s, __do_layout "
                    "code NOT generated" % code_obj.name
                    )
            else:
                prev_src.content = prev_src.content.replace(tag, "".join(buffer))

        # now let's generate the event handler stubs...
        if prev_src and not is_new:
            already_there = prev_src.event_handlers.get(code_obj.klass, {})
            buf = []
            for name, event, handler in event_handlers:
                if handler not in already_there:
                    buf.append('\n')
                    buf.append('sub %s {\n' % handler)
                    buf.append(tab + 'my ($self, $event) = @_;\n')
                    buf.append(tab + '# wxGlade: %s::%s <event_handler>\n\n' % (code_obj.klass, handler))
                    buf.append(tab + 'warn "Event handler (%s) not implemented";\n' % handler)
                    buf.append(tab + '$event->Skip;\n')
                    buf.append('\n')
                    buf.append(tab + '# end wxGlade\n}\n\n')
                    already_there[handler] = 1
            tag = '<%swxGlade event_handlers %s>' % (self.nonce, code_obj.klass)
            if prev_src.content.find(tag) < 0:
                # no event_handlers tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade event_handlers block not found for %s, "
                    "event_handlers code NOT generated" % code_obj.name
                    )
            else:
                prev_src.content = prev_src.content.replace(tag, "".join(buf))
            del buf
        else:
            already_there = {}
            for name, event, handler in event_handlers:
                if handler not in already_there:
                    write('\n')
                    write('sub %s {\n' % handler)
                    write(tab + 'my ($self, $event) = @_;\n')
                    write(tab + '# wxGlade: %s::%s <event_handler>\n\n' % (code_obj.klass, handler))
                    write(tab + 'warn "Event handler (%s) not implemented";\n' % handler)
                    write(tab + '$event->Skip;\n')
                    write('\n')
                    write(tab + '# end wxGlade\n}\n\n')
                    already_there[handler] = 1

        # the code has been generated
        self.classes[code_obj.klass].done = True

        write('\n# end of class %s\n\n1;\n\n' % code_obj.klass)

        if not self.multiple_files and prev_src:
            # if this is a new class, add its code to the new_classes list of the
            # SourceFileContent instance
            if is_new:
                prev_src.new_classes.append("".join(buffer))
            return

        if self.multiple_files:
            if prev_src:
                tag = '<%swxGlade insert new_classes>' % self.nonce
                prev_src.content = prev_src.content.replace(tag, "")

                # insert the extra modules
                tag = '<%swxGlade extra_modules>\n' % self.nonce
                code = "".join(self._current_extra_modules.keys())
                prev_src.content = prev_src.content.replace(tag, code)

                # insert the module dependencies of this class
                tag = '<%swxGlade replace dependencies>' % self.nonce
                dep_list = self.classes[code_obj.klass].dependencies.keys()
                dep_list.extend(self.dependencies.keys())
                dep_list.sort()
                code = self._tagcontent('dependencies', dep_list)
                prev_src.content = prev_src.content.replace(tag, code)

                # insert the extra code of this class
                extra_code = "".join(self.classes[code_obj.klass].extra_code[::-1])
                # if there's extra code but we are not overwriting existing
                # sources, warn the user
                if extra_code:
                    self.warning(
                        '%s (or one of its chilren) has extra code classes, '
                        'but you are not overwriting existing sources: please '
                        'check that the resulting code is correct!' % \
                        code_obj.name
                        )
                tag = '<%swxGlade replace extracode>' % self.nonce
                code = self._tagcontent('extracode', extra_code)
                prev_src.content = prev_src.content.replace(tag, code)

                # store the new file contents to disk
                self.save_file(filename, prev_src.content, content_only=True)
                return

            # create the new source file
            filename = os.path.join(self.out_dir,
                                    code_obj.klass.replace('::', os.sep) + '.pm')  # MODULE!!
            out = cStringIO.StringIO()
            write = out.write
            # write the common lines
            for line in self.header_lines:
                write(line)

            # write the module dependecies for this class
            dep_list = self.classes[code_obj.klass].dependencies.keys()
            dep_list.extend(self.dependencies.keys())
            dep_list.sort()
            code = self._tagcontent('dependencies', dep_list, True)
            write(code)

            # insert the extra code of this class
            code = self._tagcontent(
                'extracode',
                self.classes[code_obj.klass].extra_code[::-1],
                True
                )
            write(code)

            # write the class body
            for line in buffer:
                write(line)
            try:
                dirname = os.path.dirname(filename)  # create Foo in Foo::Bar,
                                                     # Foo/Bar.pm
                if not os.path.exists(dirname):
                    os.makedirs(dirname)
                # store the contents to filename
                self.save_file(filename, out.getvalue())
            except:
                import traceback
                traceback.print_exc()
            out.close()
        else:  # not self.multiple_files
            if prev_src:
                # if this is a new class, add its code to the new_classes list of the
                # SourceFileContent instance
                if is_new:
                    prev_src.new_classes.append("".join(buffer))
                elif self.classes[code_obj.klass].extra_code:
                    self._current_extra_code.extend(self.classes[code_obj.klass].extra_code[::-1])
                return
            else:
                # write the class body onto the single source file
                for dep in self.classes[code_obj.klass].dependencies:
                    self._current_extra_modules[dep] = 1
                if self.classes[code_obj.klass].extra_code:
                    self._current_extra_code.extend(self.classes[code_obj.klass].extra_code[::-1])
                write = self.output_file.write
                for line in buffer:
                    write(line)

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
            return

        try:
            init, props, layout = builder.get_code(sub_obj)
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
                evts = builder.get_events(sub_obj)
                for id, event, handler in evts:
                    klass.event_handlers.append((id, event, handler))
            elif 'events' in sub_obj.properties:
                id_name, id = self.generate_code_id(sub_obj)
                if id == '-1' or id == self.cn('wxID_ANY'):
                    id = '#$self->%s' % sub_obj.name
                for event, handler in sub_obj.properties['events'].iteritems():
                    klass.event_handlers.append((id, event, handler))

            # try to see if there's some extra code to add to this class
            if not sub_obj.preview:
                extra_code = getattr(builder, 'extracode',
                                     sub_obj.properties.get('extracode', ""))
                if extra_code:
                    extra_code = re.sub(r'\\n', '\n', extra_code)
                    klass.extra_code.append(extra_code)
                    # if we are not overwriting existing source, warn the user
                    # about the presence of extra code
                    if not self.multiple_files and self.previous_source:
                        self.warning(
                            '%s has extra code, but you are not '
                            'overwriting existing sources: please check '
                            'that the resulting code is correct!' % \
                            sub_obj.name
                            )

        else:  # the object is a sizer
            if sub_obj.base == 'wxStaticBoxSizer':
                klass.parents_init.insert(1, init.pop(0))
                # ${staticboxsizername}_staticbox
            klass.sizers_init.extend(init)

        klass.props.extend(props)
        klass.layout.extend(layout)
        if self.multiple_files and \
               (sub_obj.is_toplevel and sub_obj.base != sub_obj.klass):
            key = 'use %s;\n' % sub_obj.klass
            klass.dependencies[key] = 1
        for dep in getattr(self.obj_builders.get(sub_obj.base),
                           'import_modules', []):
            klass.dependencies[dep] = 1

    def add_sizeritem(self, toplevel, sizer, obj, option, flag, border):
        # an ugly hack to allow the addition of spacers: if obj_name can be parsed
        # as a couple of integers, it is the size of the spacer to add
        obj_name = obj.name
        try:
            w, h = [int(s) for s in obj_name.split(',')]
        except ValueError:
            if obj.in_windows:
                # attribute is a special property, which tells us if the object
                # is a local variable or an attribute of its parent
                if self.test_attribute(obj):
                    obj_name = '$self->{%s}' % obj_name

            if obj_name[0:1] != '$':
                obj_name = '$self->{%s}' % obj_name

        if toplevel.klass in self.classes:
            klass = self.classes[toplevel.klass]
        else:
            klass = self.classes[toplevel.klass] = self.ClassLines()
        buffer = '$self->{%s}->Add(%s, %s, %s, %s);\n' % \
                 (sizer.name, obj_name, option, self.cn_f(flag), self.cn_f(border))
        klass.layout.append(buffer)

    def generate_code_id(self, obj, id=None):
        if id is None:
            id = obj.properties.get('id')
        if not id:
            return '', self.cn('wxID_ANY')
        tokens = id.split('=', 1)
        if len(tokens) == 2:
            name, val = tokens
        else:
            return '', self.cn(tokens[0])   # we assume name is declared elsewhere
        if not name:
            return '', self.cn(val)
        name = name.strip()
        val = val.strip()
        if val == '?':
            val = 'Wx::NewId()'
        else:
            val = self.cn(val)
        # check to see if we have to make the var global or not...
        return ('use constant %s => %s;\n' % (name, val), name)

    def generate_code_size(self, obj):
        objname = self._get_code_name(obj)
        size = obj.properties.get('size', '').strip()
        use_dialog_units = (size[-1] == 'd')
        if not obj.parent:
            method = 'SetSize'
        else:
            method = 'SetMinSize'
        if use_dialog_units:
            return '%s->%s(%s->ConvertDialogSizeToPixels(Wx::Size->new(%s)));\n' % (
                   objname,
                   method,
                   objname,
                   size[:-1],
                   )
        else:
            return '%s->%s(Wx::Size->new(%s));\n' % (objname, method, size)

    def quote_str(self, s, translate=True, escape_chars=True):
        if not s:
            return '""'
        s = self._quote_str_pattern.sub(r'\\\\', s )
        s = s.replace('"', r'\"')
        s = s.replace('$', r'\$')
        s = s.replace('@', r'\@')
        if self._use_gettext and translate:
            return '_T("%s")' % s
        else:
            return '"%s"' % s

    def quote_key(self, s):
        """\
        returns a possibly quoted version of 's', suitable to insert in a perl
        source file as a hask key. Takes care also of gettext support
        """
        if not s:
            return '""'
        t = s
        s = self._quote_str_pattern.sub(r'\\\\', s)
        s = s.replace('"', r'\"')
        s = s.replace('$', r'\$')
        s = s.replace('@', r'\@')
        if self._use_gettext:
            return '_T("' + s + '")'
        if t == s and s.find(' ') < 0:
            return s
        else:
            return '"' + s + '"'

    def _get_code_name(self, obj):
        if obj.is_toplevel:
            return '$self'
        else:
            if self.test_attribute(obj):
                return '$self->{%s}' % obj.name
            else:  # it's an already declared lexical (my $foo)
                if obj.name[0] == '$':
                    return obj.name
                else:
                    return '$' + obj.name

# end of class PerlCodeWriter

writer = PerlCodeWriter()
"""\
The code writer is an instance of L{PerlCodeWriter}.
"""

language = writer.language
"""\
Language generated by this code generator
"""
