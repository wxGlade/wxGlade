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

    class_separator = '::'
    classattr_always = ['wxBoxSizer', 'wxStaticBoxSizer', 'wxGridSizer',
                        'wxFlexGridSizer']
    comment_sign = '#'

    global_property_writers = {
        'font':            BaseCodeWriter.FontPropertyHandler,
        'events':          BaseCodeWriter.EventsPropertyHandler,
        'extraproperties': BaseCodeWriter.ExtraPropertiesPropertyHandler,
        }

    indent_amount = 1
    indent_symbol = '\t'
    indent_level_func_body = 1

    language_note = '# To get wxPerl visit http://wxPerl.sourceforge.net/\n' \
                    '#\n'

    name_ctor = 'new'

    new_defaults = []
    """\
    Default class members, will be initialised during L{initialize()}
    """

    shebang = '#!/usr/bin/perl -w -- \n#\n'

    SourceFileContent = SourceFileContent

    tmpl_name_do_layout = '__do_layout'
    tmpl_name_set_properties = '__set_properties'

    tmpl_cfunc_end = '%(tab)sreturn $self;\n' \
                     '\n' \
                     '}\n' \
                     '\n'

    tmpl_class_end = '\n' \
                     '%(comment)s end of class %(klass)s\n' \
                     '\n' \
                     '1;\n' \
                     '\n'

    tmpl_ctor_call_layout = '\n' \
                            '%(tab)s$self->__set_properties();\n' \
                            '%(tab)s$self->__do_layout();\n\n'

    tmpl_func_do_layout = '\n' \
                          'sub __do_layout {\n' \
                          '%(tab)smy $self = shift;\n' \
                          '%(content)s' \
                          '}\n'

    tmpl_func_event_stub = """\

sub %(handler)s {
%(tab)smy ($self, $event) = @_;
%(tab)s# wxGlade: %(klass)s::%(handler)s <event_handler>
%(tab)swarn "Event handler (%(handler)s) not implemented";
%(tab)s$event->Skip;
%(tab)s# end wxGlade
}

"""

    tmpl_func_set_properties = '\n' \
                          'sub __set_properties {\n' \
                          '%(tab)smy $self = shift;\n' \
                          '%(content)s' \
                          '}\n'

    tmpl_func_empty = '%(tab)sreturn;\n'

    tmpl_sizeritem = '%s->Add(%s, %s, %s, %s);\n'

    tmpl_style = \
        '%(tab)s$style = %(style)s \n' \
        '%(tab)s%(tab)sunless defined $style;\n' \
        '\n'

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
         'wxHORIZONTAL', 'wxVERTICAL',
         'wxALIGN_CENTER', 'wxALIGN_CENTRE', 'wxALIGN_LEFT', 'wxALIGN_RIGHT',
         'wxALIGN_TOP', 'wxALIGN_BOTTOM','wxALIGN_CENTER_VERTICAL',
         'wxALIGN_CENTRE_VERTICAL', 'wxALIGN_CENTER_HORIZONTAL',
         'wxALIGN_CENTRE_HORIZONTAL',
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

        @keyword path: Output path for the generated code (a file if
                       multi_files is False, a dir otherwise)
        @keyword option: If True, generate a separate file for each custom
                         class
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

        self._initialize_stage2(out_path)

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

    def generate_code_ctor(self, code_obj, is_new, tab):
        code_lines = []
        write = code_lines.append
        
        builder = self.obj_builders[code_obj.base]
        mycn = getattr(builder, 'cn', self.cn)
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        # custom base classes support
        custom_base = getattr(code_obj, 'custom_base',
                              code_obj.properties.get('custom_base', None))
        if code_obj.preview or (custom_base and not custom_base.strip()):
            custom_base = None

        new_signature = getattr(builder, 'new_signature', [] )

        # generate constructor code
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

        elif custom_base:
            # custom base classes set, but "overwrite existing sources" not
            # set. Issue a warning about this
            self.warning(
                '%s has custom base classes, but you are not overwriting '
                'existing sources: please check that the resulting code is '
                'correct!' % code_obj.name
                )

        # __init__ begin tag
        write(self.tmpl_block_begin % {
            'class_separator': self.class_separator,
            'comment_sign':    self.comment_sign,
            'function':        self.name_ctor, 
            'klass':           self.cn_class(code_obj.klass),
            'tab':             tab,
            })

        prop = code_obj.properties
        style = prop.get("style", None)
        if style:
            stmt_style = self._format_style(style, code_obj)
            write(stmt_style % {
                'style': mycn_f(style),
                'tab': tab,
                })

        # class parent constructor
        write(tab + '$self = $self->SUPER::new( %s );\n' % ", ".join(new_signature))

        # classes[code_obj.klass].deps now contains a mapping of child to
        # parent for all children we processed...
        object_order = []
        for obj in self.classes[code_obj.klass].child_order:
            # Don't add it again if already present
            if obj in object_order:
                continue

            object_order.append(obj)

            # Insert parent and ancestor objects before the current object
            current_object = obj
            for child, parent in self.classes[code_obj.klass].deps[:]:
                if child is current_object:
                    if parent not in object_order:
                        idx = object_order.index(current_object)
                        object_order.insert(idx, parent)
                    current_object = parent

                    # We processed the dependency: remove it
                    self.classes[code_obj.klass].deps.remove((child, parent))

        # Write out the initialisation in the order we just generated
        for obj in object_order:
            if obj in self.classes[code_obj.klass].init_lines:
                for l in self.classes[code_obj.klass].init_lines[obj]:
                    write(tab + l)

        return code_lines

    def generate_code_event_bind(self, code_obj, tab, event_handlers):
        code_lines = []
        write = code_lines.append

        for win_id, event, handler in event_handlers:
            if win_id.startswith('#'):
                win_id = '$self->{%s}->GetId' % win_id[8:]
            # remove leading "Wx::" for already formatted event names
            if event.startswith('Wx::EVT'):
                event = event[4:]
            write('%(tab)sWx::Event::%(event)s($self, %(win_id)s, \\&%(handler)s);\n' % {
                'tab':     tab,
                'event':   event,
                'win_id':  win_id,
                'handler': handler,
                })

        if event_handlers:
            write('\n')

        return code_lines

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
            val = self.cn('wxNewId()')
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
        try:
            dummy = unicode(s, 'ascii')
        except UnicodeDecodeError:
            # convert byte string to unicode, escape unicode characters and
            # convert string back to ascii
            s = s.decode('utf8')
            s = s.encode('unicode-escape')
            # convert Python style to Perl style
            s = re.sub(r'\\u([0-9]{4})\b', r'\\N{U+\1}', s)

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
            return '_T("%s")' %s
        if t == s and s.find(' ') < 0:
            return s
        else:
            return '"%s"' % s

    def _add_object_format_name(self, name):
        return '#$self->%s' % name

    def _format_classattr(self, obj):
        res = BaseCodeWriter._format_classattr(self, obj)
        if not res:
            return res
        elif obj.name.startswith('$self->'):
            return obj.name
        elif obj.name.startswith('$'):
            return obj.name
        # spacer.name is "<width>, <height>" already
        elif obj.klass == 'spacer':
            return obj.name
        # Perl stores sizers always in class attributes
        elif self.test_attribute(obj) or obj.in_sizers:
            return '$self->{%s}' % obj.name
        return '$%s' % obj.name

    def _format_import(self, klass):
        stmt = 'use %s;\n' % klass
        return stmt

    def _get_class_filename(self, klass):
        """
        Returns the name for a Perl module (.pm) to store a single class
        in multi file projects.

        @param klass: Class name
        @type klass:  String
        
        @rtype: String
        """
        filename = os.path.join(
            self.out_dir,
            klass.replace('::', os.sep) + '.pm'
            )
        return filename

    def _get_code_name(self, obj):
        if obj.is_toplevel:
            return '$self'
        else:
            return self._format_classattr(obj)

# end of class PerlCodeWriter

writer = PerlCodeWriter()
"""\
The code writer is an instance of L{PerlCodeWriter}.
"""

language = writer.language
"""\
Language generated by this code generator
"""
