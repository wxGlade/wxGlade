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
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os, os.path, re
from codegen import BaseLangCodeWriter, BaseSourceFileContent, BaseWidgetHandler
import wcodegen
import compat


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
        r'#\s*wxGlade:\s*(?P<class>[\w:]+)::(?P<handler>\w+) <event_handler>'  # wxGlade event handler
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
                if not self.class_name:
                    # this is the first class declared in the file: insert the
                    # new ones before this
                    out_lines.append( '<%swxGlade insert new_classes>' % self.nonce )
                    self.new_classes_inserted = True
                self.class_name = result.group(1)
                self.class_name = self.format_classname(self.class_name)
                self.classes[self.class_name] = 1  # add the found class to the list
                                              # of classes of this module
                out_lines.append(line)
            elif not inside_block:
                result = self.rec_block_start.match(line)
                if result:
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
                    if not self.class_name:
                        out_lines.append( '<%swxGlade replace %s>' % (self.nonce, which_block) )
                    else:
                        out_lines.append( '<%swxGlade replace %s %s>' % (self.nonce, which_class, which_block) )
                else:
                    result = self.rec_event_handler.match(line)
                    if result:
                        which_handler = result.group('handler')
                        which_class = self.format_classname(result.group('class'))
                        self.event_handlers.setdefault(
                            which_class, {})[which_handler] = 1
                    if self.class_name and self.is_end_of_class(line):
                        # add extra event handlers here...
                        out_lines.append( '<%swxGlade event_handlers %s>' % (self.nonce, self.class_name) )
                    out_lines.append(line)
                    if self.is_import_line(line):
                        # add a tag to allow extra modules
                        out_lines.append( '<%swxGlade extra_modules>\n' % self.nonce )
            else:
                # ignore all the lines inside a wxGlade block
                if self.rec_block_end.match(line):
##                     self._logger.debug('end block')
                    inside_block = False
        if not self.new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not contain any class, so we must add the
            # new_classes tag at the end of the file
            out_lines.append( '<%swxGlade insert new_classes>' % self.nonce )
        # set the ``persistent'' content of the file
        self.content = out_lines

    def is_import_line(self, line):
        return line.lstrip().startswith('use Wx')



class WidgetHandler(BaseWidgetHandler):
    "Interface the various code generators for the widgets must implement"
    new_signature = []  # Constructor signature ($self->SUPER::new(@stuff)); see PerlCodeWriter.new_defaults



class PerlCodeWriter(BaseLangCodeWriter, wcodegen.PerlMixin):
    "Code writer class for writing Perl code out of the designed GUI elements; see: BaseLangCodeWriter"

    _code_statements = {
        'backgroundcolour': "%(objname)s->SetBackgroundColour(%(value)s);\n",
        'disabled':         "%(objname)s->Enable(0);\n",
        'extraproperties':  "%(objname)s->Set%(propname_cap)s(%(value)s);\n",
        'focused':          "%(objname)s->SetFocus();\n",
        'foregroundcolour': "%(objname)s->SetForegroundColour(%(value)s);\n",
        'hidden':           "%(objname)s->Show(0);\n",
        'setfont':          "%(objname)s->SetFont(Wx::Font->new(%(size)s, %(family)s, "
                            "%(style)s, %(weight)s, %(underlined)s, %(face)s));\n",
        'tooltip':          "%(objname)s->SetToolTipString(%(tooltip)s);\n",
        'tooltip_3':        "%(objname)s->SetToolTip(%(tooltip)s);\n",
        'wxcolour':         "Wx::Colour->new(%(value)s)",
        'wxnullcolour':     "Wx::NullColour",
        'wxsystemcolour':   "Wx::SystemSettings::GetColour(%(value)s)",
        }

    class_separator = '::'
    classattr_always = ['wxBoxSizer', 'wxStaticBoxSizer', 'wxGridSizer', 'wxFlexGridSizer']


    indent_amount = 1
    indent_symbol = '\t'
    indent_level_func_body = 1

    language_note = '# To get wxPerl visit http://www.wxperl.it\n' \
                    '#\n'

    name_ctor = 'new'

    new_defaults = []  # Default class members, will be initialised during new_project()

    shebang = '#!/usr/bin/perl -w -- \n#\n'

    SourceFileContent = SourceFileContent

    tmpl_name_do_layout = '__do_layout'
    tmpl_name_set_properties = '__set_properties'

    tmpl_cfunc_end = '%(tab)sreturn $self;\n' \
                     '\n' \
                     '}\n' \
                     '\n'

    tmpl_class_end = '\n%(comment)s end of class %(klass)s\n\n1;\n\n'
    tmpl_class_end_nomarker = '\n\n1;\n\n'

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
    tmpl_gridbagsizeritem = '%s->Add(%s, %s, %s, %s, %s);\n'
    tmpl_gridbagsizerspacer = '%s->Add(%s, %s, %s, %s, %s, %s);\n'
    tmpl_spacersize = '%s, %s'

    tmpl_style = \
        '%(tab)s$style = %(style)s\n' \
        '%(tab)s%(tab)sunless defined $style;\n' \
        '\n'
    tmpl_toplevel_style = tmpl_style

    tmpl_appfile = """%(overwrite)s%(header_lines)s"""

    def _get_app_template(self, app, top_win):
        'build template string for application'
        if not self.app_name: return None

        # XXX use Show() for frames/panels and ShowModal()/Destroy for dialogs
        klass = app.klass
        
        if self._use_gettext:
            gettext1 = ['%(tab)smy $local = Wx::Locale->new("English", "en", "en"); # replace with ??',
                        '%(tab)s$local->AddCatalog("%(textdomain)s"); # replace with the appropriate catalog name\n']
        else:
            gettext1 = []

        if klass:
            ret = [ 'package %(klass)s;',
                    '',
                    'use base qw(Wx::App);',
                    'use strict;',
                    '%(pl_import)s',
                    'sub OnInit {',
                    '%(tab)smy( $self ) = shift;',
                    '',
                    '%(tab)sWx::InitAllImageHandlers();',
                    '',
                    '%(tab)smy $%(top_win)s = %(top_win_class)s->new();',
                    '',
                    '%(tab)s$self->SetTopWindow($%(top_win)s);',
                    '%(tab)s$%(top_win)s->Show(1);',
                    '',
                    '%(tab)sreturn 1;',
                    '}']
            if self._mark_blocks:
                ret.append('# end of class %(klass)s')
            ret += ['',
                    'package main;',
                    '',
                    'unless(caller){'] + gettext1 + [
                    '%(tab)smy $%(name)s = %(klass)s->new();',
                    '%(tab)s$%(name)s->MainLoop();',
                    '}', '']
        else:
            ret = ['1;',
                    '',
                    'package main;',
                    '%(pl_import)s',
                    'unless(caller){'] + gettext1 + [
                    '%(tab)slocal *Wx::App::OnInit = sub{1};',
                    '%(tab)smy $%(name)s = Wx::App->new();',
                    '%(tab)sWx::InitAllImageHandlers();',
                    '',
                    '%(tab)smy $%(top_win)s = %(top_win_class)s->new();',
                    '',
                    '%(tab)s$%(name)s->SetTopWindow($%(top_win)s);',
                    '%(tab)s$%(top_win)s->Show(1);',
                    '%(tab)s$%(name)s->MainLoop();',
                    '}', '']
        return '\n'.join(ret)

    def init_lang(self, app_attrs):
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
            'use Wx qw[:allclasses];\n',
            'use strict;\n'
        ]

    def add_app(self, app_attrs, top_win):
        # add language specific mappings
        if self.multiple_files:
            self.lang_mapping['pl_import'] = "\nuse %s;\n" % top_win.klass
        else:
            self.lang_mapping['pl_import'] = ''
        BaseLangCodeWriter.add_app(self, app_attrs, top_win)

    def generate_code_ctor(self, code_obj, is_new, tab):
        code_lines = []
        write = code_lines.append

        builder = self.obj_builders[code_obj.base]
        mycn = getattr(builder, 'cn', self.cn)
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        # custom base classes support
        custom_base = getattr( code_obj, 'custom_base', code_obj.properties.get('custom_base', None) )
        if self.preview or (custom_base and not custom_base.strip()):
            custom_base = None

        new_signature = getattr(builder, 'new_signature', [])

        # generate constructor code
        if is_new:
            write('package %s;\n\n' % code_obj.klass)
            write('use Wx qw[:everything];\nuse base qw(%s);\nuse strict;\n\n' % code_obj.base.replace('wx', 'Wx::', 1))

            if self._use_gettext:
                if self.multiple_files:
                    self.classes[code_obj.klass].dependencies["use Wx::Locale gettext => '_T';\n"] = 1
                else:
                    write("use Wx::Locale gettext => '_T';\n")

            # The dependencies have to add to the package block too because global imports are not visible inside the
            # package block
            # TODO: Don't add dependencies twice with Perl

            # write the module dependencies for this class (package)
            dep_list = sorted( self.classes[code_obj.klass].dependencies.keys() )
            if dep_list:
                code = self._tagcontent('dependencies', dep_list, True)
                write(code)

            write('sub new {\n')

            write(tab + "my( $self, %s ) = @_;\n" % ", ".join(new_signature))

            if new_signature:
                for k in new_signature:
                    if k in self.new_defaults:
                        write(self.new_defaults[k])
            else:
                new_signature = ['@_[1 .. $#_]']  # shift(@_)->SUPER::new(@_);
                self._logger.info( "%s did not declare self.new_defaults ", code_obj.klass )

        elif custom_base:
            # custom base classes set, but "overwrite existing sources" not
            # set. Issue a warning about this
            self.warning( '%s has custom base classes, but you are not overwriting existing sources: '
                          'please check that the resulting code is correct!' % code_obj.name )

        if self._mark_blocks:
            # __init__ begin tag
            write(self.tmpl_block_begin % {'class_separator':self.class_separator, 'comment_sign':self.comment_sign,
                                           'function':self.name_ctor, 'klass':self.cn_class(code_obj.klass),
                                           'tab':tab} )

        style_p = code_obj.properties.get("style")
        if style_p and style_p.value_set != style_p.default_value:
            style = style_p.get_string_value()
            m_style = mycn_f( style )
            if m_style:
                stmt_style = self._format_style(style, code_obj)
                write( stmt_style % {'style':m_style, 'tab':tab} )

        # class parent constructor
        write(tab + '$self = $self->SUPER::new( %s );\n' % ", ".join(new_signature))

        # set size here to avoid problems with splitter windows
        if 'size' in code_obj.properties and code_obj.properties["size"].is_active():
            write( tab+ self.generate_code_size(code_obj) )

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

        for win_id, event, handler, unused in event_handlers:
            if win_id.startswith('#'):
                win_id = '$self->{%s}->GetId' % win_id[8:]

            if 'EVT_NAVIGATION_KEY' in event:
                tmpl = '''%(tab)s%(event)s($self, $self->can('%(handler)s'));\n'''
            else:
                tmpl = '''%(tab)s%(event)s($self, %(win_id)s, $self->can('%(handler)s'));\n'''
            details = { 'tab': tab, 'event': self.cn(event), 'handler': handler, 'win_id': win_id }
            write(tmpl % details)

        if event_handlers:
            write('\n')

        return code_lines

    def generate_code_id(self, obj, id=None):
        if id is None:
            id = obj.window_id
        if not id:
            if obj is not None and "stockitem" in obj.properties and obj.stockitem:
                return '', self.cn("wxID_" + obj.stockitem)
            return '', self.cn('wxID_ANY')
        id = str(id)
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
        return 'use constant %s => %s;\n' % (name, val), name

    def generate_code_size(self, obj):
        objname = self.format_generic_access(obj)
        size = obj.properties["size"].get_string_value()
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

    def _quote_str(self, s):
        """\
        Escape all unicode characters to there unicode code points in form
        of \\uxxxx. The returned string is a pure ascii string.

        Normal ascii characters like \\n or \\t won't be escaped.

        @note: wxGlade don't handles file encoding well currently. Thereby
               we escape all unicode characters.

        @note: The string 's' is encoded with L{self.app_encoding} already.

        @see: L{BaseLangCodeWriter._quote_str} for additional details
        @see: L{_recode_x80_xff()}
        """
        s = s.replace('$', r'\$')
        s = s.replace('@', r'\@')

        # convert all strings to unicode first
        if not isinstance(s, compat.unicode):
            s = s.decode(self.app_encoding)

        # check if it's pure ascii
        try:
            dummy = s.encode('ascii')
            if self._use_gettext:
                return '_T("%s")' % s
            else:
                return '"%s"' % s
        except UnicodeError:
            pass

        # convert unicode strings to pure ascii
        # use "raw-unicode-escape" just escaped unicode characters and not default escape sequences
        s = s.encode('raw-unicode-escape')
        s = self._recode_x80_xff(s)
        if compat.PYTHON3:
            # convert back to str (unicode)
            s = s.decode("ASCII")
        # convert Python style to Perl style
        s = re.sub(r'\\u([0-9a-f]{4})', r'\\N{U+\1}', s)

        if self._use_gettext:
            return '_T("%s")' % s
        else:
            return '"%s"' % s

    def add_object_format_name(self, name):
        return '#$self->%s' % name

    def _format_classattr(self, obj):
        res = BaseLangCodeWriter._format_classattr(self, obj)
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
        elif self.store_as_attr(obj) or obj.is_sizer:
            return '$self->{%s}' % obj.name
        return '$%s' % obj.name

    def _format_import(self, klass):
        stmt = 'use %s;\n' % klass
        return stmt

    def _get_class_filename(self, klass):
        "Returns the name for a Perl module (.pm) to store a single class in multi file projects"
        filename = os.path.join( self.out_dir, klass.replace('::', os.sep) + '.pm' )
        return filename

    def format_generic_access(self, obj):
        if obj.is_toplevel:
            return '$self'
        return self._format_classattr(obj)


writer = PerlCodeWriter()  # the code writer instance

language = writer.language  # Language generated by this code generator
