"""\
Lisp code generator

How the code is generated: every time the end of an object is reached during
the parsing of the xml tree, either the function 'add_object' or the function
'add_class' is called: the latter when the object is a toplevel one, the former
when it is not. In the last case, 'add_object' calls the appropriate ``writer''
function for the specific object, found in the 'obj_builders' dict. Such
function accepts one argument, the CodeObject representing the object for
which the code has to be written, and returns 3 lists of strings, representing
the lines to add to the '__init__', '__set_properties' and '__do_layout'
methods of the parent object.

@copyright: 2005 Surendra K Singhi <efuzzyone@users.sourceforge.net>
@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import os.path
import re

from codegen import BaseLangCodeWriter, BaseSourceFileContent, BaseWidgetHandler
import errors
import wcodegen


class SourceFileContent(BaseSourceFileContent):

    rec_block_start = re.compile(
        r'^(?P<spaces>\s*)'                     # leading spaces
        r';;;\s*'                               # comment sign
        r'begin\s+wxGlade:\s*'                  # "begin wxGlade:" statement and tailing spaces
        r'(?P<classname>[a-zA-Z_]+\w*)??'       # class or function name (non-greedy)
        r'[.]?'                                 # separator between class and function / block (non-greedy)
        r'(?P<block>\w+)'                       # function / block name
        r'\s*$'                                 # tailing spaces
        )

    rec_block_end = re.compile(
        r'^\s*'                                 # leading spaces
        r';;;\s*'                               # comment sign
        r'end\s+wxGlade'                        # "end exGlade" statement
        r'\s*$'                                 # tailing spaces
        )

    rec_class_decl = re.compile(
        r'^\s*'                                       # leading spaces
        r'\(defclass\s+([a-zA-Z_]\w*)\s*(\(\s*\))'    # "class <name>" statement
        r'\s*$'                                       # tailing spaces
        )

    rec_event_handler = re.compile(
        r'^\s+'                                              # leading spaces (mandatory)
        r'def\s+(?P<handler>[A-Za-z_]+\w*)'                  # event handler name
        r'\s*'                                               # optional spaces
        r'\(.*\):'                                           # function parameters
        r'\s*'                                               # optional spaces
        r';;;\s*wxGlade:\s*(?P<class>\w+)\.<event_handler>'  # wxGlade event handler statement with class name
        r'\s*$'                                              # tailing spaces
        )

    def build_untouched_content(self):
        BaseSourceFileContent.build_untouched_content(self)
        inside_block = False
        inside_triple_quote = False
        triple_quote_str = None
        tmp_in = self._load_file(self.name)
        out_lines = []
        for line in tmp_in:
            quote_index = -1
            if not inside_triple_quote:
                triple_dquote_index = line.find('"""')
                triple_squote_index = line.find("'''")
                if triple_squote_index == -1:
                    quote_index = triple_dquote_index
                    tmp_quote_str = '"""'
                elif triple_dquote_index == -1:
                    quote_index = triple_squote_index
                    tmp_quote_str = "'''"
                else:
                    quote_index, tmp_quote_str = min( (triple_squote_index, "'''"), (triple_dquote_index, '"""') )

            if not inside_triple_quote and quote_index != -1:
                inside_triple_quote = True
                triple_quote_str = tmp_quote_str
            if inside_triple_quote:
                end_index = line.rfind(triple_quote_str)
                if quote_index < end_index and end_index != -1:
                    inside_triple_quote = False

            result = self.rec_class_decl.match(line)
            if not inside_triple_quote and result:
                if not self.class_name:
                    # this is the first class declared in the file: insert the new ones before this
                    out_lines.append('<%swxGlade insert new_classes>' % self.nonce)
                    self.new_classes_inserted = True
                self.class_name = result.group(1)
                self.class_name = self.format_classname(self.class_name)
                self.classes[self.class_name] = 1  # add the found class to the list of classes of this module
                out_lines.append(line)
            elif not inside_block:
                result = self.rec_block_start.match(line)
                if not inside_triple_quote and result:
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
                    if not inside_triple_quote and result:
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
                    inside_block = False
        if not self.new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not contain any class, so we must add the
            # new_classes tag at the end of the file
            out_lines.append('<%swxGlade insert new_classes>' % self.nonce)
        # set the ``persistent'' content of the file
        self.content = out_lines

    def is_import_line(self, line):
        return line.startswith('(use-package :wx')



class WidgetHandler(BaseWidgetHandler):
    pass



class LispCodeWriter(BaseLangCodeWriter, wcodegen.LispMixin):
    "Code writer class for writing Lisp code out of the designed GUI elements"

    _code_statements = {
        'backgroundcolour': "(wxWindow_SetBackgroundColour %(objname)s %(value)s)\n",
        'disabled':         "(wxWindow_IsEnabled %(objname)s0)\n",
        'extraproperties':  "(%(klass)s_Set%(propname_cap)s (slot-%(objname)s obj) %(value)s)\n",
        'focused':          "(wxWindow_SetFocus %(objname)s)\n",
        'foregroundcolour': "(wxWindow_SetForegroundColour %(objname)s %(value)s)\n",
        'hidden':           "(wxWindow_Hide %(objname)s)\n",
        'setfont':          "(wxWindow_SetFont %(objname)s (wxFont_Create %(size)s %(family)s "
                            "%(style)s %(weight)s %(underlined)s %(face)s wxFONTENCODING_DEFAULT))\n",
        'tooltip':          "(wxWindow_SetToolTip %(objname)s%(tooltip)s)\n",
        'wxcolour':         "(wxColour_CreateRGB %(value)s)",
        'wxnullcolour':     "wxNullColour",
        'wxsystemcolour':   "(wxSystemSettings_GetColour %(value)s)",
        }

    class_separator = '.'
    classattr_always = ['wxBoxSizer', 'wxStaticBoxSizer', 'wxGridSizer', 'wxFlexGridSizer']

    #global_property_writers = {
        #'font':            BaseLangCodeWriter.FontPropertyHandler,
        #'events':          BaseLangCodeWriter.EventsPropertyHandler,
        #'extraproperties': BaseLangCodeWriter.ExtraPropertiesPropertyHandler,
        #}

    indent_level_func_body = 2

    name_ctor = '__init__'

    shebang = '#!/usr/bin/env lisp\n;;;\n'

    SourceFileContent = SourceFileContent

    tmpl_name_do_layout = '__do_layout'
    tmpl_name_set_properties = '__set_properties'

    tmpl_cfunc_end = '%(tab)s)\n'

    tmpl_class_end = '\n%(comment)s end of class %(klass)s\n\n\n'
    tmpl_class_end_nomarker = '\n\n\n'

    tmpl_func_do_layout = '\n' \
                          '(defmethod do-layout ((obj %(klass)s))\n' \
                          '%(content)s' \
                          '%(tab)s)\n'

    tmpl_func_event_stub = """\
(defun %(handler)s (function data event) ;;; wxGlade: %(klass)s.<event_handler>
%(tab)s(print "Event handler '%(handler)s' not implemented!")
%(tab)s(when event
%(tab)s%(tab)s(wxEvent:wxEvent_Skip event)))
"""

    tmpl_func_set_properties = '\n' \
                          '(defmethod set-properties ((obj %(klass)s))\n' \
                          '%(content)s' \
                          '%(tab)s)\n'

    tmpl_func_empty = '%(tab)spass\n'

    tmpl_appfile = """\
%(overwrite)s\
%(header_lines)s\
(require "%(top_win_class)s")

"""

    tmpl_detailed = """\
(defun init-func (fun data evt)
%(tab)s%(tab)s(let ((%(top_win)s (make-%(top_win_class)s)))
%(tab)s%(tab)s(ELJApp_SetTopWindow (slot-top-window %(top_win)s))
%(tab)s%(tab)s(wxWindow_Show (slot-top-window %(top_win)s))))
;;; end of class %(klass)s


(unwind-protect
%(tab)s(Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
%(tab)s(ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
"""

    # ZZZ merge tmpl_gettext_detailed  and tmpl_simple
    tmpl_gettext_detailed = """\
(defun init-func (fun data evt)
%(tab)s%(tab)s(let ((%(top_win)s (make-%(top_win_class)s)))
%(tab)s%(tab)s(ELJApp_SetTopWindow (slot-top-window %(top_win)s))
%(tab)s%(tab)s(wxWindow_Show (slot-top-window %(top_win)s))))
;;; end of class %(klass)s

%(tab)s(setf (textdomain) "%(textdomain)s") ;; replace with the appropriate catalog name
%(tab)s(defun _ (msgid) (gettext msgid "%(textdomain)s"))


(unwind-protect
%(tab)s(Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
%(tab)s(ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
"""
    tmpl_simple = """\
(defun init-func (fun data evt)
%(tab)s(let ((%(top_win)s (make-%(top_win_class)s)))
%(tab)s(ELJApp_SetTopWindow (slot-top-window %(top_win)s))
%(tab)s(wxWindow_Show (slot-top-window %(top_win)s))))

(unwind-protect
%(tab)s(Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
%(tab)s(ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
"""

    tmpl_gettext_simple = """\
(defun init-func (fun data evt)
%(tab)s(setf (textdomain) "%(textdomain)s") ;; replace with the appropriate catalog name
%(tab)s(defun _ (msgid) (gettext msgid "%(textdomain)s"))

%(tab)s(let ((%(top_win)s (make-%(top_win_class)s)))
%(tab)s(ELJApp_SetTopWindow (slot-top-window %(top_win)s))
%(tab)s(wxWindow_Show (slot-top-window %(top_win)s))))

(unwind-protect
%(tab)s(Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
%(tab)s(ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
"""

    def _get_app_template(self, app, top_win):
        # check for templates for detailed startup code
        if not self.app_name: return None
        klass = app.klass
        ret = None
        if klass and self._use_gettext:
            ret = self.tmpl_gettext_detailed
        elif klass and not self._use_gettext:
            ret = self.tmpl_detailed
        if ret:
            if not self._mark_blocks:
                ret.replace(";;; end of class %(klass)s\n", "")
            return ret
        
        # check for templates for simple startup code
        elif not klass and self._use_gettext:
            return self.tmpl_gettext_simple
        elif not klass and not self._use_gettext:
            return self.tmpl_simple
    def init_lang(self, app_attrs):
        self.class_lines = []

        self.header_lines = [
            """(asdf:operate 'asdf:load-op 'wxcl)\n""",
            """(use-package "FFI")\n""",
            """(ffi:default-foreign-language :stdc)\n\n""",
            ]

        self.dependencies = {
            '(use-package :wxCL)':         1,
            '(use-package :wxFrame)':      1,
            '(use-package :wx_main)':      1,
            '(use-package :wx_wrapper)':   1,
            '(use-package :wxWindow)':     1,
            '(use-package :wxColour)':     1,
            '(use-package :wxEvtHandler)': 1,
            '(use-package :wxEvent)':      1,
            }

    def check_values(self):
        BaseLangCodeWriter.check_values(self)
        if self.for_version > (2, 8):
            raise errors.WxgLispWx3NotSupported("%d.%d" % self.for_version)

    def add_app(self, app, top_win_class):
        top_win = app.top_window
        # do nothing if there is no top window
        if not top_win: return

        # add language specific mappings
        self.lang_mapping = { 'top_win': self._format_name(top_win), }
        BaseLangCodeWriter.add_app(self, app, top_win_class)

    def add_object(self, sub_obj):
        # the lisp code gen add some hard coded depedencies
        # TODO: Move the hard coded dependencies to the widgets resp. sizers

        # XXX temporarily disabled...
        #sub_obj.name = self._format_name(sub_obj.name)
        #sub_obj.parent.name = self._format_name(sub_obj.parent.name)

        # get top level source code object and the widget builder instance
        klass, builder = self._add_object_init(sub_obj)
        if not klass or not builder:
            return

        if sub_obj.name not in ("spacer","sizerslot"):
            self.class_lines.append( self._format_name(sub_obj.name) )
        if (sub_obj.klass == "wxBoxSizer"  or sub_obj.klass == "wxStaticBoxSizer" or
            sub_obj.klass == "wxGridSizer" or sub_obj.klass == "wxFlexGridSizer"):
            self.dependencies['(use-package :wxSizer)'] = 1
        else:
            if sub_obj.klass not in ("spacer", "sizerslot"):
                key = '(use-package :%s)' % sub_obj.klass
                self.dependencies[key] = 1

        if sub_obj.klass == "wxMenuBar":
            self.dependencies['(use-package :wxMenu)'] = 1

        BaseLangCodeWriter.add_object(self, sub_obj)

    def add_sizeritem(self, toplevel, sizer, obj, option, flag, border):
        # XXX remove this hack
        if obj.is_sizer:
            self.tmpl_sizeritem = '(wxSizer_AddSizer (%s obj) (%s obj) %s %s %s nil)\n'
        else:
            self.tmpl_sizeritem = '(wxSizer_AddWindow (%s obj) (%s obj) %s %s %s nil)\n'

        BaseLangCodeWriter.add_sizeritem( self, toplevel, sizer, obj, option, flag, border )

    def add_spacer(self, toplevel, sizer, obj=None, proportion=0, flag='0', border=0):
        # XXX remove this hack
        self.tmpl_sizeritem = '(wxSizer_AddWindow (%s obj) (%s obj) %s %s %s nil)\n'
        BaseLangCodeWriter.add_spacer(self, toplevel, sizer, obj, proportion, flag, border)

    def generate_code_background(self, obj):
        self.dependencies['(use-package :wxColour)'] = 1
        return BaseLangCodeWriter.generate_code_background(self, obj)

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

        # generate constructor code
        if is_new:
            base = mycn(code_obj.base)
            klass = code_obj.klass
            write('\n(defclass %s()\n' % klass)
            write(tab + "((top-window :initform nil :accessor slot-top-window)")
            for l in self.class_lines:
                write("\n")
                write(tab + "(" + l + " :initform nil :accessor slot-" + l + ")")
            write("))\n")

            write("\n(defun make-%s ()\n" % klass)
            write(tab + "(let ((obj (make-instance '%s)))\n" % klass)
            write(tab + "  (init obj)\n")
            write(tab + "  (set-properties obj)\n")
            write(tab + "  (do-layout obj)\n")
            write(tab + "  obj))\n")

            write('\n(defmethod init ((obj %s))\n' % klass)
            write("\"Method creates the objects contained in the class.\"\n")

        elif custom_base:
            # custom base classes set, but "overwrite existing sources" not
            # set. Issue a warning about this
            self.warning( '%s has custom base classes, but you are not overwriting existing sources: please check that '
                          'the resulting code is correct!' % code_obj.name )

        if self._mark_blocks:
            # __init__ begin tag
            write( self.tmpl_block_begin % {'class_separator':self.class_separator, 'comment_sign':self.comment_sign,
                                            'function':self.name_ctor, 'klass':self.cn_class(code_obj.klass),
                                            'tab':tab} )

        style_p = code_obj.properties.get("style")
        if style_p and style_p.value_set != style_p.default_value:
            m_style = mycn_f( style_p.get_string_value() )
            if m_style:
                stmt_style = self._format_style(m_style, code_obj)
                write(stmt_style % {'style': m_style, 'tab': tab} )

        # set size here to avoid problems with splitter windows
        if 'size' in code_obj.properties and code_obj.properties["size"].is_active():
            write( tab + self.generate_code_size(code_obj) )

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

        if event_handlers:
            write('\n')

        for win_id, event, handler, unused in event_handlers:
            if win_id.startswith('#'):
                win_id = win_id[1:]
            win_id = self._format_name(win_id)

            write( "%(tab)s(wxEvtHandler_Connect (slot-top-window obj) %(win_id)s (exp%(event)s)\n%(tab2)s" \
                   "(wxClosure_Create #'%(handler)s obj))\n" % 
                    {'tab':tab, 'tab2':self.tabs(2), 'win_id':win_id, 'event':self.cn(event), 'handler':handler} )

        return code_lines

    def generate_code_font(self, obj):
        self.dependencies['(use-package :wxFont)'] = 1
        return BaseLangCodeWriter.generate_code_font(self, obj)

    def generate_code_foreground(self, obj):
        self.dependencies['(use-package :wxColour)'] = 1
        return BaseLangCodeWriter.generate_code_foreground(self, obj)

    def generate_code_id(self, obj, id=None):
        if id is None:
            id = obj.window_id
        if not id:
            if obj is not None and "stockitem" in obj.properties and obj.stockitem:
                return '', self.cn("wxID_" + obj.stockitem)
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
        if '.' in name:
            return '%s = %s\n' % (name, val), name
        return 'global %s; %s = %s\n' % (name, name, val), name

    def generate_code_size(self, obj):
        objname = self.format_generic_access(obj)
        size = obj.properties["size"].get_string_value()
        use_dialog_units = (size[-1] == 'd')
        if not obj.parent:
            method = 'wxWindow_SetSize'
        else:
            method = 'SetMinSize'
        if use_dialog_units:
            return '(%s %s(%s(%s (%s))))\n' % ( method, objname, self.cn('wxDLG_SZE'), objname, size[:-1] )
        else:
            return '%s.%s((%s))\n' % (objname, method, size)

    def _quote_str(self, s):
        if self._use_gettext:
            return '(_"%s")' % s
        else:
            return '"%s"' % s

    def add_object_format_name(self, name):
        return '#obj.%s' % name

    def _format_classattr(self, obj):
        res = BaseLangCodeWriter._format_classattr(self, obj)
        if not res:
            return res
        elif obj.name.startswith('slot-'):
            return obj.name
        # spacer.name is "<width>, <height>" already, but wxLisp expect
        # a tuple instead of two single values
        elif obj.klass in ('spacer','sizerslot'):
            return '(%s)' % obj.name
        # wxList use class attributes always (unfortunately)
#        elif self.store_as_attr(obj):
#            return "slot-%s" % self._format_name(obj.name)
#        return self._format_name(obj.name)
        return 'slot-%s' % self._format_name(obj.name)

    def _format_import(self, klass):
        stmt = '(require "%s")\n' % klass
        return stmt

    def _format_style(self, style, code_obj):
        builder = self.obj_builders[code_obj.base]
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        if not style: return ''
        style = mycn_f(style)
        if not style: return ''
        style = style.strip().replace('.', '')

        if code_obj.base == "wxFrame":
            stmt = '%%(tab)s(setf (slot-top-window obj) (wxFrame_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n' % style
        elif code_obj.base == "wxDialog":
            stmt = '%%(tab)s(setf (slot-top-window obj) (wxDialog_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n' % style
            self.dependencies['(use-package :wxButton)'] = 1
        else:
            stmt = ''

        return stmt

    def _get_class_filename(self, klass):
        filename = os.path.join( self.out_dir, klass.replace('.', '_') + '.lisp' )
        return filename

    def format_generic_access(self, obj):
        if obj.is_toplevel:
            return '(slot-top-window obj)'
        else:
            obj_name = self._format_name(obj.name)
            if self.store_as_attr(obj):
                return '(slot-%s obj)' % obj_name
            else:
                return obj_name

    def _format_name(self, name):
        return name.replace('_', '-')


writer = LispCodeWriter()  # The code writer is an instance of L{LispCodeWriter}.

language = writer.language  # Language generated by this code generator
