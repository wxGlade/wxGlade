"""\
Lisp code generator

@copyright: 2005 Surendra K Singhi <efuzzyone@users.sourceforge.net>
@copyright: 2012-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import os.path
import re

from codegen import BaseLangCodeWriter, BaseSourceFileContent
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
        check_old_methods = []  # list of indices with set_properties or do_layout
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
                self.classes.add( self.class_name )  # add the found class to the list of classes of this module
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
                        if which_block in ("__do_layout","__set_properties"):
                            # probably to be removed
                            check_old_methods.append( len(out_lines) )
                        out_lines.append( '<%swxGlade replace %s %s>' % (self.nonce, which_class, which_block) )
                else:
                    result = self.rec_event_handler.match(line)
                    if not inside_triple_quote and result:
                        which_handler = result.group('handler')
                        which_class = self.format_classname(result.group('class'))
                        self.event_handlers.setdefault( which_class, set() ).add( which_handler )
                    if self.class_name and self.is_end_of_class(line):
                        # add extra event handlers here...
                        out_lines.append( '<%swxGlade event_handlers %s>' % (self.nonce, self.class_name) )
                    out_lines.append(line)
            else:
                # ignore all the lines inside a wxGlade block
                if self.rec_block_end.match(line):
                    inside_block = False
        if not self.new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not contain any class, so we must add the
            # new_classes tag at the end of the file
            out_lines.append('<%swxGlade insert new_classes>' % self.nonce)

        # when moving from 0.9 to 1.0: remove empty methods "do_layout" and "set_properties"
        while check_old_methods:
            i = check_old_methods.pop(-1)
            if out_lines[i+1].strip()==')':  # just end of block -> remove incl. trailing empty lines
                self._remove_method(out_lines, i-1, i+1)

        # set the ``persistent'' content of the file
        self.content = out_lines


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

    indent_level_func_body = 2

    name_ctor = '__init__'

    shebang = '#!/usr/bin/env lisp\n;;;\n'

    SourceFileContent = SourceFileContent

    tmpl_sizeritem = '(wxSizer_AddWindow (%s obj) (%s obj) %s %s %s nil)\n'  # will be overwritten and restored

    tmpl_cfunc_end = '%(tab)s)\n'

    tmpl_class_end = '\n%(comment)s end of class %(klass)s\n\n\n'
    tmpl_class_end_nomarker = '\n\n\n'

    tmpl_func_event_stub = """\
(defun %(handler)s (function data event) ;;; wxGlade: %(klass)s.<event_handler>
%(tab)s(print "Event handler '%(handler)s' not implemented!")
%(tab)s(when event
%(tab)s%(tab)s(wxEvent:wxEvent_Skip event)))
"""

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
                ret = ret.replace(";;; end of class %(klass)s\n", "")
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

        self.dependencies = {'(use-package :wxCL)', '(use-package :wxFrame)', '(use-package :wx_main)',
            '(use-package :wx_wrapper)', '(use-package :wxWindow)',
            '(use-package :wxColour)', '(use-package :wxEvtHandler)', '(use-package :wxEvent)'}

    def check_values(self):
        error = BaseLangCodeWriter.check_values(self)
        if not error and self.for_version > (2, 8):
            return "Generating Lisp code is only supported for wx version 2.8"
        return None

    def add_app(self, app, top_win_class):
        top_win = app.top_window
        # do nothing if there is no top window
        if not top_win: return

        # add language specific mappings
        self.lang_mapping = { 'top_win': self._format_name(top_win), }
        BaseLangCodeWriter.add_app(self, app, top_win_class)

    def add_object(self, parent_klass, parent, parent_builder, obj):
        # get the widget builder instance

        # the lisp code gen add some hard coded depedencies
        # TODO: Move the hard coded dependencies to the widgets resp. sizers

        builder = self._get_object_builder(parent_klass, obj)
        if not builder: return None

        if obj.IS_NAMED:
            self.class_lines.append( self._format_name(obj.name) )
        if obj.WX_CLASS in ("wxBoxSizer", "wxStaticBoxSizer", "wxGridSizer", "wxFlexGridSizer"):
            self.dependencies.add( '(use-package :wxSizer)' )
        else:
            if obj.WX_CLASS not in ("spacer", "sizerslot"):
                self.dependencies.add( '(use-package :%s)'%obj.get_instantiation_class() )

        if obj.WX_CLASS == "wxMenuBar":
            self.dependencies.add( '(use-package :wxMenu)' )

        return BaseLangCodeWriter.add_object(self, parent_klass, parent, parent_builder, obj)

    def generate_code_background(self, obj):
        self.dependencies.add( '(use-package :wxColour)' )
        return BaseLangCodeWriter.generate_code_background(self, obj)

    def generate_code_ctor(self, code_obj, is_new, tab):
        code_lines = []
        write = code_lines.append

        builder = self.obj_builders[code_obj.WX_CLASS]
        mycn = getattr(builder, 'cn', self.cn)
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        # custom base classes support
        custom_base = code_obj.check_prop_nodefault('custom_base') and code_obj.custom_base.strip() or None

        # generate constructor code
        if is_new:
            base = mycn(code_obj.WX_CLASS)
            write('\n(defclass %s()\n' % code_obj.klass)
            write(tab + "((top-window :initform nil :accessor slot-top-window)")
            for l in self.class_lines:
                write("\n")
                write(tab + "(" + l + " :initform nil :accessor slot-" + l + ")")
            write("))\n")

            write("\n(defun make-%s ()\n" % code_obj.klass)
            write(tab + "(let ((obj (make-instance '%s)))\n" % code_obj.klass)
            write(tab + "  (init obj)\n")
            write(tab + "  (set-properties obj)\n")
            write(tab + "  (do-layout obj)\n")
            write(tab + "  obj))\n")

            write('\n(defmethod init ((obj %s))\n' % code_obj.klass)
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

        # the optional initial code from the code properties
        if not self.preview and code_obj.check_prop("extracode_pre"):
            for l in code_obj.properties["extracode_pre"].get_lines():
                write(tab + l)

        style_p = code_obj.properties.get("style")
        if style_p and style_p.value_set != style_p.default_value:
            m_style = mycn_f( style_p.get_string_value() )
            if m_style:
                stmt_style = self._format_style(m_style, code_obj)
                write(stmt_style % {'style': m_style, 'tab': tab} )

        # set size here to avoid problems with splitter windows
        if code_obj.check_prop('size'):
            write( tab + self.generate_code_size(code_obj) )
        if code_obj.check_prop('min_size'):
            write( tab + self.generate_code_size(code_obj, code_obj.min_size, "SetMinSize") )

        for l in builder.get_properties_code(code_obj):
            write(tab + l)

        if code_obj.check_prop_truth('extraproperties'):
            for l in self.generate_code_extraproperties(code_obj):
                write(tab + l)

        # the initial and final code for the contained elements
        for l in self.classes[code_obj].init:
            write(tab + l)
        if self.classes[code_obj].final:
            write(tab + "\n")
            for l in self.classes[code_obj].final:
                write(tab + l)

        # now check if there is initial and final code for the element itself
        for l in builder.get_init_code(code_obj):
            write(tab+l)

        for l in builder.get_layout_code(code_obj):
            write(tab + l)

        # the optional final code from the code properties
        if not self.preview and code_obj.check_prop("extracode_post"):
            for l in code_obj.properties["extracode_post"].get_lines():
                write(tab + l)

        return code_lines

    def generate_code_event_bind(self, code_obj, tab, event_handlers):
        code_lines = []
        write = code_lines.append

        if event_handlers:
            write('\n')

        for obj, event, handler, unused in event_handlers:
            win_id = self._format_name('obj.%s' % obj.name if not obj.IS_CLASS else 'obj')

            write( "%(tab)s(wxEvtHandler_Connect (slot-top-window obj) %(win_id)s (exp%(event)s)\n%(tab2)s" \
                   "(wxClosure_Create #'%(handler)s obj))\n" % 
                    {'tab':tab, 'tab2':self.tabs(2), 'win_id':win_id, 'event':self.cn(event), 'handler':handler} )

        return code_lines

    def generate_code_font(self, obj, prop_name="font", method=None):
        self.dependencies.add( '(use-package :wxFont)' )
        return BaseLangCodeWriter.generate_code_font(self, obj, prop_name, method)

    def generate_code_foreground(self, obj):
        self.dependencies.add( '(use-package :wxColour)' )
        return BaseLangCodeWriter.generate_code_foreground(self, obj)

    def generate_code_id(self, obj, id=None):
        if id is None:
            id = obj.window_id
        if not id:
            if obj is not None and obj.check_prop_truth("stockitem"):
                return '', self.cn("wxID_" + obj.stockitem)
            return '', self.cn('wxID_ANY')
        tokens = id.split('=', 1)
        if len(tokens) != 2:
            return '', self.cn(tokens[0])   # we assume name is declared elsewhere
        name, val = tokens
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

    def generate_code_size(self, obj, size=None, method=None):
        objname = self.format_generic_access(obj)
        if size is None:
            size = obj.properties["size"].get_string_value()
        use_dialog_units = (size[-1] == 'd')
        if method is None:
            method = 'SetMinSize'  if obj.parent_window else  'wxWindow_SetSize'

        if use_dialog_units:
            return '(%s %s(%s(%s (%s))))\n' % ( method, objname, self.cn('wxDLG_SZE'), objname, size[:-1] )
        return '%s.%s((%s))\n' % (objname, method, size)

    def _quote_str(self, s):
        if self._use_gettext:
            return '(_"%s")' % s
        else:
            return '"%s"' % s

    def _format_classattr(self, obj):
        res = BaseLangCodeWriter._format_classattr(self, obj)
        if not res:
            return res
        elif obj.name.startswith('slot-'):
            return obj.name
        # spacer.name is "<width>, <height>" already, but wxLisp expect
        # a tuple instead of two single values
        elif obj.WX_CLASS in ('spacer','sizerslot'):
            return '(%s)' % obj.name
        # wxList use class attributes always (unfortunately)
#        elif self.store_as_attr(obj):
#            return "slot-%s" % self._format_name(obj.name)
#        return self._format_name(obj.name)
        return 'slot-%s' % self._format_name(obj.name)

    def _format_import(self, klass):
        return '(require "%s")\n' % klass

    def _format_style(self, style, code_obj):
        builder = self.obj_builders[code_obj.WX_CLASS]
        mycn_f = getattr(builder, 'cn_f', self.cn_f)

        if not style: return ''
        style = mycn_f(style)
        if not style: return ''
        style = style.strip().replace('.', '')

        if code_obj.WX_CLASS == "wxFrame":
            stmt = '%%(tab)s(setf (slot-top-window obj) (wxFrame_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n' % style
        elif code_obj.WX_CLASS == "wxDialog":
            stmt = '%%(tab)s(setf (slot-top-window obj) (wxDialog_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n' % style
            self.dependencies.add( '(use-package :wxButton)' )
        else:
            stmt = ''

        return stmt

    def _get_class_filename(self, klass):
        return os.path.join( self.out_dir, klass.replace('.', '_') + '.lisp' )

    def format_generic_access(self, obj):
        if obj.IS_CLASS:
            return '(slot-top-window obj)'
        obj_name = self._format_name(obj.name)
        if self.store_as_attr(obj):
            return '(slot-%s obj)' % obj_name
        return obj_name

    def _format_name(self, name):
        return name.replace('_', '-')


writer = LispCodeWriter()  # The code writer is an instance of LispCodeWriter.

language = writer.language  # Language generated by this code generator
