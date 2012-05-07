# lisp_codegen.py: lisp code generator
#
# Copyright (c) 2005 Surendra K Singhi <efuzzyone@users.sourceforge.net>
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
import random
import re
import sys

import common
from codegen import BaseCodeWriter,  \
                    BaseSourceFileContent, \
                    BaseWidgetHandler


class SourceFileContent(BaseSourceFileContent):

    rec_block_start = re.compile(
        r'^(?P<spaces>\s*)'                     # leading spaces
        r';;;\s*'                               # comment sign
        r'begin\s+wxGlade:\s*'                  # "begin wxGlade:" statement and tailing spaces
        r'(?P<classname>[a-zA-Z_]+\w*)??'       # class or function name (non-greedy)
        r'[.]?'                                 # separator between class and function / block (non-gready)
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
                    quote_index, tmp_quote_str = min(
                        (triple_squote_index, "'''"),
                        (triple_dquote_index, '"""'))

            if not inside_triple_quote and quote_index != -1:
                inside_triple_quote = True
                triple_quote_str = tmp_quote_str
            if inside_triple_quote:
                end_index = line.rfind(triple_quote_str)
                if quote_index < end_index and end_index != -1:
                    inside_triple_quote = False

            result = self.rec_class_decl.match(line)
            if not inside_triple_quote and result:
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
                if not inside_triple_quote and result:
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
                    if not inside_triple_quote and result:
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
        return line.startswith('(use-package :wx')

# end of class SourceFileContent


class WidgetHandler(BaseWidgetHandler):
    pass

# end of class WidgetHandler


class LispCodeWriter(BaseCodeWriter):
    """\
    Code writer class for writing Lisp code out of the designed GUI elements

    @see: L{BaseCodeWriter}
    """

    default_extensions = ['lisp']
    language = "lisp"

    code_statements = {
        'disabled':        "(wxWindow_IsEnabled %(objname)s0)\n",
        'extraproperties': "(%(klass)s_Set%(propname)s (slot-%(objname)s obj) %(value)s)\n",
        'focused':         "(wxWindow_SetFocus %(objname)s)\n",
        'hidden':          "(wxWindow_Hide %(objname)s)\n",
        'tooltip':         "(wxWindow_SetToolTip %(objname)s%(tooltip)s)\n",
        }

    comment_sign = ';;;'

    global_property_writers = {
        'font':            BaseCodeWriter.FontPropertyHandler,
        'events':          BaseCodeWriter.EventsPropertyHandler,
        'extraproperties': BaseCodeWriter.ExtraPropertiesPropertyHandler,
        }

    shebang = '#!/usr/bin/env lisp\n'

    _quote_str_pattern = re.compile(r'\\[natbv"]?')

    def cn(self, name):
        """\
        Return the name properly formatted for the selected name space.
        """
        if name[:2] == 'wx':
            return 'wx' + name[2:]
        elif name[:4] == 'EVT_':
            return 'wx' + name
        return name

    def cn_f(self, flags):
        """\
        Return the flags properly formatted for the selected name space.
        """
        return "|".join([self.cn(f) for f in str(flags).split('|')])

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
                self.output_file.write('<%swxGlade replace dependencies>\n' % self.nonce)
                self.output_file.write('<%swxGlade replace extracode>\n' % self.nonce)

    def finalize(self):
        if self.previous_source:
            # insert all the new custom classes inside the old file
            tag = '<%swxGlade insert new_classes>' % self.nonce
            if self.previous_source.new_classes:
                code = "".join(self.previous_source.new_classes)
            else:
                code = ""
            self.previous_source.content = self.previous_source.content.replace(tag, code)
            tag = '<%swxGlade extra_modules>\n' % self.nonce
            code = "".join(self._current_extra_modules.keys())
            self.previous_source.content = self.previous_source.content.replace(tag, code)

            # module dependecies of all classes
            tag = '<%swxGlade replace dependencies>' % self.nonce
            dep_list = self.dependencies.keys()
            dep_list.sort()
            code = self._tagcontent('dependencies', dep_list)
            self.previous_source.content = \
                self.previous_source.content.replace(tag, code)

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace extracode>' % self.nonce
            code = self._tagcontent(
                'extracode',
                self._current_extra_code
                )
            self.previous_source.content = \
                self.previous_source.content.replace(tag, code)

            # now remove all the remaining <123415wxGlade ...> tags from the
            # source: this may happen if we're not generating multiple files,
            # and one of the container class names is changed
            self.previous_source.content = self._content_notfound(
                self.previous_source.content,
                self.previous_source.spaces.get(tag[1], self.tabs(2)),
                )

            tags = re.findall(
                '<%swxGlade event_handlers \w+>' % self.nonce,
                self.previous_source.content
                )
            for tag in tags:
                self.previous_source.content = self.previous_source.content.replace(tag, "")

            # write the new file contents to disk
            self.save_file(
                self.previous_source.name,
                self.previous_source.content,
                content_only=True
                )

        elif not self.multiple_files:
            em = "".join(self._current_extra_modules.keys())
            content = self.output_file.getvalue().replace(
                '<%swxGlade extra_modules>\n' % self.nonce, em)

            # module dependecies of all classes
            tag = '<%swxGlade replace dependencies>' % self.nonce
            dep_list = self.dependencies.keys()
            dep_list.sort()
            code = self._tagcontent('dependencies', dep_list)
            content = content.replace(tag, code)

            # extra code (see the 'extracode' property of top-level widgets)
            tag = '<%swxGlade replace extracode>' % self.nonce
            code = self._tagcontent('extracode', self._current_extra_code)
            content = content.replace(tag, code)

            self.output_file.close()
            self.save_file(self.output_file_name, content, self._app_added)
            del self.output_file

    def setup(self):
        # scan widgets.txt for widgets, load lisp_codegen's
        widgets_file = os.path.join(common.widgets_path, 'widgets.txt')
        if not os.path.isfile(widgets_file):
            self.warning("widgets file (%s) doesn't exist" % widgets_file)
            return

        sys.path.append(common.widgets_path)
        modules = open(widgets_file)
        for line in modules:
            module_name = line.strip()
            if not module_name or module_name.startswith('#'):
                continue
            module_name = module_name.split('#')[0].strip()
            try:
                m = __import__(module_name + '.lisp_codegen', {}, {},
                               ['initialize'])
                m.initialize()
            except (ImportError, AttributeError):
                pass
##                print 'ERROR loading "%s"' % module_name
##                import traceback;
##                traceback.print_exc()
##          else:
##              print 'initialized lisp generator for ', module_name
        modules.close()

        # ...then, the sizers
        import edit_sizers.lisp_sizers_codegen
        edit_sizers.lisp_sizers_codegen.initialize()

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
        if not top_win:
            return  # do nothing if there is no top window
        lines = []
        append = lines.append
        if klass:
            tab = self.tabs(2)
            append('(defun init-func (fun data evt)\n')
        else:
            tab = self.tabs(1)
            append('(defun init-func (fun data evt)\n')
            if self._use_gettext:
                append(tab + '(setf (textdomain) "%s") ;; replace with the '
                             'appropriate catalog name\n' % name)
                append(tab + '(defun _ (msgid) (gettext msgid "%s"))\n\n' % name)

        top_win = top_win.replace('_', '-')
        append(tab + '(let ((%s (make-%s)))\n' % (top_win, top_win_class))
        if klass:
            append(tab + '(ELJApp_SetTopWindow (slot-top-window %s))\n' % top_win)
            append(tab + '(wxWindow_Show (slot-top-window %s))))\n' % top_win)
            append(';;; end of class %s\n\n' % klass)
            tab = self.tabs(1)
            if self._use_gettext:
                append(tab + '(setf (textdomain) "%s") ;; replace with the '
                             'appropriate catalog name\n' % name)
                append(tab + '(defun _ (msgid) (gettext msgid "%s"))\n\n' % name)
        else:
            append(tab + '(ELJApp_SetTopWindow (slot-top-window %s))\n' % top_win)
            append(tab + '(wxWindow_Show (slot-top-window %s))))\n' % top_win)

        append("\n(unwind-protect\n")
        append(tab + """(Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)\n""")
        append(tab + """(ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))\n""")
        if self.multiple_files:
            filename = os.path.join(self.out_dir, name + '.lisp')
            out = cStringIO.StringIO()
            write = out.write
            # write overwrite warning to standalone app file
            write(self.tmpl_overwrite % {'comment_sign': self.comment_sign,})
            # write the common lines
            for line in self.header_lines:
                write(line)
            # import the top window module
            write('(require "%s")\n\n' % top_win_class)
            # write the wxApp code
            for line in lines:
                write(line)
            self.save_file(filename, out.getvalue(), True)
            out.close()
        else:
            write = self.output_file.write
            for line in lines:
                write(line)

    def add_class(self, code_obj):
        if self.multiple_files:
            # let's see if the file to generate exists, and in this case
            # create a SourceFileContent instance
            filename = os.path.join(self.out_dir,
                                    code_obj.klass.replace('.', '_') + '.lisp')
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

        tab = indentation
        if is_new:
            base = mycn(code_obj.base)
            if code_obj.preview and code_obj.klass == base:
                klass = code_obj.klass + \
                    ('_%d' % random.randrange(10 ** 8, 10 ** 9))
            else:
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
        # __init__ begin tag
        write(indentation + ';;; begin wxGlade: %s.__init__\n' % code_obj.klass)
        prop = code_obj.properties
        style = prop.get("style", None)
        if style:
            style = mycn_f(style)

        style = style.strip().replace('.', '')
        style = style.replace('|', ' ')
        if style.find(' ') != -1:
            style = '(logior %s)' % style

        if code_obj.base == "wxFrame":
            write(indentation + "(setf (slot-top-window obj) (wxFrame_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n" % style)
        else:
            if code_obj.base == "wxDialog":
                write(indentation + "(setf (slot-top-window obj) (wxDialog_create nil wxID_ANY \"\" -1 -1 -1 -1 %s))\n" % style)
                self.dependencies['(use-package :wxButton)'] = 1

        self.init_lines = self.classes[code_obj.klass].init
        parents_init = self.classes[code_obj.klass].parents_init
        parents_init.reverse()
        for l in parents_init:
            write(tab + l)
        for l in self.init_lines:
            write(tab + l)

        # now check if there are extra lines to add to the init method
        if hasattr(builder, 'get_init_code'):
            for l in builder.get_init_code(code_obj):
                write(tab + l)

        # now let's write the "event table"...
        event_handlers = self.classes[code_obj.klass].event_handlers
        if hasattr(builder, 'get_events'):
            for id, event, handler in builder.get_events(code_obj):
                event_handlers.append((id, mycn(event), handler))
        if event_handlers:
            write('\n')
        for win_id, event, handler in event_handlers:
            if win_id.startswith('#'):
                write(
                    tab + \
                    "(wxEvtHandler_Connect (slot-top-window obj) %s (exp%s)" % (win_id[1:], event) + \
                    "\n" + self.tabs(2) + \
                    "(wxClosure_Create #'%s obj))\n" % handler
                    )
            else:
                write(tab + \
                      "(wxEvtHandler_Connect (slot-top-window obj) %s (exp%s)" % (win_id, event) + \
                      "\n" + self.tabs(2) + \
                      "(wxClosure_Create #'%s obj))\n" % handler
                      )
        # end tag
        write(tab + ')\n')
        write(tab + ';;; end wxGlade\n')
        if prev_src and not is_new:
            # replace the lines inside the ctor wxGlade block with the new ones
            tag = '<%swxGlade replace %s %s>' % (self.nonce, code_obj.klass,
                                                 '__init__')
            if prev_src.content.find(tag) < 0:
                # no __init__ tag found, issue a warning and do nothing
                self.warning(
                    "wxGlade __init__ block not found for %s, __init__ code "
                    "NOT generated" % code_obj.name
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
            write('\n(defmethod set-properties ((obj %s))\n' % klass)
        # begin tag
        write(tab + ';;; begin wxGlade: %s.__set_properties\n' % code_obj.klass)
        if not write_body:
            write(tab + 'pass\n')
        else:
            for l in obj_p:
                write(tab + l)
        # end tag
        write(tab + ')\n')
        write(tab + ';;; end wxGlade\n')
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
            write('\n' + '(defmethod do-layout ((obj %s))\n' % klass)
        layout_lines = self.classes[code_obj.klass].layout
        sizers_init_lines = self.classes[code_obj.klass].sizers_init

        # check if there are extra layout lines to add
        if hasattr(builder, 'get_layout_code'):
            extra_layout_lines = builder.get_layout_code(code_obj)
        else:
            extra_layout_lines = []

        # begin tag
        write(tab + ';;; begin wxGlade: %s.__do_layout\n' % code_obj.klass)
        if layout_lines or sizers_init_lines or extra_layout_lines:
            sizers_init_lines.reverse()
            for l in sizers_init_lines:
                write(tab + l)
            for l in layout_lines:
                write(tab + l)
            for l in extra_layout_lines:
                write(tab + l)
        else:
            write(tab + 'pass\n')
        # end tag
        write(tab + ')\n')
        write(tab + ';;; end wxGlade\n')
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
                    buf.append('(defun %s (function data event) '
                               ';;; wxGlade: %s.<event_handler>\n'
                               % (handler, code_obj.klass))
                    buf.append(tab +
                        '(print "Event handler `%s\' not implemented")\n' % \
                        handler
                        )
                    buf.append(tab + '(when event\n')
                    buf.append(tab + '(wxEvent:wxEvent_Skip event)))\n')
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
                    write('\n' + '(defun %s (function data event) '
                          ';;; wxGlade: %s.<event_handler>\n'
                          % (handler, code_obj.klass))
                    write(tab + '(print "Event handler `%s\' not implemented!")\n' %
                          handler)
                    write(tab + '(when event\n')
                    write(tab + tab + '(wxEvent:wxEvent_Skip event)))\n')
                    already_there[handler] = 1

        # the code has been generated
        self.classes[code_obj.klass].done = True

        write('\n;;; end of class %s\n\n\n' % code_obj.klass)

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
                dep_list = self.dependencies.keys()
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
                                    code_obj.klass + '.lisp')
            out = cStringIO.StringIO()
            write = out.write
            # write the common lines
            for line in self.header_lines:
                write(line)

            # write the module dependecies for this class
            dep_list = self.dependencies.keys()
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
            # store the contents to filename
            self.save_file(filename, out.getvalue())
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
        else:
            try:
                sub_obj.name = sub_obj.name.replace('_', '-')
                sub_obj.parent.name = sub_obj.parent.name.replace('_', '-')
                if(sub_obj.name != "spacer"):
                    self.class_lines.append(sub_obj.name)
                if (sub_obj.klass == "wxBoxSizer" or \
                    sub_obj.klass == "wxStaticBoxSizer" or \
                    sub_obj.klass == "wxGridSizer" or \
                    sub_obj.klass == "wxFlexGridSizer"):
                    self.dependencies['(use-package :wxSizer)'] = 1
                else:
                    if (sub_obj.klass != "spacer"):
                        key = '(use-package :%s)' % sub_obj.klass
                        self.dependencies[key] = 1

                if (sub_obj.klass == "wxMenuBar"):
                    self.dependencies['(use-package :wxMenu)'] = 1
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

                # Add a dependency of the current object on its parent
                klass.deps.append((sub_obj, sub_obj.parent))
                klass.child_order.append(sub_obj)
                klass.init_lines[sub_obj] = init

                mycn = getattr(builder, 'cn', self.cn)
                if hasattr(builder, 'get_events'):
                    evts = builder.get_events(sub_obj)
                    for id, event, handler in evts:
                        klass.event_handlers.append((id, mycn(event), handler))
                elif 'events' in sub_obj.properties:
                    id_name, id = self.generate_code_id(sub_obj)
                    if id == '-1' or id == self.cn('wxID_ANY'):
                        id = '#obj.%s' % sub_obj.name
                    for event, handler in sub_obj.properties['events'].iteritems():
                        klass.event_handlers.append((id, mycn(event), handler))

                # try to see if there's some extra code to add to this class
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
                klass.sizers_init.extend(init)

            klass.props.extend(props)
            klass.layout.extend(layout)
            if self.multiple_files and \
                   (sub_obj.is_toplevel and sub_obj.base != sub_obj.klass):
                key = '(require "%s")\n' % sub_obj.klass
                klass.dependencies[key] = 1
            for dep in getattr(self.obj_builders.get(sub_obj.base),
                               'import_modules', []):
                klass.dependencies[dep] = 1

    def add_sizeritem(self, toplevel, sizer, obj, option, flag, border):
        # an ugly hack to allow the addition of spacers: if obj_name can be parsed
        # as a couple of integers, it is the size of the spacer to add

        sizer.name = sizer.name.replace('_', '-')
        obj_name = obj.name
        try:
            w, h = [int(s) for s in obj_name.split(',')]
        except ValueError:
            pass
        else:
            obj_name = '(%d, %d)' % (w, h)  # it was the dimension of a spacer
        if toplevel.klass in self.classes:
            klass = self.classes[toplevel.klass]
        else:
            klass = self.classes[toplevel.klass] = self.ClassLines()

        flag = '%s' % self.cn_f(flag)
        flag = flag.strip().replace('|', ' ')
        if flag.find(' ') != -1:
            flag = '(logior %s)' % flag

        if (obj.klass == "wxBoxSizer" or obj.klass == "wxStaticBoxSizer"
            or obj.klass == "wxGridSizer" or obj.klass == "wxFlexGridSizer"):
            buffer = '(wxSizer_AddSizer (slot-%s  obj) (slot-%s obj) %s %s %s nil)\n' % \
                     (sizer.name, obj_name, option, flag, self.cn_f(border))
        else:
            buffer = '(wxSizer_AddWindow (slot-%s obj) (slot-%s obj) %s %s %s nil)\n' % \
                     (sizer.name, obj_name, option, flag, self.cn_f(border))
        klass.layout.append(buffer)

    def generate_code_background(self, obj):
        objname = self._get_code_name(obj)
        try:
            color = ('(wxColour_CreateRGB ') + '%s)' % \
                    self._string_to_colour(obj.properties['background'])
        except (IndexError, ValueError):  # the color is from system settings
            color = self.cn('(wxSystemSettings_GetColour ') + '%s)' % \
                    self.cn(obj.properties['background'])
        self.dependencies['(use-package :wxColour)'] = 1
        return  '(wxWindow_SetBackgroundColour %s %s)\n' % (objname, color)

    def generate_code_font(self, obj):
        font = obj.properties['font']
        size = font['size']
        family = self.cn(font['family'])
        underlined = font['underlined']
        style = self.cn(font['style'])
        weight = self.cn(font['weight'])
        face = '"%s"' % font['face'].replace('"', r'\"')
        name = self._get_code_name(obj)
        self.dependencies['(use-package :wxFont)'] = 1
        return ('(wxWindow_SetFont %s (wxFont_Create %s %s %s %s %s %s wxFONTENCODING_DEFAULT))\n' %
                (name, size, family, style, weight, underlined, face))

    def generate_code_foreground(self, obj):
        objname = self._get_code_name(obj)
        try:
            color = self.cn('(wxColour_CreateRGB ') + '%s)' % \
                    self._string_to_colour(obj.properties['foreground'])
        except (IndexError, ValueError):  # the color is from system settings
            color = self.cn('(wxSystemSettings_GetColour ') + '%s)' % \
                    self.cn(obj.properties['foreground'])
        self.dependencies['(use-package :wxColour)'] = 1
        return objname + '(wxWindow_SetForegroundColour %s %s)\n' % (objname, color)

    def generate_code_id(self, obj, id=None):
        if not id:
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
        if '.' in name:
            return ('%s = %s\n' % (name, val), name)
        return ('global %s; %s = %s\n' % (name, name, val), name)

    def generate_code_size(self, obj):
        objname = self._get_code_name(obj)
        size = obj.properties.get('size', '').strip()
        use_dialog_units = (size[-1] == 'd')
        if not obj.parent:
            method = 'wxWindow_SetSize'
        else:
            method = 'SetMinSize'
        if use_dialog_units:
            return "(" + method + ' ' + objname + '(' + self.cn('wxDLG_SZE') + \
                   '(%s (%s)))\n' % (objname, size[:-1])
        else:
            return '%s.%s((%s))\n' % (objname, method, size)

    def quote_str(self, s, translate=True, escape_chars=True):
        if not s:
            return '""'
        s = s.replace('"', r'\"')
        if escape_chars:
            s = self._quote_str_pattern.sub(self._do_replace, s)
        else:
            s = s.replace('\\', r'\\')  # just quote the backslashes
        if self._use_gettext and translate:
            return '(_"%s")' % s
        else:
            return '"%s"' % s

    def _get_code_name(self, obj):
        """\
        Returns the name of the variable ( either "(slot-top-window obj)" or
        "(slot-%s obj)"
        """
        if obj.is_toplevel:
            return '(slot-top-window obj)'
        else:
            if self.test_attribute(obj):
                return '(slot-%s obj)' % obj.name
            else:
                return obj.name

# end of class LispCodeWriter

writer = LispCodeWriter()
"""\
The code writer is an instance of L{LispCodeWriter}.
"""

language = writer.language
"""\
Language generated by this code generator
"""
