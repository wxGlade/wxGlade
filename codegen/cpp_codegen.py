# cpp_codegen.py: C++ code generator
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import sys, os, os.path
import common
from xml_parse import XmlParsingError

# these two globals must be defined for every code generator module
language = 'C++'
writer = sys.modules[__name__] # the writer is the module itself

# default extensions for generated files: a list of file extensions
default_extensions = ['h', 'cpp']

"""\
dictionary that maps the lines of code of a class to the name of such class:
the lines are divided in 3 categories: lines in the constructor,
'set_properties' and 'do_layout'
"""
classes = None

"""\
dictionary of ``writers'' for the various objects
NOTE: These are different from those of the Python generator, because they
return 4 lists: init, ids, properties, layout (see ClassLines)
ids is the list of the window ids declared, and must be an expression that
is valid inside an enum, as:
MY_ID
MY_ID = 100
but NOT: MY_ID = wxNewId()
Note that there is no trailing ';'
"""
obj_builders = {}

"""\
dictionary of 'constructor signatures' for the various widgets: this is a
list of 2-tuples (type, name) (or 3-tuples (type, name, default) for the
constructor's args. if, when adding a custom class, the signature for the
constructor is not fond, here, the one of wxWindow is used
"""
obj_constructors = {}

"""\
dictionary of ``property writer'' functions, used to set the properties of a
toplevel object
"""
obj_properties = {}

# random number used to be sure that the replaced tags in the sources are
# the right ones (see SourceFileContent and add_class)
nonce = None

# lines common to all the generated files (include of <wx/wx.h>, ...)
header_lines = []

# if True, generate a file for each custom class
multiple_files = False

# if not None, they are the header and source file to write into
output_header, output_source = None, None
# if not None, it is the directory inside which the output files are saved
out_dir = None


class ClassLines:
    """\
    Stores the lines of python code for a custom class
    """
    def __init__(self):
        self.init = [] # lines of code to insert in the constructor
        self.parents_init = [] # lines of code to insert in the constructor for
                               # container widgets (panels, splitters, ...)
        self.ids = [] # ids declared in the source (to use for Event handling):
                      # these are grouped together into a public enum in
                      # the custom class
        self.sizers_init = [] # lines related to sizer objects declarations
        self.props = [] # lines to insert in the __set_properties method
        self.layout = [] # lines to insert in the __do_layout method
        self.sub_objs = [] # list of 2-tuples (type, name) of the
                           # sub-objects which are attributes of the
                           # toplevel object
        
        self.dependencies = [] # names of the modules this class depends on
        self.done = False # if True, the code for this class has already
                          # been generated

# end of class ClassLines


class SourceFileContent:
    """\
    Keeps info about an existing file that has to be updated, to replace only
    the lines inside a wxGlade block, an to keep the rest of the file as it was
    """
    def __init__(self, name):
        self.name = name # name of the file without extension
                         # (both header and .cpp)
        self.header_content = None # content of the header file
        self.source_content = None
        self.classes = {} # classes declared in the file
        self.new_classes = [] # new classes to add to the file (they are
                              # inserted BEFORE the old ones)
        if classes is None: self.classes = {}
        self.build_untouched_content()

    def build_untouched_content(self):
        """\
        Builds a string with the contents of the file that must be left as is,
        and replaces the wxGlade blocks with tags that in turn will be replaced
        by the new wxGlade blocks
        """
        self._build_untouched(self.name + '.h', True)
        self._build_untouched(self.name + '.cpp', False)

    def _build_untouched(self, filename, is_header):
        import re
        class_name = None
        new_classes_inserted = False
        # regexp to match class declarations (this isn't very accurate -
        # doesn't match template classes, nor virtual inheritance, but
        # should be enough for most cases)
        class_decl = re.compile(r'^\s*class\s+([a-zA-Z_]\w*)\s*'
                                '(:\s*(public|protected|private)?\s+\w+'
                                '(,\s*(public|protected|private)?\s+\w+)*'
                                ')?')
        # regexps to match wxGlade blocks
        block_start = re.compile(r'^\s*//\s*begin\s+wxGlade:\s*'
                                 '(\w*)::(\w+)\s*$')
        block_end = re.compile(r'^\s*//\s*end\s+wxGlade\s*$')
        inside_block = False
        inside_comment = False
        tmp_in = open(filename)
        out_lines = []
        for line in tmp_in:
            comment_index = line.find('/*')
            if not inside_comment and comment_index != -1:
                inside_comment = True
            if inside_comment:
                end_index = line.find('*/')
                if end_index > comment_index: inside_comment = False
            if not is_header: result = None
            else: result = class_decl.match(line)
            if not inside_comment and result is not None:
                if class_name is None:
                    # this is the first class declared in the file: insert the
                    # new ones before this
                    out_lines.append('<%swxGlade insert new_classes>' %
                                     nonce)
                    new_classes_inserted = True
                class_name = result.group(1)
                self.classes[class_name] = 1 # add the found class to the list
                                             # of classes of this module
                out_lines.append(line)
            elif not inside_block:
                result = block_start.match(line)
                if not inside_comment and result is not None:
                    # replace the lines inside a wxGlade block with a tag that
                    # will be used later by add_class
                    inside_block = True
                    out_lines.append('<%swxGlade replace %s %s>' % \
                                     (nonce, result.group(1), result.group(2)))
                else: out_lines.append(line)
            else:
                # ignore all the lines inside a wxGlade block
                if block_end.match(line) is not None:
                    inside_block = False
        if is_header and not new_classes_inserted:
            # if we are here, the previous ``version'' of the file did not
            # contain any class, so we must add the new_classes tag at the
            # end of the file
            out_lines.append('<%swxGlade insert new_classes>' % nonce)
        tmp_in.close()
        # set the ``persistent'' content of the file
        if is_header: self.header_content = "".join(out_lines)
        else: self.source_content = "".join(out_lines)
        
# end of class SourceFileContent

# if not None, it is an instance of SourceFileContent that keeps info about
# the previous version of the source to generate
previous_source = None 


def tabs(number):
    return '    ' * number


def initialize(out_path, multi_files):
    """\
    Writer initialization function.
    - out_path: output path for the generated code (a file (either .h or .cpp)
      if multi_files is False, a dir otherwise)
    - multi_files: if True, generate a separate file for each custom class,
      otherwise generate only one header(.h) and one implementation(.cpp) file
    """
    global classes, header_lines, multiple_files, previous_source, nonce
    import time, random

    # this is to be more sure to replace the right tags
    nonce = '%s%s' % (str(time.time()).replace('.', ''),
                      random.randrange(10**6, 10**7))

    classes = {}
    header_lines = ['// -*- C++ -*- generated by wxGlade %s on %s\n\n' % \
                    (common.version, time.asctime()),
                    '#include <wx/wx.h>\n', '#include <wx/image.h>\n']
    multiple_files = multi_files
    if not multiple_files:
        global output_header, output_source
        name, ext = os.path.splitext(out_path)
        if os.path.isfile(name + '.h'):
            # the file exists, we must keep all the lines not inside a wxGlade
            # block. NOTE: this may cause troubles if out_path is not a valid
            # C++ file, so be careful!
            previous_source = SourceFileContent(name)
        else:
            previous_source = None
            try:
                output_header = open(name + '.h', 'w')
                output_source = open(name + '.cpp', 'w')
            except IOError:
                if locals().has_key('output_header'):
                    output_header.close()
                    ext = '.h'
                else: ext = '.cpp'
                raise XmlParsingError("Error opening '%s%s' file" % \
                                      (name, ext))
            for line in header_lines:
                output_header.write(line)
                output_source.write(line)
            # isolation directives
            oh = os.path.basename(output_header.name).upper().replace('.', '_')
            # extra headers
            for val in _obj_headers.itervalues():
                for header in val:
                    output_header.write('#include %s\n' % header)
            output_header.write('\n#ifndef %s\n#define %s\n' % (oh, oh))
            output_header.write('\n')
            output_source.write('#include "%s%s"\n\n' % \
                                (os.path.basename(name), '.h'))
    else:
        previous_source = None
        global out_dir
        if not os.path.isdir(out_path):
            raise XmlParsingError("'path' must be a directory when generating"\
                                  " multiple output files")
        out_dir = out_path


def finalize():
    """\
    Writer ``finalization'' function: flushes buffers, closes open files, ...
    """
    if previous_source is not None:
        # insert all the new custom classes inside the old file
        tag = '<%swxGlade insert new_classes>' % nonce
        if previous_source.new_classes:
            code = "".join([ c[0] for c in previous_source.new_classes])
        else:
            code = ""
        header_content = previous_source.header_content.replace(tag, code)
        extra_source = "".join([ c[1] for c in previous_source.new_classes])
        source_content = previous_source.source_content
        # now remove all the remaining <123415wxGlade ...> tags from the
        # source: this may happen if we're not generating multiple files,
        # and one of the container class names is changed
        import re
        tags = re.findall('(<%swxGlade replace ([a-zA-Z_]\w*) +(\w+)>)' %
                          nonce, header_content)
        for tag in tags:
            if tag[2] == 'methods':
                comment = '%svoid set_properties();\n%svoid do_layout();\n' \
                          % (tabs(1), tabs(1))
            else:
                comment = '// content of this block (%s) not found: ' \
                          'did you rename this class?\n' % tag[2]
            header_content = header_content.replace(tag[0], comment)
        tags = re.findall('(<%swxGlade replace ([a-zA-Z_]\w*) +(\w+)>)' %
                          nonce, source_content)
        for tag in tags:
            comment = '// content of this block not found: ' \
                      'did you rename this class?\n'
            source_content = source_content.replace(tag[0], comment)
        # write the new file contents to disk
        out = open(previous_source.name + '.h', 'w')
        out.write(header_content)
        out.close()
        out = open(previous_source.name + '.cpp', 'w')
        out.write(source_content)
        out.write('\n\n')
        out.write(extra_source)
        out.close()
    
    elif not multiple_files:
        global output_header, output_source
        oh = os.path.basename(output_header.name).upper().replace('.', '_')
        output_header.write('\n#endif // %s\n' % oh)
        output_header.close()
        output_source.close()


def add_object(top_obj, sub_obj):
    """\
    adds the code to build 'sub_obj' to the class body of 'top_obj'.
    """
    try: klass = classes[top_obj.klass]
    except KeyError: klass = classes[top_obj.klass] = ClassLines()
    try: builder = obj_builders[sub_obj.base]
    except KeyError:
        # no code generator found: write a comment about it
        klass.init.extend(['\n', '// code for %s (type %s) not generated: '
                           'no suitable writer found' % (sub_obj.name,
                                                         sub_obj.klass),'\n'])
    else:
        try:
            init, ids, props, layout = builder(sub_obj)
        except:
            print sub_obj
            raise
        if sub_obj.in_windows: # the object is a wxWindow instance
            # --- patch 2002-08-26 ------------------------------------------
            #init.reverse()
            if sub_obj.is_container and not sub_obj.is_toplevel:
                init.reverse()
                klass.parents_init.extend(init)
            else: klass.init.extend(init)
            # ---------------------------------------------------------------
            #klass.init.extend(init)
            klass.ids.extend(ids)
            if sub_obj.klass != 'spacer':
                klass.sub_objs.append( (sub_obj.klass, sub_obj.name) )
        else: # the object is a sizer
            klass.sizers_init.extend(init)
        klass.props.extend(props)
        klass.layout.extend(layout)
        if sub_obj.is_toplevel and sub_obj.base != sub_obj.klass:
            klass.dependencies.append(sub_obj.klass)
        else:
            headers = _obj_headers.get(sub_obj.base, [])
            klass.dependencies.extend(headers)


def add_sizeritem(toplevel, sizer, obj, option, flag, border):
    """\
    writes the code to add the object 'obj' to the sizer 'sizer'
    in the 'toplevel' object.
    """
    try: klass = classes[toplevel.klass]
    except KeyError: klass = classes[toplevel.klass] = ClassLines()
    name = obj.name
    if obj.base == 'wxNotebook' and not obj.is_toplevel:
        # this is an ugly TEMPORARY HACK! We must find a better way
        name += '_sizer'
    buffer = '%s->Add(%s, %s, %s, %s);\n' % \
             (sizer.name, name, option, flag, border)
    klass.layout.append(buffer)


def add_class(code_obj):
    """\
    Generates the code for a custom class.
    """
    if classes.has_key(code_obj.klass) and classes[code_obj.klass].done:
        return # the code has already been generated

    if not multiple_files:
        # in this case, previous_source is the SourceFileContent instance
        # that keeps info about the single file to generate
        prev_src = previous_source
    else:
        # let's see if the file to generate exists, and in this case
        # create a SourceFileContent instance
        filename = os.path.join(out_dir, code_obj.klass + '.h')
        if not os.path.exists(filename): prev_src = None
        else: prev_src = SourceFileContent(os.path.join(out_dir,
                                                        code_obj.klass))

    if prev_src is not None and prev_src.classes.has_key(code_obj.klass):
        # this class wasn't in the previous version of the source (if any)
        is_new = False 
    else: is_new = True

    header_buffer = []
    source_buffer = []
    hwrite = header_buffer.append
    swrite = source_buffer.append

    if not classes.has_key(code_obj.klass):
        # if the class body was empty, create an empty ClassLines
        classes[code_obj.klass] = ClassLines()

    default_sign = [('wxWindow*', 'parent'), ('int', 'id')]
    sign = obj_constructors.get(code_obj.base, default_sign)
    defaults = []
    for t in sign:
        if len(t) == 3: defaults.append(t[2])
        else: defaults.append(None)
    tmp_sign = [ t[0] + ' ' + t[1] for t in sign ]
    sign_decl2 = ', '.join(tmp_sign)
    for i in range(len(tmp_sign)):
        if defaults[i] is not None:
            tmp_sign[i] += '=%s' % defaults[i]
    sign_decl1 = ', '.join(tmp_sign)
    sign_inst = ', '.join([ t[1] for t in sign])

    if is_new:
        # header file
        hwrite('\nclass %s: public %s {\n' % (code_obj.klass, code_obj.base))
        hwrite('public:\n')
        # the first thing to add it the enum of the various ids
        hwrite(tabs(1) + '// begin wxGlade: %s::ids\n' % code_obj.klass)
        ids = classes[code_obj.klass].ids
        if ids:
            hwrite(tabs(1) + 'enum {\n')
            ids = (',\n' + tabs(2)).join(ids)
            hwrite(tabs(2) + ids)
            hwrite('\n' + tabs(1) + '};\n')
        hwrite(tabs(1) + '// end wxGlade\n\n')
        # constructor prototype
        hwrite(tabs(1) + '%s(%s);\n' % (code_obj.klass, sign_decl1))
        #hwrite('\nprivate:\n')
        # 2002-11-12: changed private to protected to allow "customization
        #             by subclassing" as in QTDesigner
        hwrite('\nprotected:\n')
        # set_properties and do_layout prototypes
        hwrite(tabs(1) + '// begin wxGlade: %s::methods\n' % code_obj.klass)
        hwrite(tabs(1) + 'void set_properties();\n')
        hwrite(tabs(1) + 'void do_layout();\n')
        hwrite(tabs(1) + '// end wxGlade\n')
        # declarations of the attributes
        hwrite('\n')
        hwrite(tabs(1) + '// begin wxGlade: %s::attributes\n' % code_obj.klass)
        for o_type, o_name in classes[code_obj.klass].sub_objs:
            hwrite(tabs(1) + '%s* %s;\n' % (o_type, o_name))
        hwrite(tabs(1) + '// end wxGlade\n')
        hwrite('};\n\n')
    elif prev_src is not None:
        hwrite(tabs(1) + '// begin wxGlade: %s::ids\n' % code_obj.klass)
        ids = classes[code_obj.klass].ids
        if ids:
            hwrite(tabs(1) + 'enum {\n')
            ids = (',\n' + tabs(2)).join(ids)
            hwrite(tabs(2) + ids)
            hwrite('\n' + tabs(1) + '};\n')
        hwrite(tabs(1) + '// end wxGlade\n')
        tag = '<%swxGlade replace %s ids>' % (nonce, code_obj.klass)
        if prev_src.header_content.find(tag) < 0:
            # no ids tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade ids block not found," \
                  " ids declarations code NOT generated"
        else:
            prev_src.header_content = prev_src.header_content.\
                                      replace(tag, "".join(header_buffer))
        header_buffer = [ tabs(1) + '// begin wxGlade: %s::methods\n' % \
                          code_obj.klass,
                          tabs(1) + 'void set_properties();\n',
                          tabs(1) + 'void do_layout();\n',
                          tabs(1) + '// end wxGlade\n' ]
        tag = '<%swxGlade replace %s methods>' % (nonce, code_obj.klass)
        if prev_src.header_content.find(tag) < 0:
            # no methods tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade methods block not found," \
                  " methods declarations code NOT generated"
        else:
            prev_src.header_content = prev_src.header_content.\
                                      replace(tag, "".join(header_buffer))
        header_buffer = []
        hwrite = header_buffer.append
        hwrite(tabs(1) + '// begin wxGlade: %s::attributes\n' % code_obj.klass)
        for o_type, o_name in classes[code_obj.klass].sub_objs:
            hwrite(tabs(1) + '%s* %s;\n' % (o_type, o_name))
        hwrite(tabs(1) + '// end wxGlade\n')
        tag = '<%swxGlade replace %s attributes>' % (nonce, code_obj.klass)
        if prev_src.header_content.find(tag) < 0:
            # no attributes tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade attributes block " \
                  "not found, attributes declarations code NOT generated"
        else:
            prev_src.header_content = prev_src.header_content.\
                                      replace(tag, "".join(header_buffer))
        

    # source file
    # set the window's style
    prop = code_obj.properties
    style = prop.get("style", None)
    if style is not None:
        sign_inst = sign_inst.replace('style', '%s' % style)
    
    # constructor
    if is_new:
        swrite('\n%s::%s(%s):\n%s%s(%s)\n{\n' % (code_obj.klass,
                                                 code_obj.klass,
                                                 sign_decl2, tabs(1),
                                                 code_obj.base, sign_inst))
    swrite(tabs(1) + '// begin wxGlade: %s::%s\n' % (code_obj.klass,
                                                     code_obj.klass))
    tab = tabs(1)
    init_lines = classes[code_obj.klass].init
    # --- patch 2002-08-26 ---------------------------------------------------
    #init_lines.reverse()
    parents_init = classes[code_obj.klass].parents_init
    parents_init.reverse()    
    for l in parents_init: swrite(tab+l)
    # ------------------------------------------------------------------------
    for l in init_lines: swrite(tab + l)
    swrite('\n' + tab + 'set_properties();\n')
    swrite(tab + 'do_layout();\n')
    # end tag
    swrite(tab + '// end wxGlade\n')
    if is_new: swrite('}\n\n')

    if prev_src is not None and not is_new:
        # replace the lines inside the constructor wxGlade block
        # with the new ones
        tag = '<%swxGlade replace %s %s>' % (nonce, code_obj.klass,
                                             code_obj.klass)
        if prev_src.source_content.find(tag) < 0:
            # no constructor tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade %s::%s block not found," \
                  " relative code NOT generated" % (code_obj.klass,
                                                    code_obj.klass)
        else:
            prev_src.source_content = prev_src.source_content.\
                                      replace(tag, "".join(source_buffer))
        source_buffer = []
        swrite = source_buffer.append
    

    # set_properties
    props_builder = obj_properties.get(code_obj.base)
    write_body = len(classes[code_obj.klass].props)
    if props_builder:
        obj_p = obj_properties[code_obj.base](code_obj)
        if not write_body: write_body = len(obj_p)
    else: obj_p = []
    if is_new:
        swrite('\nvoid %s::set_properties()\n{\n' % code_obj.klass)
    swrite(tab + '// begin wxGlade: %s::set_properties\n' % code_obj.klass)
    for l in obj_p: swrite(tab + l)
    for l in classes[code_obj.klass].props: swrite(tab); swrite(l)
    swrite(tab + '// end wxGlade\n')
    if is_new:
        swrite('}\n\n')
    
    if prev_src is not None and not is_new:
        # replace the lines inside the constructor wxGlade block
        # with the new ones
        tag = '<%swxGlade replace %s set_properties>' % (nonce, code_obj.klass)
        if prev_src.source_content.find(tag) < 0:
            # no set_properties tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade %s::set_properties block "\
                  "not found, relative code NOT generated" % (code_obj.klass)
        else:
            prev_src.source_content = prev_src.source_content.\
                                      replace(tag, "".join(source_buffer))
        source_buffer = []
        swrite = source_buffer.append


    # do_layout
    if is_new:
        swrite('\nvoid %s::do_layout()\n{\n' % code_obj.klass)
    layout_lines = classes[code_obj.klass].layout
    sizers_init_lines = classes[code_obj.klass].sizers_init
    swrite(tab + '// begin wxGlade: %s::do_layout\n' % code_obj.klass)
    sizers_init_lines.reverse()
    for l in sizers_init_lines: swrite(tab + l)
    for l in layout_lines: swrite(tab + l)
    swrite(tab + 'Layout();\n')
    swrite(tab + '// end wxGlade\n')
    if is_new:
        swrite('}\n\n')

    if prev_src is not None and not is_new:
        # replace the lines inside the constructor wxGlade block
        # with the new ones
        tag = '<%swxGlade replace %s do_layout>' % (nonce, code_obj.klass)
        if prev_src.source_content.find(tag) < 0:
            # no do_layout tag found, issue a warning and do nothing
            print >> sys.stderr, "WARNING: wxGlade %s::do_layout block "\
                  "not found, relative code NOT generated" % (code_obj.klass)
        else:
            prev_src.source_content = prev_src.source_content.\
                                      replace(tag, "".join(source_buffer))
        source_buffer = []
        swrite = source_buffer.append


    # the code has been generated
    classes[code_obj.klass].done = True

    if not multiple_files and prev_src is not None:
        # if this is a new class, add its code to the new_classes list of the
        # SourceFileContent instance
        if is_new: prev_src.new_classes.append( ("".join(header_buffer),
                                                "".join(source_buffer)) )
        return

    if multiple_files:
        if prev_src is not None:
            tag = '<%swxGlade insert new_classes>' % nonce
            prev_src.header_content = prev_src.header_content.replace(tag, "")
            
            # insert the module dependencies of this class
            extra_modules = classes[code_obj.klass].dependencies
            deps = ['// begin wxGlade: ::dependencies\n']
            for module in _unique(extra_modules):
                if module and ('"' != module[0] != '<'):
                    deps.append('#include "%s.h"\n' % module)
                else:
                    deps.append('#include %s\n' % module)
            deps.append('// end wxGlade\n')
            tag = '<%swxGlade replace dependencies>' % nonce
            prev_src.header_content = prev_src.header_content.\
                                      replace(tag, "".join(deps))
            
            # store the new file contents to disk
            try:
                hout = open(os.path.join(out_dir, code_obj.klass + '.h'), 'w')
                sout = open(os.path.join(out_dir, code_obj.klass + '.cpp'),'w')
            except:
                if locals().has_key('hout'): hout.close()
                raise IOError("cpp_codegen.add_class: %s, %s, %s" % \
                              (out_dir, prev_src.name, code_obj.klass))
            hout.write(prev_src.header_content)
            sout.write(prev_src.source_content)
            hout.close()
            sout.close()
            return

        # create the new source file
        header_file = os.path.join(out_dir, code_obj.klass + '.h')
        source_file = os.path.join(out_dir, code_obj.klass + '.cpp')
        try:
            hout = open(header_file, 'w')
            sout = open(source_file, 'w')
        except:
            if locals().has_key(hout):
                hout.close()
                filename = header_file
            else: filename = source_file
            raise IOError("cpp_codegen.add_class: %s, %s, %s" % \
                          (out_dir, filename, code_obj.klass))
        # header file
        hwrite = hout.write
        # write the common lines
        for line in header_lines: hwrite(line)
        # isolation directives
        hn = os.path.basename(header_file).upper().replace('.', '_')
        hwrite('\n#ifndef %s\n#define %s\n' % (hn, hn))
        # write the module dependecies for this class
        extra_headers = classes[code_obj.klass].dependencies
        hwrite('\n// begin wxGlade: dependencies\n')
        extra_modules = classes[code_obj.klass].dependencies
        for module in _unique(extra_modules):
            if module and ('"' != module[0] != '<'):
                hwrite('#include "%s.h"\n' % module)
            else:
                hwrite('#include %s\n' % module)
        hwrite('// end wxGlade\n')
        hwrite('\n')
        # write the class body
        for line in header_buffer: hwrite(line)
        hwrite('\n#endif // %s\n' % hn)
        hout.close()

        # source file
        swrite = sout.write
        # write the common lines
        for line in header_lines: swrite(line)
        swrite('#include "%s"\n\n' % os.path.basename(header_file))
        # write the class implementation
        for line in source_buffer: swrite(line)
        sout.close()

    else: # not multiple_files
        # write the class body onto the single source file 
        hwrite = output_header.write
        for line in header_buffer: hwrite(line)
        swrite = output_source.write
        for line in source_buffer: swrite(line)


def add_app(app_attrs, top_win_class):
    """\
    Generates the code for a wxApp instance.
    if the 'class' property has no value, the function does nothing
    """
   
    if not multiple_files: prev_src = previous_source
    else:
        filename = os.path.join(out_dir, 'main.cpp')
        if not os.path.exists(filename): prev_src = None
        else:
            # prev_src doesn't need to be a SourceFileContent instance in this
            # case, as we do nothing if it is not None
            prev_src = 1
    if prev_src is not None:
        return # do nothing if the file existed

    klass = app_attrs.get('class')
    top_win = app_attrs.get('top_window')
    if not klass or not top_win: return # do nothing in these cases

    lines = []
    append = lines.append
    tab = tabs(1)
    append('\n\nclass %s: public wxApp {\n' % klass)
    append('public:\n')
    append(tab + 'bool OnInit();\n')
    append('};\n\n')
    append('IMPLEMENT_APP(%s)\n\n' % klass)
    append('bool %s::OnInit()\n{\n' % klass)
    append(tab + 'wxInitAllImageHandlers();\n') # we add this to avoid troubles
    append(tab + '%s* %s = new %s(0, -1, "");\n' % \
           (top_win_class, top_win, top_win_class))
    append(tab + 'SetTopWindow(%s);\n' % top_win)
    append(tab + '%s->Show();\n' % top_win)
    append(tab + 'return true;\n}\n')

    if multiple_files:
        filename = os.path.join(out_dir, 'main.cpp')
        try: out = open(filename, 'w')
        except: raise IOError("cpp_codegen.add_app: %s" % filename)
        write = out.write
        # write the common lines
        for line in header_lines: write(line)
        # import the top window module
        write('#include "%s.h"\n\n' % top_win_class)
        # write the wxApp code
        for line in lines: write(line)
        out.close()
    else:
        write = output_source.write
        for line in lines: write(line)


def generate_code_size(obj):
    """\
    returns the code fragment that sets the size of the given object.
    """
    if obj.is_toplevel: name1 = ''; name2 = 'this'
    else: name1 = '%s->' % obj.name; name2 = obj.name
    size = obj.properties.get('size', '').strip()
    use_dialog_units = (size[-1] == 'd')
    if use_dialog_units:
        return name1 + 'SetSize(wxDLG_UNIT(%s, wxSize(%s)));\n' % \
               (name2, size[:-1])
    else:
        return name1 + 'SetSize(wxSize(%s));\n' % size

def _string_to_colour(s):
    return '%d, %d, %d' % (int(s[1:3], 16), int(s[3:5], 16), int(s[5:], 16))

def generate_code_foreground(obj): 
    """\
    returns the code fragment that sets the foreground colour of
    the given object.
    """
    if not obj.is_toplevel: intro = '%s->' % obj.name
    else: intro = ''
    try:
        color = 'wxColour(%s)' % \
                _string_to_colour(obj.properties['foreground'])
    except (IndexError, ValueError): # the color is from system settings
        color = 'wxSystemSettings::GetSystemColour(%s)' % \
                obj.properties['foreground']
    return intro + 'SetForegroundColour(%s);\n' % color

def generate_code_background(obj):
    """\
    returns the code fragment that sets the background colour of
    the given object.
    """
    if not obj.is_toplevel: intro = '%s->' % obj.name
    else: intro = ''
    try:
        color = 'wxColour(%s)' % \
                _string_to_colour(obj.properties['background'])
    except (IndexError, ValueError): # the color is from system settings
        color = 'wxSystemSettings::GetSystemColour(%s)' % \
                obj.properties['background']
    return intro + 'SetBackgroundColour(%s);\n' % color

def generate_code_font(obj):
    """\
    returns the code fragment that sets the font the given object.
    """
    font = obj.properties['font'] 
    size = font['size']; family = font['family']
    underlined = font['underlined']
    style = font['style']; weight = font['weight']
    face = '"%s"' % font['face'].replace('"', r'\"')
    if obj.is_toplevel: intro = ''
    else: intro = '%s->' % obj.name
    return intro + 'SetFont(wxFont(%s, %s, %s, %s, %s, %s));\n' % \
           (size, family, style, weight, underlined, face)

def generate_code_id(obj):
    """\
    returns a 2-tuple of strings representing the LOC that sets the id of the
    given object: the first line is the declaration of the variable, and is
    empty if the object's id is a constant, and the second line is the value
    of the id
    """
    id = obj.properties.get('id')
    if id is None: return '', '-1'
    tokens = id.split('=')
    if len(tokens) > 1: name, val = tokens[:2]
    else: return '', tokens[0] # we assume name is declared elsewhere
    if not name: return '', val
    return '%s = %s' % (name, val), name

def generate_code_tooltip(obj):
    """\
    returns the code fragment that sets the tooltip of
    the given object.
    """
    if not obj.is_toplevel: intro = '%s->' % obj.name
    else: intro = ''
    return intro + 'SetToolTip("%s");\n' % \
           obj.properties['tooltip'].replace('"', r'\"')

def generate_common_properties(widget):
    """\
    generates the code for various properties common to all widgets (background
    and foreground colors, font, ...)
    Returns a list of strings containing the generated code
    """
    prop = widget.properties
    out = []
    if prop.get('size', '').strip(): out.append(generate_code_size(widget))
    if prop.get('background'): out.append(generate_code_background(widget))
    if prop.get('foreground'): out.append(generate_code_foreground(widget))
    if prop.get('font'): out.append(generate_code_font(widget))
    if prop.get('tooltip'): out.append(generate_code_tooltip(widget))
    return out


# custom property handlers
class FontPropertyHandler:
    """Handler for font properties"""
    font_families = { 'default': 'wxDEFAULT', 'decorative': 'wxDECORATIVE',
                      'roman': 'wxROMAN', 'swiss': 'wxSWISS',
                      'script': 'wxSCRIPT', 'modern': 'wxMODERN',
                      'teletype': 'wxTELETYPE' }
    font_styles = { 'normal': 'wxNORMAL', 'slant': 'wxSLANT',
                    'italic': 'wxITALIC' }
    font_weights = { 'normal': 'wxNORMAL', 'light': 'wxLIGHT',
                     'bold': 'wxBOLD' }
    def __init__(self):
        self.dicts = { 'family': self.font_families, 'style': self.font_styles,
                       'weight': self.font_weights }
        self.attrs = { 'size': '0', 'style': '0', 'weight': '0', 'family': '0',
                       'underlined': '0', 'face': '' }
        self.current = None 
        self.curr_data = []
        
    def start_elem(self, name, attrs):
        self.curr_data = []
        if name != 'font' and name in self.attrs:
            self.current = name
        else: self.current = None
            
    def end_elem(self, name, code_obj):
        if name == 'font':
            code_obj.properties['font'] = self.attrs
            return True
        elif self.current is not None:
            decode = self.dicts.get(self.current)
            if decode: val = decode.get("".join(self.curr_data), '0')
            else: val = "".join(self.curr_data)
            self.attrs[self.current] = val
        
    def char_data(self, data):
        self.curr_data.append(data)

# end of class FontPropertyHandler


class DummyPropertyHandler:
    """Empty handler for properties that do not need code"""
    def start_elem(self, name, attrs): pass
    def end_elem(self, name, code_obj): return True
    def char_data(self, data): pass

# end of class DummyPropertyHandler


# dictionary whose items are custom handlers for widget properties
_global_property_writers = { 'font': FontPropertyHandler }

# dictionary of dictionaries of property handlers specific for a widget
# the keys are the class names of the widgets
# Ex: _property_writers['wxRadioBox'] = {'choices', choices_handler}
_property_writers = {}

# dictionary of additional headers for objects
_obj_headers = {}

def get_property_handler(property_name, widget_name):
    try: cls = _property_writers[widget_name][property_name]
    except KeyError: cls = _global_property_writers.get(property_name, None)
    if cls: return cls()
    return None

def add_property_handler(property_name, handler, widget_name=None):
    """\
    sets a function to parse a portion of XML to get the value of the property
    property_name. If widget_name is not None, the function is called only if
    the property in inside a widget whose class is widget_name
    """
    if widget_name is None: _global_property_writers[property_name] = handler
    else:
        try: _property_writers[widget_name][property_name] = handler
        except KeyError:
            _property_writers[widget_name] = { property_name: handler }

def add_widget_handler(widget_name, handler,
                       constructor=None,
                       properties_handler=generate_common_properties,
                       extra_headers=None):
    """\
    sets the functions to generate the code for the widget whose base class
    is 'widget_name':
     - handler: function that writes the code
     - constructor: ``signature'' of the widget's constructor
     - properties_handler: function that writes extra code to set the
                           properties of the widget if it is a toplevel class
     - extra_headers: if not None, list of extra header file, in the form
       <header.h> or "header.h"
    """
    obj_builders[widget_name] = handler
    obj_properties[widget_name] = properties_handler
    if constructor is not None:
        obj_constructors[widget_name] = constructor
    if extra_headers is not None:
        _obj_headers[widget_name] = extra_headers


def _unique(sequence):
    """\
    Strips all duplicates from sequence. Works only if items of sequence
    are hashable
    """
    tmp = {}
    for item in sequence: tmp[item] = 1
    return tmp.keys()
