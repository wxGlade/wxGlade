#!/usr/bin/env python
# wxglade.py: entry point of wxGlade
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import os, os.path, sys

# check to see if the Python release supports boolean identifiers
# and bool built-in function (>= Python 2.2.1).
try:
    True, False, bool
except NameError:
    setattr(__builtins__, 'True', 1)
    setattr(__builtins__, 'False', not True)
    def bool(value): return not not value
    setattr(__builtins__, 'bool', bool)

def _fix_path(path):
    """\
    Returns an absolute version of path, accroding to the invoking dir of
    wxglade (which can be different from '.' if it is invoked from a shell
    script)
    """
    if not os.path.isabs(path):
        return os.path.join(os.getenv('WXGLADE_INVOKING_DIR', '.'), path)
    return path

def parse_command_line():
    import getopt, common
    try: options, args = getopt.getopt(sys.argv[1:], "g:o:", ['generate-code=',
                                                              'output='])
    except getopt.GetoptError: usage()
    return options, args

def command_line_code_generation(options, args):
    """\
    tarts a code generator without starting the GUI.
    """
    import common
    if not options: usage()
    if not options[0]:
        usage() # a language for code generation must be provided
    if len(args) != 1: usage() # an input file name must be provided
    
    common.use_gui = False # don't import wxPython.wx
    common.load_code_writers()
    common.load_widgets()
    common.load_sizers()
    try:
        from xml_parse import CodeWriter
        out_path = None
        language = ''
        for option, arg in options:
            if option == '-g' or option == '--generate-code':
                language = arg
            elif option == '-o' or option == '--output':
                out_path = _fix_path(arg)
        writer = common.code_writers[language]
        CodeWriter(writer, _fix_path(args[0]), out_path=out_path)
    except KeyError:
        print >> sys.stderr, 'Error: no writer for language "%s" available' % \
              language
        sys.exit(1)
    except Exception, e:
        print >> sys.stderr, "Error: %s" % e
        import traceback; traceback.print_exc()
        sys.exit(1)
    sys.exit(0)

def usage():
    """\
    Prints a help message about the usage of wxGlade from the command line.
    """
    msg = """\
wxGlade usage:
- to start the GUI: python wxglade.py [XML_FILE]
- to generate code from the command line: python wxglade.py OPTIONS... FILE
  OPTIONS are the following:
  -g, --generate-code=LANGUAGE  (required) give the output language
  -o, --output=PATH             (optional) name of the output file (in
                                single-file mode) or directory (in
                                multi-file mode)
    """
    print msg
    print 'Valid LANGUAGE values:',
    import common
    common.use_gui = False
    common.load_code_writers()
    for value in common.code_writers: print value,
    print '\n'
    sys.exit(1)


if __name__ == "__main__":
    # append the widgets dir to the
    # app's search path
    sys.path.append('widgets')
    # before running the GUI, let's see if there are command line options for
    # code generation
    if len(sys.argv) == 1:
        # if there was no option, start the app in GUI mode
        import main
        main.main()
    else:
        options, args = parse_command_line()
        if not options:
            # start the app in GUI mode, opening the given file
            import main
            filename = _fix_path(args[0])
            main.main(filename)
        else:
            command_line_code_generation(options, args)
