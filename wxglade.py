#!/usr/bin/env python
# wxglade.py: entry point of wxGlade
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

# check to see if the Python release
# supports boolean identifiers (>= Python 2.2.1)
try:
    True, False
except NameError:
    setattr(__builtins__, 'True', 1)
    setattr(__builtins__, 'False', not True)

if __name__ == "__main__":
    import sys
    # append the widgets dir to the
    # app's search path
    sys.path.append('widgets')
    import main
    # start the whole app
    main.main()

