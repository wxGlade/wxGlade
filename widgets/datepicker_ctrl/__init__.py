# __init__.py: datepicker_ctrl widget module initialization
# $Header: /home/alb/tmp/wxglade_cvs_backup/wxGlade/widgets/datepicker_ctrl/__init__.py,v 1.1 2006/10/20 11:53:07 guyru Exp $

# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import datepicker_ctrl
        return datepicker_ctrl.initialize()
