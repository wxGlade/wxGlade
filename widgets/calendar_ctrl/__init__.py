# __init__.py: calendar_ctrl widget module initialization
# $Header: /home/alb/tmp/wxglade_cvs_backup/wxGlade/widgets/calendar_ctrl/__init__.py,v 1.1 2006/10/14 21:24:59 guyru Exp $

# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import calendar_ctrl
        return calendar_ctrl.initialize()
