# clipboard.py: support for cut & paste of wxGlade widgets
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

class wxGladeClipboard:
    """\
    class used to copy widgets to/from the clipboard
    """
    __dict = None
    def __init__(self, xml_str="", option=0, flag=0, border=0):
        if wxGladeClipboard.__dict is None: wxGladeClipboard.__dict = {}
        self.__dict__ = wxGladeClipboard.__dict
        self.xml_str = xml_str
        self.option = option
        self.flag = flag
        self.border = border

clipboard = wxGladeClipboard()

def copy(widget):
    """\
    copies widget and all its children to the clipboard
    """
    from cStringIO import StringIO
    xml_str = StringIO()
    oldname = widget.name
    newname = widget.name + '_copy'
    # generate a unique name for the copy
    import common
    i = 1
    while common.app_tree.has_name(newname):
        newname = '%s_copy_%s' % (oldname, i)
        i += 1
    widget.name = newname
    widget.node.write(xml_str, 0)
    widget.name = oldname
    flag = widget.get_int_flag() 
    option = widget.get_option()
    border = widget.get_border()
    global clipboard
    clipboard = wxGladeClipboard(xml_str.getvalue(), option, flag, border)
    #print clipboard.xml_str

def cut(widget):
    copy(widget)
    widget.remove()

def paste(parent, sizer, pos):
    """\
    copies a widget (and all its children) from the clipboard to the given
    destination (parent, sizer and position inside the sizer)
    returns True if there was something to paste, False otherwise
    """
    xml_str = clipboard.xml_str
    if xml_str:
        #log = open('clipboard_log.txt', 'w')
        #log.write(xml_str)
        #log.close()
        import xml_parse
##         class XmlClipboardObject:
##             def __init__(self, **kwds):
##                 self.__dict__.update(kwds)
##         sizer = XmlClipboardObject(obj=sizer, parent=parent)
##         par = XmlClipboardObject(obj=parent, parent=parent)
##         sizeritem = xml_parse.Sizeritem()
##         sizeritem.option = clipboard.option
##         sizeritem.flag = clipboard.flag
##         sizeritem.border = clipboard.border
##         sizeritem.pos = pos
##         si = XmlClipboardObject(obj=sizeritem, parent=parent)
##         parser = xml_parse.XmlWidgetBuilder(True, par, sizer, si)
        parser = xml_parse.ClipboardXmlWidgetBuilder(parent, sizer, pos,
                                                     clipboard.option,
                                                     clipboard.flag,
                                                     clipboard.border)
        parser.parse_string(xml_str)
        return True # widget hierarchy pasted
    return False # there's nothing to paste
