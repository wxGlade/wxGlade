# MenuTree.py: A class to represent a menu on a wxMenuBar
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

class MenuTree:
    """\
    A class to represent a menu on a wxMenuBar
    """
    class Node:
        def __init__(self, label="", id="", name="", checkable=""):
            self.label = label
            self.id = id
            self.name = name
            self.checkable = checkable
            self.children = []
            self.parent = None
            
        def write(self, outfile, tabs, top=False):
            from xml.sax.saxutils import escape, quoteattr
            import common
            fwrite = outfile.write
            tstr = '    ' * (tabs+1)
            label = common._encode_to_xml(self.label)
            if not top and not self.children:
                fwrite('%s<item>\n' % ('    ' * tabs))
                fwrite('%s<label>%s</label>\n' % (tstr, escape(label)))
                fwrite('%s<id>%s</id>\n' % (tstr, escape(self.id)))
                fwrite('%s<name>%s</name>\n' % (tstr, escape(self.name)))
                try: checkable = int(self.checkable)
                except: checkable = 0
                fwrite('%s<checkable>%s</checkable>\n' % (tstr, checkable))
                fwrite('%s</item>\n' % ('    ' * tabs))
            else:
                name = quoteattr(self.name)
                fwrite('    ' * tabs + '<menu name=%s ' % name)
                fwrite('label=%s>\n' % (quoteattr(label)))
                for c in self.children: c.write(outfile, tabs+1)
                fwrite('    ' * tabs + '</menu>\n')
                
    #end of class Node
    
    def __init__(self, name, label):
        self.root = self.Node(label, "", name, "")

    def write(self, outfile, tabs):
        self.root.write(outfile, tabs, top=True)
        
#end of class MenuTree
