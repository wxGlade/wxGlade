#!/usr/bin/env python
# xrc_import.py: Converts an XRC resource file (in a format wxGlade likes,
# i.e. all windows inside sizers, no widget unknown to wxGlade, ...) into a
# WXG file
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import xml.dom.minidom
import sys, getopt, os.path, time

_version = '0.0.1'

try: True, False
except NameError: True, False = 1, 0

def get_child_elems(node):
    def ok(n): return n.nodeType == n.ELEMENT_NODE
    return filter(ok, node.childNodes)

_counter_name = 1

def convert(input, output):
    global _counter_name
    _counter_name = 1
    document = xml.dom.minidom.parse(input)
    fix_fake_panels(document)
    set_base_classes(document)
    fix_properties(document)
    fix_widgets(document)
    output = open(output, 'w')
    write_output(document, output)


def write_output(document, output):
    """\
    This code has been adapted from XRCed 0.0.7-3.
    Many thanks to its author Roman Rolinsky.
    """
    from cStringIO import StringIO
    dom_copy = xml.dom.minidom.Document()
    tmp_out = StringIO()

    def indent(node, level=0):
        # Copy child list because it will change soon
        children = node.childNodes[:]
        # Main node doesn't need to be indented
        if level:
            text = dom_copy.createTextNode('\n' + '    ' * level)
            node.parentNode.insertBefore(text, node)
        if children:
            # Append newline after last child, except for text nodes
            if children[-1].nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
                text = dom_copy.createTextNode('\n' + '    ' * level)
                node.appendChild(text)
            # Indent children which are elements
            for n in children:
                if n.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
                    indent(n, level + 1)

    comment = dom_copy.createComment(' generated by xrc_import %s on %s '
                                     % (_version, time.asctime()))
    dom_copy.appendChild(comment)
    main_node = dom_copy.appendChild(document.documentElement.cloneNode(True))
    indent(main_node)
    dom_copy.writexml(tmp_out)
    comment_done = False
    for line in tmp_out.getvalue().split('\n'):
        if not comment_done and line.startswith('<!--'):
            line = line.replace('-->', '-->\n\n')
            comment_done = True
        if line.strip():
            output.write(line)
            output.write('\n')
    dom_copy.unlink()
    output.close()


def set_base_classes(document):
    for elem in document.getElementsByTagName('object'):
        klass = elem.getAttribute('class')
        if klass.startswith('wx'):
            elem.setAttribute('base', 'Edit' + klass[2:])
            name = elem.getAttribute('name')
            if not name:
                global _counter_name
                elem.setAttribute('name', 'object_%s' % _counter_name)
                _counter_name += 1


_props = {
    'bg': 'background',
    'fg': 'foreground',
    'content': 'choices',
    'item': 'choice',
    'growablerows': 'growable_rows',
    'growablecols': 'growable_cols'
    }

def fix_properties(document):
    for prop in _props:
        for elem in document.getElementsByTagName(prop):
            elem.tagName = _props[prop]
    document.documentElement.tagName = 'application'
    document.documentElement.removeAttribute('version')


def fix_widgets(document):
    fix_menubars(document)
    fix_notebooks(document)
    fix_spacers(document)
    fix_sliders(document)
    fix_toplevel_names(document)


def fix_menubars(document):
    def ok(elem): return elem.getAttribute('class') == 'wxMenuBar'
    menubars = filter(ok, document.getElementsByTagName('object'))
    for mb in menubars:
        fix_menus(document, mb)
        mb_prop = document.createElement('menubar')
        mb_prop.appendChild(document.createTextNode('1'))
        mb.parentNode.insertBefore(mb_prop, mb)


def fix_menus(document, menubar):
    def ok(elem): return elem.getAttribute('class') == 'wxMenu'
    menus = filter(ok, get_child_elems(menubar))
    menus_node = document.createElement('menus')
    for menu in menus:
        try:
            label = [ c for c in get_child_elems(menu)
                      if c.tagName == 'label' ][0]
            label = label.firstChild.data
        except IndexError: label = ''
        new_menu = document.createElement('menu')
        new_menu.setAttribute('name', menu.getAttribute('name'))
        new_menu.setAttribute('label', label)
        fix_sub_menus(document, menu, new_menu)
        menus = document.createElement('menus')
        menus.appendChild(new_menu)
        menubar.removeChild(menu).unlink()
        menubar.appendChild(menus)


def fix_sub_menus(document, menu, new_menu):
    for child in get_child_elems(menu):
        klass = child.getAttribute('class')
        elem = document.createElement('')
        if klass == 'wxMenuItem':
            elem.tagName = 'item'
            name = document.createElement('name')
            name.appendChild(document.createTextNode(
                child.getAttribute('name')))
            elem.appendChild(name)
            for c in get_child_elems(child):
                elem.appendChild(c)
        elif klass == 'separator':
            elem.tagName = 'item'
            for name in 'label', 'id', 'name':
                e = document.createElement(name)
                e.appendChild(document.createTextNode('---'))
                elem.appendChild(e)
        elif klass == 'wxMenu':
            elem.tagName = 'menu'
            elem.setAttribute('name', child.getAttribute('name'))
            try:
                label = [ c for c in get_child_elems(child) if
                          c.tagName == 'label' ][0]
                label = label.firstChild.data
            except IndexError: label = ''
            elem.setAttribute('label', label)
            fix_sub_menus(document, child, elem)
        if elem.tagName: new_menu.appendChild(elem)


def fix_notebooks(document):
    def ispage(node): return node.getAttribute('class') == 'notebookpage'
    def isnotebook(node): return node.getAttribute('class') == 'wxNotebook'
    for nb in filter(isnotebook, document.getElementsByTagName('object')):
        pages = filter(ispage, get_child_elems(nb))
        tabs = document.createElement('tabs')
        try:
            us = filter(lambda n: n.tagName == 'usenotebooksizer',
                        get_child_elems(nb))[0]
            nb.removeChild(us).unlink()
        except IndexError:
            pass
        for page in pages:
            tab = document.createElement('tab')
            obj = None
            for c in get_child_elems(page):
                if c.tagName == 'label':
                    tab.appendChild(c.firstChild)
                elif c.tagName == 'object':
                    tab.setAttribute('window', c.getAttribute('name'))
                    c.setAttribute('base', 'NotebookPane')
                    obj = c
            tabs.appendChild(tab)
            nb.replaceChild(obj, page)
        nb.insertBefore(tabs, nb.firstChild)


def fix_fake_panels(document):
    def isframe(node): return node.getAttribute('class') == 'wxFrame'
    for frame in filter(isframe, document.getElementsByTagName('object')):
        for c in get_child_elems(frame):
            if c.tagName == 'object' and c.getAttribute('class') == 'wxPanel' \
               and c.getAttribute('name') == '':
                elems = get_child_elems(c)
                if len(elems) == 1 and \
                       elems[0].getAttribute('class').find('Sizer') != -1:
                    frame.replaceChild(elems[0], c)


def fix_spacers(document):
    def isspacer(node): return node.getAttribute('class') == 'spacer'
    for spacer in filter(isspacer, document.getElementsByTagName('object')):
        spacer.setAttribute('name', 'spacer')
        spacer.setAttribute('base', 'EditSpacer')
        sizeritem = document.createElement('object')
        sizeritem.setAttribute('class', 'sizeritem')
        for child in get_child_elems(spacer):
            if child.tagName == 'size':
                w, h = [s.strip() for s in child.firstChild.data.split(',')]
                width = document.createElement('width')
                width.appendChild(document.createTextNode(w))
                height = document.createElement('height')
                height.appendChild(document.createTextNode(h))
                spacer.removeChild(child).unlink()
                spacer.appendChild(width)
                spacer.appendChild(height)
            else:
                sizeritem.appendChild(spacer.removeChild(child))
        spacer.parentNode.replaceChild(sizeritem, spacer)
        sizeritem.appendChild(spacer)


def fix_toplevel_names(document):
    for widget in get_child_elems(document.documentElement):
        klass = widget.getAttribute('class')
        if klass == 'wxPanel':
            widget.setAttribute('base', 'EditTopLevelPanel')
        widget.setAttribute('class', klass.replace('wx', 'My'))


def fix_sliders(document):
    def isslider(node):
        klass = node.getAttribute('class')
        return klass == 'wxSlider' or klass == 'wxSpinCtrl'
    for slider in filter(isslider, document.getElementsByTagName('object')):
        for child in get_child_elems(slider):
            if child.tagName == 'range':
                m1, m2 = [s.strip() for s in child.firstChild.data.split(',')]
                min = document.createElement('min')
                min.appendChild(document.createTextNode(m1))
                max = document.createElement('max')
                max.appendChild(document.createTextNode(m2))
                slider.removeChild(child).unlink()
                slider.appendChild(min)
                slider.appendChild(max)
        

def usage():
    msg = """\
usage: python xrc_import.py OPTIONS <INPUT_FILE.xrc> [WXG_FILE]

OPTIONS:
  -d, --debug: debug mode, i.e. you can see the whole traceback of each error
  
If WXG_FILE is not given, it defaults to INPUT_FILE.wxg
    """
    print msg
    raise sys.exit(1)


def print_exception(exc):
    msg = """\
An error occurred while trying to convert the XRC file. Here's the short error
message:
\t%s\n
If you think this is a bug, or if you want to know more about the cause of the
error, run this script again in debug mode (-d switch). If you find a bug,
please report it to the mailing list (wxglade-general@lists.sourceforge.net),
or enter a bug report at the SourceForge bug tracker.

Please note that this doesn't handle ALL XRC files correctly, but only those
which already are in a format which wxGlade likes: this means that every
non-toplevel widget must be inside sizers and that the resource must not
contain widgets or properties unknown to wxGlade.
    """ % str(exc)
    print >> sys.stderr, msg
    raise sys.exit(1)


if __name__ == '__main__':
    try: options, args = getopt.getopt(sys.argv[1:], "d", ['debug'])
    except getopt.GetoptError: usage()
    if not args: usage()
    input = args[0]
    try: output = args[1]
    except IndexError: output = os.path.splitext(input)[0] + '.wxg'
    if not options:
        try: convert(input, output)
        except Exception, e: # catch the exception and print a nice message
            print_exception(e)
    else: # if in debug mode, let the traceback be printed
        convert(input, output)
