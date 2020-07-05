"""
Utilities, e.g. for debugging

@copyright: 2018-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from __future__ import print_function

import sys, os
import wx


def hx(obj):
    return hex(id(obj)).upper()


class StructurePrinter:
    # print the structure and sizes of a window with all it's children
    def __init__(self, window):
        self.window(window, 0)

    def _sizer_item(self, si, indent):
        expand = (si.GetFlag() & wx.EXPAND) and "EXPAND" or ""
        print( "  "*indent, " SI:", si.GetMinSize(), " ", si.GetSize(), " ", si.GetProportion(), expand )

    def sizer(self, sizer, si, indent=0):
        cname = sizer.GetClassName() # u'wxPanel'
        print( "  "*indent, "%s"%cname, sizer.GetSize(), sizer )

        if si: self._sizer_item(si, indent)
        print()

        # list all by sizer
        for si in sizer.GetChildren():
            if si is None:
                print("Child is None")
                continue
            child = si.GetWindow()
            if child is not None:
                self.window(child, si, indent+1)
                continue
            child = si.GetSizer()
            if child is not None:
                self.sizer(child, si, indent+1)
                continue
            child = si.GetSpacer()
            if child is not None:
                self.spacer(child, si, indent+1)
                continue
            raise ValueError("XXX")

    def window(self, widget, si, indent=0):
        cname = widget.GetClassName() # u'wxPanel'
        name  = widget.GetName() # u'panel'
        HEX = hx(widget)
        try:
            best_size = widget.GetBestSize()
        except AttributeError:
            best_size ="???"
        print( "  "*indent, "%s: %s %s"%(cname, name, HEX), widget.GetSize(), best_size, widget.GetEffectiveMinSize() )

        if si: self._sizer_item(si, indent)

        sizer = widget.GetSizer()

        if sizer:
            # list all by sizer
            self.sizer(sizer, None, indent+1)
        else:
            # list all children
            for child in widget.GetChildren():
                self.window(child, None, indent+1)
        print()

    def spacer(self, spacer, si, indent=0):
        # a spacer
        print( "  "*indent, "Spacer", spacer.Get() )
        self._sizer_item(si, indent)
        print()


class TreePrinter:
    # print the structure of the TreeCtrl
    def __init__(self, tree):
        self.tree = tree
        root = tree.GetRootItem()
        self.prn(root, 0)

    def _get_children_items(self, widget):
        item = widget.item
        items = []
        child_item, cookie = self.GetFirstChild(item)
        while child_item.IsOk():
            items.append(child_item)
            child_item, cookie = self.GetNextChild(item, cookie)
        return items

    def XXX(self, items):
        for child_item in items:
            editor = self.tree._GetItemData(child_item)
            if editor is not None and (not children or not editor in children):
                self._SetItemData(child_item, None)
                editor.item = None  # is probably None already
                item_editors.append(None)
            else:
                item_editors.append(editor)


    def prn(self, item, indent=0):
        items = self.tree._get_children_items(item)
        if not items: return
        for child_item in items:
            editor = self.tree._GetItemData(child_item)
            if editor.item is None:
                status = "XXX Missing"
            elif editor.item != child_item:
                status = "XXX Mismatch"
            else:
                status = ""
            print( "  "*indent, hx(child_item), editor.WX_CLASS or editor.name, hx(editor), status )
            self.prn(child_item, indent+1)

        print()


def trace1(func, *args, **kwargs):
    # use built-in trace module to write an execution trace to stdout
    # e.g. utilities.trace1(clipboard.paste, focused_widget)
    import sys, trace

    tracer = trace.Trace(ignoredirs=[sys.prefix, sys.exec_prefix],
                         ignoremods=["gui_mixins", "new_properties", "decorators", "__init__", "clipboard"], 
                         trace=1, count=0)

    editor = common.root.children[-1]
    tracer.runfunc(editor.create_widgets)

    # make a report, placing output in the current directory
    r = tracer.results()
    if filename:
        r.write_results_file(path, lines, lnotab, lines_hit, encoding=None)
    else:
        r.write_results(show_missing=True, coverdir=".")



def trace(filename, func, *args, **kwargs):
    sys.settrace(trace_calls)
    func(*args, **kwargs)
    sys.settrace(None)

def start_trace(filename):
    sys.settrace(trace_calls)

def stop_trace():
    sys.settrace(None)


_FILES = {}
def _get_line(filename, line_no):
    if not filename in _FILES:
        _FILES[filename] = open(filename, "r").readlines()
    return _FILES[filename][line_no-1]


def trace_lines(frame, event, arg):
    if event != 'line': return
    co = frame.f_code
    func_name = co.co_name
    line_no = frame.f_lineno
    filename = co.co_filename
    if os.path.isfile(filename):
        print( '  %30s: %5d' % (func_name, line_no), _get_line(filename, line_no), end="" )
    else:
        print( '  %30s: %5d' % (func_name, line_no) )

def trace_calls(frame, event, arg):
    if event != 'call': return
    co = frame.f_code
    func_name = co.co_name
    
    if func_name in ("__getattr__", "<genexpr>", "<listcomp>"): return
    
    if func_name in ("cn",): return
    if func_name.startswith("_",): return
    if func_name in ("wxname2attr",): return

    filename = co.co_filename
    if filename.startswith(sys.prefix) or filename.startswith(sys.exec_prefix): return

    modulename = filename.split(os.sep)[-1].split(".")[0]
    if modulename in ("new_properties", "gui_mixins", "decorators", "log", "main", "misc", "compat",): return

    line_no = frame.f_lineno
    print( ' -> %s:%s %s' % (modulename, line_no, func_name))

    return trace_lines

