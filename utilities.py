"""
Utilities, e.g. for debugging

@copyright: 2018-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from __future__ import print_function

import wx

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
        try:
            best_size = widget.GetBestSize()
        except AttributeError:
            best_size ="???"
        print( "  "*indent, "%s: %s"%(cname, name), widget.GetSize(), best_size, widget.GetEffectiveMinSize() )

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


def hx(obj):
    return hex(id(obj)).upper()

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
