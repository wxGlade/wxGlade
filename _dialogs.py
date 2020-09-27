# -*- coding: UTF-8 -*-
#
# generated by wxGlade 1.0.0a8 on Sat Sep 26 22:33:56 2020
#

import wx

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
# end wxGlade


class MakeRowColGrowableDlg(wx.Dialog):
    def __init__(self, *args, **kwds):
        # begin wxGlade: MakeRowColGrowableDlg.__init__
        kwds["style"] = kwds.get("style", 0) | wx.DEFAULT_DIALOG_STYLE
        wx.Dialog.__init__(self, *args, **kwds)
        self.SetTitle("Make row and/or column growable")

        sizer_1 = wx.BoxSizer(wx.VERTICAL)

        static_text_1 = wx.StaticText(self, wx.ID_ANY, "You have set a widget's layout to EXPAND.\n\nThe containing row and column are not growable.\nSo EXPAND does not have an effect.")
        sizer_1.Add(static_text_1, 0, wx.ALL | wx.EXPAND, 8)

        sizer_2 = wx.StaticBoxSizer(wx.StaticBox(self, wx.ID_ANY, "Make growable"), wx.VERTICAL)
        sizer_1.Add(sizer_2, 1, wx.ALL | wx.EXPAND, 6)

        self.cb_row_growable = wx.CheckBox(self, wx.ID_ANY, "Row")
        sizer_2.Add(self.cb_row_growable, 0, wx.ALL, 4)

        self.cb_col_growable = wx.CheckBox(self, wx.ID_ANY, "Column")
        sizer_2.Add(self.cb_col_growable, 0, wx.ALL, 4)

        sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_1.Add(sizer_3, 0, wx.ALL | wx.EXPAND, 2)

        self.cb_dont_show_again = wx.CheckBox(self, wx.ID_ANY, "Don't show this dialog again")
        sizer_3.Add(self.cb_dont_show_again, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 5)

        sizer_3.Add((20, 20), 1, wx.ALIGN_CENTER_VERTICAL, 0)

        self.button_OK = wx.Button(self, wx.ID_OK, "")
        sizer_3.Add(self.button_OK, 0, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 5)

        self.SetSizer(sizer_1)
        sizer_1.Fit(self)

        self.Layout()
        # end wxGlade

# end of class MakeRowColGrowableDlg

class SelectArtDialog(wx.Dialog):
    def __init__(self, *args, **kwds):
        # begin wxGlade: SelectArtDialog.__init__
        kwds["style"] = kwds.get("style", 0) | wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER
        wx.Dialog.__init__(self, *args, **kwds)
        self.SetSize((300, 600))
        self.SetTitle("ART selector")

        sizer_1 = wx.BoxSizer(wx.VERTICAL)

        label_1 = wx.StaticText(self, wx.ID_ANY, "Select row and hit OK or Enter:")
        sizer_1.Add(label_1, 0, wx.ALL, 8)

        self.art_names = ["NEW", "FILE_OPEN", "FILE_SAVE", "FILE_SAVE_AS", "NEW_DIR", "FOLDER_OPEN",
        "CUT", "COPY", "PASTE", "UNDO", "REDO",
        "PLUS", "MINUS", "CLOSE", "QUIT", "PRINT", "DELETE", "HELP",
        "FIND", "FIND_AND_REPLACE", "ADD_BOOKMARK", "DEL_BOOKMARK",
        "GO_FORWARD", "GO_BACK", "GO_UP", "GO_DOWN", "GO_HOME", "GOTO_FIRST", "GOTO_LAST", "GO_TO_PARENT", "GO_DIR_UP",
        "LIST_VIEW", "REPORT_VIEW",
        "NORMAL_FILE", "EXECUTABLE_FILE", "FOLDER",
        "HELP_BOOK", "HELP_FOLDER", "HELP_PAGE", "HELP_SIDE_PANEL", "HELP_SETTINGS",
        "TICK_MARK", "CROSS_MARK",
        "MISSING_IMAGE",
        "HARDDISK", "FLOPPY", "CDROM",
        "ERROR", "QUESTION", "WARNING", "INFORMATION", "TIP"]
        # not for 2.8: "GOTO_FIRST", "GOTO_LAST", "PLUS", "MINUS"

        self.art_list = wx.ImageList(16, 16)

        self.listctrl = wx.ListCtrl(self, wx.ID_ANY, style=wx.LC_HRULES | wx.LC_REPORT | wx.LC_VRULES)
        #self.listctrl.AppendColumn("Image", format=wx.LIST_FORMAT_LEFT, width=50)
        #self.listctrl.AppendColumn("Name", format=wx.LIST_FORMAT_LEFT, width=200)
        self.listctrl.InsertColumn(0, "Image", format=wx.LIST_FORMAT_LEFT, width=50)
        self.listctrl.InsertColumn(1, "Name", format=wx.LIST_FORMAT_LEFT, width=200)

        for i, name in enumerate(self.art_names):
            art_id = getattr(wx, "ART_%s"%name)
            bmp = wx.ArtProvider.GetBitmap(art_id, wx.ART_TOOLBAR, (16,16))
            self.art_list.Add(bmp)

        self.listctrl.SetImageList(self.art_list, wx.IMAGE_LIST_SMALL)
        for i, name in enumerate(self.art_names):
            self.listctrl.InsertItem( i, i )  # i is index into ImageList
            self.listctrl.SetItem(i, 1, name)


        #index = self.list.InsertItem(self.list.GetItemCount(), data[0], self.idx1)
        sizer_1.Add(self.listctrl, 1, wx.ALL | wx.EXPAND, 4)

        sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_1.Add(sizer_3, 0, wx.ALL | wx.EXPAND, 4)

        label_2 = wx.StaticText(self, wx.ID_ANY, "Width")
        sizer_3.Add(label_2, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT, 4)

        self.spin_width = wx.SpinCtrl(self, wx.ID_ANY, "24", min=1, max=100)
        sizer_3.Add(self.spin_width, 0, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 3)

        label_3 = wx.StaticText(self, wx.ID_ANY, "Height")
        sizer_3.Add(label_3, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT, 6)

        self.spin_height = wx.SpinCtrl(self, wx.ID_ANY, "24", min=1, max=100)
        sizer_3.Add(self.spin_height, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT, 3)

        label_4 = wx.StaticText(self, wx.ID_ANY, "pixels")
        sizer_3.Add(label_4, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT, 6)

        static_line_1 = wx.StaticLine(self, wx.ID_ANY)
        sizer_1.Add(static_line_1, 0, wx.EXPAND, 0)

        sizer_2 = wx.StdDialogButtonSizer()
        sizer_1.Add(sizer_2, 0, wx.ALIGN_RIGHT | wx.ALL, 8)

        self.button_OK = wx.Button(self, wx.ID_OK, "")
        self.button_OK.SetDefault()
        sizer_2.AddButton(self.button_OK)

        self.button_CANCEL = wx.Button(self, wx.ID_CANCEL, "")
        sizer_2.AddButton(self.button_CANCEL)

        sizer_2.Realize()

        self.SetSizer(sizer_1)

        self.SetAffirmativeId(self.button_OK.GetId())
        self.SetEscapeId(self.button_CANCEL.GetId())

        self.Layout()

        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, lambda evt: self.EndModal(wx.ID_OK), self.listctrl)
        # end wxGlade

# end of class SelectArtDialog
