#!/usr/bin/env python
# -*- coding: UTF-8 -*-
#
#

import wx

# begin wxGlade: dependencies
import gettext
# end wxGlade

# begin wxGlade: extracode
# end wxGlade


class StockAction(wx.Frame):
    def __init__(self, *args, **kwds):
        # begin wxGlade: StockAction.__init__
        kwds["style"] = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL
        wx.Frame.__init__(self, *args, **kwds)

        self.__set_properties()
        self.__do_layout()
        # end wxGlade

    def __set_properties(self):
        # begin wxGlade: StockAction.__set_properties
        self.SetTitle(_("Stock Action"))
        self.SetSize((150,150))
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: StockAction.__do_layout
        self.Layout()
        # end wxGlade

# end of class StockAction
