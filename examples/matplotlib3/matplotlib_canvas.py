#!/usr/bin/env python
# -*- coding: UTF-8 -*-

# a class to make matplotib.backend_wxagg.FigureCanvasWxAgg compatible to wxGlade
# functionality from NavigationToolbar2 is integrated, such that wx buttons etc. can be used

import wx

import matplotlib
from matplotlib.figure import Figure
from matplotlib.backends import wx_compat as wxc


_USE_AGG = False
_USE_AGG = True

if _USE_AGG :
    from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
else:
    from matplotlib.backends.backend_wxagg import FigureCanvasWx as FigureCanvas



class MatplotlibCanvas(FigureCanvas):
    def __init__(self, parent, id=wx.ID_ANY):
        figure = self.figure = Figure()
        #self.axes = None
        FigureCanvas.__init__(self, parent, id, figure)

    def clear(self, types="all"):
        if types=="plots":
            for axes in self.figure.axes:
                axes.clear()
        elif types in ("figures","all"):
            self.figure.clear()
            #self.axes = None
        self.draw()

    def ensure_axes(self, subplots):
        # to be called before plotting, in case "figures" or "all" were cleared
        # returns True if this is the first plot
        #if self.axes is None:
            # default: 1x1 grid, first subplot
        print ("ensure_axes", subplots)
        return self.figure.add_subplot(*subplots)
        #return self.axes

    def cleanup(self):
        if not _USE_AGG:
            if self.renderer.gc.gfx_ctx:
                self.renderer.gc.gfx_ctx.Destroy()
                self.renderer.gc.gfx_ctx = None

    ####################################################################################################################
    # some internals...
    def _onSize(self, evt):
        # slightly extended re-implementation of FigureCanvasWxAgg to ensure fixed size if required
        # if your canvas size is variable or it's on a scrolled panel, then you may delete it
        print("Sizes", self.GetClientSize(), self.GetSize(), "min/max:", self.GetMinSize(), self.GetMaxSize())
        sz = self.GetParent().GetSizer()
        if sz: si = sz.GetItem(self)
        if not sz or not si or (not si.Proportion and not si.Flag & wx.EXPAND):
            # not managed by a sizer at all or with a constant size
            size = self.GetMinSize()
        else:
            # variable size, managed by a wx.Sizer
            size = self.GetClientSize()
        if getattr(self, "_width", None) and size==(self._width,self._height):
            # no change in size
            return
        self._width, self._height = size
        # Create a new, correctly sized bitmap
        self.bitmap = wxc.EmptyBitmap(self._width, self._height)

        self._isDrawn = False

        if self._width <= 1 or self._height <= 1:
            return  # Empty figure

        dpival = self.figure.dpi
        winch = self._width / dpival
        hinch = self._height / dpival
        self.figure.set_size_inches(winch, hinch, forward=False)

        # Rendering will happen on the associated paint event
        # so no need to do anything here except to make sure
        # the whole background is repainted.
        self.Refresh(eraseBackground=False)
        FigureCanvas.resize_event(self)
