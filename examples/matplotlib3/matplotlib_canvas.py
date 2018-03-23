#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import wx

from matplotlib.figure import Figure

_USE_AGG = True

if _USE_AGG :
    from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
else:
    from matplotlib.backends.backend_wx import FigureCanvasWx as FigureCanvas



class MatplotlibCanvas(FigureCanvas):
    def __init__(self, parent, id=wx.ID_ANY):
        figure = self.figure = Figure()
        FigureCanvas.__init__(self, parent, id, figure)

    def cleanup(self):
        if not _USE_AGG:
            # avoid crash
            if self.renderer.gc.gfx_ctx:
                self.renderer.gc.gfx_ctx.Destroy()
                self.renderer.gc.gfx_ctx = None

