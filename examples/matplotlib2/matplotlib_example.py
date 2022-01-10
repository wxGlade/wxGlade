#!/usr/bin/env python
# -*- coding: UTF-8 -*-



import wx
import matplotlib_GUI


class MyFrame(matplotlib_GUI.MyFrame):

    def on_button_plot(self, event):
        import numpy
        # check arguments and indicate errors
        xmin = xmax = step = None
        try:
            xmin = float( self.text_xmin.GetValue() )
            self.text_xmin.SetBackgroundColour(wx.WHITE)
        except:
            self.text_xmin.SetBackgroundColour(wx.RED)
        try:
            xmax = float( self.text_max.GetValue() )
            self.text_max.SetBackgroundColour(wx.WHITE)
        except:
            self.text_max.SetBackgroundColour(wx.RED)
        try:
            step = float( self.text_xstep.GetValue() )
            self.text_xstep.SetBackgroundColour(wx.WHITE)
        except:
            self.text_xstep.SetBackgroundColour(wx.RED)

        if xmin is None or xmax is None or step is None: return

        # plot y = f(x)
        x = numpy.arange(xmin, xmax, step)
        # build globals with some functions
        g = {}
        for name in ["sin","cos","tan","ufunc","square"]:
            g[name] = getattr(numpy, name)
        y = eval(self.text_function.GetValue(), g, {"numpy":numpy, "x":x})
        self.matplotlib_canvas.axes.plot(x,y)
        self.matplotlib_canvas.draw()
        event.Skip()

    def on_button_clear(self, event):
        self.matplotlib_canvas.clear()
        event.Skip()


class MyApp(wx.App):
    def OnInit(self):
        self.frame = MyFrame(None, wx.ID_ANY, "")
        self.SetTopWindow(self.frame)
        self.frame.Show()
        return True


if __name__ == "__main__":
    app = MyApp(0)
    app.MainLoop()
