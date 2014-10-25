if __name__ == "__main__":
    myapp = wx.PySimpleApp(0)
    wx.InitAllImageHandlers()
    appframe = MyAppFrame(None, wx.ID_ANY, "")
    myapp.SetTopWindow(appframe)
    appframe.Show()
    myapp.MainLoop()
