class MyStartApp(wx.App):
    def OnInit(self):
        appframe = MyAppFrame(None, wx.ID_ANY, "")
        self.SetTopWindow(appframe)
        appframe.Show()
        return True

# end of class MyStartApp

if __name__ == "__main__":
    gettext.install("myapp") # replace with the appropriate catalog name

    myapp = MyStartApp(0)
    myapp.MainLoop()
