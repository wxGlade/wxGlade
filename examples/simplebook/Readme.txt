This example shows how to use the "Base class" property to turn a Notebook
into a Simplebook, i.e. a notebook without tabs.

The Simplebook class is not available in wxWidgets 2.8 or wxPython Classic.

 - the property "Class" is set to "MySimplebook"; therefore a class is created
 - the property "Base class" is set to "wx.Simplebook"; therefore the class is
   derived from wx.Simplebook instead of wx.Notebook
