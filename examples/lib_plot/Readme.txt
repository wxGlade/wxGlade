This (Python only) example shows how to use the Custom Widget to include a
wx.lib.plot widget.
This widget allows plotting of data.
The example uses the numpy numerical library.
It does not work with wxPython 3.0.2 due to this bug:
 http://trac.wxwidgets.org/ticket/16767#no1


 - the property "Class" is set to "wx.lib.plot.PlotCanvas"
 - on the "Code" page, the first property ensures that wx.lib.plot is imported
 - on the "Code" page, the last property initializes a list for storage of plot lines
 - the "Plot" button has an event handler to create the plots
 - the "Clear" button has a lambda function as event handler to clear the plot
