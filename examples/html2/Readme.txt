This (Python only) example shows how to use the Custom Widget to include a
wx.html2.WebView widget.
This widget uses platform specific rendering engines to support HTML, Javascript
and CSS.

This widget is not available on wxPython 2.8.


 - the property "Class" is set to "wx.html2.WebView"
 - on the "Widget" page, the "Custom constructor" is set to wx.html2.WebView.New
   as the class is not to be instantiated, but a factory function needs to be
   called
 - on the "Code" page, the first property ensures that wx.html2 is imported

The buttons that are included, have lambda functions as event handler to
implement some basic navigation capabilities.
