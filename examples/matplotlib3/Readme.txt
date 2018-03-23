This example shows how to embed a matplotlib canvas and demonstrates
some building blocks required for a real-world application.

The main program is "matplotlib_example.py"


This example does not work with wxPython 2.8.
A matplotlib version >= 2.2 is required.



########################

 - the property "Class" is set to "wx.html2.WebView"
 - on the "Widget" page, the "Custom constructor" is set to wx.html2.WebView.New
   as the class is not to be instantiated, but a factory function needs to be
   called
 - on the "Code" page, the first property ensures that wx.html2 is imported

The buttons that are included, have lambda functions as event handler to
implement some basic navigation capabilities.
