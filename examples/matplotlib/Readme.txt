This (Python only) example shows how to use the Custom Widget to include a
Matplotlib canvas.
This widget allows plotting of data, images and geometrical shapes.
Matplotlib requires at least wxPython 3.0.2 and other libraries.

 - the property "Class" is set to "FigureCanvas"
 - on the "Widget" page, the argument "figure" is added
 - on the "Code" page, the first property ensures that matplotlib, Figure
   and FigureCanvas are imported
 - on the "Code" page, the second property creates the Figure instance and adds
   a plot already
 - the "Plot" button has an event handler to create the plots
 - the "Clear" button has a lambda function as event handler to clear the plot

All widget setup code is in matplotlib_example.wxg.
This approach is OK for a quick & dirty prototype.
The advantage is that all code is contained within wxGlade and therefore you may
just copy it from one project or window to another.

Please see matplotlib2 for a better and more maintainable structure.
