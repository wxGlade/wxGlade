This (Python only) example shows how to use the Custom Widget to include a
Matplotlib canvas.
This widget allows plotting of data, images and geometrical shapes.
Matplotlib requires at least wxPython 3.0.2 and other libraries.

The functionality is the same as with the matplotlib example, but the
structure is more readable and easier to maintain:

 - the property "Class" is set to "matplotlib_canvas.MatplotlibCanvas"
 - the file "matplotlib_canvas.py" contains the class MatplotlibCanvas
   with the required setup code
 - the required arguments for MatplotlibCanvas are just "parent" and "id",
   i.e. just as for any other wx widget;
   no additional arguments need to be defined on the "Widget" page
 - on the "Code" page, the first property ensures that matplotlib_canvas
   is imported
 - the "Plot" and "Clear" buttons have event handlers to create and
   clear the plots
