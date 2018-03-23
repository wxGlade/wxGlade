This example shows how to embed a matplotlib canvas and demonstrates
some building blocks required for a real-world application.
GUI elements are connected to the canvas via the Toolmanager, which
has been part of recent Matplotlib releases. The example does not yet
include the standard Matplotlib buttons/toolbar.

This example is Python 3 only. It should work with Python 2.7 with only a
few changes, though.
A matplotlib version >= 2.2 is required.


 - the property "Class" is set to "matplotlib_canvas.MatplotlibCanvas"
 - on the "Code" page, the first property ensures that matplotlib_canvas is imported

The main program is "matplotlib_example.py"
This contains also the drawing code to make things easier to understand, especially
for the differences when using canvas (pixel) coordinates vs. axis coordinates.
For your own program, you should better move the drawing code to e.g.
matplotlib_canvas.MatplotlibCanvas instead of mixing GUI and Matplotlib code.
