.. |br| raw:: html

   <br/>

################
Custom Widget
################

.. |custom| image:: images/custom.png



Sometimes you may want to use a widget that is not supported by wxGlade.

For this, just insert a CustomWidget |custom| as placeholder.
|br|
You will be prompted for a class name. In the *Design* and *Preview* windows, just a placeholder will be shown.

**E.g. the Properties window to integrate "wx.html.HtmlWindow" could look like this:**

.. |CustomWidgetPropertiesCommon| image:: images/CustomWidgetPropertiesCommon.png
   :width: 300
   :align: middle
   :alt: Instance class: wx.html.HtmlWindow

.. |CustomWidgetProperties| image:: images/CustomWidgetProperties.png
   :width: 300
   :align: middle

.. |CustomWidgetPropertiesCode| image:: images/CustomWidgetPropertiesCode.png
   :width: 300
   :align: middle
   :alt: import wx.html



.. list-table::
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **Properties -> Common:** |br| |br|
       Property "Instance Class" is the name of the class that will be instantiated. |br|
       In this example: :guilabel:`wx.html.HtmlWindow`
     - |CustomWidgetPropertiesCommon| 
   * - **Properties -> Widget:** |br| |br|
       The arguments for instantiation of the class.
       |br| Usually you will at least enter :guilabel:`$parent` here.
     - |CustomWidgetProperties| 
   * - **Properties -> Code:** |br| |br|
       Enter the import statement here, if required.
     - |CustomWidgetPropertiesCode| 


**The generated code for this example will look like this:**

**Import** statement at the head of the file::

    # begin wxGlade: extracode
    import wx.html
    # end wxGlade

**Instantiation** of the class::

    self.html = wx.html.HtmlWindow(self.panel_1, wx.ID_ANY)

The Arguments :guilabel:`$parent` and :guilabel:`$id` were replaced with the required code. There are two more magic arguments: :guilabel:`$width` and :guilabel:`$height`.

The files can be found in the folder ``wxglade/examples/html``:
 * `html_example.wxg <../../examples/html/html_example.wxg>`_
 * `html_example.py <../../examples/html/html_example.py>`_

Custom constructor
==================

For most applications you would probably be more interested in using :code:`wx.html2.WebView` as this will
display non-trivial HTML, JavaScript and CSS pages.

See ``wxglade/examples/html2`` for an example application.

The :code:`html2.WebView` class is a bit special as it is not to be instantiated directly.
Instead, :code:`wx.html2.WebView.New` is to be called. In the .wxg file this is handled by setting
*Properties -> Widget -> Custom constructor ->* to :guilabel:`wx.html2.WebView.New`.



Example 'matplotlib': matplotlib canvas, quick and dirty
========================================================

The above example was rather simple to implement as the class :code:`ImagePanel` did not require any extra
arguments or code. It was just called with the parent window and the default ID as arguments. |br|
Sometimes, the widget to be used needs some things to be set up before it can be created. |br|
E.g. if you want to use the matplotlib :code:`FigureCanvas`, this needs a :code:`Figure` instance to be created and supplied as argument. To use it from within wxGlade, you may write a wrapper class around it or enter the required extra code in wxGlade.


This code creates a matplotlib canvas and plots a sine function::

    import matplotlib
    from matplotlib.figure import Figure
    from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas

    # create the figure with a single plot and create a canvas with the figure
    figure = self.matplotlib_figure = Figure()
    self.matplotlib_axes = figure.add_subplot(111)  # 1x1 grid, first subplot
    self.matplotlib_canvas = FigureCanvas(self.panel_1, wx.ID_ANY, figure)
   
   # draw a sine function
   import numpy
   x = numpy.arange(0,10,0.1)
   y = numpy.sin(x)
   self.matplotlib_axes.plot(x, y)
   # show the plot
   self.matplotlib_canvas.draw()


This example shows how to use the wxGlade CustomWidget |custom| to include a matplotlib canvas in your application:

.. |matplotlib_class| image:: images/matplotlib_class.png
   :width: 460
   :align: middle
   :alt: Instance class: FigureCanvas

.. |matplotlib_arguments| image:: images/matplotlib_arguments.png
   :width: 460
   :align: middle
   :alt: Class instantiation arguments

.. |matplotlib_code| image:: images/matplotlib_code.png
   :width: 460
   :align: middle
   :alt: extra import and setup code

.. |matplotlib_screenshot| image:: images/matplotlib_screenshot.png
   :width: 320
   :align: middle
   :alt: the running application


.. list-table::
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **Properties -> Common:** |br| |br|
       The class :guilabel:`FigureCanvas` will be instantiated.
     - |matplotlib_class| 
   * - **Properties -> Widget:** |br| |br|
       The class will be instantiated with the arguments :guilabel:`$parent`, :guilabel:`$id` and :guilabel:`figure`. |br|
       The argument :guilabel:`figure` is non-standard.
       It will be defined in *Properties* -> Code.
     - |matplotlib_arguments| 
   * - **Properties -> Code:** |br| |br|
       The import statement will make the required classes and modules available on module level. |br| |br|
       Right before class instantiation, a Figure instance with a single subplot will be created.
     - |matplotlib_code| 
   * - **Result:** |br| |br|
       The Python file has a very basic function plotter in it's event handler for the "Plot" button.
     - |matplotlib_screenshot| 


The files can be found in the folder ``wxglade/examples/matplotlib``:
 * `matplotlib_example.wxg <../../examples/matplotlib/matplotlib_example.wxg>`_
 * `matplotlib_example.py <../../examples/matplotlib/matplotlib_example.py>`_

To run the example, you need to have numpy and matplotlib installed, of course.



The above approach is OK for a quick & dirty prototype. The advantage is that all code is contained within wxGlade
and therefore you may just copy it from one project or window to another.

Once things get more complex, it's better to implement a custom class which does not require such extra code.
The resulting code will be cleaner and also easier to maintain and extend.


Example 'matplotlib2': matplotlib canvas, well structured
=========================================================

This example has the same functionality, but shows how the code can be structured in a more readable and maintainable way.

 * ``matplotlib_canvas.py``: a class :code:`MatplotlibCanvas` as layer between :code:`Matplotlib.FigureCanvas`
 * ``matplotlib_GUI.py``: the GUI code, generated from wxGlade file ``matplotlib_GUI.wxg``
 * ``matplotlib_example.py``: the main file with the `business logic`

The files can be found in the folder ``wxglade/examples/matplotlib2``:
 * `matplotlib_canvas.wxg <../../examples/matplotlib2/matplotlib_canvas.wxg>`_
 * `matplotlib_GUI.py <../../examples/matplotlib2/matplotlib_GUI.py>`_
 * `matplotlib_GUI.wxg <../../examples/matplotlib2/matplotlib_GUI.wxg>`_


Example 'matplotlib3': matplotlib canvas, comprehensive embedding example
=========================================================================

This example shows most of the building blocks that you will need for integration of a Matplotlib canvas.

It shows how to plot and draw, zoom and drag, select items, export images and so on.
It shows also how to draw graphics primitives like lines or circles on the whole canvas, i.e without plot axes.


This example is only for Python 3 and Matplotlib versions >=2.2.

 * ``matplotlib_canvas.py``: a class :code:`MatplotlibCanvas` as layer between :code:`Matplotlib.FigureCanvas`
 * ``matplotlib_GUI.py``: the GUI code, generated from wxGlade file ``matplotlib_GUI.wxg``
 * ``matplotlib_example.py``: the main file with the `business logic`

The files can be found in the folder ``wxglade/examples/matplotlib2``:
 * `matplotlib_canvas.wxg <../../examples/matplotlib3/matplotlib_canvas.wxg>`_
 * `matplotlib_GUI.py <../../examples/matplotlib3/matplotlib_GUI.py>`_
 * `matplotlib_GUI.wxg <../../examples/matplotlib3/matplotlib_GUI.wxg>`_


More Examples
=============

In subfolders of ``wxglade/examples/matplotlib2`` there are more examples:

 * ``html2``: use :code:`wx.html2.WebView` to display HTML with Javascript and CSS (not available on wxPython 2.8)
 * ``lib_plot``: use :code:`wx.lib.plot.PlotCanvas` for plotting
 * ``SpeedMeter``: demonstrate :code:`wx.lib.agw.speedmeter.SpeedMeter`
 * ``py_shell``: show how to embed a Python shell :code:`wx.py.shell.Shell`
