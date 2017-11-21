.. |br| raw:: html

   <br/>

################
Custom Widget
################

.. |custom| image:: images/custom.png


Very often you may want to use a widget that is not supported by wxGlade.

For this, just insert a Custom Widget |custom| as placeholder.
|br|
You will be prompted for a class name. In the *Design* and *Preview* windows, just a placeholder will be shown.

**The Properties window for an example "ImagePanel" could look like this:**

.. |CustomWidgetPropertiesCommon| image:: images/CustomWidgetPropertiesCommon.png
   :width: 300
   :align: middle

.. |CustomWidgetProperties| image:: images/CustomWidgetProperties.png
   :width: 300
   :align: middle

.. |CustomWidgetPropertiesCode| image:: images/CustomWidgetPropertiesCode.png
   :width: 300
   :align: middle



.. list-table::
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **Properties -> Common:** |br| |br|
       Property "Class" is the name of the class that will be instantiated. |br|
       In this example: ``ImagePanel``
     - |CustomWidgetPropertiesCommon| 
   * - **Properties -> Widget:** |br| |br|
       The arguments for instantiation of the class.
       |br| Usually you will at least enter ``$parent`` here.
     - |CustomWidgetProperties| 
   * - **Properties -> Code:** |br| |br|
       Enter the import statement here, if required. |br| |br|
       For our example, ``import ImagePanel`` could be used, |br|
       but then the "Class" property must be fully qualified:|br|
       ``ImagePanel.ImagePanel``
     - |CustomWidgetPropertiesCode| 


**The generated code for this example will look like this:**

**Import** statement at the head of the file::

    # begin wxGlade: extracode
    from ImagePanel import ImagePanel
    # end wxGlade

**Instantiation** of the class::

    self.panel_image_left = ImagePanel(self.main_panel, wx.ID_ANY)

The Arguments ``$parent`` and ``$id`` were replaced with the required code. There are two more magic arguments: ``$width`` and ``$height``.


Example: matplotlib canvas
==========================

The above example was rather simple to implement as the class ``ImagePanel`` did not require any extra
arguments or code. It was just called with the parent window and the default ID as arguments. |br|
Sometimes, the widget to be used needs some things to be set up before it can be created. |br|
E.g. if you want to use the matplotlib ``FigureCanvas``, this needs a ``Figure`` instance to be created and supplied as argument. To use it from within wxGlade, you may write a wrapper class around it or enter the required extra code in wxGlade.


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


This example shows how to use the wxGlade Custom Widget |custom| to include a matplotlib canvas in your application:

.. |matplotlib_class| image:: images/matplotlib_class.png
   :width: 460
   :align: middle
   :alt: Class name

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
       The class ``FigureCanvas`` will be instantiated.
     - |matplotlib_class| 
   * - **Properties -> Widget:** |br| |br|
       The class will be instantiated with the arguments ``$parent``, ``$id`` and ``figure``. |br|
       The argument ``figure`` is non-standard.
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
