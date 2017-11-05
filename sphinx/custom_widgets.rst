
################
Custom Widget
################

.. |custom| image:: images/custom.png


Very often you may want to use a widget that is not supported by wxGlade.

For this, just insert a Custom Widget |custom| as placeholder.
You will be prompted for a class name. In the Design and Preview windows, just a placeholder will be shown.

**The *Properties* window for an example "ImagePanel" could look like this:**

.. |CustomWidgetPropertiesCommon| image:: images/CustomWidgetPropertiesCommon.png
   :width: 300
   :align: middle

.. |CustomWidgetProperties| image:: images/CustomWidgetProperties.png
   :width: 300
   :align: middle

.. |CustomWidgetPropertiesCode| image:: images/CustomWidgetPropertiesCode.png
   :width: 300
   :align: middle


   
.. |br| raw:: html

   <br/>


.. list-table::
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **Properties -> Common:** |br| |br|
       Property "Class" is the name of the class that will be instantiated.
     - |CustomWidgetPropertiesCommon| 
   * - **Properties -> Widget:** |br| |br|
       The arguments for instantiation of the class.
       |br| Usually you will at least enter $parent$ here.
     - |CustomWidgetProperties| 
   * - **Properties -> Code:** |br| |br|
       Enter the import statement here, if required.
     - |CustomWidgetPropertiesCode| 


**The generated code for this example will look like this:**

**Import** statement at the head of the file::

    # begin wxGlade: extracode
    from ImagePanel import ImagePanel
    # end wxGlade

**Instantiation** of the class::

    self.panel_image_left = ImagePanel(self.main_panel, wx.ID_ANY)

The Arguments $parent and $id were replaced with the required code. There are two more magic arguments: $width and $height
