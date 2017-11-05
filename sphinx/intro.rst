.. |br| raw:: html

   <br/>


################
wxGlade Overview
################

***************
Running wxGlade
***************

**Start:**
To run wxGlade, start the wxglade.py or wxglade.pyw program file with the Python interpreter.

**Other Prerequisites:**
On Windows, the win32 extensions should be installed: https://sourceforge.net/projects/pywin32/

**Supported Python and wxPython Versions:**
 * The *classic* wxPython versions 2.8 and 3.0 are supported as well as wxPython *Phoenix* running under Python 3
 * Phoenix plus Python 2 will probably also work, but this is not tested at all. wxPython 2.8 is generally less tested than the more recent versions.
 * When running Python 3 + Phoenix or Python 2 + Classic, the generated Python code will not be compatible to the other version.


***************
Program Windows
***************

wxGlade consists of five main windows:

(click on one of the images to see it full size)


.. |wPalette| image:: images/wPalette.png
   :width: 200
   :align: middle

.. |wTree| image:: images/wTree.png
   :width: 200
   :align: middle

.. |wProperties| image:: images/wProperties.png
   :width: 200
   :align: middle

.. |wDesign| image:: images/wDesign.png
   :width: 200
   :align: middle


.. list-table::
   :widths: 20 80
   :header-rows: 0
   :align: center

   * - |wPalette| 
     - Main **Palette** window: |br|
       This is the main window with the main window. |br|
       There is an icon for each window and control type that can be added to a project.
   * - |wTree|
     - **Tree** window: |br|
       This window visualizes and allows to edit the structure of the project with it's
       application, windows, sizers and controls. |br|
       On selection of an item, it's properties will be shown in the Properties window.
   * - |wProperties|
     - **Properties** window: |br|
       This allows to display and edit properties of applications, windows and controls. |br|
       To edit an item, select it in the Tree or Design window.            
   * - |wDesign|
     - **Design** window: |br|
       Visualizes the design of the window; it is not 100% identical to the final window. |br|
       To open this window, double-click a window's icon in the Tree window.
   * -
     - **Preview** window |br|
       A preview of the current window; should be 100% identical to the window as when it
       is used later on.  


.. seealso:: :doc:`reference` **for keyboard shortcuts and mouse actions.**

********
Examples
********

Before doing anything else, you may want to open an example file showing most of the supported controls:

* for wxPython 3.0 and Phoenix: :file:`examples/AllControls_30.wxg`
* for wxPython 2.8: :file:`examples/AllControls_28.wxg`

Each example file includes a frame with most of the supported widgets and also a menu bar, a tool bar and a status bar.

**Required steps:**
    * open the file by dragging it to the main window or by selecting the menu item File->Open
    * double-click "All_Widgets" in the *Tree* window to open the *Design* window and have a look around
    * create Python source code:
    
      * press :kbd:`Ctrl-G` or 
      * select the toplevel object "Application" and hit the button "Generate Source" in the *Properties* window
    
    * press :kbd:`F5` to create a preview
