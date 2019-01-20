
.. |br| raw:: html

   <br/>


.. |green| raw:: html

   <font color="green">


.. |blue| raw:: html

   <font color="blue">

.. |endcolor| raw:: html

   </font>


################
wx Basics
################


*******
Concept
*******

wxPython is a Wrapper around the wxWidgets GUI library, which is written in C++.

You may want to visit these pages for more information:
 * The wxPython home page: https://wxpython.org/
 * An overview including a `Hello World` application example: https://wxpython.org/pages/overview/
 * A more comprehensive `Getting Started` guide: https://wiki.wxpython.org/Getting%20Started
 * The wxWidgets home page: https://www.wxwidgets.org/



The wxGlade documentation is intended to be readable without previous wx knowledge.
It will teach you how to create windows, handle events and how to integrate with your 'business logic' code.
Sooner or later you will have to look into the wx documentation, though.



************************
Sizers (Layout Managers)
************************


With wxWidgets/wxPython and similar toolkits, usually controls are not placed at pixel positions on their windows, but the layout of a window is managed by *sizers*.
 - There are *horizontal box sizers*, *vertical box sizers* and *grid sizers*.
 - The *box sizers* may have a label and a box around them. In that case they're called *static box sizers*.
 - Each sizer and contained sizer items can be fixed size or grow to fill the available space, e.g. when the window is resized.

Sizer Examples
==============



.. |vertical| image:: images/vertical.png
    :width: 120

.. |horizontal| image:: images/horizontal.png
    :width: 280

.. |horizontal2| image:: images/horizontal2.png
    :width: 280

.. |horizontal3| image:: images/horizontal3.png
    :width: 280

.. |static_horizontal| image:: images/static_horizontal.png
    :width: 280

.. |grid1| image:: images/grid1.png
    :width: 300

.. |grid2| image:: images/grid2.png
    :width: 300

.. |flex_grid| image:: images/flex_grid.png
    :width: 300

.. |gridbag| image:: images/gridbag.png
    :width: 300

.. list-table::
   :widths: 60 40
   :header-rows: 0
   :align: center

   * - **Vertical BoxSizer**

       * This sizer stacks the controls it manages one above the other.
       * This example shows three buttons arranged in this way.

     - |vertical| 
   * - **Horizontal BoxSizer**

       * This sizer arranges its controls alongside of one another.
       * This example shows three button arranged in this way.

     - |horizontal| 
   * - **Allowing controls to grow and/or expand**

       * Controls in a horizontal sizer can "grow" horizontally, or "expand" vertically in response to changes in window size.
       * In this three button example, using a horizontal BoxSizer:

        * The middle button has been allowed to grow horizontally.
        * The third button has been allowed to expand vertically.

     - |horizontal2| 
   * - **Alignment of controls**

       * The alignment of controls within a sizer can also be specified. |br|
         This example also uses a horizontal BoxSizer to specify that:

        * The first button is "top" aligned.
        * The middle button is "bottom" aligned.
        * The third button has been given a border that provides some space around it on all sides.

     - |horizontal3|
   * - **StaticBoxSizer**

       * This sizer puts a border around its edges and provides a label to describe its contents.
       * In this example, a horizontal StaticBoxSizer has been used to contain the three buttons
         (which have the same properties set as those in the previous example). |br|
       * StaticBoxSizers can be either horizontal or vertical, just like their "non-static" equivalents.

     - |static_horizontal|
   * - **GridSizer**

       * GridSizers arrange their contained controls in a grid made up of equally sized rows and columns.
       * In this example, the grid has been set to two rows and two columns.
       * Each cell in the example contains a button, which has been centred in the cell.

     - |grid1|
   * - **Expanding, growing and aligning in a grid cell**

       In this example:
        * The first button has been aligned LEFT.
        * The second button has been aligned BOTTOM.
        * The third button has been allowed to expand.
        * The fourth button has been aligned RIGHT and CENTER.

     - |grid2|
   * - **FlexGridSizer**

       * The FlexGridSizer allows a little more flexibility over cell sizes by allowing individual
         rows and columns to grow and/or expand.
       * In this example, the first column has been allowed to grow horizontally and
         the second one allowed to grow vertically. |br|
         Note that this becomes obvious when the window is resized by the user.

     - |flex_grid|
   * - **GridBagSizer**

       * This example uses a 3x3 grid and five buttons.
       * It shows how a GridBagSizer can allow controls to span multiple columns and/or rows.
       * All the buttons have their EXPAND property set so that they fill all the space in the cell(s).

     - |gridbag|



Example application: Calculator window
======================================

.. |Calculator_06_sizers| image:: images/Calculator_06_sizers.png
   :width: 200
   :align: middle

.. |Calculator_06_tree| image:: images/Calculator_06_tree.png
   :width: 200
   :align: middle

.. |sizer_h| image:: images/sizer_h.png

.. |sizer| image:: images/sizer.png


+----------------------------------------------------------------------+
|  .. image:: images/Calculator_06_preview.png                         |
|     :width: 200                                                      |
+----------------------------------------------------------------------+

This window is managed by |green| **one vertical box sizer with six slots** |endcolor| for the five rows, plus a horizontal line and five |blue| **horizontal box sizers** |endcolor| for the horizontally arranged controls they contain (e.g. one label and one button):

.. list-table::
   :header-rows: 0
   :align: center

   * - The *Design* and *Preview* windows look like this, but without the colored frames for the horizontal and vertical sizers: |br|
       |Calculator_06_sizers| |br|
       Each blue frame is a horizontal sizer with two slots each.
       The (invisible) borders between slots are indicated by dashed lines. |br| |br|
       The inputs for Value 1 and 2 are set to grow horizontally;
       the Result output is growing horizontally and EXPANDing vertically. 
     - In the *Tree* window, you can see the hierarchical structure: |br|
       |Calculator_06_tree| |br|
       Note that horizontal and vertical sizers are |br| visualized with different icons: |sizer_h| |sizer| .


Later we'll have a look at alternative structures which allow better alignment of the fields.

.. note::

    * For your own projects, always use the simplest available sizers.
      Usually you will need mainly box sizers and maybe one or two FlexGridSizers.
    * Use nested sizers to match the hierarchical / logical structure of your project. This will make it easy
      to re-arrange things to find the best user interface.
    * Never ever try to use a GridBagSizer as main sizer of a window trying to resemble pixel placement or
      Tkinter's grid geometry manager. This is a mess to create and maintain.
      Actually, a GridBagSizer is almost never needed.



.. |wPalette| image:: images/wPalette.png
   :width: 200
   :align: middle


wxGlade Requirements and Restrictions
=====================================

The user interface and internal data structures of wxGlade impose some restrictions on the structure of a window.
A frame or panel cannnot have a widget as direct child. They always need a toplevel sizer first. So don't be surprised to see constructions like these:

 - frame -> sizer with single slot -> panel -> sizer ....
 - frame -> sizer with single slot -> notebook -> ...

On the other hand, a notebook or a splitter can have widgets as direct children.
