
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


`wxPython <https://wxpython.org/>`_ is a Wrapper around the `wxWidgets <https://www.wxwidgets.org/>`_ GUI library, which is written in C++.

See here for an overview, including Hello World application example: https://wxpython.org/pages/overview/

Here you can find a more comprehensive Getting Started guide: https://wiki.wxpython.org/Getting%20Started


The wxGlade documentation is intended to be readable without previous wx knowledge.
It will teach you how to create windows, handle events and how to integrate with your 'business logic' code.
Sooner or later you will have to look into the wx documentation, though.



************************
Sizers (Layout Managers)
************************


With wxWidgets/wxPython and similar toolkits, usually controls are not placed at pixel positions on their windows, but the layout of a window is managed by *sizers*.
 - There are *horizontal box sizers*, *vertical box sizers* and *grid sizers*.
 - The *box sizers* may have a label and a box around them. In this case they're called *static box sizers*.
 - Each sizer and contained sizer items can be fixed size or grow to fill the available space, e.g. when the window is resized.

Examples
========



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
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **vertical BoxSizer**

       * with three buttons |br|
         i.e. three slots, filled |br|
         with a button each

     - |vertical| 
   * - **horizontal BoxSizer**

       * with three buttons

     - |horizontal| 
   * - same sizer,

       * but one button **growing** (horizontally) and |br|
         one **expanding** (vertically)

     - |horizontal2| 
   * - same sizer,

       * but one button **aligned** top,
       * one bottom and
       * one with a border

     - |horizontal3|
   * - same as **horizontal StaticBoxSizer**
     - |static_horizontal|
   * - **GridSizer**

       * with two rows and two columns,
       * all columns and all rows have the same size,
       * all buttons are centered

     - |grid1|
   * - same,

       * with one button aligned left,
       * one aligned bottom,
       * one expanding and
       * one aligned right/center

     - |grid2|
   * - same as **FlexGridSizer**

       * with growing column #1 and
       * one growing row #2

     - |flex_grid|
   * - **GridBagSizer**

       * with 3x3 cells and five buttons;
       * three buttons are spanning multiple rows/cols

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

This window is managed by |green| **one vertical box sizer with six slots** |endcolor| for the five rows plus a horizontal line and five |blue| **horizontal box sizers** |endcolor| for e.g. one label and one button:

   
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


.. |wPalette| image:: images/wPalette.png
   :width: 200
   :align: middle


wxGlade Requirements / Restrictions
===================================

The user interface and internal data structures of wxGlade impose some restrictions on the structure of a window.
A frame or panel can't have a widget as direct child; they always need a toplevel sizer first. So don't be surprised to see constructions like:

 - frame -> sizer with single slot -> panel -> sizer ....
 - frame -> sizer with single slot -> notebook -> ...

On the other hand, a notebook or a splitter can have widgets as direct children.
