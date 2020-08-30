
.. |sizer_h| image:: images/sizer_h.png

.. |sizer| image:: images/sizer.png

.. |static_text| image:: images/static_text.png

.. |text_ctrl| image:: images/text_ctrl.png

.. |panel| image:: images/panel.png

.. |frame| image:: images/frame.png

.. |button| image:: images/button.png

.. |radio_box| image:: images/radio_box.png

.. |static_line| image:: images/static_line.png

.. |notebook| image:: images/notebook.png

.. |sizer_slot| image:: images/sizer_slot.png


.. |spacer| image:: images/spacer.png

.. |grid_sizer| image:: images/grid_sizer.png

.. |custom| image:: images/custom.png


.. |br| raw:: html

   <br/>

################################
Tutorial Project: Calculator
################################



*********************************************************************
Part 1: Create Calculator window and the first widget
*********************************************************************

In this section we will go through the steps to create the simple calculator window from the previous page. In later sections we will modify it to highlight certain features like other sizer types, menus and status bars.

Create the basic structure:
===========================

.. |NewFrame_CalculatorFrame| image:: images/NewFrame_CalculatorFrame.png
    :width: 30
    :alt: New Frame dialog

.. |NewSizer_CalculatorHorizontal| image:: images/NewSizer_CalculatorHorizontal.png
    :width: 30
    :alt: New Sizer dialog



1. Select "File->New" to **create a new file** and "File->Save" to save a .wxg file to a directory.

2. **Add the frame**:

   * click on the Frame icon |frame| on the "Windows" line of the *Palette* panels
   * as class name, enter :guilabel:`CalculatorFrame`: |NewFrame_CalculatorFrame|
   * leave "Add panel and sizer" checked
3. A frame with a panel and a sizer |sizer| with one slot is now visible in the *Tree* view and the *Design* window
   (there the slot is visualized by a hatched area)
4. To extend the **vertical box sizer** to six slots:

   * go to the slot in the *Tree* view or the *Design* window
   * use right mouse click "Add slots" or hit :kbd:`Ctrl-Shift-A`
   * select :guilabel:`5` and hit :guilabel:`OK`
   * alternatively you may hit :kbd:`Ctrl-A` five times
6. Now add a **horizontal box sizer** to the first slot:

   * in the *Palette* on the "Sizer" line click on the BoxSizer icon |sizer| to start the placement
   * in the *Tree* view or the *Design* window, click on the first slot to place the sizer there
   * a dialog will open
   * set "Orientation"  to :guilabel:`Horizontal` and the number of slots to :guilabel:`2`  |NewSizer_CalculatorHorizontal|
7. Place a **StaticText** |static_text| as label (found on the "Static" line of *Palette*) in the top left slot.
   You will be asked to enter a label text. Enter :guilabel:`Value 1:` for this.
8. Place a **TextControl** |text_ctrl| (found on the "Inputs" line) in the top right slot


.. note::
   It would be possible to place sizer and controls directly on the frame, but usually a panel is added first,
   as a frame is really just a frame around the other things. |br|
   Especially for Windows, always add a panel to have correct background colours and keyboard navigation
   between widgets (using the :kbd:`Tab` key)


Adjust properties:
==================

**Your design should now look like this:**

+-------------------------------------------+
| .. image:: images/Calculator_01.png       |
|    :width: 200                            |
+-------------------------------------------+

The window title is not meaningful. |br|
Also, layout and alignment need to be changed, but we'll change this later on.


**So, some properties of the label and the text need to be modified:**

1. Select the frame in the *Tree* or the *Design* window.
2. If not yet visible, make the *Properties* notebook of the main window visible by dragging the sashes
   (the separators between the three parts of the main window).
3. Go to *Properties -> Widget -> Title* and change it from :guilabel:`frame` to :guilabel:`Calculator`.
  (You could also edit the title directly in the *Tree* window by clicking into the label and/or pressing :kbd:`F2`.)
4. You may want to define a default size for the frame: |br|
   Go to *Properties -> Common -> Size* and set it to :guilabel:`400,300`. |br|
   Without this, the frame will be created with the minimum possible size to fit all controls.


This file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-01.wxg <../../examples/Calculator/Calculator-01.wxg>`_


*********************************************************************
Part 2: Add the remaining rows and widgets
*********************************************************************

We need almost the same combination of sizer+label+text for the second value and the result, so we'll copy the horizontal sizer including it's content from the first to the third and fifth slot of the vertical sizer.
Having done so, we will modify the label fields of the newly copied wxStaticText widgets to :guilabel:`Value 2` and :guilabel:`Result` (consistent with the layout for the calculator presented on the :doc:`wxbasics` page).
|br|
After that, we'll add the "Operator" radio box and the buttons.


There are several methods to copy:
==================================

.. |SizerHandle| image:: images/SizerHandle.png
    :width: 30
    :alt: Sizer Handle


* **Copy & Paste in Design Window:**

 * select the "handle" of the sizer, which is shown in red here: |SizerHandle|
 * then hit :kbd:`Ctrl-C` to copy the selected sizer (or use the context menu)
 * select the third, empty slot, where you want to place a copy; it will be hatched in blue
 * then hit :kbd:`Ctrl-V` (or use the context menu)
 * select the fith, empty slot
 * then hit :kbd:`Ctrl-V` (or use the context menu)

* **Copy & Paste in the Tree view of the main Window:**

 * select the sizer node
 * then hit :kbd:`Ctrl-C` to copy the selected sizer (or use the context menu)
 * select the third, empty slot, where you want to place a copy
 * then hit :kbd:`Ctrl-V` (or use the context menu)
 * select the fith, empty slot
 * then hit :kbd:`Ctrl-V` (or use the context menu)

You may of course copy in the *Design* window and paste into the *Tree* or vice versa.
 
* **Drag & Drop in Design Window:**

 * hold the :kbd:`Ctrl` key and drag the sizer handle to the empty slot

* **Drag & Drop in the Tree view of the main Window:**

 * hold the :kbd:`Ctrl` key and drag the sizer node / icon in the *Tree* view to the empty slot


If you don't hold the :kbd:`Ctrl` key, the item will not be copied, but moved.

You may drag items between different windows, e.g. from *Design* window to the *Tree* view or to another
running wxGlade instance.



Further Editing
===============

The TextCtrl adjacent to the "Result" StaticText should display multiple lines of text and should not be editable.
To modify accordingly, just follow these steps:

 1. Select the text control :guilabel:`txt_ctrl_3` |br| (depending on the exact steps of editing, the name might be slightly different)
 2. Go to *Properties -> Widget*
 3. In the section "Style", enable :guilabel:`wxTE_MULTILINE` and :guilabel:`wxTE_READONLY`
 4. Go to *Properties -> Common*
 5. Click the "..." button right to :guilabel:`Background` and select
    :guilabel:`System Colour: wxSYS_COLOUR_BTNFACE` or |br|
    another colour or just enter :guilabel:`wxSYS_COLOUR_BTNFACE` into the text control.

The last step is to have a grey background.
For single line read-only text controls, this is done automatically by wx, but not with multiple lines.


**In the next steps fill the remaining slots of the main sizer:**

* insert a two-slot horizontal sizer with a label :guilabel:`Operator:` and a radio box into SLOT 1 |radio_box|
* place a horizontal line into SLOT 3 |static_line|
* insert a two-slot horizontal sizer with two buttons into SLOT 5 |button|
* For the first button, select the checkbox *Properties -> Widget -> Default*. |br|
  This will make this button the default one, i.e. when the user hits Enter it will have the same effect as
  pressing this default button.


.. note::

    * When you want to place multiple elements of the same type,
      just hold the :kbd:`Ctrl` key when placing an element to stay in placement mode (on Mac OS use the :kbd:`Shift` key).
    * When you have placed an element in the wrong slot,
      just drag it to the right slot or delete it with the :kbd:`Del` key or the context menu.


**Define the options "+-*/" for the radio box by going to the "Widget" tab:**

.. |Calculator_Radio| image:: images/Calculator_Radio.png
    :height: 160

.. list-table::
   :header-rows: 0

   * -
       * Select the label in the *Tree* or the *Design* window.
       * Hit "Add" to add choices, modify them and then hit the "Apply" button.
       * See the bottom part of the screenshot:

     - |Calculator_Radio|



Change labels and names:
========================

After copying, the controls will have default values and names. Usually you have to change them.
You can do this in the *Properties* notebook or directly in the *Tree* view of the main window.
To edit in the *Tree* view, you can select and then click on the label or press :kbd:`F2`.

This needs to be done now:

* Set the names of the text controls to :guilabel:`text_value1`, :guilabel:`text_value2` and :guilabel:`text_result`.
* Set the name of the radio box to :guilabel:`radiobox_operator`
* Set the labels to :guilabel:`Value 2:` and :guilabel:`Result:`
* Set the name of the first button to :guilabel:`button_execute` and the label to :guilabel:`Execute`.
* Set the name of the second button to :guilabel:`button_reset` and the label to :guilabel:`Reset`.


For our example project where multiple labels and names have to be modified, it's much faster to do so in the *Tree*
window.

Example for editing in the *Tree* view of the main window:

  .. |Tree_Rename0| image:: images/Tree_Rename0.png

  .. |Tree_Rename1| image:: images/Tree_Rename1.png

* The name is :guilabel:`button_1` and the label is :guilabel:`button_1`
* The displayed text in the *Tree* window is :guilabel:`button_1: "button_1"`  |br|
  |Tree_Rename0|
* Change to :guilabel:`button_execute: "Execute"`  |br|
  |Tree_Rename1|


**The Tree structure of your design should now look like this:**

+-------------------------------------------+
| .. image:: images/Calculator_02_Tree.png  |
|     :width: 150                           |
+-------------------------------------------+

The file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-02.wxg <../../examples/Calculator/Calculator-02.wxg>`_

Final Steps: Layout
===================

**Your design should now look like this:**

+-------------------------------------------+
| .. image:: images/Calculator_02.png       |
|     :width: 150                           |
+-------------------------------------------+

So all elements are there, but the layout is not yet what we want to have:

1. We want the rows with the values, operators and buttons **not to grow vertically**:

   * select one of the horizontal sizers (e.g. :guilabel:`sizer_2` around :guilabel:`Value 1`)
   * set *Properties -> Layout -> Proportion* to :guilabel:`0`
   * select the next of the sizers (e.g. around :guilabel:`Operator`)
   * either set *Properties -> Layout -> Proportion* to :guilabel:`0`
     or just hit :kbd:`Ctrl-Y` to apply the last change to the selected sizer
   * do the same for the remaining sizers (e.g. around :guilabel:`Value 2` and the buttons)

2. We want the text controls to **grow horizontally** to fill the available space:

   * select one of the text controls (e.g. :guilabel:`text_value1`)
   * set *Properties -> Layout -> Proportion* to :guilabel:`1`
   * do the same for the other controls, either by modifying the Proportion properties
     or by hitting :kbd:`Ctrl-Y` to apply the last change to the selected control

3. The labels :guilabel:`Value 1`, :guilabel:`Operator` and :guilabel:`Value 2` should better
   **align vertically** with their text controls:

   * select label
   * set the checkbox *Properties -> Layout -> Alignment ->* :guilabel:`wxALIGN_CENTER_VERTICAL`
   * do the same for the other two labels

4. We want the Result text control to **fill the available vertical space**:

   * select text control
   * set the checkbox *Properties -> Layout -> Alignment ->* :guilabel:`wxEXPAND` 

5. We want the buttons at the bottom to be **centered**; so the layout of the sizer around them needs to be set:

   * select sizer
   * go to *Properties -> Layout -> Alignment*
   * de-select the checkbox :guilabel:`wxEXPAND`
   * select the checkbox :guilabel:`wxALIGN_CENTER_HORIZONTAL`

6. We want a small **border above and beyond** the horizontal line and around each of the buttons:

   * select the horizontal line :guilabel:`static_line_1`
   * set *Properties -> Layout -> Border* to :guilabel:`5`
   * uncheck *Properties -> Layout -> Border ->* :guilabel:`wxLEFT` and :guilabel:`wxRIGHT`
   * select the first button
   * set *Properties -> Layout -> Border* to :guilabel:`5`
   * do the same with the second button (e.g. by selecting it and then hitting :kbd:`Ctrl-Y`)

7. We want a small **border around** beyond the pairs of label and value:

   * select the horizontal sizer :guilabel:`sizer_2` around the first line
   * set *Properties -> Layout -> Border* to :guilabel:`4`
   * do the same with the sizers around :guilabel:`Operator` and :guilabel:`Value 2`
   (e.g. by selecting it and then hitting :kbd:`Ctrl-Y`)


At this point, it's time for a **preview**: |br|
Select "Preview" from the context menu or the main menu or from the *Properties* notebook when the frame is selected.
Alternatively, press :kbd:`F5`

+----------------------------------------------+
| .. image:: images/Calculator_03_preview.png  |
|     :width: 150                              |
+----------------------------------------------+

This file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-03.wxg <../../examples/Calculator/Calculator-03.wxg>`_.


If you are curious how a design turns into Python source code, you may have a look at the section :doc:`source_code` before going through the next sections.
|br|
Actually, "Preview" internally creates and executes Python source code.



Excursion: Layout properties: Proportion and Alignment->wxEXPAND:
=================================================================

.. |Calculator06_P0P0| image:: images/Calculator06_P0P0.png
    :width: 200
    :alt: Horizontal Sizer: Proportion 0, Text Ctrl: Proportion 0 -> no growth

.. |Calculator06_P1P0| image:: images/Calculator06_P1P0.png
    :width: 200
    :alt: Horizontal Sizer: Proportion 1 -> the sizer slot grows vertically, as it is within a vertical sizer

.. |Calculator06_P1P1| image:: images/Calculator06_P1P1.png
    :width: 200
    :alt: Text Ctrl: Proportion 1 -> the control grows horizontally, as it is within a horizontal sizer

.. |Calculator06_P1P1EX| image:: images/Calculator06_P1P1EX.png
    :width: 200
    :alt: Text Ctrl: EXPAND -> the control expands vertically, as it is within a horizontal sizer


The "Proportion" of :guilabel:`1` makes the element grow with a proportional factor of 1 when the horizontal sizer is growing. Please note that this growth happens only in the "main" direction of the sizer; the sizer is horizontal, so this change makes the text control grow in width.
|br|
For a growth in the other direction, :guilabel:`wxEXPAND` in the "Alignment" box would need to be checked.

|br|
You may try this to see the difference:


.. list-table::
   :header-rows: 0
   :align: center

   * - Horizontal Sizer: Proportion 0 |br|
       TextCtrl: Proportion 0
       |br| |br|
       The first row occupies the minimum height as |br|
       defined by the label and the text control.
       |br| |br|
       The text control occupies its minimum width, as defined.
     - |Calculator06_P0P0|

   * - Horizontal Sizer: **Proportion 1** |br|
       TextCtrl: Proportion 0 |br| |br|
       The first row consists of the horizontal sizer. |br|
       It grew vertically, as it is placed in a vertical sizer.
     - |Calculator06_P1P0|

   * - Horizontal Sizer: Proportion 1 |br|
       TextCtrl: **Proportion 1** |br| |br|
       The text control grew in width, as it is in a horizontal sizer.
       |br| |br|
     - |Calculator06_P1P1|

   * - Horizontal Sizer: Proportion 1 |br|
       TextCtrl: Proportion 1, **EXPAND** |br| |br|
       The text control expanded vertically, i.e. perpendicular |br|
       to the main direction of the surrounding horizontal sizer.
       |br| |br|
     - |Calculator06_P1P1EX|



*********************************************************************
Part 3: Add a Notebook
*********************************************************************

For many applications it's desirable to place the GUI elements on a notebook control.
For our calculator e.g. we may want to have the controls on the first page and a log on the second page:


+----------------------------------------------------------+
| .. image:: images/Calculator_Notebook_Design.png         |
|     :height: 120                                         |
|     :alt: Calculator with a notebook: Design Window      |
|                                                          |
| .. image:: images/Calculator_Notebook_Tree.png           |
|     :height: 120                                         |
|     :alt: Calculator with a notebook: Tree Window        |
+----------------------------------------------------------+

A notebook can be added to an empty sizer slot or as direct child of a frame, splitter or another notebook.
In our case, we want to keep the existing controls.
So we cut the panel sizer, add a notebook and then paste the panel to the first notebook page.

**Cut the toplevel panel:**

* Go to the toplevel panel :guilabel:`panel_1` in the *Tree* view or the *Design* window
  and select "Cut" or hit :kbd:`Ctrl-X`:

.. |Calculator_Notebook_CutPanel| image:: images/Calculator_Notebook_CutPanel.png
    :height: 120
    :alt: Right mouse click on panel -> "Cut"


.. list-table::
   :header-rows: 0

   * - |Calculator_Notebook_CutPanel|

The panel and it's children are now on the clipboard and can be pasted back later.
The frame does not have a child now; this is visualized by an empty slot. Note that in the Tree view
this :guilabel:`SLOT` looks different from a sizer slot:

.. |Calculator_Notebook_FrameSlot| image:: images/Calculator_Notebook_FrameSlot.png
    :height: 120
    :alt: Frame without child, visualized with just an empty SLOT


.. list-table::
   :header-rows: 0

   * - |Calculator_Notebook_FrameSlot|


**Add a notebook:**

* click on the Notebook icon |notebook| in the section "Containers" of the *Palette* panel to start placing a notebook
* click on the hatched area in the *Design* window or on the frame's slot |frame_slot| in the *Tree* control of the
  main window
* in the dialog, select :guilabel:`wxNB_TOP` to place the notebook tabs at the top;
  keep "Pages" :guilabel:`1` and uncheck :guilabel:`Add panels`.

The newly created notebook will have one page already, which does not contain anything.
It will have a default name like :guilabel:`notebook_1_pane_1`.



**Paste the panel to the notebook page:**

 * Go to the empty notebook page :guilabel:`[notebook_1_pane_1]` in the *Tree* view or the *Design* window
   and select "Paste" or hit :kbd:`Ctrl-V`: 

The panel including sizer and all controls is now the first page of the notebook:

.. |Calculator_Notebook_Pasted| image:: images/Calculator_Notebook_Pasted.png
    :height: 120

.. list-table::
   :header-rows: 0

   * - |Calculator_Notebook_Pasted|



**Add second notebook page and set the tab labels:**

.. |Calculator_Notebook| image:: images/Calculator_Notebook.png
    :height: 160

.. list-table::
   :header-rows: 0

   * -
       * click on the notebook |br|
         in the *Design* window or the *Tree* window
       * the *Properties* window should now display |br|
         the property editor for the notebook
       * go to the tab "Widget", click "Add" to add a page and |br|
         enter the headings for the notebook pages
       * click the "Apply" button

     - |Calculator_Notebook|


Alternatively, you may select "Add Notebook Tab" in the *Tree* view or the *Design* window and rename the tabs in 
the *Tree* view. E.g. by changing :guilabel:`[notebook_1_pane_1] panel_1` to :guilabel:`[Calculator] panel_1`.

Once you are familiar with the *Tree* view, this is the fastest way.

.. |Calculator_Notebook_Renaming| image:: images/Calculator_Notebook_Renaming.png
    :height: 120

.. |Calculator_Notebook_Adding| image:: images/Calculator_Notebook_Adding.png
    :height: 120

.. list-table::
   :header-rows: 0

   * - |Calculator_Notebook_Renaming|
   * - |Calculator_Notebook_Adding|



.. note::
  After adding a page to a notebook, this will contain a panel by default.
  You may delete this if you want e.g. to place a splitter on the page or e.g. a text control that should fill the
  whole page.


**Fill the second page:**

There are two ways to place the text control on the second page:

1. Add a single-slot sizer to the notebook page panel. Then place the text control in the sizer slot.
2. Remove the panel from the notebook page and add a text control instead.

Here we will take the second option:

* remove the panel (which probably has the name :guilabel:`notebook_1_pane_1`)
* the empty page will be displayed in the *Tree* view as :guilabel:`PAGE 1`
* add a text control to :guilabel:`PAGE 1`
* set the widget style to :guilabel:`wxTE_MULTILINE` and :guilabel:`wxTE_READONLY`
* set the widget background color to :guilabel:`wxSYS_COLOUR_BTNFACE`
* rename to :guilabel:`text_log`


This file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-04-Notebook.wxg <../../examples/Calculator/Calculator-04-Notebook.wxg>`_


*********************************************************************
Part 4: Use of Spacers
*********************************************************************

Sometimes you need to keep some space in a sizer, either with a fixed size or growable, e.g. to have a gap between two controls or to align two controls to the left and the right edge of a window or to align a control to the center.

To add a spacer, click the Spacer icon |spacer| and place it in a sizer slot.

In our example, we may place a spacer to the left and right of the operator radio box to center it:


.. |Calculator_Spacers_Design0| image:: images/Calculator_Spacers_Design0.png
    :width: 180
    :alt: Calculator with empty slots for spacers

.. |Calculator_Spacers_Design| image:: images/Calculator_Spacers_Design.png
    :width: 180
    :alt: Calculator with Spacers

.. |Calculator_Spacers_Tree| image:: images/Calculator_Spacers_Tree.png
    :width: 180
    :alt: Calculator with Spacers: Tree 

.. |Calculator_Spacers_Properties| image:: images/Calculator_Spacers_Properties.png
    :width: 180
    :alt: Spacer Properties

.. |Calculator_Spacers2| image:: images/Calculator_Spacers2.png
    :width: 180
    :alt: One spacer with height 10 and one with 20


.. list-table::
   :header-rows: 0
   :align: center

   * - 
       * add two empty slots to the left and right:  |br|
         (To add the slots, right-click on the radio box in the *Tree* or the *Design* window and select
         "Insert Slot before" and "Add Slot".)
     - |Calculator_Spacers_Design0|

   * - 
       * insert spacers |spacer| into these slots |br| (e.g. with "Width" :guilabel:`20` and
         "Height" :guilabel:`20`) 
       * set "Proportion" to :guilabel:`1` |br| 
         to make them grow 
     - |Calculator_Spacers_Design|

   * - |br| In the *Tree* view you can see the structure:
     - |Calculator_Spacers_Tree|
   * - |br| In the *Properties* notebook you can see |br|
       the settings to make the spacers grow:
     - |Calculator_Spacers_Properties|
   * - As the spacers have a height of :guilabel:`20`, they're now looking a bit odd in the *Design* window. |br|
       You may want to make them disappear by setting the theight to :guilabel:`0` or to fill the available space
       by checking :guilabel:`EXPAND`. |br|
       For the actual window this will not make a difference in this case.
     - |Calculator_Spacers2|


This file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-05-Spacers.wxg <../../examples/Calculator/Calculator-05-Spacers.wxg>`_

*********************************************************************
Part 5: Use of Grid Sizers
*********************************************************************

The current version doesn't look perfect as the controls are not vertically aligned.
To change this, the labels can be modified to have the same fixed size.
This may cause problems when running on a different platform with a different font size.
Instead, we will now move the controls into a grid sizer.

There are three grid sizers which are supported by wxGlade:

* *GridSizer*: all columns have the same width, all rows have the same height
* *FlexGridSizer*: all rows and columns may have different sizes. |br|
  Any row(s) and/or column(s) can be defined to be 'growable'
* *GridBagSizer*: a grid that allows items to span multiple rows or columns. |br|
  The editing logic is a bit different from the other sizers.


For our calculator we need the *FlexGridSizer* as the first column is fixed and the second column should grow.
The result will look like this:


.. |CalculatorFlexGridSizerDesign| image:: images/CalculatorFlexGridSizerDesign.png
    :height: 150
    :alt: CalculatorFlexGridSizer Design

.. |CalculatorFlexGridSizerTree| image:: images/CalculatorFlexGridSizerTree.png
    :height: 150
    :alt: CalculatorFlexGridSizer Tree 


+----------------------------------------------------------------+
|  |CalculatorFlexGridSizerDesign| |CalculatorFlexGridSizerTree| |
+----------------------------------------------------------------+


.. |VerticalSizer_InsertSlot| image:: images/Calculator_GridSizer_InsertSlot.png
    :height: 80
    :alt: InsertSlot into vertical sizer: right mouse click on sizer XXX


.. |FlexGridSizerDlg| image:: images/FlexGridSizerDlg.png
    :height: 80
    :alt: FlexGridSizer Dialog

.. |FlexGridSizer_Properties| image:: images/FlexGridSizer_Properties.png
    :height: 80
    :alt: FlexGridSizer Properties



**To get there:**

(You may want to start from
`Calculator-05-Spacers.wxg <../../examples/Calculator/Calculator-05-Spacers.wxg>`_
if your current file is too different, e.g. because you did not complete Part 4 above.)

* Insert/add a slot to the vertical sizer inside the notebook and panel: |br|
  |VerticalSizer_InsertSlot|
* Add a GridSizer |grid_sizer| to this slot
* In the grid sizer dialog, select :guilabel:`FlexGrid`, :guilabel:`4` rows and :guilabel:`2` cols: |br|
  |FlexGridSizerDlg|
* Move the labels and controls to the slots of the newly created sizer.
* Delete the old sizers and spacers.
* In *Properties -> Grid*: make column 2 and row 4 growable: |br|
  |FlexGridSizer_Properties|
* To make the text controls :guilabel:`text_value1` and :guilabel:`text_value2` actually fill the growable column: |br|
  activate *Properties -> Layout -> Alignment ->* :guilabel:`wxEXPAND` for both of them.

The logic with :guilabel:`wxEXPAND` is a bit different than with non-grid sizers.
If it is active, the control will grow horizontally and vertically.
If you want to avoid this, you need to add a horizontal or vertical box sizer 'between' grid sizer and control.

This file can be found in the folder ``wxglade/examples/Calculator``:
`Calculator-06-GridSizer.wxg <../../examples/Calculator/Calculator-06-GridSizer.wxg>`_

*********************************************************************
Summary
*********************************************************************

You should know by now how to

 * create a window structure without menu, tool or status bar
 * lay out and align controls using sizers
 * edit the structure in the *Design* window and the *Tree* view, including Cut/Copy/Paste and Drag and Drop
 * modify layout and other properties in the *Properties* notebook


Some things to remember, to save you a lot of work:

 * Names and labels can be edited directly in the in the *Tree* view.
 * When you want to apply changes to multiple widgets, edit the first, then go to the next and use 
   Re-do or Repeat to apply one or more changes:

   * Re-do: :kbd:`Ctrl-Y` or "Edit->Re-do" or toolbar right-arrow
   * Repeat: :kbd:`Ctrl-R` or "Edit->Re-do" or toolbar second right-arrow


Please keep in mind how to create a good structure:

  * Always use the simplest available sizers.
    Usually you will need mainly box sizers and maybe one or two FlexGridSizers.
  * Use nested sizers to match the hierarchical / logical structure of your project. This will make it easy
    to re-arrange things to find the best user interface.
  * Never ever try to use a grid sizer as main sizer of a window trying to resemble pixel placement or
    Tkinter's grid geometry manager. This is a mess to create and maintain.


*********************************************************************
Next steps
*********************************************************************


 - create source code and add event handlers - see :doc:`source_code`
 - add a menu - see :doc:`menu_status_tool`

