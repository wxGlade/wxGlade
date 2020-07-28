
.. |br| raw:: html

   <br/>


################################
Menu, Status Bar, Tool Bar
################################

*********************************************************************
Menu Editor
*********************************************************************

wxGlade includes a simple menu editor.

To attach a menu to a frame, go to *Properties -> Widget* and check :guilabel:`Has MenuBar`. |br|
This will add a menubar icon to the *Tree*, just below the frame's icon. |br|
To open the menu editor, click the "Edit menus..." button.

.. |AllWidgets_28_Properties_w_MenuBar| image:: images/AllWidgets_28_Properties_w_MenuBar.png
   :height: 140

.. |AllWidgets_28_Tree_w_MenuBar| image:: images/AllWidgets_28_Tree_w_MenuBar.png
   :height: 140

.. |AllWidgets_28_Properties_EditMenus| image:: images/AllWidgets_28_Properties_EditMenus.png
   :height: 140

.. |AllWidgets_28_MenuEditor| image:: images/AllWidgets_28_MenuEditor.png
   :height: 220

.. |AllWidgets_28_MenuPreview| image:: images/AllWidgets_28_MenuPreview.png


The following screenshots are from the file `wxglade/examples/Allwidgets_28.wxg <../../examples/Allwidgets_28.wxg>`_.


.. list-table::
   :widths: 40 60
   :header-rows: 0
   :align: center

   * - **Properties Window:** |br| |br|
       Example of a frame with a menu bar |br| |br|
       ``Has MenuBar`` is checked
     - |AllWidgets_28_Properties_w_MenuBar| 
   * - **Tree Window:** |br| |br|
       Example of a frame with a menu bar
     - |AllWidgets_28_Tree_w_MenuBar| 
   * - **Properties Window:** |br|
       |br| Press the "Edit menus..." button |br|
       to open the menu editor
     - |AllWidgets_28_Properties_EditMenus| 
   * - **Menu Editor** |br| |br|
       The bottom part just lists the items, |br| where the hierarchy is visualized by indentation.
     - |AllWidgets_28_MenuEditor|
   * - **The Menu** |br| |br|
       (In the example, "Unix" and Windows" |br| are radio type menu items.)
     - |AllWidgets_28_MenuPreview|
   

**Example:**

As an exercise, we will now add a "File" menu with two entries to our calculator window.

 * When you hit "Edit menus..." for the first time, the bottom part of the editor window is almost empty. It will just contain a default entry "item".
 * To create the required menu structure, change the label to :guilabel:`File`.
 * To create the first item, hit "Add" and then the ">" button to turn it into a submenu item and then change the label to :guilabel:`Reset`. Give this item a name :guilabel:`i_reset`. The item will then be stored with this attribute name, such that it can e.g. enabled and disabled programmatically.
 * Create an item :guilabel:`Exit` with event handler :guilabel:`on_menu_File_Exit`
 
As of now, these items would not yet call any code when selected. So the "Event Handler" field needs to be filled with e.g. :guilabel:`on_menu_File_Reset` and :guilabel:`on_menu_File_Exit` for the two items.

When done and after hitting the "Start generating source files", the editor and the created code should look like this:



.. list-table::

    * - **Menu Editor** |br| |br| 
        with two items: |br| |br|
        For the :guilabel:`Reset` item, |br|
        we set a name :guilabel:`i_reset`.
      - .. image:: images/Calculator07_Menu_Editor.png
            :height: 200

    * - **Generated code** |br| |br|
        including two event handlers |br| |br| |br|
        The :guilabel:`Reset` menu item is assigned to |br|
        :code:`self.frame_menubar.i_reset` |br|
        such that it can be accessed easily, |br|
        e.g. for disabling it.
      - ::

            class CalculatorFrame(wx.Frame):
                def __init__(self, *args, **kwds):
                    # begin wxGlade: CalculatorFrame.__init__
                    kwds["style"] = wx.DEFAULT_FRAME_STYLE
                    wx.Frame.__init__(self, *args, **kwds)
                    ...
                    # Menu Bar
                    self.frame_menubar = wx.MenuBar()
                    wxglade_tmp_menu = wx.Menu()
                    self.frame_menubar.i_reset = \
                        wxglade_tmp_menu.Append(wx.ID_ANY, "Reset", "Reset results")
                    self.Bind( wx.EVT_MENU, self.on_menu_File_Reset,
                               id=self.frame_menubar.i_reset.GetId() )
                    item = wxglade_tmp_menu.Append(wx.ID_ANY, "Exit", "Exit application")
                    self.Bind( wx.EVT_MENU, self.on_menu_File_Exit, id=item.GetId() )
                    self.frame_menubar.Append(wxglade_tmp_menu, "File")
                    self.SetMenuBar(self.frame_menubar)
                    # Menu Bar end
                    ...
                    
                def on_menu_File_Reset(self, event):  # wxGlade: MyFrame.<event_handler>
                    print("Event handler 'on_menu_File_Reset' not implemented!")
                    event.Skip()

                def on_menu_File_Exit(self, event):  # wxGlade: MyFrame.<event_handler>
                    print("Event handler 'on_menu_File_Exit' not implemented!")
                    event.Skip()

    * - **Handler implementation** |br| |br|
        in derived class |br| |br|
        including initial disabling of |br|
        :code:`self.frame_menubar.i_reset`
      - ::

            class MyFrame(CalculatorFrame):
                def __init__(self, *args, **kwds):
                    CalculatorFrame.__init__(self, *args, **kwds)
                    # insert more initialization code here
                    self.frame_menubar.i_reset.Enable(False)
            
                def on_menu_File_Reset(self, event):
                    self.text_result.Clear()
                    self.frame_menubar.i_reset.Enable(False)   # cleared already
            
                def on_menu_File_Exit(self, event):
                    self.Close()

                def on_execute_button_clicked(self, event):
                    # ....
                    self.frame_menubar.i_reset.Enable(True)
                    event.Skip()
            
                def on_reset_button_clicked(self, event):
                    self.text_result.Clear()
                    self.frame_menubar.i_reset.Enable(False)   # cleared already
                    event.Skip()


You can implement the handler either in a derived class or directly in the file that wxGlade has written. |br|
In the latter case, you should have enabled *Properties -> Application ->* :guilabel:`Keep user sources`.


The example menu is part of the example at wxglade/examples/Calculator:
 * `Calculator-07-Import.wxg <../../examples/Calculator/Calculator-07-Import.wxg>`_.
 * `Calculator_GUI.py <../../examples/Calculator/Calculator_GUI.py>`_.
 * `Calculator_Main.py <../../examples/Calculator/Calculator_Main.py>`_.


Lambda Event Handlers
=====================

When creating Python code, you may also specify a lambda function as event handler.

E.g. you may want to have three menu items named :code:`Insert A`, :code:`Insert B` and :code:`Insert C`.
Instead of three separate handlers, you may enter three lambda functions that will call the same method or function
with different arguments each:


+-----------------------------------------------------------------------+
|.. image:: images/MenuLambdaEventHandlers.png                          |
|    :alt: lambda evt: self.on_menu_insert_abc("A")                     |
+-----------------------------------------------------------------------+


Of course you need to implement a method :code:`on_menu_insert_abc`.


*********************************************************************
Status Bar Editor
*********************************************************************

To attach a status bar to a frame, go to *Properties -> Widget* and check cHas StatusBar`.
This will add a statusbar icon to the *Tree* window, just below the frame's icon.
(Similar to :guilabel:`Has MenuBar` in the first screenshot on this page.)

To add/remove fields to the status bar, go to *Properties -> Widget -> Fields* and use
the "Add"/"Insert"/"Remove"/"Apply" buttons.
If you set the "Size" of a field to a negative value like :guilabel:`-1` or :guilabel:`-2`, it will **grow to fill** the available space.

**Example:**

.. |Calculator_06_statusbar_Properties| image:: images/Calculator_06_statusbar_Properties.png
   :height: 120
   :align: middle
   :alt: Example Field List

.. |Calculator_06_statusbar| image:: images/Calculator_06_statusbar.png
   :height: 120
   :align: middle
   :alt: Example Status Bar


.. list-table::
   :header-rows: 0
   
   * - **Statusbar: Properties / Field Editor:** |br| |br| |br|
       two growing and two fixed size fields
     - |Calculator_06_statusbar_Properties|
   * - **Toolbar** |br| |br| |br|
       two growing and two fixed size fields
     - |Calculator_06_statusbar|
   


*********************************************************************
Tool Bar Editor
*********************************************************************

The logic for creating and editing a toolbar is the same as with menu bars.

Buttons and other controls are not supported yet
