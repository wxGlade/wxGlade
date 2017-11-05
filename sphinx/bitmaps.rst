
################
Bitmaps
################

.. |br| raw:: html

   <br/>


.. |BitmapPath| image:: images/BitmapPath.png


For some widgets you need to specify one or more bitmaps. There are several options how to do this:
 * from a bitmap file
 * from a named variable: statement "var:"
 * empty bitmap: statement "empty:"
 * from wxArtProvider: statement "art:"
 * from literal code: statement "code:"


From Bitmap File / Path
=======================

Specify the absolute or relative path to the bitmap file.

For an absolute path, you may use the file dialog by clicking the button "...".

.. list-table::
    :widths: 20 80

    * - **Example**
      - ::

            /usr/share/icons/application.png

        |BitmapPath|

    * - **Created code** |br|
        C++
      - ::

            wxBitmap("/usr/share/icons/application.png", wxBITMAP_TYPE_ANY)


From named variable: Statement **var**
======================================

.. list-table::
    :widths: 20 80

    * - **Syntax**
      - ::

            var:<variable name>

    * - **Example**
      - ::

            var:my_bitmap_path

    * - **Created code** |br|
        C++
      - ::

            wxBitmap(my_bitmap_path, wxBITMAP_TYPE_ANY)



Empty Bitmap: Statement **empty**
=================================

Creates an empty bitmap of the specified size (min. 1,1)


.. list-table::
    :widths: 20 80

    * - **Syntax**
      - ::

            empty:<width>,<height>

    * - **Example**
      - ::

            empty:32,32

    * - **Created code** |br|
        C++
      - ::

            wx.EmptyBitmap(32, 32)
    * - |br| Python
      - ::

            wxBitmap(32, 32)


From wxArtProvider: Statement **art**
=====================================

Create a bitmap using wxArtProvider.

See https://docs.wxpython.org/wx.ArtProvider.html for usage of wxArtProvider.

.. list-table::
    :widths: 20 80

    * - **Syntax**
      - ::

            art:<ArtID>,<ArtClient>
            
        or::

            art:<ArtID>,<ArtClient>,<width>,<height>


    * - **Example**
      - ::

            art:wxART_GO_UP,wxART_OTHER,32,32

    * - **Created code** |br|
        Perl
      - ::

            Wx::ArtProvider::GetBitmap(wxART_GO_UP, wxART_OTHER, Wx::Size->new(32, 32))


From Code: Statement **code**
=============================

This just inserts the given code.

.. list-table::
    :widths: 20 80

    * - **Syntax**
      - ::

            code:<code chunk to return a wxBitmap>

    * - **Example**
      - ::

            code:if (x == 0) get_bitmap1() else get_bitmap2();

    * - **Created code** |br|
        C++
      - ::

            if (x == 0) get_bitmap1() else get_bitmap2();





