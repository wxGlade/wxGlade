// -*- C++ -*-
//
// generated by wxGlade
//

#ifndef TEST_NO_CUSTOM_CLASS09_H
#define TEST_NO_CUSTOM_CLASS09_H

#include <wx/wx.h>
#include <wx/image.h>

// begin wxGlade: ::dependencies
// end wxGlade

// begin wxGlade: ::extracode
import my_panel
// end wxGlade


class MyDialog: public wxDialog {
public:
    // begin wxGlade: MyDialog::ids
    // end wxGlade

    MyDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:

protected:
    // begin wxGlade: MyDialog::attributes
    my_panel.MyPanel* panel_1;
    // end wxGlade
}; // wxGlade: end class


#endif // TEST_NO_CUSTOM_CLASS09_H
