// -*- C++ -*-
//
// generated by wxGlade
//
// Example for compiling a single file project under Linux using g++:
//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp
//
// Example for compiling a multi file project under Linux using g++:
//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp
//

#ifndef NESTED_SIZERS_H
#define NESTED_SIZERS_H

#include <wx/wx.h>
#include <wx/image.h>

// begin wxGlade: ::dependencies
#include <wx/spinctrl.h>
// end wxGlade

// begin wxGlade: ::extracode
// end wxGlade


class MyDialog: public wxDialog {
public:
    // begin wxGlade: MyDialog::ids
    // end wxGlade

    MyDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:

protected:
    // begin wxGlade: MyDialog::attributes
    wxStaticBoxSizer* colour_settings_sizer;
    wxBitmapButton* bitmap_button_1;
    wxSpinCtrl* inner_red_spin_ctrl;
    // end wxGlade
}; // wxGlade: end class


#endif // NESTED_SIZERS_H