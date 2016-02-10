// -*- C++ -*-
//
//
// Example for compiling a single file project under Linux using g++:
//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp
//
// Example for compiling a multi file project under Linux using g++:
//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp
//

#ifndef APP_WO_ATTRS_H
#define APP_WO_ATTRS_H

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/intl.h>

#ifndef APP_CATALOG
#define APP_CATALOG "app"  // replace with the appropriate catalog name
#endif


// begin wxGlade: ::dependencies
// end wxGlade

// begin wxGlade: ::extracode
// end wxGlade


class StockAction: public wxFrame {
public:
    // begin wxGlade: StockAction::ids
    // end wxGlade

    StockAction(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:
    // begin wxGlade: StockAction::methods
    void set_properties();
    void do_layout();
    // end wxGlade

protected:
    // begin wxGlade: StockAction::attributes
    // end wxGlade
}; // wxGlade: end class


#endif // APP_WO_ATTRS_H
