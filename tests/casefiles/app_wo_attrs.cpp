// -*- C++ -*-
//
//
// Example for compiling a single file project under Linux using g++:
//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp
//
// Example for compiling a multi file project under Linux using g++:
//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp
//

#include "app_wo_attrs.h"

// begin wxGlade: ::extracode
// end wxGlade



StockAction::StockAction(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE|wxTAB_TRAVERSAL)
{
    // begin wxGlade: StockAction::StockAction

    set_properties();
    do_layout();
    // end wxGlade
}


void StockAction::set_properties()
{
    // begin wxGlade: StockAction::set_properties
    SetTitle(_("Stock Action"));
    SetSize(wxSize(150,150));
    // end wxGlade
}


void StockAction::do_layout()
{
    // begin wxGlade: StockAction::do_layout
    Layout();
    // end wxGlade
}

