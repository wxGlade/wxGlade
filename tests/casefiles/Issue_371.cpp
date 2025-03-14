// -*- C++ -*-
//
// generated by wxGlade
//
// Example for compiling a single file project under Linux using g++:
//  g++ MyApp.cpp `wx-config --cxxflags --libs` -o MyApp
//
// Example for compiling a multi file project under Linux using g++:
//  g++ main.cpp `wx-config --cxxflags --libs` -o MyApp Dialog1.cpp Frame1.cpp
//

#include "Issue_371.h"

// begin wxGlade: ::extracode
// end wxGlade



FrameMain::FrameMain(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    // begin wxGlade: FrameMain::FrameMain
    SetSize(wxSize(985, 852));
    SetTitle(_("Frame"));
    wxIcon _icon;
    _icon.CopyFromBitmap(wxICON(icon));
    SetIcon(_icon);
    wxBoxSizer* sizer_top = new wxBoxSizer(wxHORIZONTAL);
    panel_top = new wxPanel(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_STATIC|wxTAB_TRAVERSAL);
    sizer_top->Add(panel_top, 1, wxEXPAND, 0);
    
    SetSizer(sizer_top);
    Layout();
    // end wxGlade
}

