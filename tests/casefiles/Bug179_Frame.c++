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

#include <wx/wx.h>
#include "Bug179_Frame.hpp"

// begin wxGlade: ::extracode
// end wxGlade


Bug179_Frame::Bug179_Frame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, style)
{
    // begin wxGlade: Bug179_Frame::Bug179_Frame
    SetTitle(_("frame_1"));
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    label_1 = new wxStaticText(this, wxID_ANY, _("Just a label"));
    sizer_1->Add(label_1, 1, wxALL|wxEXPAND, 5);
    
    SetSizer(sizer_1);
    sizer_1->Fit(this);
    Layout();
    // end wxGlade
}

