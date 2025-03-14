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

#include "bars_wo_parent.h"

// begin wxGlade: ::extracode
// end wxGlade



MyMenuBar::MyMenuBar():
    wxMenuBar()
{
    // begin wxGlade: MyMenuBar::MyMenuBar
    wxMenu *wxglade_tmp_menu;
    wxglade_tmp_menu = new wxMenu();
    Append(wxglade_tmp_menu, wxT("File"));
    // end wxGlade
}


MyToolBar::MyToolBar(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style):
    wxToolBar(parent, id, pos, size, style)
{
    // begin wxGlade: MyToolBar::MyToolBar
    Realize();
    // end wxGlade
}


MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, style)
{
    // begin wxGlade: MyFrame::MyFrame
    SetSize(wxSize(200, 200));
    SetTitle(wxT("frame_1"));
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    label_1 = new wxStaticText(this, wxID_ANY, wxT("placeholder - every design\nneeds a toplevel window"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER_HORIZONTAL);
    sizer_1->Add(label_1, 1, wxALL|wxEXPAND, 0);
    
    SetSizer(sizer_1);
    Layout();
    // end wxGlade
}


class MyApp: public wxApp {
public:
    bool OnInit();
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
    wxInitAllImageHandlers();
    MyFrame* frame_1 = new MyFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(frame_1);
    frame_1->Show();
    return true;
}
