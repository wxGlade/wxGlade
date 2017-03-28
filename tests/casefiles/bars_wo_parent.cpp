// -*- C++ -*-
//
// generated by wxGlade "faked test version"
//
// Example for compiling a single file project under Linux using g++:
//  g++ MyApp.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp
//
// Example for compiling a multi file project under Linux using g++:
//  g++ main.cpp $(wx-config --libs) $(wx-config --cxxflags) -o MyApp Dialog1.cpp Frame1.cpp
//

#include "bars_wo_parent.h"

// begin wxGlade: ::extracode
// end wxGlade



MyMenuBar::MyMenuBar():
    wxMenuBar()
{
    // begin wxGlade: MyMenuBar::MyMenuBar
    wxMenu* wxglade_tmp_menu;
    wxglade_tmp_menu = new wxMenu();
    this->Append(wxglade_tmp_menu, wxT("File"));

    set_properties();
    do_layout();
    // end wxGlade
}


void MyMenuBar::set_properties()
{
    // begin wxGlade: MyMenuBar::set_properties
    // end wxGlade
}


void MyMenuBar::do_layout()
{
    // begin wxGlade: MyMenuBar::do_layout
    // end wxGlade
}


MyToolBar::MyToolBar(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style):
    wxToolBar(parent, id, pos, size, style)
{
    // begin wxGlade: MyToolBar::MyToolBar

    set_properties();
    do_layout();
    // end wxGlade
}


void MyToolBar::set_properties()
{
    // begin wxGlade: MyToolBar::set_properties
    Realize();
    // end wxGlade
}


void MyToolBar::do_layout()
{
    // begin wxGlade: MyToolBar::do_layout
    // end wxGlade
}


MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, style)
{
    // begin wxGlade: MyFrame::MyFrame
    label_1 = new wxStaticText(this, wxID_ANY, wxT("placeholder - every design\nneeds a toplevel window"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER);

    set_properties();
    do_layout();
    // end wxGlade
}


void MyFrame::set_properties()
{
    // begin wxGlade: MyFrame::set_properties
    SetTitle(wxT("frame_1"));
    SetSize(wxSize(200, 200));
    // end wxGlade
}


void MyFrame::do_layout()
{
    // begin wxGlade: MyFrame::do_layout
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(label_1, 1, wxALIGN_CENTER|wxALL|wxEXPAND, 0);
    SetSizer(sizer_1);
    Layout();
    SetSize(wxSize(200, 200));
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
