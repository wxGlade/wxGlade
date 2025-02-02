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

#include "bug166.h"

// begin wxGlade: ::extracode
// end wxGlade



MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, style)
{
    // begin wxGlade: MyFrame::MyFrame
    SetTitle(wxT("frame_1"));
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* sizer_2 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(sizer_2, 1, 0, 0);
    const wxString choice_1_choices[] = {
        wxT("Pure ASCII"),
        wxT("German Umlauts äöüÄÖÜß"),
    };
    choice_1 = new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 2, choice_1_choices);
    choice_1->SetSelection(1);
    sizer_2->Add(choice_1, 1, wxALL|wxEXPAND, 5);
    label_1 = new wxStaticText(this, wxID_ANY, wxT("German Umlauts äöüÄÖÜß"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER_HORIZONTAL);
    sizer_2->Add(label_1, 1, wxALL|wxEXPAND, 5);
    
    SetSizer(sizer_1);
    sizer_1->Fit(this);
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
