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

#include "Frame_Size.h"




MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    SetSize(wxSize(400, 300));
    SetTitle(wxT("frame"));
    panel_1 = new wxPanel(this, wxID_ANY);
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    text_ctrl_1 = new wxTextCtrl(panel_1, wxID_ANY, wxEmptyString);
    sizer_1->Add(text_ctrl_1, 0, 0, 0);
    button_1 = new wxButton(panel_1, wxID_ANY, wxT("button_1"));
    sizer_1->Add(button_1, 0, 0, 0);
    
    panel_1->SetSizer(sizer_1);
    Layout();
}


MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    SetTitle(wxT("frame"));
    panel_1 = new wxPanel(this, wxID_ANY);
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    text_ctrl_1 = new wxTextCtrl(panel_1, wxID_ANY, wxEmptyString);
    sizer_1->Add(text_ctrl_1, 0, 0, 0);
    button_1 = new wxButton(panel_1, wxID_ANY, wxT("button_1"));
    sizer_1->Add(button_1, 0, 0, 0);
    
    panel_1->SetSizer(sizer_1);
    sizer_1->Fit();
    Layout();
}


class MyApp: public wxApp {
public:
    bool OnInit();
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
    wxInitAllImageHandlers();
    MyFrame* frame = new MyFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(frame);
    frame->Show();
    return true;
}
