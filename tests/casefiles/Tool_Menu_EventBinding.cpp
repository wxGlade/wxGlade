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

#include "Tool_Menu_EventBinding.h"

// begin wxGlade: ::extracode
// end wxGlade



MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    // begin wxGlade: MyFrame::MyFrame
    SetSize(wxSize(400, 300));
    SetTitle(wxT("frame"));
    frame_menubar = new wxMenuBar();
    wxMenu *wxglade_tmp_menu;
    wxMenuItem *wxglade_tmp_item;
    wxglade_tmp_menu = new wxMenu();
    item1 = wxglade_tmp_menu->Append(wxID_ANY, wxT("My Menu Item 1"), wxEmptyString);
    Bind(wxEVT_MENU, &MyFrame::on_menu_item1, this, item1->GetId());
    wxglade_tmp_item = wxglade_tmp_menu->Append(wxID_ANY, wxT("My Menu Item 1"), wxT("without attribute name"));
    Bind(wxEVT_MENU, &MyFrame::on_menu_item2, this, wxglade_tmp_item->GetId());
    frame_menubar->Append(wxglade_tmp_menu, wxT("Menu 1"));
    SetMenuBar(frame_menubar);
    frame_toolbar = new wxToolBar(this, -1);
    SetToolBar(frame_toolbar);
    wxToolBarToolBase *wxglade_tmp_tool;
    wxglade_tmp_tool = frame_toolbar->AddTool(wxID_ANY, wxT("My Tool"), wxBitmap(wxT("..\\..\\icons\\button.png"), wxBITMAP_TYPE_ANY), wxNullBitmap, wxITEM_NORMAL, wxEmptyString, wxEmptyString);
    Bind(wxEVT_MENU, &MyFrame::on_my_tool, this, wxglade_tmp_tool->GetId());
    frame_toolbar->Realize();
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(0, 0, 0, 0, 0);
    
    SetSizer(sizer_1);
    Layout();
    // end wxGlade
}


BEGIN_EVENT_TABLE(MyFrame, wxFrame)
    // begin wxGlade: MyFrame::event_table
    // end wxGlade
END_EVENT_TABLE();


void MyFrame::on_menu_item1(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_menu_item1) not implemented yet"));
}

void MyFrame::on_menu_item2(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_menu_item2) not implemented yet"));
}

void MyFrame::on_my_tool(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_my_tool) not implemented yet"));
}


// wxGlade: add MyFrame event handlers


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
