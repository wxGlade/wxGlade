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

#include "Event_Binding.h"

// begin wxGlade: ::extracode
// end wxGlade



MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    // begin wxGlade: MyFrame::MyFrame
    SetSize(wxSize(400, 300));
    SetTitle(wxT("frame"));
    panel_1 = new wxPanel(this, wxID_ANY);
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    button_1 = new wxButton(panel_1, wxID_ANY, wxT("button_1"));
    sizer_1->Add(button_1, 0, 0, 0);
    grid_1 = new wxGrid(panel_1, wxID_ANY);
    grid_1->CreateGrid(10, 4);
    grid_1->SetSelectionMode(wxGrid::wxGridSelectRows);
    sizer_1->Add(grid_1, 1, wxEXPAND, 0);
    
    panel_1->SetSizer(sizer_1);
    Layout();
    // end wxGlade
}


BEGIN_EVENT_TABLE(MyFrame, wxFrame)
    // begin wxGlade: MyFrame::event_table
    EVT_LEFT_DOWN(wxID_ANY, MyFrame::on_left_down_panel)
    EVT_BUTTON(wxID_ANY, MyFrame::on_button)
    EVT_LEFT_DOWN(wxID_ANY, MyFrame::on_left_down)
    EVT_MOUSE_EVENTS(wxID_ANY, MyFrame::on_mouse_events)
    EVT_GRID_CMD_CELL_CHANGED(wxID_ANY, MyFrame::on_grid_cmd_cell_changed)
    EVT_TEXT_ENTER(wxID_ANY, MyFrame::on_grid_text_enter)
    EVT_LEFT_DOWN(wxID_ANY, MyFrame::on_left_down_frame)
    // end wxGlade
END_EVENT_TABLE();


void MyFrame::on_left_down_panel(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_left_down_panel) not implemented yet"));
}

void MyFrame::on_button(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_button) not implemented yet"));
}

void MyFrame::on_left_down(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_left_down) not implemented yet"));
}

void MyFrame::on_mouse_events(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_mouse_events) not implemented yet"));
}

void MyFrame::on_grid_cmd_cell_changed(wxGridEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_grid_cmd_cell_changed) not implemented yet"));
}

void MyFrame::on_grid_text_enter(wxGridEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_grid_text_enter) not implemented yet"));
}

void MyFrame::on_left_down_frame(wxCommandEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_left_down_frame) not implemented yet"));
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
