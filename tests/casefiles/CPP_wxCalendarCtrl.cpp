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

#include "CPP_wxCalendarCtrl.h"

// begin wxGlade: ::extracode
// end wxGlade



MyFrame::MyFrame(wxWindow* parent, int id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    // begin wxGlade: MyFrame::MyFrame
    calendar_ctrl_1 = new wxCalendarCtrl(this, wxID_ANY);

    set_properties();
    do_layout();
    // end wxGlade
}


BEGIN_EVENT_TABLE(MyFrame, wxFrame)
    // begin wxGlade: MyFrame::event_table
    EVT_CALENDAR_SEL_CHANGED(wxID_ANY, MyFrame::myEVTCALENDAR_SEL_CHANGED)
    // end wxGlade
END_EVENT_TABLE();


void MyFrame::myEVTCALENDAR_SEL_CHANGED(wxCalendarEvent &event)
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::myEVTCALENDAR_SEL_CHANGED) not implemented yet"));
}


// wxGlade: add MyFrame event handlers


void MyFrame::set_properties()
{
    // begin wxGlade: MyFrame::set_properties
    SetTitle(wxT("frame_1"));
    // end wxGlade
}


void MyFrame::do_layout()
{
    // begin wxGlade: MyFrame::do_layout
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(calendar_ctrl_1, 0, 0, 0);
    SetSizer(sizer_1);
    sizer_1->Fit(this);
    Layout();
    // end wxGlade
}

