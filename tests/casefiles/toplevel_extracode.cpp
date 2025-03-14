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

#include "toplevel_extracode.h"

// begin wxGlade: ::extracode
// end wxGlade



MyFrame::MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
    // begin wxGlade: MyFrame::MyFrame
    # frame extra code before
    SetSize(wxSize(400, 300));
    SetTitle(wxT("frame"));
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(0, 0, 0, 0, 0);
    
    SetSizer(sizer_1);
    Layout();
    # frame extra code after
    // end wxGlade
}


BEGIN_EVENT_TABLE(MyFrame, wxFrame)
    // begin wxGlade: MyFrame::event_table
    EVT_CLOSE(MyFrame::on_close_frame)
    EVT_MENU_CLOSE(MyFrame::on_menu_close_frame)
    // end wxGlade
END_EVENT_TABLE();


void MyFrame::on_close_frame(wxCloseEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_close_frame) not implemented yet"));
}

void MyFrame::on_menu_close_frame(wxMenuEvent &event)  // wxGlade: MyFrame.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyFrame::on_menu_close_frame) not implemented yet"));
}


// wxGlade: add MyFrame event handlers


MyDialog::MyDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
    // begin wxGlade: MyDialog::MyDialog
    # dialog extra code before
    SetTitle(wxT("dialog"));
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(0, 0, 0, 0, 0);
    
    SetSizer(sizer_1);
    sizer_1->Fit(this);
    Layout();
    # dialog extra code after
    // end wxGlade
}


BEGIN_EVENT_TABLE(MyDialog, wxDialog)
    // begin wxGlade: MyDialog::event_table
    EVT_CLOSE(MyDialog::on_close_dialog)
    // end wxGlade
END_EVENT_TABLE();


void MyDialog::on_close_dialog(wxCloseEvent &event)  // wxGlade: MyDialog.<event_handler>
{
    event.Skip();
    // notify the user that he hasn't implemented the event handler yet
    wxLogDebug(wxT("Event handler (MyDialog::on_close_dialog) not implemented yet"));
}


// wxGlade: add MyDialog event handlers


MyMenuBar::MyMenuBar():
    wxMenuBar()
{
    // begin wxGlade: MyMenuBar::MyMenuBar
    # menubar extracode before
    # menubar extracode after
    // end wxGlade
}


wxToolBar::wxToolBar(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style):
    wxToolBar(parent, id, pos, size, style)
{
    // begin wxGlade: wxToolBar::wxToolBar
    # toolbar extracode before
    Realize();
    # toolbar extracode after
    // end wxGlade
}


MyDialog1::MyDialog1(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style):
    wxPanel(parent, id, pos, size, wxTAB_TRAVERSAL)
{
    // begin wxGlade: MyDialog1::MyDialog1
    # panel extracode before
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(0, 0, 0, 0, 0);
    
    SetSizer(sizer_1);
    sizer_1->Fit(this);
    # panel extracode after
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
    MyFrame* frame = new MyFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(frame);
    frame->Show();
    return true;
}
