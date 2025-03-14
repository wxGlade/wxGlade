// -*- C++ -*-
//
// generated by wxGlade
//

#ifndef TOOL_MENU_EVENTBINDING_H
#define TOOL_MENU_EVENTBINDING_H

#include <wx/wx.h>
#include <wx/image.h>

// begin wxGlade: ::dependencies
// end wxGlade

// begin wxGlade: ::extracode
// end wxGlade


class MyFrame: public wxFrame {
public:
    // begin wxGlade: MyFrame::ids
    // end wxGlade

    MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:

protected:
    // begin wxGlade: MyFrame::attributes
    wxMenuItem* item1;
    wxMenuBar* frame_menubar;
    wxToolBar* frame_toolbar;
    // end wxGlade

    DECLARE_EVENT_TABLE();

public:
    virtual void on_menu_item1(wxCommandEvent &event); // wxGlade: <event_handler>
    virtual void on_menu_item2(wxCommandEvent &event); // wxGlade: <event_handler>
    virtual void on_my_tool(wxCommandEvent &event); // wxGlade: <event_handler>
}; // wxGlade: end class


#endif // TOOL_MENU_EVENTBINDING_H
