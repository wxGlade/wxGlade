// -*- C++ -*-
//
// generated by wxGlade
//

#ifndef PANELCLASS_H
#define PANELCLASS_H

#include <wx/wx.h>
#include <wx/image.h>

// begin wxGlade: ::dependencies
// end wxGlade

// begin wxGlade: ::extracode
// end wxGlade


class DebugPanel: public wxPanel {
public:
    // begin wxGlade: DebugPanel::ids
    // end wxGlade

    DebugPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: DebugPanel::attributes
    // end wxGlade
}; // wxGlade: end class


class Frame: public wxFrame {
public:
    // begin wxGlade: Frame::ids
    // end wxGlade

    Frame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:

protected:
    // begin wxGlade: Frame::attributes
    DebugPanel* notebook_1_Debug;
    // end wxGlade
}; // wxGlade: end class


#endif // PANELCLASS_H
