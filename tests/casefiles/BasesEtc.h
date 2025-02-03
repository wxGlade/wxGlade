// -*- C++ -*-
//
// generated by wxGlade
//

#ifndef BASESETC_H
#define BASESETC_H

#include <wx/wx.h>
#include <wx/image.h>

// begin wxGlade: ::dependencies
#include <wx/notebook.h>
#include <wx/splitter.h>
// end wxGlade

// begin wxGlade: ::extracode
import wx.html
import mynotebook
import mysplitter
import mypanel
import wx.html
import mysplitter
import mynotebook
import mypanel
import mytoolbar
import mystatusbar
import mymenubar
// end wxGlade


class MyFrame: public wxFrame {
public:
    // begin wxGlade: MyFrame::ids
    // end wxGlade

    MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:

protected:
    // begin wxGlade: MyFrame::attributes
    wxMenuBar* frame_menubar;
    wxStatusBar* frame_statusbar;
    wxToolBar* frame_toolbar;
    wxPanel* panel_x;
    wxNotebook* notebook_1;
    wxPanel* notebook_1_pane_1;
    wxSplitterWindow* window_1;
    wxPanel* window_1_pane_1;
    wxScrolledWindow* window_1_pane_2_scrolled;
    wx.html.HtmlWindow* html;
    // end wxGlade
}; // wxGlade: end class


class NotebookPageWithBases: public NotebookPage, public notebookpage.NotebookPage {
public:
    // begin wxGlade: NotebookPageWithBases::ids
    // end wxGlade

    NotebookPageWithBases(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: NotebookPageWithBases::attributes
    // end wxGlade
}; // wxGlade: end class


class TestNotebookWithBasesInFrame: public TestNotebook, public testnotebook.TestNotebook {
public:
    // begin wxGlade: TestNotebookWithBasesInFrame::ids
    // end wxGlade

    TestNotebookWithBasesInFrame(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: TestNotebookWithBasesInFrame::attributes
    NotebookPageWithBases* notebook_1_pane_1;
    // end wxGlade
}; // wxGlade: end class


class SplitterWindowWithBasesInFrame: public TestSplitterWindow {
public:
    // begin wxGlade: SplitterWindowWithBasesInFrame::ids
    // end wxGlade

    SplitterWindowWithBasesInFrame(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxSP_3D);

private:

protected:
    // begin wxGlade: SplitterWindowWithBasesInFrame::attributes
    wxPanel* window_1_pane_1;
    wxPanel* window_1_pane_2;
    // end wxGlade
}; // wxGlade: end class


class TestPanelWithBasesInFrame: public TestPanel, public testpanel.TestPanel {
public:
    // begin wxGlade: TestPanelWithBasesInFrame::ids
    // end wxGlade

    TestPanelWithBasesInFrame(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: TestPanelWithBasesInFrame::attributes
    mynotebook.MyNoteBook* notebook_1;
    mysplitter.MySplitterWindow* window_1;
    wx.html.HtmlWindow* html;
    // end wxGlade
}; // wxGlade: end class


class MyFrameWithBases: public TestFrame, public testframe.TestFrame {
public:
    // begin wxGlade: MyFrameWithBases::ids
    // end wxGlade

    MyFrameWithBases(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:

protected:
    // begin wxGlade: MyFrameWithBases::attributes
    mymenubar.MyMenuBar* frame_copy_menubar;
    mystatusbar.MyStatusBar* frame_copy_statusbar;
    mytoolbar.MyToolBar* frame_copy_toolbar;
    mypanel.MyPanel* panel_1;
    // end wxGlade
}; // wxGlade: end class


class MyDialog: public wxDialog {
public:
    // begin wxGlade: MyDialog::ids
    // end wxGlade

    MyDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:

protected:
    // begin wxGlade: MyDialog::attributes
    // end wxGlade
}; // wxGlade: end class


class MyPanel: public wxPanel {
public:
    // begin wxGlade: MyPanel::ids
    // end wxGlade

    MyPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: MyPanel::attributes
    // end wxGlade
}; // wxGlade: end class


class MyMDIChildFrame: public wxMDIChildFrame {
public:
    // begin wxGlade: MyMDIChildFrame::ids
    // end wxGlade

    MyMDIChildFrame(wxMDIParentFrame* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:

protected:
    // begin wxGlade: MyMDIChildFrame::attributes
    // end wxGlade
}; // wxGlade: end class


class MyMenuBar: public wxMenuBar {
public:
    // begin wxGlade: MyMenuBar::ids
    // end wxGlade

    MyMenuBar();

private:

protected:
    // begin wxGlade: MyMenuBar::attributes
    // end wxGlade
}; // wxGlade: end class


class wxToolBar: public wxToolBar {
public:
    // begin wxGlade: wxToolBar::ids
    // end wxGlade

    wxToolBar(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxTB_HORIZONTAL|wxNO_BORDER);

private:

protected:
    // begin wxGlade: wxToolBar::attributes
    // end wxGlade
}; // wxGlade: end class


class MyDialogWithBases: public MyDialogBase, public mydialogbases.MyDialogBase {
public:
    // begin wxGlade: MyDialogWithBases::ids
    // end wxGlade

    MyDialogWithBases(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:

protected:
    // begin wxGlade: MyDialogWithBases::attributes
    // end wxGlade
}; // wxGlade: end class


class MyPanelWithBases: public MyPanelBase, public mypanelbases.MyPanelBase {
public:
    // begin wxGlade: MyPanelWithBases::ids
    // end wxGlade

    MyPanelWithBases(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: MyPanelWithBases::attributes
    // end wxGlade
}; // wxGlade: end class


class MyPanelScrolled: public wxScrolledWindow {
public:
    // begin wxGlade: MyPanelScrolled::ids
    // end wxGlade

    MyPanelScrolled(wxWindow* parent, wxWindowID id, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=0);

private:

protected:
    // begin wxGlade: MyPanelScrolled::attributes
    // end wxGlade
}; // wxGlade: end class


#endif // BASESETC_H
