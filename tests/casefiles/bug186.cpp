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

#include "bug186.h"

// begin wxGlade: ::extracode
// end wxGlade



Frame186::Frame186(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxFrame(parent, id, title, pos, size, style)
{
    // begin wxGlade: Frame186::Frame186
    SetSize(wxSize(300, 300));
    SetTitle(_("frame_1"));
    Bug186_Frame_menubar = new wxMenuBar();
    File = new wxMenu();
    File->Append(myMagicMenu, _("Magic"), wxEmptyString);
    Bug186_Frame_menubar->Append(File, _("File"));
    SetMenuBar(Bug186_Frame_menubar);
    Bug186_Frame_toolbar = new wxToolBar(this, -1);
    SetToolBar(Bug186_Frame_toolbar);
    Bug186_Frame_toolbar->AddTool(myMagicTool, _("Magic"), wxBitmap(32, 32), wxNullBitmap, wxITEM_NORMAL, _("Do a MAGIC action"), _("It's really MAGIC"));
    Bug186_Frame_toolbar->Realize();
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* sizer_2 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(sizer_2, 1, wxEXPAND, 0);
    text_ctrl_1 = new wxTextCtrl(this, wxID_ANY, _("Id: automatic (default behaviour)"));
    sizer_2->Add(text_ctrl_1, 1, wxALL|wxEXPAND, 5);
    text_ctrl_2 = new wxTextCtrl(this, 12123, _("Id: numeric value \"12123\""));
    sizer_2->Add(text_ctrl_2, 1, wxALL|wxEXPAND, 5);
    text_ctrl_3 = new wxTextCtrl(this, wxID_ANY, _("Id: predefined identify: \"wxID_ANY\""));
    sizer_2->Add(text_ctrl_3, 1, wxALL|wxEXPAND, 5);
    text_ctrl_4 = new wxTextCtrl(this, myButtonId, _("Id: variable assignment \"myButtonId=?\""));
    sizer_2->Add(text_ctrl_4, 1, wxALL|wxEXPAND, 5);
    
    SetSizer(sizer_1);
    Layout();
    // end wxGlade
}


class MyApp: public wxApp {
public:
    bool OnInit();
protected:
    wxLocale m_locale;  // locale we'll be using
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
    m_locale.Init();
#ifdef APP_LOCALE_DIR
    m_locale.AddCatalogLookupPathPrefix(wxT(APP_LOCALE_DIR));
#endif
    m_locale.AddCatalog(wxT(APP_CATALOG));

    wxInitAllImageHandlers();
    Frame186* Bug186_Frame = new Frame186(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(Bug186_Frame);
    Bug186_Frame->Show();
    return true;
}
