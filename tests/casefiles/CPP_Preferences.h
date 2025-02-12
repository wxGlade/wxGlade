// -*- C++ -*-
//
// generated by wxGlade
//

#ifndef CPP_PREFERENCES_H
#define CPP_PREFERENCES_H

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/intl.h>

#ifndef APP_CATALOG
#define APP_CATALOG "app"  // replace with the appropriate catalog name
#endif


// begin wxGlade: ::dependencies
#include <wx/notebook.h>
#include <wx/spinctrl.h>
// end wxGlade

// begin wxGlade: ::extracode
#define _icon_path wxT("icons/icon.png")
// end wxGlade


class wxGladePreferencesUI: public wxDialog {
public:
    // begin wxGlade: wxGladePreferencesUI::ids
    // end wxGlade

    wxGladePreferencesUI(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:

protected:
    // begin wxGlade: wxGladePreferencesUI::attributes
    wxNotebook* notebook_1;
    wxPanel* notebook_1_pane_1;
    wxCheckBox* use_menu_icons;
    wxCheckBox* frame_tool_win;
    wxCheckBox* show_progress;
    wxCheckBox* remember_geometry;
    wxCheckBox* show_sizer_handle;
    wxCheckBox* use_kde_dialogs;
    wxTextCtrl* open_save_path;
    wxTextCtrl* codegen_path;
    wxSpinCtrl* number_history;
    wxSpinCtrl* buttons_per_row;
    wxPanel* notebook_1_pane_2;
    wxCheckBox* use_dialog_units;
    wxCheckBox* wxg_backup;
    wxCheckBox* codegen_backup;
    wxCheckBox* allow_duplicate_names;
    wxCheckBox* default_border;
    wxSpinCtrl* default_border_size;
    wxCheckBox* autosave;
    wxSpinCtrl* autosave_delay;
    wxCheckBox* write_timestamp;
    wxCheckBox* write_generated_from;
    wxRadioBox* backup_suffix;
    wxTextCtrl* local_widget_path;
    wxButton* choose_widget_path;
    wxButton* ok;
    wxButton* cancel;
    // end wxGlade
}; // wxGlade: end class


#endif // CPP_PREFERENCES_H
