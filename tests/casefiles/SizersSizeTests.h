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

#ifndef SIZERSSIZETESTS_H
#define SIZERSSIZETESTS_H

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/intl.h>

#ifndef APP_CATALOG
#define APP_CATALOG "App"  // replace with the appropriate catalog name
#endif


// begin wxGlade: ::dependencies
#include <wx/notebook.h>
// end wxGlade

// begin wxGlade: ::extracode
// end wxGlade


class MyFrame: public wxFrame {
public:
    // begin wxGlade: MyFrame::ids
    // end wxGlade

    MyFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_FRAME_STYLE);

private:
    // begin wxGlade: MyFrame::methods
    void set_properties();
    void do_layout();
    // end wxGlade

protected:
    // begin wxGlade: MyFrame::attributes
    wxTextCtrl* _0_N_N;
    wxTextCtrl* _1_N_N;
    wxTextCtrl* _0_X_N;
    wxTextCtrl* _1_X_N;
    wxTextCtrl* _0_N_F;
    wxTextCtrl* _1_N_F;
    wxTextCtrl* _0_X_F;
    wxTextCtrl* _1_X_F;
    wxTextCtrl* _0_N_N_copy;
    wxTextCtrl* _1_N_N_copy;
    wxTextCtrl* _0_X_N_copy;
    wxTextCtrl* _1_X_N_copy;
    wxTextCtrl* _0_N_F_copy;
    wxTextCtrl* _1_N_F_copy;
    wxTextCtrl* _0_X_F_copy;
    wxTextCtrl* _1_X_F_copy;
    wxTextCtrl* _0_N_N_copy_1;
    wxTextCtrl* _1_N_N_copy_1;
    wxTextCtrl* _0_X_N_copy_1;
    wxTextCtrl* _1_X_N_copy_1;
    wxTextCtrl* _0_N_F_copy_1;
    wxTextCtrl* _1_N_F_copy_1;
    wxTextCtrl* _0_X_F_copy_1;
    wxTextCtrl* _1_X_F_copy_1;
    wxTextCtrl* _0_N_N_copy_2;
    wxTextCtrl* _1_N_N_copy_2;
    wxTextCtrl* _0_X_N_copy_2;
    wxTextCtrl* _1_X_N_copy_2;
    wxTextCtrl* _0_N_F_copy_2;
    wxTextCtrl* _1_N_F_copy_2;
    wxTextCtrl* _0_X_F_copy_2;
    wxTextCtrl* _1_X_F_copy_2;
    wxTextCtrl* _0_N_N_copy_3;
    wxTextCtrl* _1_N_N_copy_3;
    wxTextCtrl* _0_X_N_copy_3;
    wxTextCtrl* _1_X_N_copy_3;
    wxTextCtrl* _0_N_F_copy_3;
    wxTextCtrl* _1_N_F_copy_3;
    wxTextCtrl* _0_X_F_copy_3;
    wxTextCtrl* _1_X_F_copy_3;
    wxTextCtrl* _0_N_N_copy_4;
    wxTextCtrl* _1_N_N_copy_4;
    wxTextCtrl* _0_X_N_copy_4;
    wxTextCtrl* _1_X_N_copy_4;
    wxTextCtrl* _0_N_F_copy_4;
    wxTextCtrl* _1_N_F_copy_4;
    wxTextCtrl* _0_X_F_copy_4;
    wxTextCtrl* _1_X_F_copy_4;
    wxTextCtrl* _0_N_N_copy_5;
    wxTextCtrl* _1_N_N_copy_5;
    wxTextCtrl* _0_X_N_copy_5;
    wxTextCtrl* _1_X_N_copy_5;
    wxTextCtrl* _0_N_F_copy_5;
    wxTextCtrl* _1_N_F_copy_5;
    wxTextCtrl* _0_X_F_copy_5;
    wxTextCtrl* _1_X_F_copy_5;
    wxPanel* notebook_1_pane_1;
    wxTextCtrl* _0_N_N_copy_6;
    wxTextCtrl* _1_N_N_copy_6;
    wxTextCtrl* _0_X_N_copy_6;
    wxTextCtrl* _1_X_N_copy_6;
    wxTextCtrl* _0_N_F_copy_6;
    wxTextCtrl* _1_N_F_copy_6;
    wxTextCtrl* _0_X_F_copy_6;
    wxTextCtrl* _1_X_F_copy_6;
    wxTextCtrl* _0_N_N_copy_copy;
    wxTextCtrl* _1_N_N_copy_copy;
    wxTextCtrl* _0_X_N_copy_copy;
    wxTextCtrl* _1_X_N_copy_copy;
    wxTextCtrl* _0_N_F_copy_copy;
    wxTextCtrl* _1_N_F_copy_copy;
    wxTextCtrl* _0_X_F_copy_copy;
    wxTextCtrl* _1_X_F_copy_copy;
    wxTextCtrl* _0_N_N_copy_7;
    wxTextCtrl* _1_N_N_copy_7;
    wxTextCtrl* _0_X_N_copy_7;
    wxTextCtrl* _1_X_N_copy_7;
    wxTextCtrl* _0_N_F_copy_7;
    wxTextCtrl* _1_N_F_copy_7;
    wxTextCtrl* _0_X_F_copy_7;
    wxTextCtrl* _1_X_F_copy_7;
    wxTextCtrl* _0_N_N_copy_8;
    wxTextCtrl* _1_N_N_copy_8;
    wxTextCtrl* _0_X_N_copy_8;
    wxTextCtrl* _1_X_N_copy_8;
    wxTextCtrl* _0_N_F_copy_8;
    wxTextCtrl* _1_N_F_copy_8;
    wxTextCtrl* _0_X_F_copy_8;
    wxTextCtrl* _1_X_F_copy_8;
    wxTextCtrl* _0_N_N_copy_9;
    wxTextCtrl* _1_N_N_copy_9;
    wxTextCtrl* _0_X_N_copy_9;
    wxTextCtrl* _1_X_N_copy_9;
    wxTextCtrl* _0_N_F_copy_9;
    wxTextCtrl* _1_N_F_copy_9;
    wxTextCtrl* _0_X_F_copy_9;
    wxTextCtrl* _1_X_F_copy_9;
    wxTextCtrl* _0_N_N_copy_10;
    wxTextCtrl* _1_N_N_copy_10;
    wxTextCtrl* _0_X_N_copy_10;
    wxTextCtrl* _1_X_N_copy_10;
    wxTextCtrl* _0_N_F_copy_10;
    wxTextCtrl* _1_N_F_copy_10;
    wxTextCtrl* _0_X_F_copy_10;
    wxTextCtrl* _1_X_F_copy_10;
    wxTextCtrl* _0_N_N_copy_11;
    wxTextCtrl* _1_N_N_copy_11;
    wxTextCtrl* _0_X_N_copy_11;
    wxTextCtrl* _1_X_N_copy_11;
    wxTextCtrl* _0_N_F_copy_11;
    wxTextCtrl* _1_N_F_copy_11;
    wxTextCtrl* _0_X_F_copy_11;
    wxTextCtrl* _1_X_F_copy_11;
    wxPanel* notebook_1_StaticBoxSizer;
    wxTextCtrl* _0_N_N_copy_12;
    wxTextCtrl* _1_N_N_copy_12;
    wxTextCtrl* _0_X_N_copy_12;
    wxTextCtrl* _1_X_N_copy_12;
    wxTextCtrl* _0_N_F_copy_12;
    wxTextCtrl* _1_N_F_copy_12;
    wxTextCtrl* _0_X_F_copy_12;
    wxTextCtrl* _1_X_F_copy_12;
    wxTextCtrl* _0_N_N_copy_copy_copy;
    wxTextCtrl* _1_N_N_copy_copy_copy;
    wxTextCtrl* _0_X_N_copy_copy_copy;
    wxTextCtrl* _1_X_N_copy_copy_copy;
    wxTextCtrl* _0_N_F_copy_copy_copy;
    wxTextCtrl* _1_N_F_copy_copy_copy;
    wxTextCtrl* _0_X_F_copy_copy_copy;
    wxTextCtrl* _1_X_F_copy_copy_copy;
    wxTextCtrl* _0_N_N_copy_13;
    wxTextCtrl* _1_N_N_copy_13;
    wxTextCtrl* _0_X_N_copy_13;
    wxTextCtrl* _1_X_N_copy_13;
    wxTextCtrl* _0_N_F_copy_13;
    wxTextCtrl* _1_N_F_copy_13;
    wxTextCtrl* _0_X_F_copy_13;
    wxTextCtrl* _1_X_F_copy_13;
    wxTextCtrl* _0_N_N_copy_14;
    wxTextCtrl* _1_N_N_copy_14;
    wxTextCtrl* _0_X_N_copy_14;
    wxTextCtrl* _1_X_N_copy_14;
    wxTextCtrl* _0_N_F_copy_14;
    wxTextCtrl* _1_N_F_copy_14;
    wxTextCtrl* _0_X_F_copy_14;
    wxTextCtrl* _1_X_F_copy_14;
    wxTextCtrl* _0_N_N_copy_15;
    wxTextCtrl* _1_N_N_copy_15;
    wxTextCtrl* _0_X_N_copy_15;
    wxTextCtrl* _1_X_N_copy_15;
    wxTextCtrl* _0_N_F_copy_15;
    wxTextCtrl* _1_N_F_copy_15;
    wxTextCtrl* _0_X_F_copy_15;
    wxTextCtrl* _1_X_F_copy_15;
    wxTextCtrl* _0_N_N_copy_16;
    wxTextCtrl* _1_N_N_copy_16;
    wxTextCtrl* _0_X_N_copy_16;
    wxTextCtrl* _1_X_N_copy_16;
    wxTextCtrl* _0_N_F_copy_16;
    wxTextCtrl* _1_N_F_copy_16;
    wxTextCtrl* _0_X_F_copy_16;
    wxTextCtrl* _1_X_F_copy_16;
    wxTextCtrl* _0_N_N_copy_17;
    wxTextCtrl* _1_N_N_copy_17;
    wxTextCtrl* _0_X_N_copy_17;
    wxTextCtrl* _1_X_N_copy_17;
    wxTextCtrl* _0_N_F_copy_17;
    wxTextCtrl* _1_N_F_copy_17;
    wxTextCtrl* _0_X_F_copy_17;
    wxTextCtrl* _1_X_F_copy_17;
    wxPanel* notebook_1_GridSizer;
    wxTextCtrl* _0_N_N_copy_18;
    wxTextCtrl* _1_N_N_copy_18;
    wxTextCtrl* _0_X_N_copy_18;
    wxTextCtrl* _1_X_N_copy_18;
    wxTextCtrl* _0_N_F_copy_18;
    wxTextCtrl* _1_N_F_copy_18;
    wxTextCtrl* _0_X_F_copy_18;
    wxTextCtrl* _1_X_F_copy_18;
    wxTextCtrl* _0_N_N_copy_copy_copy_copy;
    wxTextCtrl* _1_N_N_copy_copy_copy_copy;
    wxTextCtrl* _0_X_N_copy_copy_copy_copy;
    wxTextCtrl* _1_X_N_copy_copy_copy_copy;
    wxTextCtrl* _0_N_F_copy_copy_copy_copy;
    wxTextCtrl* _1_N_F_copy_copy_copy_copy;
    wxTextCtrl* _0_X_F_copy_copy_copy_copy;
    wxTextCtrl* _1_X_F_copy_copy_copy_copy;
    wxTextCtrl* _0_N_N_copy_19;
    wxTextCtrl* _1_N_N_copy_19;
    wxTextCtrl* _0_X_N_copy_19;
    wxTextCtrl* _1_X_N_copy_19;
    wxTextCtrl* _0_N_F_copy_19;
    wxTextCtrl* _1_N_F_copy_19;
    wxTextCtrl* _0_X_F_copy_19;
    wxTextCtrl* _1_X_F_copy_19;
    wxTextCtrl* _0_N_N_copy_20;
    wxTextCtrl* _1_N_N_copy_20;
    wxTextCtrl* _0_X_N_copy_20;
    wxTextCtrl* _1_X_N_copy_20;
    wxTextCtrl* _0_N_F_copy_20;
    wxTextCtrl* _1_N_F_copy_20;
    wxTextCtrl* _0_X_F_copy_20;
    wxTextCtrl* _1_X_F_copy_20;
    wxTextCtrl* _0_N_N_copy_21;
    wxTextCtrl* _1_N_N_copy_21;
    wxTextCtrl* _0_X_N_copy_21;
    wxTextCtrl* _1_X_N_copy_21;
    wxTextCtrl* _0_N_F_copy_21;
    wxTextCtrl* _1_N_F_copy_21;
    wxTextCtrl* _0_X_F_copy_21;
    wxTextCtrl* _1_X_F_copy_21;
    wxTextCtrl* _0_N_N_copy_22;
    wxTextCtrl* _1_N_N_copy_22;
    wxTextCtrl* _0_X_N_copy_22;
    wxTextCtrl* _1_X_N_copy_22;
    wxTextCtrl* _0_N_F_copy_22;
    wxTextCtrl* _1_N_F_copy_22;
    wxTextCtrl* _0_X_F_copy_22;
    wxTextCtrl* _1_X_F_copy_22;
    wxTextCtrl* _0_N_N_copy_23;
    wxTextCtrl* _1_N_N_copy_23;
    wxTextCtrl* _0_X_N_copy_23;
    wxTextCtrl* _1_X_N_copy_23;
    wxTextCtrl* _0_N_F_copy_23;
    wxTextCtrl* _1_N_F_copy_23;
    wxTextCtrl* _0_X_F_copy_23;
    wxTextCtrl* _1_X_F_copy_23;
    wxPanel* notebook_1_FlexGridSizer;
    wxTextCtrl* _0_N_N_copy_24;
    wxTextCtrl* _1_N_N_copy_24;
    wxTextCtrl* _0_X_N_copy_24;
    wxTextCtrl* _1_X_N_copy_24;
    wxTextCtrl* _0_N_F_copy_24;
    wxTextCtrl* _1_N_F_copy_24;
    wxTextCtrl* _0_X_F_copy_24;
    wxTextCtrl* _1_X_F_copy_24;
    wxTextCtrl* _0_N_N_copy_copy_copy_copy_copy;
    wxTextCtrl* _1_N_N_copy_copy_copy_copy_copy;
    wxTextCtrl* _0_X_N_copy_copy_copy_copy_copy;
    wxTextCtrl* _1_X_N_copy_copy_copy_copy_copy;
    wxTextCtrl* _0_N_F_copy_copy_copy_copy_copy;
    wxTextCtrl* _1_N_F_copy_copy_copy_copy_copy;
    wxTextCtrl* _0_X_F_copy_copy_copy_copy_copy;
    wxTextCtrl* _1_X_F_copy_copy_copy_copy_copy;
    wxTextCtrl* _0_N_N_copy_25;
    wxTextCtrl* _1_N_N_copy_25;
    wxTextCtrl* _0_X_N_copy_25;
    wxTextCtrl* _1_X_N_copy_25;
    wxTextCtrl* _0_N_F_copy_25;
    wxTextCtrl* _1_N_F_copy_25;
    wxTextCtrl* _0_X_F_copy_25;
    wxTextCtrl* _1_X_F_copy_25;
    wxTextCtrl* _0_N_N_copy_26;
    wxTextCtrl* _1_N_N_copy_26;
    wxTextCtrl* _0_X_N_copy_26;
    wxTextCtrl* _1_X_N_copy_26;
    wxTextCtrl* _0_N_F_copy_26;
    wxTextCtrl* _1_N_F_copy_26;
    wxTextCtrl* _0_X_F_copy_26;
    wxTextCtrl* _1_X_F_copy_26;
    wxTextCtrl* _0_N_N_copy_27;
    wxTextCtrl* _1_N_N_copy_27;
    wxTextCtrl* _0_X_N_copy_27;
    wxTextCtrl* _1_X_N_copy_27;
    wxTextCtrl* _0_N_F_copy_27;
    wxTextCtrl* _1_N_F_copy_27;
    wxTextCtrl* _0_X_F_copy_27;
    wxTextCtrl* _1_X_F_copy_27;
    wxTextCtrl* _0_N_N_copy_28;
    wxTextCtrl* _1_N_N_copy_28;
    wxTextCtrl* _0_X_N_copy_28;
    wxTextCtrl* _1_X_N_copy_28;
    wxTextCtrl* _0_N_F_copy_28;
    wxTextCtrl* _1_N_F_copy_28;
    wxTextCtrl* _0_X_F_copy_28;
    wxTextCtrl* _1_X_F_copy_28;
    wxTextCtrl* _0_N_N_copy_29;
    wxTextCtrl* _1_N_N_copy_29;
    wxTextCtrl* _0_X_N_copy_29;
    wxTextCtrl* _1_X_N_copy_29;
    wxTextCtrl* _0_N_F_copy_29;
    wxTextCtrl* _1_N_F_copy_29;
    wxTextCtrl* _0_X_F_copy_29;
    wxTextCtrl* _1_X_F_copy_29;
    wxPanel* notebook_1_GridBagSizer;
    wxTextCtrl* _0_N_N_border_10_none;
    wxTextCtrl* _1_N_N_border_0_all;
    wxTextCtrl* _0_X_N_border_5_LEFTRIGHT;
    wxTextCtrl* _1_X_N_border_15_BOTTOM;
    wxTextCtrl* _0_N_N_copy_copy_1;
    wxTextCtrl* _0_N_N_copy_31;
    wxTextCtrl* _0_N_N_copy_32;
    wxPanel* notebook_1_BorderTest;
    wxNotebook* notebook_1;
    // end wxGlade
}; // wxGlade: end class


#endif // SIZERSSIZETESTS_H
