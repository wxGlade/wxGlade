#!/usr/bin/perl -w -- 
#
# generated by wxGlade
#
# To get wxPerl visit http://www.wxperl.it
#

use Wx;
use strict;

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
import wx.html
import mynotebook
import mysplitter
import mypanel
import mytoolbar
import mystatusbar
import mymenubar
# end wxGlade

package MyFrame;

use Wx qw[:everything];
use base qw(Wx::Frame);
use strict;

sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyFrame::new
    $style = wxDEFAULT_FRAME_STYLE
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetSize(Wx::Size->new(400, 300));
    $self->SetTitle("frame");
    
    

    # Menu Bar

    $self->{frame_menubar} = Wx::MenuBar->new();
    my $wxglade_tmp_menu;
    $self->SetMenuBar($self->{frame_menubar});
    
    # Menu Bar end

    
    $self->{frame_statusbar} = $self->CreateStatusBar(1);
    $self->{frame_statusbar}->SetStatusWidths(-1);
    
    # statusbar fields
    my( @frame_statusbar_fields ) = (
        "frame_statusbar",
    );
    
    if( @frame_statusbar_fields ) {
        $self->{frame_statusbar}->SetStatusText($frame_statusbar_fields[$_], $_)
        for 0 .. $#frame_statusbar_fields ;
    }
    
    
    # Tool Bar
    $self->{frame_toolbar} = Wx::ToolBar->new($self, -1);
    $self->SetToolBar($self->{frame_toolbar});
    $self->{frame_toolbar}->Realize();
    # Tool Bar end
    
    $self->{panel_x} = Wx::Panel->new($self, wxID_ANY);
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{notebook_1} = Wx::Notebook->new($self->{panel_x}, wxID_ANY);
    $self->{sizer_1}->Add($self->{notebook_1}, 1, wxEXPAND, 0);
    
    $self->{notebook_1_pane_1} = Wx::Panel->new($self->{notebook_1}, wxID_ANY);
    $self->{notebook_1}->AddPage($self->{notebook_1_pane_1}, "notebook_1_pane_1");
    
    $self->{sizer_1}->Add(20, 20, 0, 0, 0);
    
    $self->{window_1} = Wx::SplitterWindow->new($self->{panel_x}, wxID_ANY);
    $self->{window_1}->SetMinimumPaneSize(20);
    $self->{sizer_1}->Add($self->{window_1}, 1, wxEXPAND, 0);
    
    $self->{window_1_pane_1} = Wx::Panel->new($self->{window_1}, wxID_ANY);
    
    $self->{window_1_pane_2_scrolled} = Wx::ScrolledWindow->new($self->{window_1}, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
    $self->{window_1_pane_2_scrolled}->SetScrollRate(10, 10);
    
    $self->{html} = wx.html.HtmlWindow->new($self->{panel_x}, wxID_ANY);
    $self->{sizer_1}->Add($self->{html}, 1, wxALL|wxEXPAND, 3);
    
    $self->{window_1}->SplitVertically($self->{window_1_pane_1}, $self->{window_1_pane_2_scrolled}, );
    
    $self->{panel_x}->SetSizer($self->{sizer_1});
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyFrame

1;

package NotebookPageWithBases;

use Wx qw[:everything];
use base qw(Wx::Panel);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: NotebookPageWithBases::new
    $style = wxTAB_TRAVERSAL
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class NotebookPageWithBases

1;

package TestNotebookWithBasesInFrame;

use Wx qw[:everything];
use base qw(Wx::Notebook);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: TestNotebookWithBasesInFrame::new
    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    
    $self->{notebook_1_pane_1} = NotebookPageWithBases->new($self, wxID_ANY);
    $self->AddPage($self->{notebook_1_pane_1}, "notebook_1_pane_1");
    # end wxGlade
    return $self;

}


# end of class TestNotebookWithBasesInFrame

1;

package SplitterWindowWithBasesInFrame;

use Wx qw[:everything];
use base qw(Wx::SplitterWindow);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: SplitterWindowWithBasesInFrame::new
    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    
    $self->{window_1_pane_1} = Wx::Panel->new($self, wxID_ANY);
    
    $self->{window_1_pane_2} = Wx::Panel->new($self, wxID_ANY);
    $self->SplitVertically($self->{window_1_pane_1}, $self->{window_1_pane_2}, );
    # end wxGlade
    return $self;

}


# end of class SplitterWindowWithBasesInFrame

1;

package TestPanelWithBasesInFrame;

use Wx qw[:everything];
use base qw(Wx::Panel);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: TestPanelWithBasesInFrame::new
    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{notebook_1} = mynotebook.MyNoteBook->new($self, wxID_ANY);
    $self->{sizer_1}->Add($self->{notebook_1}, 1, wxEXPAND, 0);
    
    $self->{window_1} = mysplitter.MySplitterWindow->new($self, wxID_ANY);
    $self->{sizer_1}->Add($self->{window_1}, 1, wxEXPAND, 0);
    
    $self->{html} = wx.html.HtmlWindow->new($self, wxID_ANY);
    $self->{sizer_1}->Add($self->{html}, 1, wxALL|wxEXPAND, 3);
    
    $self->SetSizer($self->{sizer_1});
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class TestPanelWithBasesInFrame

1;

package MyFrameWithBases;

use Wx qw[:everything];
use base qw(Wx::Frame);
use strict;

sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyFrameWithBases::new
    $style = wxDEFAULT_FRAME_STYLE
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetSize(Wx::Size->new(400, 300));
    $self->SetTitle("frame");
    
    

    # Menu Bar

    $self->{frame_copy_menubar} = mymenubar.MyMenuBar->new();
    my $wxglade_tmp_menu;
    $self->SetMenuBar($self->{frame_copy_menubar});
    
    # Menu Bar end

    
    $self->{frame_copy_statusbar} = $self->CreateStatusBar(1);
    $self->{frame_copy_statusbar}->SetStatusWidths(-1);
    
    # statusbar fields
    my( @frame_copy_statusbar_fields ) = (
        "frame_copy_statusbar",
    );
    
    if( @frame_copy_statusbar_fields ) {
        $self->{frame_copy_statusbar}->SetStatusText($frame_copy_statusbar_fields[$_], $_)
        for 0 .. $#frame_copy_statusbar_fields ;
    }
    
    
    # Tool Bar
    $self->{frame_copy_toolbar} = mytoolbar.MyToolBar->new($self, -1);
    $self->SetToolBar($self->{frame_copy_toolbar});
    $self->{frame_copy_toolbar}->Realize();
    # Tool Bar end
    
    $self->{panel_1} = mypanel.MyPanel->new($self, wxID_ANY);
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyFrameWithBases

1;

package MyDialog;

use Wx qw[:everything];
use base qw(Wx::Dialog);
use strict;

sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyDialog::new
    $style = wxDEFAULT_DIALOG_STYLE
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetTitle("dialog");
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyDialog

1;

package MyPanel;

use Wx qw[:everything];
use base qw(Wx::Panel);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyPanel::new
    $style = wxTAB_TRAVERSAL
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyPanel

1;

package MyMDIChildFrame;

use Wx qw[:everything];
use base qw(Wx::MDIChildFrame);
use strict;

sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyMDIChildFrame::new
    $style = wxDEFAULT_FRAME_STYLE
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetSize(Wx::Size->new(400, 300));
    $self->SetTitle("frame_1");
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyMDIChildFrame

1;

package MyMenuBar;

use Wx qw[:everything];
use base qw(Wx::MenuBar);
use strict;

sub new {
    my( $self,  ) = @_;
    # begin wxGlade: MyMenuBar::new
    $self = $self->SUPER::new( @_[1 .. $#_] );
    my $wxglade_tmp_menu;
    # end wxGlade
    return $self;

}


# end of class MyMenuBar

1;

package wxToolBar;

use Wx qw[:everything];
use base qw(Wx::ToolBar);
use strict;

sub new {
    my( $self,  ) = @_;
    # begin wxGlade: wxToolBar::new
    $self = $self->SUPER::new( @_[1 .. $#_] );
    $self->Realize();
    # end wxGlade
    return $self;

}


# end of class wxToolBar

1;

package MyDialogWithBases;

use Wx qw[:everything];
use base qw(Wx::Dialog);
use strict;

sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyDialogWithBases::new
    $style = wxDEFAULT_DIALOG_STYLE
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetTitle("dialog");
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyDialogWithBases

1;

package MyPanelWithBases;

use Wx qw[:everything];
use base qw(Wx::Panel);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyPanelWithBases::new
    $style = wxTAB_TRAVERSAL
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyPanelWithBases

1;

package MyPanelScrolled;

use Wx qw[:everything];
use base qw(Wx::ScrolledWindow);
use strict;

sub new {
    my( $self, $parent, $id, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: MyPanelScrolled::new
    $style = wxTAB_TRAVERSAL
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $pos, $size, $style, $name );
    $self->SetScrollRate(10, 10);
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_1}->Add(0, 0, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyPanelScrolled

1;

package MyApp;

use base qw(Wx::App);
use strict;

sub OnInit {
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $frame = MyFrame->new();

    $self->SetTopWindow($frame);
    $frame->Show(1);

    return 1;
}
# end of class MyApp

package main;

my $app = MyApp->new();
$app->MainLoop();
