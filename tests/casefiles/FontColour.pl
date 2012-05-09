#!/usr/bin/perl -w -- 
#
# generated by wxGlade "faked test version"
#
# To get wxPerl visit http://wxPerl.sourceforge.net/
#

use Wx 0.15 qw[:allclasses];
use strict;

# begin wxGlade: dependencies
use Wx::Locale gettext => '_T';
# end wxGlade

# begin wxGlade: extracode
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
        $self->{text_ctrl_1} = Wx::TextCtrl->new($self, wxID_ANY, _T("Some Input"), wxDefaultPosition, wxDefaultSize, wxTE_READONLY);

        $self->__set_properties();
        $self->__do_layout();

        # end wxGlade
        return $self;

}


sub __set_properties {
        my $self = shift;

        # begin wxGlade: MyFrame::__set_properties

        $self->SetTitle(_T("frame_1"));
        $self->{text_ctrl_1}->SetMinSize(Wx::Size->new(379, 23));
        $self->{text_ctrl_1}->SetBackgroundColour(Wx::Colour->new(0, 255, 127));
        $self->{text_ctrl_1}->SetForegroundColour(Wx::Colour->new(255, 0, 0));
        $self->{text_ctrl_1}->SetFont(Wx::Font->new(10, wxDEFAULT, wxNORMAL, wxBOLD, 0, ""));
        $self->{text_ctrl_1}->SetFocus();

        # end wxGlade
}

sub __do_layout {
        my $self = shift;

        # begin wxGlade: MyFrame::__do_layout

        $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
        $self->{sizer_1}->Add($self->{text_ctrl_1}, 1, wxALL|wxEXPAND, 5);
        $self->SetSizer($self->{sizer_1});
        $self->{sizer_1}->Fit($self);
        $self->Layout();

        # end wxGlade
}

# end of class MyFrame

1;

package MyApp;

use base qw(Wx::App);
use strict;

sub OnInit {
        my( $self ) = shift;

        Wx::InitAllImageHandlers();

        my $frame_1 = MyFrame->new();

        $self->SetTopWindow($frame_1);
        $frame_1->Show(1);

        return 1;
}
# end of class MyApp

package main;

unless(caller){
        my $local = Wx::Locale->new("English", "en", "en"); # replace with ??
        $local->AddCatalog("app"); # replace with the appropriate catalog name

        my $app = MyApp->new();
        $app->MainLoop();
}
