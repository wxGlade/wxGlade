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
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{text_ctrl_1} = Wx::TextCtrl->new($self, wxID_ANY, "Some Input", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
    $self->{text_ctrl_1}->SetBackgroundColour(Wx::Colour->new(0, 255, 127));
    $self->{text_ctrl_1}->SetForegroundColour(Wx::Colour->new(255, 0, 0));
    $self->{text_ctrl_1}->SetFont(Wx::Font->new(16, wxDEFAULT, wxNORMAL, wxBOLD, 0, ""));
    $self->{text_ctrl_1}->SetFocus();
    $self->{sizer_1}->Add($self->{text_ctrl_1}, 1, wxALL|wxEXPAND, 5);
    
    my $label_1 = Wx::StaticText->new($self, wxID_ANY, "label_1");
    $self->{sizer_1}->Add($label_1, 0, 0, 0);
    
    my $label_2 = Wx::StaticText->new($self, wxID_ANY, "label_2");
    $label_2->SetFont(Wx::Font->new(8, wxDECORATIVE, wxSLANT, wxLIGHT, 0, ""));
    $self->{sizer_1}->Add($label_2, 0, 0, 0);
    
    my $label_3 = Wx::StaticText->new($self, wxID_ANY, "label_3");
    $label_3->SetFont(Wx::Font->new(8, wxROMAN, wxITALIC, wxBOLD, 0, ""));
    $self->{sizer_1}->Add($label_3, 0, 0, 0);
    
    my $label_4 = Wx::StaticText->new($self, wxID_ANY, "label_4");
    $label_4->SetFont(Wx::Font->new(8, wxSCRIPT, wxNORMAL, wxNORMAL, 0, ""));
    $self->{sizer_1}->Add($label_4, 0, 0, 0);
    
    my $label_5 = Wx::StaticText->new($self, wxID_ANY, "label_5");
    $label_5->SetFont(Wx::Font->new(10, wxSWISS, wxNORMAL, wxNORMAL, 0, ""));
    $self->{sizer_1}->Add($label_5, 0, 0, 0);
    
    my $label_6 = Wx::StaticText->new($self, wxID_ANY, "label_6");
    $label_6->SetFont(Wx::Font->new(12, wxMODERN, wxNORMAL, wxNORMAL, 1, ""));
    $self->{sizer_1}->Add($label_6, 0, 0, 0);
    
    $self->SetSizer($self->{sizer_1});
    
    $self->Layout();
    # end wxGlade
    return $self;

}


# end of class MyFrame

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
