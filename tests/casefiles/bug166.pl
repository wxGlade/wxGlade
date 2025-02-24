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
    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->SetTitle("frame_1");
    
    $self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->{sizer_2} = Wx::BoxSizer->new(wxVERTICAL);
    $self->{sizer_1}->Add($self->{sizer_2}, 1, 0, 0);
    
    $self->{choice_1} = Wx::Choice->new($self, wxID_ANY, wxDefaultPosition, wxDefaultSize, ["Pure ASCII", "German Umlauts \N{U+00e4}\N{U+00f6}\N{U+00fc}\N{U+00c4}\N{U+00d6}\N{U+00dc}\N{U+00df}"], );
    $self->{choice_1}->SetSelection(1);
    $self->{sizer_2}->Add($self->{choice_1}, 1, wxALL|wxEXPAND, 5);
    
    $self->{label_1} = Wx::StaticText->new($self, wxID_ANY, "German Umlauts \N{U+00e4}\N{U+00f6}\N{U+00fc}\N{U+00c4}\N{U+00d6}\N{U+00dc}\N{U+00df}", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER_HORIZONTAL);
    $self->{sizer_2}->Add($self->{label_1}, 1, wxALL|wxEXPAND, 5);
    
    $self->SetSizer($self->{sizer_1});
    $self->{sizer_1}->Fit($self);
    
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

    my $frame_1 = MyFrame->new();

    $self->SetTopWindow($frame_1);
    $frame_1->Show(1);

    return 1;
}
# end of class MyApp

package main;

my $app = MyApp->new();
$app->MainLoop();
