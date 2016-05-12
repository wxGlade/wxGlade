#!/usr/bin/perl -w --
#
#
# To get wxPerl visit http://www.wxperl.it
#

use Wx qw[:allclasses];
use strict;

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
# end wxGlade

package StockAction;

use Wx qw[:everything];
use base qw(Wx::Frame);
use strict;

use Wx::Locale gettext => '_T';
sub new {
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;
    $name   = ""                 unless defined $name;

    # begin wxGlade: StockAction::new
    $style = wxDEFAULT_FRAME_STYLE|wxTAB_TRAVERSAL
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );

    $self->__set_properties();
    $self->__do_layout();

    # end wxGlade
    return $self;

}


sub __set_properties {
    my $self = shift;
    # begin wxGlade: StockAction::__set_properties
    $self->SetTitle(_T("Stock Action"));
    $self->SetSize(Wx::Size->new(150,150));
    # end wxGlade
}

sub __do_layout {
    my $self = shift;
    # begin wxGlade: StockAction::__do_layout
    $self->Layout();
    # end wxGlade
}

# end of class StockAction

1;

