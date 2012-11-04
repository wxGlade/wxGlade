package MyStartApp;

use base qw(Wx::App);
use strict;

sub OnInit {
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $appframe = MyAppFrame->new();

    $self->SetTopWindow($appframe);
    $appframe->Show(1);

    return 1;
}
# end of class MyStartApp

package main;

unless(caller){
    my $local = Wx::Locale->new("English", "en", "en"); # replace with ??
    $local->AddCatalog("myapp"); # replace with the appropriate catalog name

    my $myapp = MyStartApp->new();
    $myapp->MainLoop();
}
