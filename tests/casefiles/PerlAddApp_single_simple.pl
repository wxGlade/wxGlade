1;

package main;

unless(caller){
    local *Wx::App::OnInit = sub{1};
    my $myapp = Wx::App->new();
    Wx::InitAllImageHandlers();

    my $appframe = MyAppFrame->new();

    $myapp->SetTopWindow($appframe);
    $appframe->Show(1);
    $myapp->MainLoop();
}
