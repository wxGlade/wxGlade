1;

package main;

unless(caller){
    my $local = Wx::Locale->new("English", "en", "en"); # replace with ??
    $local->AddCatalog("myapp"); # replace with the appropriate catalog name

    local *Wx::App::OnInit = sub{1};
    my $myapp = Wx::App->new();
    Wx::InitAllImageHandlers();

    my $appframe = MyAppFrame->new();

    $myapp->SetTopWindow($appframe);
    $appframe->Show(1);
    $myapp->MainLoop();
}
