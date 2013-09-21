
class MyApp: public wxApp {
public:
    bool OnInit();
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
    wxInitAllImageHandlers();
    MyAppFrame* appframe = new MyAppFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(appframe);
    appframe->Show();
    return true;
}
