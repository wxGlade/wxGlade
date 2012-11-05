
class MyStartApp: public wxApp {
public:
    bool OnInit();
};

IMPLEMENT_APP(MyStartApp)

bool MyStartApp::OnInit()
{
    wxInitAllImageHandlers();
    MyAppFrame* appframe = new MyAppFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(appframe);
    appframe->Show();
    return true;
}
