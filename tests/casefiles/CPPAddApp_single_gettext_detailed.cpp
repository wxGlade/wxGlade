
class MyStartApp: public wxApp {
public:
    bool OnInit();
protected:
    wxLocale m_locale;  // locale we'll be using
};

IMPLEMENT_APP(MyStartApp)

bool MyStartApp::OnInit()
{
    m_locale.Init();
#ifdef APP_LOCALE_DIR
    m_locale.AddCatalogLookupPathPrefix(wxT(APP_LOCALE_DIR));
#endif
    m_locale.AddCatalog(wxT(APP_CATALOG));

    wxInitAllImageHandlers();
    MyAppFrame* appframe = new MyAppFrame(NULL, wxID_ANY, wxEmptyString);
    SetTopWindow(appframe);
    appframe->Show();
    return true;
}
