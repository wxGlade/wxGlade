"""\
wxStaticText widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# keep in sync: wxDialog, wxPanel and wxStaticBitmap
config = {
    'wxklass': 'wxStaticText',
    'style_defs': {
        'wxST_NO_AUTORESIZE': {
            'desc': _("By default, the control will adjust its size to "
                      "exactly fit to the size of the text when "
                      "SetLabel() is called. If this style flag is given, "
                      "the control will not change its size (this style is "
                      "especially useful with controls which also have the "
                      "wxALIGN_RIGHT or the wxALIGN_CENTRE_HORIZONTAL "
                      "style because otherwise they won't make sense any "
                      "longer after a call to SetLabel())."),
        },
        'wxST_ELLIPSIZE_START': {
            'desc': _('If the label text width exceeds the control width, '
                      'replace the beginning of the label with an ellipsis; '
                      'uses wxControl::Ellipsize.'),
            'supported_by': ('wx3',),
        },
        'wxST_ELLIPSIZE_MIDDLE': {
            'desc': _('If the label text width exceeds the control width, '
                      'replace the middle of the label with an ellipsis; '
                      'uses wxControl::Ellipsize.'),
            'supported_by': ('wx3',),
        },
        'wxST_ELLIPSIZE_END': {
            'desc': _('If the label text width exceeds the control width, '
                      'replace the end of the label with an ellipsis; '
                      'uses wxControl::Ellipsize.'),
            'supported_by': ('wx3',),
        },
        'wxALIGN_CENTRE':{'rename_to':'wxALIGN_CENTER_HORIZONTAL'},
        'wxALIGN_CENTER':{'rename_to':'wxALIGN_CENTER_HORIZONTAL'},
    },
    'style_list': ['wxALIGN_LEFT', 'wxALIGN_RIGHT', 'wxALIGN_CENTER_HORIZONTAL',
                   'wxST_NO_AUTORESIZE', 'wxST_ELLIPSIZE_START',
                   'wxST_ELLIPSIZE_MIDDLE', 'wxST_ELLIPSIZE_END'],
}




