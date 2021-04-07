"""\
wxHyperlinkCtrl widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxHyperlinkCtrl',
    'supported_by': ('wx28', 'wx3'),
    'style_defs': {
        'wxHL_ALIGN_LEFT': {
            'desc': _('Align the text to the left.'),
            'exclude': 'wxHL_ALIGN_CENTRE|wxHL_ALIGN_RIGHT'
        },
        'wxHL_ALIGN_RIGHT': {
            'desc': _('Align the text to the right.\n'
                      'This style is not supported under Windows.'),
            'exclude': 'wxHL_ALIGN_LEFT|wxHL_ALIGN_CENTRE'
        },
        'wxHL_ALIGN_CENTRE': {
            'desc': _('Centre the text (horizontally).\n'
                      'This style is not supported under Windows.'),
            'exclude': 'wxHL_ALIGN_LEFT|wxHL_ALIGN_RIGHT'
        },
        'wxHL_CONTEXTMENU': {
            'desc': _('Pop up a context menu when the hyperlink is '
                      'right-clicked. The context menu contains a "Copy '
                      'URL" menu item which is automatically handled by '
                      'the hyperlink and which just copies in the clipboard '
                      'the URL (not the label) of the control.'),
        },
        'wxHL_DEFAULT_STYLE': {
            'desc': _('The default style for wxHyperlinkCtrl'),
            'combination': 'wxHL_ALIGN_CENTRE|wxHL_CONTEXTMENU|wxBORDER_NONE',
            'default_style': True,
        },
    },
    'style_list': ['wxHL_DEFAULT_STYLE', 'wxHL_ALIGN_LEFT', 'wxHL_ALIGN_RIGHT', 'wxHL_ALIGN_CENTRE',
                   'wxHL_CONTEXTMENU', "wxBORDER_NONE"],
    'events': {
        'EVT_HYPERLINK': {
            'type': 'wxHyperlinkEvent',
        },
    },
}
