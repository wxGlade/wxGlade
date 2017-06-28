"""\
wxNotebook widget configuration

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxNotebook',
    'style_defs': {
        'wxNB_TOP': {
            'desc': _('Place tabs on the top side.'),
            'exclude': 'wxNB_LEFT|wxNB_RIGHT|wxNB_BOTTOM',
        },
        'wxNB_LEFT': {
            'desc': _('Place tabs on the left side.'),
            'exclude': 'wxNB_TOP|wxNB_RIGHT|wxNB_BOTTOM',
        },
        'wxNB_RIGHT': {
            'desc': _('Place tabs on the right side.'),
            'exclude': 'wxNB_TOP|wxNB_LEFT|wxNB_BOTTOM',
        },
        'wxNB_BOTTOM': {
            'desc': _('Place tabs under instead of above the notebook pages.'),
            'exclude': 'wxNB_TOP|wxNB_LEFT|wxNB_RIGHT',
        },
        'wxNB_FIXEDWIDTH': {
            'desc': _('(Windows only) All tabs will have same width.'),
        },
        'wxNB_MULTILINE': {
            'desc': _('(Windows only) There can be several rows of tabs.'),
        },
        'wxNB_NOPAGETHEME': {
            'desc': _('(Windows only) Display a solid colour on notebook '
                      'pages, and not a gradient, which can reduce '
                      'performance.'),
        },
        'wxNB_FLAT': {
            'desc': _('(Windows CE only) Show tabs in a flat style.'),
        },
    },
    'default_style': 'wxNB_TOP',
    'style_list': ['wxNB_TOP', 'wxNB_LEFT', 'wxNB_RIGHT', 'wxNB_BOTTOM',
                   'wxNB_FIXEDWIDTH', 'wxNB_MULTILINE', 'wxNB_NOPAGETHEME',
                   'wxNB_FLAT'],
    'events': {
        'default': {
            'type': 'wxNotebookEvent',
        },
        'EVT_NOTEBOOK_PAGE_CHANGED': {
            'type_wx3': 'wxBookCtrlEvent',
        },
        'EVT_NOTEBOOK_PAGE_CHANGING': {
            'type_wx3': 'wxBookCtrlEvent',
        },
    },
}




