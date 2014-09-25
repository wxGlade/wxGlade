"""\
wxCheckBox widget configuration

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxCheckBox',
    'style_defs': {
        'wxCAL_SUNDAY_FIRST': {
            'desc': _('Show Sunday as the first day in the week')
        },
        'wxCAL_MONDAY_FIRST': {
            'desc': _('Show Monday as the first day in the week')
        },
        'wxCAL_SHOW_HOLIDAYS': {
            'desc': _('Show Monday as the first day in the week')
        },
        'wxCAL_NO_YEAR_CHANGE': {
            'desc': _('Disable the year changing')
        },
        'wxCAL_NO_MONTH_CHANGE': {
            'desc': _('Disable the month (and, implicitly, the year) '
                      'changing')
        },
        'wxCAL_SHOW_SURROUNDING_WEEKS': {
            'desc': _('Show the neighbouring weeks in the previous and next '
                      'months')
        },
        'wxCAL_SEQUENTIAL_MONTH_SELECTION': {
            'desc': _('Use alternative, more compact, style for the month '
                      'and year selection controls.')
        },
    },
    'box_label': _('Style'),
    'style_list': ['wxCAL_SUNDAY_FIRST', 'wxCAL_MONDAY_FIRST',
                   'wxCAL_SHOW_HOLIDAYS', 'wxCAL_NO_YEAR_CHANGE',
                   'wxCAL_NO_MONTH_CHANGE', 'wxCAL_SHOW_SURROUNDING_WEEKS',
                   'wxCAL_SEQUENTIAL_MONTH_SELECTION'],
}




