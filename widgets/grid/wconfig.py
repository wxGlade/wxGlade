"""\
wxGrid widget configuration

@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxGrid',
    'style_defs': {},
    'events': {
        'default': {
            'type': 'wxGridEvent',
        },
        'EVT_TEXT_ENTER': {},
        'EVT_GRID_CMD_CELL_LEFT_CLICK': {},
        'EVT_GRID_CMD_CELL_RIGHT_CLICK': {},
        'EVT_GRID_CMD_CELL_LEFT_DCLICK': {},
        'EVT_GRID_CMD_CELL_RIGHT_DCLICK': {},
        'EVT_GRID_CMD_LABEL_LEFT_CLICK': {},
        'EVT_GRID_CMD_LABEL_RIGHT_CLICK': {},
        'EVT_GRID_CMD_LABEL_LEFT_DCLICK': {},
        'EVT_GRID_CMD_LABEL_RIGHT_DCLICK': {},
        'EVT_GRID_CMD_CELL_CHANGE': {},
        
        'EVT_GRID_CMD_CELL_CHANGING': {},  # for Phoenix: before change
        'EVT_GRID_CMD_CELL_CHANGED': {},   # for Phoenix: after change
        'EVT_GRID_CMD_SELECT_CELL': {},
        'EVT_GRID_CMD_EDITOR_HIDDEN': {},
        'EVT_GRID_CMD_EDITOR_SHOWN': {},
        'EVT_GRID_CMD_COL_SIZE': {
            'type': 'wxGridSizeEvent',
        },
        'EVT_GRID_CMD_ROW_SIZE': {
            'type': 'wxGridSizeEvent',
        },
        'EVT_GRID_CMD_RANGE_SELECT': {
            'type': 'wxGridRangeSelectEvent',
        },
        'EVT_GRID_CMD_EDITOR_CREATED': {
            'type': 'wxGridEditorCreatedEvent',
        },
    },
}
