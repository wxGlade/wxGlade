"""\
wxGrid widget configuration

@copyright: 2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

config = {
    'wxklass': 'wxGrid',
    'style_defs': {},
    'events': {
        'EVT_TEXT_ENTER': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_CELL_LEFT_CLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_CELL_RIGHT_CLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_CELL_LEFT_DCLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_CELL_RIGHT_DCLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_LABEL_LEFT_CLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_LABEL_RIGHT_CLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_LABEL_LEFT_DCLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_LABEL_RIGHT_DCLICK': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_CELL_CHANGE': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_SELECT_CELL': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_EDITOR_HIDDEN': {
            'type': 'wxGridEvent',
        },
        'EVT_GRID_CMD_EDITOR_SHOWN': {
            'type': 'wxGridEvent',
        },
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
