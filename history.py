"""\
history for undo/redo/repeat

copyright: 2017 Dietmar Schwertberger
license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, config, clipboard


# use such?
class PropertyValue(object):
    def __init__(self, deactivated, value, modified):
        self.deactivated = deactivated
        self.value = value
        self.modified = modified

class HistoryItem(object):
    def __init__(self, prop):
        path = common.app_tree.get_widget_path(prop.owner)
        self.path = path
        self.name = prop.name

class HistoryPropertyItem(HistoryItem):
    def __init__(self, prop, old, new):
        HistoryItem.__init__(self, prop)
        if isinstance(old, tuple): old = PropertyValue(*old)
        if isinstance(new, tuple): new = PropertyValue(*new)
        self.old = old
        self.new = new

class HistorySetPropertyItem(HistoryPropertyItem):
    def __init__(self, prop, value, checked):
        HistoryItem.__init__(self, prop)
        self.value = value
        self.checked = checked


class History(object):
    def __init__(self, depth=20):
        self.actions = []
        self.actions_redo = [] # on undo, the action is moved from actions to actions_redo
        self.depth = depth
        self._buffer = None
        self._redo_widget = None # the widget that originally was modified
        self._repeating = False
        #if config.debugging:
            #self._redo_widget = ('frame', 'sizer_1', 'button_3')
            #self.actions = [('property', ('frame', 'sizer_1', 'button_3'), 'background', 'set', (False, None, False), (False, '#00ffff', True))]

    def undo(self, focused_widget):
        pass

    def redo(self, focused_widget):
        if not self.actions_redo:
            self.repeat(focused_widget, multiple=False)
            return

    def repeat(self, focused_widget, multiple=True):
        "apply action(s) to another widget"
        if not self.actions or not isinstance(self.actions[0], HistoryPropertyItem): return
        if not self._redo_widget: return
        path = common.app_tree.get_widget_path(focused_widget)
        if path==self._redo_widget: return


        # find all actions that could be repeated; they need to be HistoryPropertyItems from the _redo_widget
        repeat_actions = []
        repeat_actions_s = set()  # set of names, to avoid multiple changes of the same property
        for i,action in enumerate(self.actions):
            if not isinstance(action, HistoryPropertyItem):break
            if repeat_actions and action.path!=self._redo_widget: break
            if action.path==self._redo_widget:
                if action.name in focused_widget.properties and not action.name in repeat_actions_s:
                    repeat_actions.append( action )
                    repeat_actions_s.add( action.name )
                    if not multiple: break

        repeat_actions.reverse()

        # apply to the new widget
        self._repeating = True  # don't set self._redo_widget
        for action in repeat_actions:
            prop = focused_widget.properties[action.name]
            if isinstance(action, HistorySetPropertyItem):
                prop._change_value(action.value, action.checked)
            elif isinstance(action, HistoryPropertyItem):
                force = action.new.deactivated!=prop.deactivated
                prop._check_for_user_modification(action.new.value, force=force, activate=not action.new.deactivated)
        self._repeating = False

    def _add_item(self, item):
        self.actions.insert(0, item)
        if len(self.actions)>self.depth:
            del self.actions[-1]
        if not self._repeating and isinstance(item, HistoryPropertyItem):
            self._redo_widget = item.path

        if config.debugging:
            print("UndoBuffer:")
            for entry in self.actions:
                print(entry)

    def property_changing(self, prop):
        "to be called when property value is still the old one"
        value = prop.value
        if isinstance(value, set):
            value = set(set)  # track modifications only
        elif isinstance(value, list):
            value = value[:]
        self._buffer = (prop.deactivated, prop.value, prop.modified)

    def property_changed(self, prop, user=True):
        "argument user: True if set by the user, False if set in dependence to another change"
        old = self._buffer
        new = (prop.deactivated, prop.value, prop.modified)
        self._buffer = None
        if new==old: return
        self._add_item( HistoryPropertyItem(prop,old, new) )

    def set_property_changed(self, prop, value, checked, user=True):
        self._add_item( HistorySetPropertyItem(prop, value, checked) )

    def widget_added(self):
        self.actions.append( ("widget", path, "add", xml_data))
        pass

    def widget_removing(self, widget):
        self._buffer = clipboard.dump_widget(widget)

    def widget_removed(self):
        self.actions.append( ("widget", path, "remove", self._buffer))
        self._buffer = None
