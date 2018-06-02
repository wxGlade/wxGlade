"""\
history for undo/redo/repeat

copyright: 2017-2018 Dietmar Schwertberger
license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, config, clipboard


class PropertyValue(object):
    def __init__(self, deactivated, value, modified):
        self.deactivated = deactivated
        self.value = value
        self.modified = modified
    def __repr__(self):
        return "(%r, %r, %r)"%(self.deactivated, self.value, self.modified)

class HistoryItem(object):
    def __init__(self, prop):
        path = common.app_tree.get_widget_path(prop.owner)
        self.path = path
        self.name = prop.name
    def get_key(self):
        return self.name

class HistoryPropertyItem(HistoryItem):
    def __init__(self, prop, old, new):
        HistoryItem.__init__(self, prop)
        if isinstance(old, tuple): old = PropertyValue(*old)
        if isinstance(new, tuple): new = PropertyValue(*new)
        self.old = old
        self.new = new
    def __repr__(self):
        return "%s(%s, %r, %r, %r)"%(self.__class__.__name__, self.path, self.name, self.old, self.new)


class HistorySetPropertyItem(HistoryPropertyItem):
    def __init__(self, prop, value, checked):
        HistoryItem.__init__(self, prop)
        self.value = value
        self.checked = checked
    def __repr__(self):
        return "%s(%s, %s, %r, %r)"%(self.__class__.__name__, self.path, self.name, self.value, self.checked)
    def get_key(self):
        return (self.name, self.value)


class History(object):
    def __init__(self, depth=20):
        self.actions = []
        self.actions_redo = [] # on undo, the action is moved from actions to actions_redo
        self.depth = depth
        self._buffer = None
        self._redo_widget = None # the widget that originally was modified
        self._redo_info = []  # name of properties
        self._repeating = False
        self.can_redo = self.can_repeat = False

    def set_widget(self, widget):
        # for enabling/disabling tools and menus
        path = common.app_tree.get_widget_path(widget)
        if path==self._redo_widget or self._redo_widget is None:
            self.can_repeat = self.can_redo = False
        else:
            self.can_redo = True
            self.can_repeat = len(self._redo_info) > 1

    def undo(self, focused_widget):
        pass

    def redo(self, focused_widget):
        if not self.actions_redo:
            self.repeat(focused_widget, multiple=False)
            return

    def repeat(self, focused_widget, multiple=True):
        "apply action(s) to another widget"
        if focused_widget is None: return
        if not self.actions or not isinstance(self.actions[0], HistoryPropertyItem): return
        if not self._redo_widget: return
        path = common.app_tree.get_widget_path(focused_widget)
        if path==self._redo_widget: return

        # find all actions that could be repeated; they need to be HistoryPropertyItems from the _redo_widget
        repeat_actions = []
        repeat_actions_keys = set()  # set of names, to avoid multiple changes of the same property
        for i,action in enumerate(self.actions):
            if not isinstance(action, HistoryPropertyItem):break
            if repeat_actions and action.path!=self._redo_widget: break
            if action.path==self._redo_widget:
                action_key = action.get_key()  # this may be a tuple for HistorySetPropertyItem
                if action.name in focused_widget.properties and not action_key in repeat_actions_keys:
                    repeat_actions.append( action )
                    repeat_actions_keys.add( action_key )
                    if not multiple: break

        repeat_actions.reverse()

        # apply to the new widget
        self._repeating = True  # don't set self._redo_widget
        for action in repeat_actions:
            if config.debugging:
                print("Repeating %s"%action)
            prop = focused_widget.properties[action.name]
            if isinstance(action, HistorySetPropertyItem):
                prop._change_value(action.value, action.checked)
            elif isinstance(action, HistoryPropertyItem):
                if prop.deactivated is None:
                    # a property that can not be deactivated
                    prop._check_for_user_modification(action.new.value)
                else:
                    force = action.new.deactivated!=prop.deactivated
                    prop._check_for_user_modification(action.new.value, force=force, activate=not action.new.deactivated)
        self._repeating = False

    def _add_item(self, item):
        self.actions.insert(0, item)
        if len(self.actions)>self.depth:
            del self.actions[-1]
        if not self._repeating and isinstance(item, HistoryPropertyItem):
            path = item.path
            if path != self._redo_widget:
                self._redo_widget = path
                del self._redo_info[:]
            key = item.get_key()
            if not key in self._redo_info:
                self._redo_info.append(key)

        if self.actions_redo:
            del self.actions_redo[:]

        if config.debugging:
            print("UndoBuffer:")
            for entry in self.actions:
                print(entry)

    ####################################################################################################################
    # interface from Property instances
    def property_changing(self, prop):
        "to be called when property value is still the old one"
        value = prop.value
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
