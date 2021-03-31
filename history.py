"""\
history for undo/redo/repeat

copyright: 2017-2020 Dietmar Schwertberger
license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, config, clipboard, misc
import wx


def copy_value(prop):
    # make a copy for lists or sets
    if hasattr(prop, "value_set"):
        value = prop.value_set
    else:
        value = prop.value
    if isinstance(value, list):
        if value and isinstance(value[0], list):
            return [v[:] for v in value]
        else:
            return value[:]
    elif isinstance(value, set):
        return set(value)
    return value


class PropertyValue(object):
    # used by HistoryPropertyItem to track old and new value and state of a property
    def __init__(self, prop):
        self.deactivated = prop.deactivated
        self.value = copy_value(prop)
        self.modified = prop.modified

    def set(self, p):
        # apply value to a property instance
        if hasattr(p, "value_set"):
            p.value_set.clear()
            p.value_set.update(self.value)
            p.value = None
        else:
            value = p.value
            # lists and sets are modified in place
            if isinstance(value, list):
                value[:] = self.value
            elif isinstance(value, set):  # not yet used
                value.clear()
                value.update(self.value)
            else:
                p.value = self.value
        p.modified = self.modified
        activate = self.deactivated!=p.deactivated
        p.deactivated = self.deactivated
        p.update_display()
        if activate: p.activate_controls()

    def __eq__(self, other):
        return self.deactivated==other.deactivated and self.value==other.value and self.modified==other.modified

    def __repr__(self):
        return "(%r, %r, %r)"%(self.deactivated, self.value, self.modified)


class HistoryItem(object):
    def __init__(self, prop):
        self.path = prop.owner.get_path()
        self.name = prop.name

    def get_key(self):
        return self.name


class HistoryPropertyItem(HistoryItem):
    def __init__(self, prop):
        HistoryItem.__init__(self, prop)
        self._prop = prop  # keep a reference until finalize is called
        self.old = PropertyValue(prop)
        self.new = None
        self.dependent = []

    def finalize(self, monitor):
        self.new = PropertyValue(self._prop)
        self._prop = None
        # check whether other, depending properties were changed as well
        for prop, old in monitor:
            new = PropertyValue(prop)
            if new!=old:
                self.dependent.append( [prop.owner.get_path(), prop.name, old, new] )

    def undo(self):
        owner = common.root.find_widget_from_path(self.path)
        p = owner.properties[self.name]
        self.old.set(p)
        changed = [self.name]
        for path, name, old, new in self.dependent:
            changed.append(name)
            if path==self.path:
                old.set( owner.properties[name] )
            else:
                owner_ = common.root.find_widget_from_path(self.path)
                old.set( owner_.properties[name] )
        owner.properties_changed(changed)
        misc.set_focused_widget(owner)

    def redo(self):
        owner = common.root.find_widget_from_path(self.path)
        p = owner.properties[self.name]
        self.new.set(p)
        changed = [self.name]
        for path, name, old, new in self.dependent:
            changed.append(name)
            if path==self.path:
                new.set( owner.properties[name] )
            else:
                owner_ = common.root.find_widget_from_path(self.path)
                new.set( owner_.properties[name] )
        owner.properties_changed(changed)
        misc.set_focused_widget(owner)

    def __repr__(self):
        return "%s(%s, %r, %r, %r)"%(self.__class__.__name__, self.path, self.name, self.old, self.new)


class HistorySetPropertyItem(HistoryPropertyItem):
    # same as before, but tracks the flag that was checked/unchecked (self.flag_value, self.checked)
    def __init__(self, prop):
        HistoryPropertyItem.__init__(self, prop)
        self.flag_value = self.checked = None

    def __repr__(self):
        return "%s(%s, %s, %r, %r)"%(self.__class__.__name__, self.path, self.name, self.flag_value, self.checked)

    def get_key(self):
        return (self.name, self.flag_value)


class HistoryRemovedItem(HistoryItem):
    def __init__(self, IS_SLOT, index, path, xml_data, slot_path=None, tab_name=None):
        self.IS_SLOT = IS_SLOT
        self.index = index
        self.path = path
        self.xml_data = xml_data
        self.slot_path = slot_path
        self.slot_tab = tab_name

    def undo(self):
        path = self.slot_path or self.path.rsplit("/",1)[0]  # slot or parent
        widget = common.root.find_widget_from_path(path)
        if widget.IS_ROOT:# or widget.IS_CONTAINER:
            widget.clipboard_paste(self.xml_data, self.index)
        elif self.IS_SLOT:
            widget.insert_item(None, self.index)  # placeholder
            if self.slot_tab is not None:
                tabs_p = widget.properties["tabs"]
                tabs_p.value.insert(self.index, [self.slot_tab])
                tabs_p.reset()
            if widget.IS_SIZER:
                from edit_sizers import SizerSlot as Slot
            else:
                from edit_base import Slot
            slot = Slot(widget, index=self.index)
            if widget.widget:
                slot.create_widget()
                widget.child_widget_created(slot, 0)
            # update structure
            misc.rebuild_tree( widget, focus=False )
            # update following slots
            for c in widget.children[self.index+1:]:
                if c.IS_SLOT: common.app_tree.refresh(c)
            misc.set_focused_widget(slot)
        else:
            widget.clipboard_paste(self.xml_data)

    def redo(self):
        widget = common.root.find_widget_from_path(self.path)
        widget.remove(user=False)


class HistoryFilledSlot(HistoryItem):
    def __init__(self, item):
        self.path = item.get_path()
        self.xml_data = None  # will be set on undo
        self.slot_path = None

    def undo(self):
        item = common.root.find_widget_from_path(self.path)
        if self.xml_data is None:
            self.xml_data = clipboard.dump_widget(item)
        slot = item.remove(user=False)
        self.slot_path = slot.get_path()

    def redo(self):
        slot = common.root.find_widget_from_path(self.slot_path)
        clipboard._paste(slot.parent, slot.index, self.xml_data, rebuild_tree=True)


class HistorySizerSlots(HistoryItem):
    # for added/inserted slots
    def __init__(self, sizer, index, count=1):
        self.path = sizer.get_path()
        self.index = index
        self.count = count

    def undo(self):
        sizer = common.root.find_widget_from_path(self.path)
        for n in range(self.count):
            sizer.children[self.index].remove(user=False)

    def redo(self):
        sizer = common.root.find_widget_from_path(self.path)
        with sizer.window.frozen():
            for n in range(self.count):
                if self.index==-1:
                    sizer._add_slot()
                else:
                    sizer._insert_slot(self.index+n)
            if sizer.widget: sizer.layout()
        misc.rebuild_tree( sizer, recursive=False, focus=False )


class HistoryGridSizerRowCol(HistoryItem):
    # for added / removed rows / cols
    def __init__(self, sizer, type, index, count=1, inserted_slots=None):
        # count: negative if removed
        self.path = sizer.get_path()
        self.type = type  # "row" or "col"
        self.index = index  # row or column index
        self.count = count
        self.inserted_slots = inserted_slots

    def undo(self):
        sizer = common.root.find_widget_from_path(self.path)
        if self.type=="row" and self.count==1:
            sizer.remove_row(self.index, user=False, remove_slots=self.inserted_slots)
        elif self.type=="row" and self.count==-1:
            sizer.insert_row(self.index, user=False)
        elif self.type=="col" and self.count==1:
            sizer.remove_col(self.index, user=False, remove_slots=self.inserted_slots)
        elif self.type=="col" and self.count==-1:
            sizer.insert_col(self.index, user=False)

    def redo(self):
        sizer = common.root.find_widget_from_path(self.path)
        if self.type=="row" and self.count==1:
            sizer.insert_row(self.index, user=False)
        elif self.type=="row" and self.count==-1:
            sizer.remove_row(self.index, user=False)
        elif self.type=="col" and self.count==1:
            sizer.insert_col(self.index, user=False)
        elif self.type=="col" and self.count==-1:
            sizer.remove_col(self.index, user=False)


class History(object):
    def __init__(self, depth=20):
        self.actions = []
        self.actions_redo = [] # on undo, the action is moved from actions to actions_redo
        self.depth = depth
        self._buffer = None
        self._redo_widget = None # the widget that originally was modified
        self._redo_info = []  # name of properties
        self._repeating = False
        self.can_undo = self.can_redo = self.can_repeat = False

    def reset(self):
        self.actions.clear()
        self.actions_redo.clear()
        self.can_undo = False
        self.can_redo = False
        self.can_repeat = len(self._redo_info) > 1

    def set_widget(self, widget):
        # for enabling/disabling tools and menus
        path = widget and widget.get_path() or []
        if path==self._redo_widget or self._redo_widget is None:
            self.can_repeat = self.can_redo = False
        else:
            self.can_redo = True
            self.can_repeat = len(self._redo_info) > 1
        self.can_undo = bool(self.actions)

    def undo(self, focused_widget):
        if not self.actions:
            return wx.Bell()
        action = self.actions.pop(0)
        action.undo()
        self.actions_redo.append(action)

    def redo(self, focused_widget):
        if not self.actions_redo:
            # XXX check whether it's the same
            repeated = self.repeat(focused_widget, multiple=False)
            if not repeated: wx.Bell()
            return
        action = self.actions_redo.pop(-1)
        action.redo()
        self.actions.insert(0, action)

    def repeat(self, focused_widget, multiple=True):
        "apply action(s) to another widget"
        if focused_widget is None: return False
        if not self.actions or not isinstance(self.actions[0], HistoryPropertyItem): return False
        if not self._redo_widget: return False
        path = focused_widget.get_path()
        if path==self._redo_widget: return False

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
        return True

    def add_item(self, item):
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
            self.actions_redo.clear()

        if config.debugging:
            print("UndoBuffer:")
            for entry in self.actions:
                print(entry)

    ####################################################################################################################
    # property changes: interface from Property instances
    def property_changing(self, prop):
        "to be called when property value is still the old one"
        if config.debugging: print("property_changing", prop)
        self._buffer = HistoryPropertyItem(prop)
        self._monitor = []  # list of (property, PropertyValue)

    def set_property_changing(self, prop):
        # same as before, but this will track the clicked flag
        if config.debugging: print("set_property_changing", prop)
        self._buffer = HistorySetPropertyItem(prop)
        self._monitor = []  # list of (property, PropertyValue)

    def monitor_property(self, prop):
        # monitor dependent properties; these will be un-/re-done together with the main property
        if self._buffer is None: return
        self._monitor.append( (prop, PropertyValue(prop)))

    def _finalize_item(self, stop=False):
        # helper
        item = self._buffer
        self._buffer = None  if not stop else  False
        item.finalize(self._monitor)
        self._monitor.clear()
        return item

    def property_changed(self, prop, user=True):
        "argument user: True if set by the user, False if set in dependence to another change"
        if config.debugging: print("property_changed", prop, user)
        if self._buffer is False:  # e.g. handled already by another item, e.g. HistoryNotebookTabsItem
            self._buffer = None
            return

        item = self._finalize_item()
        if item.new==item.old: return
        self.add_item(item)

    def set_property_changed(self, prop, value, checked, user=True):
        if self._buffer:
            # track which checkbox was checked
            self._buffer.flag_value = value
            self._buffer.checked = checked
        self.property_changed(prop, user=True)

    ####################################################################################################################
    # structural changes
    def widget_added(self, widget):
        self.add_item( HistoryFilledSlot(widget) )

    def widget_removing(self, widget):
        # store information and XML data before the widget is actually removed
        self._buffer = (widget.IS_SLOT, widget.index, widget.get_path(), clipboard.dump_widget(widget))
        self._tab = None
        parent = widget.parent
        if widget.IS_SLOT and parent.IS_CONTAINER:
            if parent.check_prop("tabs"):
                self._tab = parent.tabs[widget.index][0]

    def widget_removed(self, slot=None):
        if slot: slot = slot.get_path()
        self.add_item( HistoryRemovedItem(*self._buffer, slot, self._tab) )
        self._buffer = None

    # sizers
    def sizer_slots_added(self, sizer, index, count):
        # called from SizerBase.insert_slot and add_slot
        self.add_item( HistorySizerSlots(sizer, index, count) )

    def gridsizer_row_col_changed(self, sizer, type, index, count, inserted_slots=None):
        self.add_item( HistoryGridSizerRowCol(sizer, type, index, count, inserted_slots) )
