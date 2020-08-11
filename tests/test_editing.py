"""
@copyright: 2019-2020 Dietmar Schwertberger

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from testsupport_new import WXGladeGUITest

import common, clipboard
import unittest, wx, time


class TestEditing(WXGladeGUITest):
    "Test for e.g. cut/paste; to be extended..."

    def test_crash_on_cut_paste(self):
        "with NEW_STRUCTURE at first there were crashes when cutting the static box sizer"
        basename = 'crash_on_cut_paste'
        infilename = self._get_casefile_path( '%s.wxg'%basename )
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        editor = common.root.find_widget_from_path( 'App/frame/sizer_limit/panel_3/sizer_8' )
        common.app_tree.show_toplevel( None, common.root.find_widget_from_path('App/frame') )
        self._process_wx_events()
        parent = editor.parent

        # cut
        data = clipboard.dump_widget(editor)
        editor.remove()
        self._process_wx_events()
        # paste again
        parent.clipboard_paste(data)
        self._process_wx_events()

        # save and check .wxg file
        generated_filename = self._get_outputfile_path(infilename)
        common.main._save_app(generated_filename)
        self._compare_files(infilename, generated_filename)

        # generate and check python code
        app = common.root

        expected_filename = self._get_casefile_path( '%s.py'%basename )
        generated_filename = self._get_outputfile_path( '%s.py'%basename )
        app.properties["output_path"].set(generated_filename)
        common.app_tree.root.generate_code()
        self._compare_files(expected_filename, generated_filename, check_mtime=True)

    def sleep(self, dt=1.0):
        end = time.time() + dt
        while time.time() < end:
            self._process_wx_events()

    def check_no_overlap(self, editor, rectangles=None):
        # recursively check that all widgets have been created and sizer children do not overlap
        if rectangles is None: rectangles = []
        for child in editor.get_all_children():
            if child.IS_SLOT and child.overlapped: continue
            if editor.IS_SIZER and not child.IS_SIZER:
                rect = child.widget.GetRect()
                for r in rectangles:
                    self.assertFalse( rect.Intersects(r) )
                rectangles.append(rect)
            elif editor.IS_SIZER and child.IS_SIZER:
                self.check_no_overlap(child, rectangles)
            self.check_no_overlap(child)

    def test_editing_1(self):
        basename = 'Test_Editing'
        infilename = self._get_casefile_path( '%s.wxg'%basename )
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        wx.SafeYield()
        app = common.root  # shortcut
        common.app_tree.show_toplevel( None, app.children[0] )
        # ensure that there's no overlap of elements
        self.check_no_overlap(app.children[0])

        # cut static box sizer
        widget = app.find_widget_from_path("app/frame/notebook_1/panel_1/sizer_2/sizer_1")
        parent = widget.parent
        index = widget.index
        data = self.simulate_cut(widget)
        # paste again
        self.simulate_paste(parent, index, data)

        # insert panel into splitter; change "Scrollable" to test re-creation
        widget = app.find_widget_from_path("app/frame/notebook_1/window_1/SLOT 1")
        import widgets.panel.panel
        panel = widgets.panel.panel.builder(widget.parent, widget.index)
        self.assertTrue(isinstance(panel.widget, wx.Panel))
        panel.properties["scrollable"].set(True, notify=True)
        self.assertTrue(isinstance(panel.widget, wx.ScrolledWindow))
        #panel.widget.GetSize()
        #wx.Size(404, 659)
        #self.sleep(1.0)

        # set span of button inside gridbag sizer
        widget = app.find_widget_from_path("app/frame/notebook_1/window_1/window_1_pane_1/grid_sizer_1/button_3")
        widget.properties["span"].set((2,2), notify=True)
        #self.sleep(1.0)

        # XXX test change_sizer

        ## save and check .wxg file
        #generated_filename = self._get_outputfile_path(infilename)
        #common.main._save_app(generated_filename)
        #self._compare_files(infilename, generated_filename)

    def test_editing_2(self):
        basename = 'Test_Editing2'
        infilename = self._get_casefile_path( '%s.wxg'%basename )
        common.main._open_app(infilename, use_progress_dialog=False, add_to_history=False)
        wx.SafeYield()
        app = common.root  # shortcut
        common.app_tree.show_toplevel( None, app.children[0] )
        # ensure that there's no overlap of elements
        self.check_no_overlap(app.children[0])
        
        # change font size for static text and asserts it's size change
        text = common.root.find_widget_from_path("app/frame/notebook_1/panel_1/sizer_2/static_text_1")
        rect1 = text.widget.GetRect()
        size1 = text.widget.GetSize()
        font_p = text.properties["font"]
        font_p.set( (42, 'default', 'normal', 'normal', 0, ''), notify=True)
        self.check_no_overlap(text.parent)
        return

        # cut static box sizer
        widget = app.find_widget_from_path("app/frame/notebook_1/panel_1/sizer_2/sizer_1")
        parent = widget.parent
        index = widget.index
        data = self.simulate_cut(widget)
        # paste again
        self.simulate_paste(parent, index, data)

        # insert panel into splitter; change "Scrollable" to test re-creation
        widget = app.find_widget_from_path("app/frame/notebook_1/window_1/SLOT 1")
        import widgets.panel.panel
        panel = widgets.panel.panel.builder(widget.parent, widget.index)
        self.assertTrue(isinstance(panel.widget, wx.Panel))
        panel.properties["scrollable"].set(True, notify=True)
        self.assertTrue(isinstance(panel.widget, wx.ScrolledWindow))

        # set span of button inside gridbag sizer
        widget = app.find_widget_from_path("app/frame/notebook_1/window_1/window_1_pane_1/grid_sizer_1/button_3")
        widget.properties["span"].set((2,2), notify=True)


if __name__ == '__main__':
    unittest.main(exit=False)
