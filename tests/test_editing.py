"""
@copyright: 2019 Dietmar Schwertberger

@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


from testsupport_new import WXGladeGUITest

import common, clipboard
import unittest, os


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


if __name__ == '__main__':
    unittest.main(exit=False)
