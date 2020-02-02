"""\
Code shared between different language code generators

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class CheckBoxMixin(object):
    "Generic code to handle wxCheckbox code in all language code generators"

    def _prepare_checkbox_content(self, obj):
        """
        Prepare template variables for 3-state checkbox

        obj: Instance of xml_parse.CodeObject
        obj: xml_parse.CodeObject"""
        checked = obj.checked
        if self.cn_f('wxCHK_3STATE') in self.tmpl_dict['style']:
            checked = self.config['number2state'][checked]
            checked = self.cn_f(checked)
            self.tmpl_dict['value_3state'] = checked
        else:
            self.has_setvalue1 = checked

    def _get_checkbox_code(self):
        "Returns code to set the state of a 3-state checkbox to prop_lines."
        if self.cn_f('wxCHK_3STATE') in self.tmpl_dict['style']:
            return [self.tmpl_set3statevalue % self.tmpl_dict]
        return []
