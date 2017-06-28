"""\
Code shared between different language code generators

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class CheckBoxMixin(object):
    """\
    Generic code to handle wxCheckbox code in all language code generators
    """

    def _prepare_checkbox_content(self, obj):
        """\
        Prepare template variables for 3-state checkbox

        @param obj: Instance of L{xml_parse.CodeObject}
        @type obj: xml_parse.CodeObject
        """
        checked = obj.checked
        if self.cn_f('wxCHK_3STATE') in self.tmpl_dict['style']:
            checked = self.config['number2state'][checked]
            checked = self.cn_f(checked)
            self.tmpl_dict['value_3state'] = checked
        else:
            self.has_setvalue1 = checked

    def _get_checkbox_code(self, prop_lines):
        """\
        Add code to set the state of a 3-state checkbox to prop_lines.

        @param prop_lines:
        @type prop_lines: list
        """
        if self.cn_f('wxCHK_3STATE') in self.tmpl_dict['style']:
            prop_lines.append(self.tmpl_set3statevalue % self.tmpl_dict)
        return
