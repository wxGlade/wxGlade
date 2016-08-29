"""\
Code shared between different language code generators

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class RadioBoxMixin(object):
    "Generic code to handle wxRadioBox code in all language code generators"

    default_style = 'wxRA_SPECIFY_COLS'
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        super(RadioBoxMixin, self)._prepare_tmpl_content(obj)

        # wx raises an assertion if choices are empty and majorDim is 0
        majorDimension = obj.properties.get('dimension', '1')
        choices = obj.properties.get('choices', [])
        if not choices and majorDimension == '0':
            majorDimension = '1'

        self.tmpl_dict['majorDimension'] = majorDimension

        return