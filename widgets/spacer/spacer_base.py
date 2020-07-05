"""\
Code generator functions for spacers

@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class SpacerMixin(object):
    "Generic code to handle spacer code in all language code generators"

    def get_code(self, obj):
        sizer = obj.parent  # parent is always a sizer
        sizer_name = self.codegen._format_classattr(sizer)
        size = (obj.width, obj.height)
        flag = self.cn_f(obj.properties["flag"].get_string_value()) or '0'
        if sizer.WX_CLASS!="wxGridBagSizer":
            size = self.codegen.tmpl_spacersize%size
            stmt = self.codegen.tmpl_sizeritem % ( sizer_name, size, obj.proportion, flag, obj.border )
        else:
            # GridBagSizer
            index = sizer._get_row_col(obj.index)
            stmt = self.codegen.tmpl_gridbagsizerspacer % ( sizer_name, size[0], size[1], index, obj.span, flag, obj.border )
        return [stmt], []
