"""\
Code generator functions for spacers

@copyright: 2019-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class SpacerMixin(object):
    "Generic code to handle spacer code in all language code generators"

    def get_code(self, obj):
        sizer = obj.parent  # parent is always a sizer
        sizer_name = self.codegen._format_classattr(sizer)
        width  = self.codegen.generate_code_dim(obj.properties["width" ], "x")
        height = self.codegen.generate_code_dim(obj.properties["height"], "y")
        border = self.codegen.generate_code_dim(obj.properties["border"], "x")
        flag = self.cn_f(obj.properties["flag"].get_string_value()) or '0'
        if sizer.klass!="wxGridBagSizer":
            size = self.codegen.tmpl_spacersize%(width, height)
            stmt = self.codegen.tmpl_sizeritem % ( sizer_name, size, obj.proportion, flag, border )
        else:
            # GridBagSizer
            pos = sizer._get_row_col(obj.pos)
            stmt = self.codegen.tmpl_gridbagsizerspacer % ( sizer_name, width, height, pos, obj.span, flag, border )
        return [stmt], []
