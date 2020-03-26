"""
Handles the template tags and description

@copyright: 2002-2007 Alberto Griggio
@author: Guy Rutenberg
@author: Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import glob
import logging
import os
import wx
from xml.dom import minidom
from xml.sax import saxutils

import common
import config
import misc
import templates_ui


class Template(object):
    "A class that handles the specific aspects of template files"

    def __init__(self, filename=None):
        self.author = ''
        self.description = ''
        self.instructions = ''
        self.filename = filename

        if filename is not None:
            filexml = minidom.parse(filename)
            # we have no use for all the xml data in the file. We only care
            # about what is between the "description" tags
            templatedata = filexml.getElementsByTagName('templatedata')
            if len(templatedata):
                desc_xml = templatedata[0]
                try:
                    self.author = saxutils.unescape( desc_xml.getElementsByTagName('author')[0].firstChild.data )
                except (IndexError, AttributeError): self.author = ''
                try:
                    self.description = saxutils.unescape(
                        desc_xml.getElementsByTagName('description')[0].firstChild.data )
                except (IndexError, AttributeError): self.description = ''
                try:
                    self.instructions = saxutils.unescape(
                        desc_xml.getElementsByTagName( 'instructions')[0].firstChild.data)
                except (IndexError, AttributeError): self.instructions = ''
            else:
                self.author = ''
                self.description=''
                self.instructions=''

    def write(self, output, tabs):
        outer_tab = u'    ' * tabs
        stmt = [u'%s<templatedata>\n' % outer_tab]
        stmt += common.format_xml_tag(u'author', self.author, tabs + 1)
        stmt += common.format_xml_tag(u'description', self.description, tabs + 1)
        stmt += common.format_xml_tag(u'instructions', self.instructions, tabs + 1)
        stmt.append( u'%s</templatedata>\n' % outer_tab )
        output.extend(stmt)



class TemplateListDialog(templates_ui.TemplateListDialog):
    """\
    Class TemplateListDialog

    @ivar _logger: Class specific logging instance
    """

    def __init__(self):
        templates_ui.TemplateListDialog.__init__(self, None, -1, "")
        self.templates = []
        self.fill_template_list()
        self.selected_template = None

    def get_selected(self):
        index = self.template_names.GetSelection()
        if index >= 0:
            return self.templates[index]
        else:
            return None

    def on_open(self, event):
        self.selected_template = self.get_selected()
        self.EndModal(wx.ID_OPEN)

    def on_select_template(self, event):
        self.selected_template = self.get_selected()
        if self.selected_template is not None:
            t = Template(self.selected_template)
            self.set_template_name(self.template_names.GetStringSelection())
            self.author.SetValue(misc.wxstr(t.author))
            self.description.SetValue(misc.wxstr(t.description))
            self.instructions.SetValue(misc.wxstr(t.instructions))
            if os.path.dirname(self.selected_template) == config.templates_path:
                self.btn_delete.Disable()
                self.btn_edit.Disable()
            else:
                self.btn_delete.Enable()
                self.btn_edit.Enable()
        else:
            self.set_template_name("")
            self.author.SetValue("")
            self.description.SetValue("")
            self.instructions.SetValue("")
        if event:
            event.Skip()

    def set_template_name(self, name):
        self.template_name.SetLabel(_("wxGlade template:\n") + misc.wxstr(name))

    def on_edit(self, event):
        self.selected_template = self.get_selected()
        self.EndModal(wx.ID_EDIT)

    def on_delete(self, event):
        self.selected_template = self.get_selected()
        if self.selected_template is not None:
            name = self.template_names.GetStringSelection()
            if wx.MessageBox( _("Delete template '%s'?") % misc.wxstr(name),
                              _("Are you sure?"), style=wx.YES|wx.NO|wx.CENTRE) == wx.YES:
                try:
                    os.unlink(self.selected_template)
                except Exception:
                    logging.exception(_('Internal Error'))
                self.fill_template_list()
                self.selected_template = None

    def fill_template_list(self):
        self.templates = load_templates()
        self.template_names.Clear()
        if self.templates:
            for n in self.templates:
                self.template_names.Append( os.path.splitext(os.path.basename(n))[0] )
                # show details of first template
                self.template_names.SetSelection(0)
                self.on_select_template(None)

# end of class TemplateListDialog


def load_templates():
    "Finds all the available templates"
    if config.appdata_path != config.wxglade_path:
        extra = glob.glob( os.path.join(config.appdata_path, "templates", "*.wgt") )
    else:
        extra = []
    return sorted(glob.glob(os.path.join(config.templates_path, "*.wgt"))) + sorted(extra)


def select_template():
    "Returns the filename of a template to load"
    dlg = TemplateListDialog()
    dlg.btn_delete.Hide()
    dlg.btn_edit.Hide()
    if dlg.ShowModal() == wx.ID_OPEN:
        ret = dlg.selected_template
    else:
        ret = None
    dlg.Destroy()
    return ret


def save_template(data=None):
    "Returns an out file name and template description for saving a template"
    dlg = templates_ui.TemplateInfoDialog(None, -1, "")
    if data is not None:
        dlg.template_name.SetValue( misc.wxstr(os.path.basename(os.path.splitext(data.filename)[0])) )
        dlg.author.SetValue(misc.wxstr(data.author))
        dlg.description.SetValue(misc.wxstr(data.description))
        dlg.instructions.SetValue(misc.wxstr(data.instructions))
    ret = None
    retdata = Template()
    if dlg.ShowModal() == wx.ID_OK:
        ret = dlg.template_name.GetValue().strip()
        retdata.author = dlg.author.GetValue()
        retdata.description = dlg.description.GetValue()
        retdata.instructions = dlg.instructions.GetValue()
        if not ret:
            wx.MessageBox( _("Can't save a template with an empty name"), _("Error"), wx.OK|wx.ICON_ERROR )
    dlg.Destroy()
    name = ret
    if ret:
        template_directory = os.path.join(config.appdata_path, 'templates')
        if not os.path.exists(template_directory):
            try:
                os.makedirs(template_directory)
            except EnvironmentError:
                logging.exception( _('ERROR creating directory "%s"'), template_directory )
                return None, retdata
        ret = os.path.join(template_directory, ret + '.wgt')
    if ret and os.path.exists(ret) and \
       wx.MessageBox( _("A template called '%s' already exists:\ndo you want to overwrite it?") % name,
                      _("Question"), wx.YES|wx.NO|wx.ICON_QUESTION) != wx.YES:
        ret = None
    return ret, retdata


def manage_templates():
    dlg = TemplateListDialog()
    dlg.btn_open.Hide()
    #dlg.btn_edit.Hide()
    ret = None
    if dlg.ShowModal() == templates_ui.ID_EDIT:
        ret = dlg.selected_template
    dlg.Destroy()
    return ret

