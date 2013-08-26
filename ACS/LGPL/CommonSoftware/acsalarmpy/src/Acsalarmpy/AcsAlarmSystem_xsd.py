#!/usr/bin/env python

#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2011
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
#
# Generated Tue Oct  7 21:14:02 2008 by generateDS.py.
#

import sys
import getopt
from string import lower as str_lower
from xml.dom import minidom
from xml.dom import Node

#
# If you have installed IPython you can uncomment and use the following.
# IPython is available from http://ipython.scipy.org/.
#

## from IPython.Shell import IPShellEmbed
## args = ''
## ipshell = IPShellEmbed(args,
##     banner = 'Dropping into IPython',
##     exit_msg = 'Leaving Interpreter, back to program.')

# Then use the following line where and when you want to drop into the
# IPython shell:
#    ipshell('<some message> -- Entering ipshell.\nHit Ctrl-D to exit')

#
# Support/utility functions.
#

def showIndent(outfile, level):
    for idx in range(level):
        outfile.write('    ')

def quote_xml(inStr):
    s1 = (isinstance(inStr, basestring) and inStr or
          '%s' % inStr)
    s1 = s1.replace('&', '&amp;')
    s1 = s1.replace('<', '&lt;')
    s1 = s1.replace('>', '&gt;')
    return s1

def quote_attrib(inStr):
    s1 = (isinstance(inStr, basestring) and inStr or
          '%s' % inStr)
    s1 = s1.replace('&', '&amp;')
    s1 = s1.replace('"', '&quot;')
    s1 = s1.replace('<', '&lt;')
    s1 = s1.replace('>', '&gt;')
    return s1

def quote_python(inStr):
    s1 = inStr
    if s1.find("'") == -1:
        if s1.find('\n') == -1:
            return "'%s'" % s1
        else:
            return "'''%s'''" % s1
    else:
        if s1.find('"') != -1:
            s1 = s1.replace('"', '\\"')
        if s1.find('\n') == -1:
            return '"%s"' % s1
        else:
            return '"""%s"""' % s1


class MixedContainer:
    # Constants for category:
    CategoryNone = 0
    CategoryText = 1
    CategorySimple = 2
    CategoryComplex = 3
    # Constants for content_type:
    TypeNone = 0
    TypeText = 1
    TypeString = 2
    TypeInteger = 3
    TypeFloat = 4
    TypeDecimal = 5
    TypeDouble = 6
    TypeBoolean = 7
    def __init__(self, category, content_type, name, value):
        self.category = category
        self.content_type = content_type
        self.name = name
        self.value = value
    def getCategory(self):
        return self.category
    def getContenttype(self, content_type):
        return self.content_type
    def getValue(self):
        return self.value
    def getName(self):
        return self.name
    def export(self, outfile, level, name):
        if self.category == MixedContainer.CategoryText:
            outfile.write(self.value)
        elif self.category == MixedContainer.CategorySimple:
            self.exportSimple(outfile, level, name)
        else:    # category == MixedContainer.CategoryComplex
            self.value.export(outfile, level, name)
    def exportSimple(self, outfile, level, name):
        if self.content_type == MixedContainer.TypeString:
            outfile.write('<%s>%s</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeInteger or \
                self.content_type == MixedContainer.TypeBoolean:
            outfile.write('<%s>%d</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeFloat or \
                self.content_type == MixedContainer.TypeDecimal:
            outfile.write('<%s>%f</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeDouble:
            outfile.write('<%s>%g</%s>' % (self.name, self.value, self.name))
    def exportLiteral(self, outfile, level, name):
        if self.category == MixedContainer.CategoryText:
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s", "%s"),\n' % \
                (self.category, self.content_type, self.name, self.value))
        elif self.category == MixedContainer.CategorySimple:
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s", "%s"),\n' % \
                (self.category, self.content_type, self.name, self.value))
        else:    # category == MixedContainer.CategoryComplex
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s",\n' % \
                (self.category, self.content_type, self.name,))
            self.value.exportLiteral(outfile, level + 1)
            showIndent(outfile, level)
            outfile.write(')\n')


class _MemberSpec(object):
    def __init__(self, name='', data_type='', container=0):
        self.name = name
        self.data_type = data_type
        self.container = container
    def set_name(self, name): self.name = name
    def get_name(self): return self.name
    def set_data_type(self, data_type): self.data_type = data_type
    def get_data_type(self): return self.data_type
    def set_container(self, container): self.container = container
    def get_container(self): return self.container


#
# Data representation classes.
#

class alarm_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, alarms_to_create=None, alarms_to_update=None, alarms_to_remove=None):
        self.alarms_to_create = alarms_to_create
        self.alarms_to_update = alarms_to_update
        self.alarms_to_remove = alarms_to_remove
    def factory(*args_, **kwargs_):
        if alarm_definitions.subclass:
            return alarm_definitions.subclass(*args_, **kwargs_)
        else:
            return alarm_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarms_to_create(self): return self.alarms_to_create
    def set_alarms_to_create(self, alarms_to_create): self.alarms_to_create = alarms_to_create
    def get_alarms_to_update(self): return self.alarms_to_update
    def set_alarms_to_update(self, alarms_to_update): self.alarms_to_update = alarms_to_update
    def get_alarms_to_remove(self): return self.alarms_to_remove
    def set_alarms_to_remove(self, alarms_to_remove): self.alarms_to_remove = alarms_to_remove
    def export(self, outfile, level, namespace_='', name_='alarm-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-definitions'):
        if self.get_alarms_to_create() != None :
            if self.alarms_to_create:
                self.alarms_to_create.export(outfile, level, namespace_, name_='alarms-to-create')
        if self.get_alarms_to_update() != None :
            if self.alarms_to_update:
                self.alarms_to_update.export(outfile, level, namespace_, name_='alarms-to-update')
        if self.get_alarms_to_remove() != None :
            if self.alarms_to_remove:
                self.alarms_to_remove.export(outfile, level, namespace_, name_='alarms-to-remove')
    def exportLiteral(self, outfile, level, name_='alarm-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarms_to_create:
            showIndent(outfile, level)
            outfile.write('alarms_to_create=alarm_definition_listType(\n')
            self.alarms_to_create.exportLiteral(outfile, level, name_='alarms_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.alarms_to_update:
            showIndent(outfile, level)
            outfile.write('alarms_to_update=alarm_definition_listType(\n')
            self.alarms_to_update.exportLiteral(outfile, level, name_='alarms_to_update')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.alarms_to_remove:
            showIndent(outfile, level)
            outfile.write('alarms_to_remove=alarm_definition_listType(\n')
            self.alarms_to_remove.exportLiteral(outfile, level, name_='alarms_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarms-to-create':
            obj_ = alarm_definition_listType.factory()
            obj_.build(child_)
            self.set_alarms_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarms-to-update':
            obj_ = alarm_definition_listType.factory()
            obj_.build(child_)
            self.set_alarms_to_update(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarms-to-remove':
            obj_ = alarm_definition_listType.factory()
            obj_.build(child_)
            self.set_alarms_to_remove(obj_)
# end class alarm_definitions


class source_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, sources_to_create=None, sources_to_update=None, sources_to_remove=None):
        self.sources_to_create = sources_to_create
        self.sources_to_update = sources_to_update
        self.sources_to_remove = sources_to_remove
    def factory(*args_, **kwargs_):
        if source_definitions.subclass:
            return source_definitions.subclass(*args_, **kwargs_)
        else:
            return source_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_sources_to_create(self): return self.sources_to_create
    def set_sources_to_create(self, sources_to_create): self.sources_to_create = sources_to_create
    def get_sources_to_update(self): return self.sources_to_update
    def set_sources_to_update(self, sources_to_update): self.sources_to_update = sources_to_update
    def get_sources_to_remove(self): return self.sources_to_remove
    def set_sources_to_remove(self, sources_to_remove): self.sources_to_remove = sources_to_remove
    def export(self, outfile, level, namespace_='', name_='source-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='source-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='source-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='source-definitions'):
        if self.get_sources_to_create() != None :
            if self.sources_to_create:
                self.sources_to_create.export(outfile, level, namespace_, name_='sources-to-create')
        if self.get_sources_to_update() != None :
            if self.sources_to_update:
                self.sources_to_update.export(outfile, level, namespace_, name_='sources-to-update')
        if self.get_sources_to_remove() != None :
            if self.sources_to_remove:
                self.sources_to_remove.export(outfile, level, namespace_, name_='sources-to-remove')
    def exportLiteral(self, outfile, level, name_='source-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.sources_to_create:
            showIndent(outfile, level)
            outfile.write('sources_to_create=source_definition_listType(\n')
            self.sources_to_create.exportLiteral(outfile, level, name_='sources_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.sources_to_update:
            showIndent(outfile, level)
            outfile.write('sources_to_update=source_definition_listType(\n')
            self.sources_to_update.exportLiteral(outfile, level, name_='sources_to_update')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.sources_to_remove:
            showIndent(outfile, level)
            outfile.write('sources_to_remove=source_definition_listType(\n')
            self.sources_to_remove.exportLiteral(outfile, level, name_='sources_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'sources-to-create':
            obj_ = source_definition_listType.factory()
            obj_.build(child_)
            self.set_sources_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'sources-to-update':
            obj_ = source_definition_listType.factory()
            obj_.build(child_)
            self.set_sources_to_update(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'sources-to-remove':
            obj_ = source_definition_listType.factory()
            obj_.build(child_)
            self.set_sources_to_remove(obj_)
# end class source_definitions


class category_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, categories_to_create=None, categories_to_update=None, categories_to_remove=None):
        self.categories_to_create = categories_to_create
        self.categories_to_update = categories_to_update
        self.categories_to_remove = categories_to_remove
    def factory(*args_, **kwargs_):
        if category_definitions.subclass:
            return category_definitions.subclass(*args_, **kwargs_)
        else:
            return category_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_categories_to_create(self): return self.categories_to_create
    def set_categories_to_create(self, categories_to_create): self.categories_to_create = categories_to_create
    def get_categories_to_update(self): return self.categories_to_update
    def set_categories_to_update(self, categories_to_update): self.categories_to_update = categories_to_update
    def get_categories_to_remove(self): return self.categories_to_remove
    def set_categories_to_remove(self, categories_to_remove): self.categories_to_remove = categories_to_remove
    def export(self, outfile, level, namespace_='', name_='category-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='category-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='category-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='category-definitions'):
        if self.get_categories_to_create() != None :
            if self.categories_to_create:
                self.categories_to_create.export(outfile, level, namespace_, name_='categories-to-create')
        if self.get_categories_to_update() != None :
            if self.categories_to_update:
                self.categories_to_update.export(outfile, level, namespace_, name_='categories-to-update')
        if self.get_categories_to_remove() != None :
            if self.categories_to_remove:
                self.categories_to_remove.export(outfile, level, namespace_, name_='categories-to-remove')
    def exportLiteral(self, outfile, level, name_='category-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.categories_to_create:
            showIndent(outfile, level)
            outfile.write('categories_to_create=category_definition_listType(\n')
            self.categories_to_create.exportLiteral(outfile, level, name_='categories_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.categories_to_update:
            showIndent(outfile, level)
            outfile.write('categories_to_update=category_definition_listType(\n')
            self.categories_to_update.exportLiteral(outfile, level, name_='categories_to_update')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.categories_to_remove:
            showIndent(outfile, level)
            outfile.write('categories_to_remove=category_definition_listType(\n')
            self.categories_to_remove.exportLiteral(outfile, level, name_='categories_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'categories-to-create':
            obj_ = category_definition_listType.factory()
            obj_.build(child_)
            self.set_categories_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'categories-to-update':
            obj_ = category_definition_listType.factory()
            obj_.build(child_)
            self.set_categories_to_update(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'categories-to-remove':
            obj_ = category_definition_listType.factory()
            obj_.build(child_)
            self.set_categories_to_remove(obj_)
# end class category_definitions


class alarm_definitionType(object):
    subclass = None
    superclass = None
    def __init__(self, visual_fields=None, instant=False, cause='', action='', consequence='', priority='0', responsible_id='', piquetGSM='', help_url='', source_name='', location=None, piquetEmail=''):
        self.visual_fields = visual_fields
        self.instant = instant
        self.cause = cause
        self.action = action
        self.consequence = consequence
        self.priority = priority
        self.responsible_id = responsible_id
        self.piquetGSM = piquetGSM
        self.help_url = help_url
        self.source_name = source_name
        self.location = location
        self.piquetEmail = piquetEmail
    def factory(*args_, **kwargs_):
        if alarm_definitionType.subclass:
            return alarm_definitionType.subclass(*args_, **kwargs_)
        else:
            return alarm_definitionType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_visual_fields(self): return self.visual_fields
    def set_visual_fields(self, visual_fields): self.visual_fields = visual_fields
    def get_instant(self): return self.instant
    def set_instant(self, instant): self.instant = instant
    def get_cause(self): return self.cause
    def set_cause(self, cause): self.cause = cause
    def validate_cause(self, value):
        # validate type cause
        pass
    def get_action(self): return self.action
    def set_action(self, action): self.action = action
    def validate_action(self, value):
        # validate type action
        pass
    def get_consequence(self): return self.consequence
    def set_consequence(self, consequence): self.consequence = consequence
    def validate_consequence(self, value):
        # validate type consequence
        pass
    def get_priority(self): return self.priority
    def set_priority(self, priority): self.priority = priority
    def validate_priority(self, value):
        # validate type priority
        pass
    def get_responsible_id(self): return self.responsible_id
    def set_responsible_id(self, responsible_id): self.responsible_id = responsible_id
    def validate_responsible_id(self, value):
        # validate type responsible_id
        pass
    def get_piquetGSM(self): return self.piquetGSM
    def set_piquetGSM(self, piquetGSM): self.piquetGSM = piquetGSM
    def validate_piquetGSM(self, value):
        # validate type piquetGSM
        pass
    def get_help_url(self): return self.help_url
    def set_help_url(self, help_url): self.help_url = help_url
    def validate_help_url(self, value):
        # validate type help_url
        pass
    def get_source_name(self): return self.source_name
    def set_source_name(self, source_name): self.source_name = source_name
    def validate_source_name(self, value):
        # validate type source_name
        pass
    def get_location(self): return self.location
    def set_location(self, location): self.location = location
    def get_piquetEmail(self): return self.piquetEmail
    def set_piquetEmail(self, piquetEmail): self.piquetEmail = piquetEmail
    def validate_piquetEmail(self, value):
        # validate type piquetEmail
        pass
    def export(self, outfile, level, namespace_='', name_='alarm-definitionType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-definitionType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-definitionType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-definitionType'):
        if self.visual_fields:
            self.visual_fields.export(outfile, level, namespace_, name_='visual-fields', )
        showIndent(outfile, level)
        outfile.write('<%sinstant>%s</%sinstant>\n' % (namespace_, str_lower(str(self.get_instant())), namespace_))
        if self.get_cause() != None :
            if self.get_cause() != "" :
                showIndent(outfile, level)
                outfile.write('<%scause>%s</%scause>\n' % (namespace_, quote_xml(self.get_cause()), namespace_))
        if self.get_action() != None :
            if self.get_action() != "" :
                showIndent(outfile, level)
                outfile.write('<%saction>%s</%saction>\n' % (namespace_, quote_xml(self.get_action()), namespace_))
        if self.get_consequence() != None :
            if self.get_consequence() != "" :
                showIndent(outfile, level)
                outfile.write('<%sconsequence>%s</%sconsequence>\n' % (namespace_, quote_xml(self.get_consequence()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%spriority>%s</%spriority>\n' % (namespace_, quote_xml(self.get_priority()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%sresponsible-id>%s</%sresponsible-id>\n' % (namespace_, quote_xml(self.get_responsible_id()), namespace_))
        if self.get_piquetGSM() != None :
            if self.get_piquetGSM() != "" :
                showIndent(outfile, level)
                outfile.write('<%spiquetGSM>%s</%spiquetGSM>\n' % (namespace_, quote_xml(self.get_piquetGSM()), namespace_))
        if self.get_help_url() != None :
            if self.get_help_url() != "" :
                showIndent(outfile, level)
                outfile.write('<%shelp-url>%s</%shelp-url>\n' % (namespace_, quote_xml(self.get_help_url()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%ssource-name>%s</%ssource-name>\n' % (namespace_, quote_xml(self.get_source_name()), namespace_))
        if self.get_location() != None :
            if self.location:
                self.location.export(outfile, level, namespace_, name_='location')
        if self.get_piquetEmail() != None :
            if self.get_piquetEmail() != "" :
                showIndent(outfile, level)
                outfile.write('<%spiquetEmail>%s</%spiquetEmail>\n' % (namespace_, quote_xml(self.get_piquetEmail()), namespace_))
    def exportLiteral(self, outfile, level, name_='alarm-definitionType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.visual_fields:
            showIndent(outfile, level)
            outfile.write('visual_fields=visual_fields(\n')
            self.visual_fields.exportLiteral(outfile, level, name_='visual_fields')
            showIndent(outfile, level)
            outfile.write('),\n')
        showIndent(outfile, level)
        outfile.write('instant=%s,\n' % self.get_instant())
        showIndent(outfile, level)
        outfile.write('cause=%s,\n' % quote_python(self.get_cause()))
        showIndent(outfile, level)
        outfile.write('action=%s,\n' % quote_python(self.get_action()))
        showIndent(outfile, level)
        outfile.write('consequence=%s,\n' % quote_python(self.get_consequence()))
        showIndent(outfile, level)
        outfile.write('priority=%s,\n' % quote_python(self.get_priority()))
        showIndent(outfile, level)
        outfile.write('responsible_id=%s,\n' % quote_python(self.get_responsible_id()))
        showIndent(outfile, level)
        outfile.write('piquetGSM=%s,\n' % quote_python(self.get_piquetGSM()))
        showIndent(outfile, level)
        outfile.write('help_url=%s,\n' % quote_python(self.get_help_url()))
        showIndent(outfile, level)
        outfile.write('source_name=%s,\n' % quote_python(self.get_source_name()))
        if self.location:
            showIndent(outfile, level)
            outfile.write('location=locationType(\n')
            self.location.exportLiteral(outfile, level, name_='location')
            showIndent(outfile, level)
            outfile.write('),\n')
        showIndent(outfile, level)
        outfile.write('piquetEmail=%s,\n' % quote_python(self.get_piquetEmail()))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'visual-fields':
            obj_ = visual_fields.factory()
            obj_.build(child_)
            self.set_visual_fields(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'instant':
            if child_.firstChild:
                sval_ = child_.firstChild.nodeValue
                if sval_ in ('true', '1'):
                    ival_ = True
                elif sval_ in ('false', '0'):
                    ival_ = False
                else:
                    raise ValueError('requires boolean -- %s' % child_.toxml())
                self.instant = ival_
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'cause':
            cause_ = ''
            for text__content_ in child_.childNodes:
                cause_ += text__content_.nodeValue
            self.cause = cause_
            self.validate_cause(self.cause)    # validate type cause
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'action':
            action_ = ''
            for text__content_ in child_.childNodes:
                action_ += text__content_.nodeValue
            self.action = action_
            self.validate_action(self.action)    # validate type action
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'consequence':
            consequence_ = ''
            for text__content_ in child_.childNodes:
                consequence_ += text__content_.nodeValue
            self.consequence = consequence_
            self.validate_consequence(self.consequence)    # validate type consequence
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'priority':
            priority_ = ''
            for text__content_ in child_.childNodes:
                priority_ += text__content_.nodeValue
            self.priority = priority_
            self.validate_priority(self.priority)    # validate type priority
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'responsible-id':
            responsible_id_ = ''
            for text__content_ in child_.childNodes:
                responsible_id_ += text__content_.nodeValue
            self.responsible_id = responsible_id_
            self.validate_responsible_id(self.responsible_id)    # validate type responsible_id
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'piquetGSM':
            piquetGSM_ = ''
            for text__content_ in child_.childNodes:
                piquetGSM_ += text__content_.nodeValue
            self.piquetGSM = piquetGSM_
            self.validate_piquetGSM(self.piquetGSM)    # validate type piquetGSM
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'help-url':
            help_url_ = ''
            for text__content_ in child_.childNodes:
                help_url_ += text__content_.nodeValue
            self.help_url = help_url_
            self.validate_help_url(self.help_url)    # validate type help_url
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'source-name':
            source_name_ = ''
            for text__content_ in child_.childNodes:
                source_name_ += text__content_.nodeValue
            self.source_name = source_name_
            self.validate_source_name(self.source_name)    # validate type source_name
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'location':
            obj_ = locationType.factory()
            obj_.build(child_)
            self.set_location(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'piquetEmail':
            piquetEmail_ = ''
            for text__content_ in child_.childNodes:
                piquetEmail_ += text__content_.nodeValue
            self.piquetEmail = piquetEmail_
            self.validate_piquetEmail(self.piquetEmail)    # validate type piquetEmail
# end class alarm_definitionType


class visual_fields(object):
    subclass = None
    superclass = None
    def __init__(self, system_name='', identifier='', problem_description=''):
        self.system_name = system_name
        self.identifier = identifier
        self.problem_description = problem_description
    def factory(*args_, **kwargs_):
        if visual_fields.subclass:
            return visual_fields.subclass(*args_, **kwargs_)
        else:
            return visual_fields(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_system_name(self): return self.system_name
    def set_system_name(self, system_name): self.system_name = system_name
    def validate_system_name(self, value):
        # validate type system_name
        pass
    def get_identifier(self): return self.identifier
    def set_identifier(self, identifier): self.identifier = identifier
    def validate_identifier(self, value):
        # validate type identifier
        pass
    def get_problem_description(self): return self.problem_description
    def set_problem_description(self, problem_description): self.problem_description = problem_description
    def validate_problem_description(self, value):
        # validate type problem_description
        pass
    def export(self, outfile, level, namespace_='', name_='visual-fields'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='visual-fields')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='visual-fields'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='visual-fields'):
        showIndent(outfile, level)
        outfile.write('<%ssystem-name>%s</%ssystem-name>\n' % (namespace_, quote_xml(self.get_system_name()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%sidentifier>%s</%sidentifier>\n' % (namespace_, quote_xml(self.get_identifier()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%sproblem-description>%s</%sproblem-description>\n' % (namespace_, quote_xml(self.get_problem_description()), namespace_))
    def exportLiteral(self, outfile, level, name_='visual-fields'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('system_name=%s,\n' % quote_python(self.get_system_name()))
        showIndent(outfile, level)
        outfile.write('identifier=%s,\n' % quote_python(self.get_identifier()))
        showIndent(outfile, level)
        outfile.write('problem_description=%s,\n' % quote_python(self.get_problem_description()))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'system-name':
            system_name_ = ''
            for text__content_ in child_.childNodes:
                system_name_ += text__content_.nodeValue
            self.system_name = system_name_
            self.validate_system_name(self.system_name)    # validate type system_name
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'identifier':
            identifier_ = ''
            for text__content_ in child_.childNodes:
                identifier_ += text__content_.nodeValue
            self.identifier = identifier_
            self.validate_identifier(self.identifier)    # validate type identifier
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'problem-description':
            problem_description_ = ''
            for text__content_ in child_.childNodes:
                problem_description_ += text__content_.nodeValue
            self.problem_description = problem_description_
            self.validate_problem_description(self.problem_description)    # validate type problem_description
# end class visual_fields


class system_name(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if system_name.subclass:
            return system_name.subclass(*args_, **kwargs_)
        else:
            return system_name(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='system-name'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='system-name')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='system-name'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='system-name'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='system-name'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class system_name


class identifier(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if identifier.subclass:
            return identifier.subclass(*args_, **kwargs_)
        else:
            return identifier(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='identifier'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='identifier')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='identifier'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='identifier'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='identifier'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class identifier


class problem_description(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if problem_description.subclass:
            return problem_description.subclass(*args_, **kwargs_)
        else:
            return problem_description(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='problem-description'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='problem-description')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='problem-description'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='problem-description'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='problem-description'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class problem_description


class cause(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if cause.subclass:
            return cause.subclass(*args_, **kwargs_)
        else:
            return cause(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='cause'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='cause')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='cause'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='cause'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='cause'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class cause


class action(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if action.subclass:
            return action.subclass(*args_, **kwargs_)
        else:
            return action(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='action'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='action')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='action'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='action'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='action'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class action


class consequence(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if consequence.subclass:
            return consequence.subclass(*args_, **kwargs_)
        else:
            return consequence(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='consequence'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='consequence')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='consequence'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='consequence'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='consequence'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class consequence


class priority(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if priority.subclass:
            return priority.subclass(*args_, **kwargs_)
        else:
            return priority(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='priority'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='priority')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='priority'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='priority'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='priority'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class priority


class responsible_id(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if responsible_id.subclass:
            return responsible_id.subclass(*args_, **kwargs_)
        else:
            return responsible_id(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='responsible-id'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='responsible-id')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='responsible-id'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='responsible-id'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='responsible-id'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class responsible_id


class piquetGSM(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if piquetGSM.subclass:
            return piquetGSM.subclass(*args_, **kwargs_)
        else:
            return piquetGSM(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='piquetGSM'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='piquetGSM')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='piquetGSM'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='piquetGSM'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='piquetGSM'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class piquetGSM


class help_url(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if help_url.subclass:
            return help_url.subclass(*args_, **kwargs_)
        else:
            return help_url(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='help-url'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='help-url')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='help-url'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='help-url'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='help-url'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class help_url


class source_name(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if source_name.subclass:
            return source_name.subclass(*args_, **kwargs_)
        else:
            return source_name(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='source-name'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='source-name')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='source-name'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='source-name'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='source-name'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class source_name


class piquetEmail(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if piquetEmail.subclass:
            return piquetEmail.subclass(*args_, **kwargs_)
        else:
            return piquetEmail(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='piquetEmail'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='piquetEmail')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='piquetEmail'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='piquetEmail'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='piquetEmail'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class piquetEmail


class category_definitionType(object):
    subclass = None
    superclass = None
    def __init__(self, description=''):
        self.description = description
    def factory(*args_, **kwargs_):
        if category_definitionType.subclass:
            return category_definitionType.subclass(*args_, **kwargs_)
        else:
            return category_definitionType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_description(self): return self.description
    def set_description(self, description): self.description = description
    def validate_description(self, value):
        # validate type description
        pass
    def export(self, outfile, level, namespace_='', name_='category-definitionType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='category-definitionType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='category-definitionType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='category-definitionType'):
        showIndent(outfile, level)
        outfile.write('<%sdescription>%s</%sdescription>\n' % (namespace_, quote_xml(self.get_description()), namespace_))
    def exportLiteral(self, outfile, level, name_='category-definitionType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('description=%s,\n' % quote_python(self.get_description()))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'description':
            description_ = ''
            for text__content_ in child_.childNodes:
                description_ += text__content_.nodeValue
            self.description = description_
            self.validate_description(self.description)    # validate type description
# end class category_definitionType


class description(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if description.subclass:
            return description.subclass(*args_, **kwargs_)
        else:
            return description(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='description'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='description')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='description'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='description'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='description'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class description


class source_definitionType(object):
    subclass = None
    superclass = None
    def __init__(self, description='', connection_timeout='', responsible_id=''):
        self.description = description
        self.connection_timeout = connection_timeout
        self.responsible_id = responsible_id
    def factory(*args_, **kwargs_):
        if source_definitionType.subclass:
            return source_definitionType.subclass(*args_, **kwargs_)
        else:
            return source_definitionType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_description(self): return self.description
    def set_description(self, description): self.description = description
    def validate_description(self, value):
        # validate type description
        pass
    def get_connection_timeout(self): return self.connection_timeout
    def set_connection_timeout(self, connection_timeout): self.connection_timeout = connection_timeout
    def validate_connection_timeout(self, value):
        # validate type connection_timeout
        pass
    def get_responsible_id(self): return self.responsible_id
    def set_responsible_id(self, responsible_id): self.responsible_id = responsible_id
    def validate_responsible_id(self, value):
        # validate type responsible_id
        pass
    def export(self, outfile, level, namespace_='', name_='source-definitionType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='source-definitionType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='source-definitionType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='source-definitionType'):
        showIndent(outfile, level)
        outfile.write('<%sdescription>%s</%sdescription>\n' % (namespace_, quote_xml(self.get_description()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%sconnection-timeout>%s</%sconnection-timeout>\n' % (namespace_, quote_xml(self.get_connection_timeout()), namespace_))
        showIndent(outfile, level)
        outfile.write('<%sresponsible-id>%s</%sresponsible-id>\n' % (namespace_, quote_xml(self.get_responsible_id()), namespace_))
    def exportLiteral(self, outfile, level, name_='source-definitionType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('description=%s,\n' % quote_python(self.get_description()))
        showIndent(outfile, level)
        outfile.write('connection_timeout=%s,\n' % quote_python(self.get_connection_timeout()))
        showIndent(outfile, level)
        outfile.write('responsible_id=%s,\n' % quote_python(self.get_responsible_id()))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'description':
            description_ = ''
            for text__content_ in child_.childNodes:
                description_ += text__content_.nodeValue
            self.description = description_
            self.validate_description(self.description)    # validate type description
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'connection-timeout':
            connection_timeout_ = ''
            for text__content_ in child_.childNodes:
                connection_timeout_ += text__content_.nodeValue
            self.connection_timeout = connection_timeout_
            self.validate_connection_timeout(self.connection_timeout)    # validate type connection_timeout
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'responsible-id':
            responsible_id_ = ''
            for text__content_ in child_.childNodes:
                responsible_id_ += text__content_.nodeValue
            self.responsible_id = responsible_id_
            self.validate_responsible_id(self.responsible_id)    # validate type responsible_id
# end class source_definitionType


class connection_timeout(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if connection_timeout.subclass:
            return connection_timeout.subclass(*args_, **kwargs_)
        else:
            return connection_timeout(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='connection-timeout'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='connection-timeout')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='connection-timeout'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='connection-timeout'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='connection-timeout'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class connection_timeout


class locationType(object):
    subclass = None
    superclass = None
    def __init__(self, building='', floor='', room='', mnemonic='', position=''):
        self.building = building
        self.floor = floor
        self.room = room
        self.mnemonic = mnemonic
        self.position = position
    def factory(*args_, **kwargs_):
        if locationType.subclass:
            return locationType.subclass(*args_, **kwargs_)
        else:
            return locationType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_building(self): return self.building
    def set_building(self, building): self.building = building
    def validate_building(self, value):
        # validate type building
        pass
    def get_floor(self): return self.floor
    def set_floor(self, floor): self.floor = floor
    def validate_floor(self, value):
        # validate type floor
        pass
    def get_room(self): return self.room
    def set_room(self, room): self.room = room
    def validate_room(self, value):
        # validate type room
        pass
    def get_mnemonic(self): return self.mnemonic
    def set_mnemonic(self, mnemonic): self.mnemonic = mnemonic
    def validate_mnemonic(self, value):
        # validate type mnemonic
        pass
    def get_position(self): return self.position
    def set_position(self, position): self.position = position
    def validate_position(self, value):
        # validate type position
        pass
    def export(self, outfile, level, namespace_='', name_='locationType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='locationType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='locationType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='locationType'):
        if self.get_building() != None :
            if self.get_building() != "" :
                showIndent(outfile, level)
                outfile.write('<%sbuilding>%s</%sbuilding>\n' % (namespace_, quote_xml(self.get_building()), namespace_))
        if self.get_floor() != None :
            if self.get_floor() != "" :
                showIndent(outfile, level)
                outfile.write('<%sfloor>%s</%sfloor>\n' % (namespace_, quote_xml(self.get_floor()), namespace_))
        if self.get_room() != None :
            if self.get_room() != "" :
                showIndent(outfile, level)
                outfile.write('<%sroom>%s</%sroom>\n' % (namespace_, quote_xml(self.get_room()), namespace_))
        if self.get_mnemonic() != None :
            if self.get_mnemonic() != "" :
                showIndent(outfile, level)
                outfile.write('<%smnemonic>%s</%smnemonic>\n' % (namespace_, quote_xml(self.get_mnemonic()), namespace_))
        if self.get_position() != None :
            if self.get_position() != "" :
                showIndent(outfile, level)
                outfile.write('<%sposition>%s</%sposition>\n' % (namespace_, quote_xml(self.get_position()), namespace_))
    def exportLiteral(self, outfile, level, name_='locationType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('building=%s,\n' % quote_python(self.get_building()))
        showIndent(outfile, level)
        outfile.write('floor=%s,\n' % quote_python(self.get_floor()))
        showIndent(outfile, level)
        outfile.write('room=%s,\n' % quote_python(self.get_room()))
        showIndent(outfile, level)
        outfile.write('mnemonic=%s,\n' % quote_python(self.get_mnemonic()))
        showIndent(outfile, level)
        outfile.write('position=%s,\n' % quote_python(self.get_position()))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'building':
            building_ = ''
            for text__content_ in child_.childNodes:
                building_ += text__content_.nodeValue
            self.building = building_
            self.validate_building(self.building)    # validate type building
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'floor':
            floor_ = ''
            for text__content_ in child_.childNodes:
                floor_ += text__content_.nodeValue
            self.floor = floor_
            self.validate_floor(self.floor)    # validate type floor
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'room':
            room_ = ''
            for text__content_ in child_.childNodes:
                room_ += text__content_.nodeValue
            self.room = room_
            self.validate_room(self.room)    # validate type room
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'mnemonic':
            mnemonic_ = ''
            for text__content_ in child_.childNodes:
                mnemonic_ += text__content_.nodeValue
            self.mnemonic = mnemonic_
            self.validate_mnemonic(self.mnemonic)    # validate type mnemonic
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'position':
            position_ = ''
            for text__content_ in child_.childNodes:
                position_ += text__content_.nodeValue
            self.position = position_
            self.validate_position(self.position)    # validate type position
# end class locationType


class building(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if building.subclass:
            return building.subclass(*args_, **kwargs_)
        else:
            return building(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='building'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='building')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='building'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='building'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='building'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class building


class floor(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if floor.subclass:
            return floor.subclass(*args_, **kwargs_)
        else:
            return floor(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='floor'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='floor')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='floor'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='floor'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='floor'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class floor


class room(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if room.subclass:
            return room.subclass(*args_, **kwargs_)
        else:
            return room(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='room'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='room')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='room'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='room'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='room'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class room


class mnemonic(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if mnemonic.subclass:
            return mnemonic.subclass(*args_, **kwargs_)
        else:
            return mnemonic(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='mnemonic'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='mnemonic')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='mnemonic'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='mnemonic'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='mnemonic'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class mnemonic


class position(object):
    subclass = None
    superclass = None
    def __init__(self, valueOf_=''):
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if position.subclass:
            return position.subclass(*args_, **kwargs_)
        else:
            return position(*args_, **kwargs_)
    factory = staticmethod(factory)
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='position'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='position')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='position'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='position'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='position'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class position


class reduction_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, links_to_create=None, links_to_remove=None, thresholds=None):
        self.links_to_create = links_to_create
        self.links_to_remove = links_to_remove
        self.thresholds = thresholds
    def factory(*args_, **kwargs_):
        if reduction_definitions.subclass:
            return reduction_definitions.subclass(*args_, **kwargs_)
        else:
            return reduction_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_links_to_create(self): return self.links_to_create
    def set_links_to_create(self, links_to_create): self.links_to_create = links_to_create
    def get_links_to_remove(self): return self.links_to_remove
    def set_links_to_remove(self, links_to_remove): self.links_to_remove = links_to_remove
    def get_thresholds(self): return self.thresholds
    def set_thresholds(self, thresholds): self.thresholds = thresholds
    def export(self, outfile, level, namespace_='', name_='reduction-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='reduction-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='reduction-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='reduction-definitions'):
        if self.get_links_to_create() != None :
            if self.links_to_create:
                self.links_to_create.export(outfile, level, namespace_, name_='links-to-create')
        if self.get_links_to_remove() != None :
            if self.links_to_remove:
                self.links_to_remove.export(outfile, level, namespace_, name_='links-to-remove')
        if self.get_thresholds() != None :
            if self.thresholds:
                self.thresholds.export(outfile, level, namespace_, name_='thresholds')
    def exportLiteral(self, outfile, level, name_='reduction-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.links_to_create:
            showIndent(outfile, level)
            outfile.write('links_to_create=reduction_link_definition_listType(\n')
            self.links_to_create.exportLiteral(outfile, level, name_='links_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.links_to_remove:
            showIndent(outfile, level)
            outfile.write('links_to_remove=reduction_link_definition_listType(\n')
            self.links_to_remove.exportLiteral(outfile, level, name_='links_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.thresholds:
            showIndent(outfile, level)
            outfile.write('thresholds=thresholds(\n')
            self.thresholds.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'links-to-create':
            obj_ = reduction_link_definition_listType.factory()
            obj_.build(child_)
            self.set_links_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'links-to-remove':
            obj_ = reduction_link_definition_listType.factory()
            obj_.build(child_)
            self.set_links_to_remove(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'thresholds':
            obj_ = thresholds.factory()
            obj_.build(child_)
            self.set_thresholds(obj_)
# end class reduction_definitions


class thresholds(object):
    subclass = None
    superclass = None
    def __init__(self, threshold=None):
        if threshold is None:
            self.threshold = []
        else:
            self.threshold = threshold
    def factory(*args_, **kwargs_):
        if thresholds.subclass:
            return thresholds.subclass(*args_, **kwargs_)
        else:
            return thresholds(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_threshold(self): return self.threshold
    def set_threshold(self, threshold): self.threshold = threshold
    def add_threshold(self, value): self.threshold.append(value)
    def insert_threshold(self, index, value): self.threshold[index] = value
    def export(self, outfile, level, namespace_='', name_='thresholds'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='thresholds')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='thresholds'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='thresholds'):
        for threshold_ in self.get_threshold():
            threshold_.export(outfile, level, namespace_, name_='threshold')
    def exportLiteral(self, outfile, level, name_='thresholds'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('threshold=[\n')
        level += 1
        for threshold in self.threshold:
            showIndent(outfile, level)
            outfile.write('threshold(\n')
            threshold.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'threshold':
            obj_ = threshold.factory()
            obj_.build(child_)
            self.threshold.append(obj_)
# end class thresholds


class threshold(object):
    subclass = None
    superclass = None
    def __init__(self, value=-1, alarm_definition=None):
        self.value = value
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if threshold.subclass:
            return threshold.subclass(*args_, **kwargs_)
        else:
            return threshold(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def get_value(self): return self.value
    def set_value(self, value): self.value = value
    def export(self, outfile, level, namespace_='', name_='threshold'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='threshold')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='threshold'):
        outfile.write(' value="%d"' % self.get_value())
    def exportChildren(self, outfile, level, namespace_='', name_='threshold'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='threshold'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('value = "%s",\n' % (self.get_value(),))
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('value'):
            try:
                self.value = int(attrs.get('value').value)
            except ValueError, exp:
                raise ValueError('Bad integer attribute (value): %s' % exp)
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class threshold


class reduction_linkType(object):
    subclass = None
    superclass = None
    def __init__(self, typexx='', parent=None, child=None):
        self.typexx = typexx
        self.parent = parent
        self.child = child
    def factory(*args_, **kwargs_):
        if reduction_linkType.subclass:
            return reduction_linkType.subclass(*args_, **kwargs_)
        else:
            return reduction_linkType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_parent(self): return self.parent
    def set_parent(self, parent): self.parent = parent
    def get_child(self): return self.child
    def set_child(self, child): self.child = child
    def get_type(self): return self.typexx
    def set_type(self, typexx): self.typexx = typexx
    def export(self, outfile, level, namespace_='', name_='reduction-linkType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='reduction-linkType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='reduction-linkType'):
        outfile.write(' type="%s"' % (quote_attrib(self.get_type()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='reduction-linkType'):
        if self.parent:
            self.parent.export(outfile, level, namespace_, name_='parent', )
        if self.child:
            self.child.export(outfile, level, namespace_, name_='child', )
    def exportLiteral(self, outfile, level, name_='reduction-linkType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('typexx = "%s",\n' % (self.get_type(),))
    def exportLiteralChildren(self, outfile, level, name_):
        if self.parent:
            showIndent(outfile, level)
            outfile.write('parent=parent(\n')
            self.parent.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.child:
            showIndent(outfile, level)
            outfile.write('child=child(\n')
            self.child.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('type'):
            self.typexx = attrs.get('type').value
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'parent':
            obj_ = parent.factory()
            obj_.build(child_)
            self.set_parent(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'child':
            obj_ = child.factory()
            obj_.build(child_)
            self.set_child(obj_)
# end class reduction_linkType


class parent(object):
    subclass = None
    superclass = None
    def __init__(self, alarm_definition=None):
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if parent.subclass:
            return parent.subclass(*args_, **kwargs_)
        else:
            return parent(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def export(self, outfile, level, namespace_='', name_='parent'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='parent')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='parent'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='parent'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='parent'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class parent


class child(object):
    subclass = None
    superclass = None
    def __init__(self, alarm_definition=None):
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if child.subclass:
            return child.subclass(*args_, **kwargs_)
        else:
            return child(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def export(self, outfile, level, namespace_='', name_='child'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='child')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='child'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='child'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='child'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class child


class mask_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, masks_to_create=None, masks_to_remove=None):
        self.masks_to_create = masks_to_create
        self.masks_to_remove = masks_to_remove
    def factory(*args_, **kwargs_):
        if mask_definitions.subclass:
            return mask_definitions.subclass(*args_, **kwargs_)
        else:
            return mask_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_masks_to_create(self): return self.masks_to_create
    def set_masks_to_create(self, masks_to_create): self.masks_to_create = masks_to_create
    def get_masks_to_remove(self): return self.masks_to_remove
    def set_masks_to_remove(self, masks_to_remove): self.masks_to_remove = masks_to_remove
    def export(self, outfile, level, namespace_='', name_='mask-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='mask-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='mask-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='mask-definitions'):
        if self.get_masks_to_create() != None :
            if self.masks_to_create:
                self.masks_to_create.export(outfile, level, namespace_, name_='masks-to-create')
        if self.get_masks_to_remove() != None :
            if self.masks_to_remove:
                self.masks_to_remove.export(outfile, level, namespace_, name_='masks-to-remove')
    def exportLiteral(self, outfile, level, name_='mask-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.masks_to_create:
            showIndent(outfile, level)
            outfile.write('masks_to_create=mask_definition_listType(\n')
            self.masks_to_create.exportLiteral(outfile, level, name_='masks_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.masks_to_remove:
            showIndent(outfile, level)
            outfile.write('masks_to_remove=mask_definition_listType(\n')
            self.masks_to_remove.exportLiteral(outfile, level, name_='masks_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'masks-to-create':
            obj_ = mask_definition_listType.factory()
            obj_.build(child_)
            self.set_masks_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'masks-to-remove':
            obj_ = mask_definition_listType.factory()
            obj_.build(child_)
            self.set_masks_to_remove(obj_)
# end class mask_definitions


class mode_maskType(object):
    subclass = None
    superclass = None
    def __init__(self, machine_mode='', alarm_definition=None):
        self.machine_mode = machine_mode
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if mode_maskType.subclass:
            return mode_maskType.subclass(*args_, **kwargs_)
        else:
            return mode_maskType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def get_machine_mode(self): return self.machine_mode
    def set_machine_mode(self, machine_mode): self.machine_mode = machine_mode
    def export(self, outfile, level, namespace_='', name_='mode-maskType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='mode-maskType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='mode-maskType'):
        outfile.write(' machine-mode="%s"' % (quote_attrib(self.get_machine_mode()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='mode-maskType'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='mode-maskType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('machine_mode = "%s",\n' % (self.get_machine_mode(),))
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('machine-mode'):
            self.machine_mode = attrs.get('machine-mode').value
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class mode_maskType


class maintenance_maskType(object):
    subclass = None
    superclass = None
    def __init__(self, to='', fromxx='', alarm_definition=None):
        self.to = to
        self.fromxx = fromxx
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if maintenance_maskType.subclass:
            return maintenance_maskType.subclass(*args_, **kwargs_)
        else:
            return maintenance_maskType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def get_to(self): return self.to
    def set_to(self, to): self.to = to
    def get_from(self): return self.fromxx
    def set_from(self, fromxx): self.fromxx = fromxx
    def export(self, outfile, level, namespace_='', name_='maintenance-maskType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='maintenance-maskType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='maintenance-maskType'):
        outfile.write(' to="%s"' % (quote_attrib(self.get_to()), ))
        outfile.write(' from="%s"' % (quote_attrib(self.get_from()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='maintenance-maskType'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='maintenance-maskType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('to = "%s",\n' % (self.get_to(),))
        showIndent(outfile, level)
        outfile.write('fromxx = "%s",\n' % (self.get_from(),))
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('to'):
            self.to = attrs.get('to').value
        if attrs.get('from'):
            self.fromxx = attrs.get('from').value
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class maintenance_maskType


class alarm_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, alarm_definition=None):
        if alarm_definition is None:
            self.alarm_definition = []
        else:
            self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if alarm_definition_listType.subclass:
            return alarm_definition_listType.subclass(*args_, **kwargs_)
        else:
            return alarm_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def add_alarm_definition(self, value): self.alarm_definition.append(value)
    def insert_alarm_definition(self, index, value): self.alarm_definition[index] = value
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def export(self, outfile, level, namespace_='', name_='alarm-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-definition-listType'):
        for alarm_definition_ in self.get_alarm_definition():
            alarm_definition_.export(outfile, level, namespace_, name_='alarm_definition')
    def exportLiteral(self, outfile, level, name_='alarm-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('alarm_definition=[\n')
        level += 1
        for alarm_definition in self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition(\n')
            alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.alarm_definition.append(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class alarm_definition_listType


class category_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, category_definition=None):
        if category_definition is None:
            self.category_definition = []
        else:
            self.category_definition = category_definition
    def factory(*args_, **kwargs_):
        if category_definition_listType.subclass:
            return category_definition_listType.subclass(*args_, **kwargs_)
        else:
            return category_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_category_definition(self): return self.category_definition
    def set_category_definition(self, category_definition): self.category_definition = category_definition
    def add_category_definition(self, value): self.category_definition.append(value)
    def insert_category_definition(self, index, value): self.category_definition[index] = value
    def export(self, outfile, level, namespace_='', name_='category-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='category-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='category-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='category-definition-listType'):
        for category_definition_ in self.get_category_definition():
            category_definition_.export(outfile, level, namespace_, name_='category_definition')
    def exportLiteral(self, outfile, level, name_='category-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('category_definition=[\n')
        level += 1
        for category_definition in self.category_definition:
            showIndent(outfile, level)
            outfile.write('category_definition(\n')
            category_definition.exportLiteral(outfile, level, name_='category_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'category-definition':
            obj_ = category_definition.factory()
            obj_.build(child_)
            self.category_definition.append(obj_)
# end class category_definition_listType


class source_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, source_definition=None):
        if source_definition is None:
            self.source_definition = []
        else:
            self.source_definition = source_definition
    def factory(*args_, **kwargs_):
        if source_definition_listType.subclass:
            return source_definition_listType.subclass(*args_, **kwargs_)
        else:
            return source_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_source_definition(self): return self.source_definition
    def set_source_definition(self, source_definition): self.source_definition = source_definition
    def add_source_definition(self, value): self.source_definition.append(value)
    def insert_source_definition(self, index, value): self.source_definition[index] = value
    def validate_source_definition(self, value):
        # validate type source-definition
        pass
    def export(self, outfile, level, namespace_='', name_='source-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='source-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='source-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='source-definition-listType'):
        for source_definition_ in self.get_source_definition():
            source_definition_.export(outfile, level, namespace_, name_='source_definition')
    def exportLiteral(self, outfile, level, name_='source-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('source_definition=[\n')
        level += 1
        for source_definition in self.source_definition:
            showIndent(outfile, level)
            outfile.write('source_definition(\n')
            source_definition.exportLiteral(outfile, level, name_='source_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'source-definition':
            obj_ = source_definition.factory()
            obj_.build(child_)
            self.source_definition.append(obj_)
            self.validate_source_definition(self.source_definition)    # validate type source-definition
# end class source_definition_listType


class mask_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, maintenance_mask=None, mode_mask=None):
        if maintenance_mask is None:
            self.maintenance_mask = []
        else:
            self.maintenance_mask = maintenance_mask
        if mode_mask is None:
            self.mode_mask = []
        else:
            self.mode_mask = mode_mask
    def factory(*args_, **kwargs_):
        if mask_definition_listType.subclass:
            return mask_definition_listType.subclass(*args_, **kwargs_)
        else:
            return mask_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_maintenance_mask(self): return self.maintenance_mask
    def set_maintenance_mask(self, maintenance_mask): self.maintenance_mask = maintenance_mask
    def add_maintenance_mask(self, value): self.maintenance_mask.append(value)
    def insert_maintenance_mask(self, index, value): self.maintenance_mask[index] = value
    def get_mode_mask(self): return self.mode_mask
    def set_mode_mask(self, mode_mask): self.mode_mask = mode_mask
    def add_mode_mask(self, value): self.mode_mask.append(value)
    def insert_mode_mask(self, index, value): self.mode_mask[index] = value
    def export(self, outfile, level, namespace_='', name_='mask-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='mask-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='mask-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='mask-definition-listType'):
        for maintenance_mask_ in self.get_maintenance_mask():
            maintenance_mask_.export(outfile, level, namespace_, name_='maintenance_mask')
        for mode_mask_ in self.get_mode_mask():
            mode_mask_.export(outfile, level, namespace_, name_='mode_mask')
    def exportLiteral(self, outfile, level, name_='mask-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('maintenance_mask=[\n')
        level += 1
        for maintenance_mask in self.maintenance_mask:
            showIndent(outfile, level)
            outfile.write('maintenance_mask(\n')
            maintenance_mask.exportLiteral(outfile, level, name_='maintenance_mask')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
        showIndent(outfile, level)
        outfile.write('mode_mask=[\n')
        level += 1
        for mode_mask in self.mode_mask:
            showIndent(outfile, level)
            outfile.write('mode_mask(\n')
            mode_mask.exportLiteral(outfile, level, name_='mode_mask')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'maintenance-mask':
            obj_ = maintenance_mask.factory()
            obj_.build(child_)
            self.maintenance_mask.append(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'mode-mask':
            obj_ = mode_mask.factory()
            obj_.build(child_)
            self.mode_mask.append(obj_)
# end class mask_definition_listType


class reduction_link_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, reduction_link=None):
        if reduction_link is None:
            self.reduction_link = []
        else:
            self.reduction_link = reduction_link
    def factory(*args_, **kwargs_):
        if reduction_link_definition_listType.subclass:
            return reduction_link_definition_listType.subclass(*args_, **kwargs_)
        else:
            return reduction_link_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_reduction_link(self): return self.reduction_link
    def set_reduction_link(self, reduction_link): self.reduction_link = reduction_link
    def add_reduction_link(self, value): self.reduction_link.append(value)
    def insert_reduction_link(self, index, value): self.reduction_link[index] = value
    def export(self, outfile, level, namespace_='', name_='reduction-link-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='reduction-link-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='reduction-link-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='reduction-link-definition-listType'):
        for reduction_link_ in self.get_reduction_link():
            reduction_link_.export(outfile, level, namespace_, name_='reduction_link')
    def exportLiteral(self, outfile, level, name_='reduction-link-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('reduction_link=[\n')
        level += 1
        for reduction_link in self.reduction_link:
            showIndent(outfile, level)
            outfile.write('reduction_link(\n')
            reduction_link.exportLiteral(outfile, level, name_='reduction_link')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'reduction-link':
            obj_ = reduction_link.factory()
            obj_.build(child_)
            self.reduction_link.append(obj_)
# end class reduction_link_definition_listType


class alarm_category_definitions(object):
    subclass = None
    superclass = None
    def __init__(self, category_links_to_create=None, category_links_to_remove=None):
        self.category_links_to_create = category_links_to_create
        self.category_links_to_remove = category_links_to_remove
    def factory(*args_, **kwargs_):
        if alarm_category_definitions.subclass:
            return alarm_category_definitions.subclass(*args_, **kwargs_)
        else:
            return alarm_category_definitions(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_category_links_to_create(self): return self.category_links_to_create
    def set_category_links_to_create(self, category_links_to_create): self.category_links_to_create = category_links_to_create
    def get_category_links_to_remove(self): return self.category_links_to_remove
    def set_category_links_to_remove(self, category_links_to_remove): self.category_links_to_remove = category_links_to_remove
    def export(self, outfile, level, namespace_='', name_='alarm-category-definitions'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-category-definitions')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-category-definitions'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-category-definitions'):
        if self.get_category_links_to_create() != None :
            if self.category_links_to_create:
                self.category_links_to_create.export(outfile, level, namespace_, name_='category-links-to-create')
        if self.get_category_links_to_remove() != None :
            if self.category_links_to_remove:
                self.category_links_to_remove.export(outfile, level, namespace_, name_='category-links-to-remove')
    def exportLiteral(self, outfile, level, name_='alarm-category-definitions'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.category_links_to_create:
            showIndent(outfile, level)
            outfile.write('category_links_to_create=alarm_category_link_definition_listType(\n')
            self.category_links_to_create.exportLiteral(outfile, level, name_='category_links_to_create')
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.category_links_to_remove:
            showIndent(outfile, level)
            outfile.write('category_links_to_remove=alarm_category_link_definition_listType(\n')
            self.category_links_to_remove.exportLiteral(outfile, level, name_='category_links_to_remove')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'category-links-to-create':
            obj_ = alarm_category_link_definition_listType.factory()
            obj_.build(child_)
            self.set_category_links_to_create(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'category-links-to-remove':
            obj_ = alarm_category_link_definition_listType.factory()
            obj_.build(child_)
            self.set_category_links_to_remove(obj_)
# end class alarm_category_definitions


class alarm_category_linkType(object):
    subclass = None
    superclass = None
    def __init__(self, category=None, alarm=None):
        self.category = category
        self.alarm = alarm
    def factory(*args_, **kwargs_):
        if alarm_category_linkType.subclass:
            return alarm_category_linkType.subclass(*args_, **kwargs_)
        else:
            return alarm_category_linkType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_category(self): return self.category
    def set_category(self, category): self.category = category
    def get_alarm(self): return self.alarm
    def set_alarm(self, alarm): self.alarm = alarm
    def export(self, outfile, level, namespace_='', name_='alarm-category-linkType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-category-linkType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-category-linkType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-category-linkType'):
        if self.category:
            self.category.export(outfile, level, namespace_, name_='category', )
        if self.alarm:
            self.alarm.export(outfile, level, namespace_, name_='alarm', )
    def exportLiteral(self, outfile, level, name_='alarm-category-linkType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.category:
            showIndent(outfile, level)
            outfile.write('category=category(\n')
            self.category.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
        if self.alarm:
            showIndent(outfile, level)
            outfile.write('alarm=alarm(\n')
            self.alarm.exportLiteral(outfile, level)
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'category':
            obj_ = category.factory()
            obj_.build(child_)
            self.set_category(obj_)
        elif child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm':
            obj_ = alarm.factory()
            obj_.build(child_)
            self.set_alarm(obj_)
# end class alarm_category_linkType


class category(object):
    subclass = None
    superclass = None
    def __init__(self, category_definition=None):
        self.category_definition = category_definition
    def factory(*args_, **kwargs_):
        if category.subclass:
            return category.subclass(*args_, **kwargs_)
        else:
            return category(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_category_definition(self): return self.category_definition
    def set_category_definition(self, category_definition): self.category_definition = category_definition
    def export(self, outfile, level, namespace_='', name_='category'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='category')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='category'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='category'):
        if self.category_definition:
            self.category_definition.export(outfile, level, namespace_, name_='category-definition', )
    def exportLiteral(self, outfile, level, name_='category'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.category_definition:
            showIndent(outfile, level)
            outfile.write('category_definition=category_definition(\n')
            self.category_definition.exportLiteral(outfile, level, name_='category_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'category-definition':
            obj_ = category_definition.factory()
            obj_.build(child_)
            self.set_category_definition(obj_)
# end class category


class alarm(object):
    subclass = None
    superclass = None
    def __init__(self, alarm_definition=None):
        self.alarm_definition = alarm_definition
    def factory(*args_, **kwargs_):
        if alarm.subclass:
            return alarm.subclass(*args_, **kwargs_)
        else:
            return alarm(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_definition(self): return self.alarm_definition
    def set_alarm_definition(self, alarm_definition): self.alarm_definition = alarm_definition
    def validate_alarm_definition(self, value):
        # validate type alarm-definition
        pass
    def export(self, outfile, level, namespace_='', name_='alarm'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm'):
        if self.alarm_definition:
            self.alarm_definition.export(outfile, level, namespace_, name_='alarm-definition', )
    def exportLiteral(self, outfile, level, name_='alarm'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        if self.alarm_definition:
            showIndent(outfile, level)
            outfile.write('alarm_definition=alarm_definition(\n')
            self.alarm_definition.exportLiteral(outfile, level, name_='alarm_definition')
            showIndent(outfile, level)
            outfile.write('),\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-definition':
            obj_ = alarm_definition.factory()
            obj_.build(child_)
            self.set_alarm_definition(obj_)
            self.validate_alarm_definition(self.alarm_definition)    # validate type alarm-definition
# end class alarm


class alarm_category_link_definition_listType(object):
    subclass = None
    superclass = None
    def __init__(self, alarm_category_link=None):
        if alarm_category_link is None:
            self.alarm_category_link = []
        else:
            self.alarm_category_link = alarm_category_link
    def factory(*args_, **kwargs_):
        if alarm_category_link_definition_listType.subclass:
            return alarm_category_link_definition_listType.subclass(*args_, **kwargs_)
        else:
            return alarm_category_link_definition_listType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_alarm_category_link(self): return self.alarm_category_link
    def set_alarm_category_link(self, alarm_category_link): self.alarm_category_link = alarm_category_link
    def add_alarm_category_link(self, value): self.alarm_category_link.append(value)
    def insert_alarm_category_link(self, index, value): self.alarm_category_link[index] = value
    def export(self, outfile, level, namespace_='', name_='alarm-category-link-definition-listType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-category-link-definition-listType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-category-link-definition-listType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-category-link-definition-listType'):
        for alarm_category_link_ in self.get_alarm_category_link():
            alarm_category_link_.export(outfile, level, namespace_, name_='alarm_category_link')
    def exportLiteral(self, outfile, level, name_='alarm-category-link-definition-listType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('alarm_category_link=[\n')
        level += 1
        for alarm_category_link in self.alarm_category_link:
            showIndent(outfile, level)
            outfile.write('alarm_category_link(\n')
            alarm_category_link.exportLiteral(outfile, level, name_='alarm_category_link')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'alarm-category-link':
            obj_ = alarm_category_link.factory()
            obj_.build(child_)
            self.alarm_category_link.append(obj_)
# end class alarm_category_link_definition_listType


class configuration_property(object):
    subclass = None
    superclass = None
    def __init__(self, name='', valueOf_=''):
        self.name = name
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if configuration_property.subclass:
            return configuration_property.subclass(*args_, **kwargs_)
        else:
            return configuration_property(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_name(self): return self.name
    def set_name(self, name): self.name = name
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='configuration-property'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='configuration-property')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='configuration-property'):
        outfile.write(' name="%s"' % (quote_attrib(self.get_name()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='configuration-property'):
        outfile.write(quote_xml('%s' % self.valueOf_))
    def exportLiteral(self, outfile, level, name_='configuration-property'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('name = "%s",\n' % (self.get_name(),))
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('name'):
            self.name = attrs.get('name').value
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class configuration_property


class alarm_system_configurationListType(object):
    subclass = None
    superclass = None
    def __init__(self, configuration_property=None):
        if configuration_property is None:
            self.configuration_property = []
        else:
            self.configuration_property = configuration_property
    def factory(*args_, **kwargs_):
        if alarm_system_configurationListType.subclass:
            return alarm_system_configurationListType.subclass(*args_, **kwargs_)
        else:
            return alarm_system_configurationListType(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_configuration_property(self): return self.configuration_property
    def set_configuration_property(self, configuration_property): self.configuration_property = configuration_property
    def add_configuration_property(self, value): self.configuration_property.append(value)
    def insert_configuration_property(self, index, value): self.configuration_property[index] = value
    def validate_configuration_property(self, value):
        # validate type configuration-property
        pass
    def export(self, outfile, level, namespace_='', name_='alarm-system-configurationListType'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-system-configurationListType')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-system-configurationListType'):
        pass
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-system-configurationListType'):
        for configuration_property_ in self.get_configuration_property():
            configuration_property_.export(outfile, level, namespace_, name_='configuration_property')
    def exportLiteral(self, outfile, level, name_='alarm-system-configurationListType'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        pass
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('configuration_property=[\n')
        level += 1
        for configuration_property in self.configuration_property:
            showIndent(outfile, level)
            outfile.write('configuration_property(\n')
            configuration_property.exportLiteral(outfile, level, name_='configuration_property')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        pass
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == 'configuration-property':
            obj_ = configuration_property.factory()
            obj_.build(child_)
            self.configuration_property.append(obj_)
            self.validate_configuration_property(self.configuration_property)    # validate type configuration-property
# end class alarm_system_configurationListType


class category_definition(category_definitionType):
    subclass = None
    superclass = category_definitionType
    def __init__(self, description='', path='', valueOf_=''):
        category_definitionType.__init__(self, description)
        self.path = path
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if category_definition.subclass:
            return category_definition.subclass(*args_, **kwargs_)
        else:
            return category_definition(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_path(self): return self.path
    def set_path(self, path): self.path = path
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='category-definition'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='category-definition')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='category-definition'):
        category_definitionType.exportAttributes(self, outfile, level, namespace_, name_='category-definition')
        outfile.write(' path="%s"' % (quote_attrib(self.get_path()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='category-definition'):
        category_definitionType.exportChildren(self, outfile, level, namespace_, name_)
    def exportLiteral(self, outfile, level, name_='category-definition'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('path = "%s",\n' % (self.get_path(),))
        category_definitionType.exportLiteralAttributes(self, outfile, level, name_)
    def exportLiteralChildren(self, outfile, level, name_):
        category_definitionType.exportLiteralChildren(self, outfile, level, name_)
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('path'):
            self.path = attrs.get('path').value
        category_definitionType.buildAttributes(self, attrs)
    def buildChildren(self, child_, nodeName_):
        category_definitionType.buildChildren(self, child_, nodeName_)
# end class category_definition


class source_definition(source_definitionType):
    subclass = None
    superclass = source_definitionType
    def __init__(self, description='', connection_timeout='', responsible_id='', name='', valueOf_=''):
        source_definitionType.__init__(self, description, connection_timeout, responsible_id)
        self.name = name
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if source_definition.subclass:
            return source_definition.subclass(*args_, **kwargs_)
        else:
            return source_definition(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_name(self): return self.name
    def set_name(self, name): self.name = name
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='source-definition'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='source-definition')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='source-definition'):
        source_definitionType.exportAttributes(self, outfile, level, namespace_, name_='source-definition')
        outfile.write(' name="%s"' % (quote_attrib(self.get_name()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='source-definition'):
        source_definitionType.exportChildren(self, outfile, level, namespace_, name_)
    def exportLiteral(self, outfile, level, name_='source-definition'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('name = "%s",\n' % (self.get_name(),))
        source_definitionType.exportLiteralAttributes(self, outfile, level, name_)
    def exportLiteralChildren(self, outfile, level, name_):
        source_definitionType.exportLiteralChildren(self, outfile, level, name_)
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('name'):
            self.name = attrs.get('name').value
        source_definitionType.buildAttributes(self, attrs)
    def buildChildren(self, child_, nodeName_):
        source_definitionType.buildChildren(self, child_, nodeName_)
# end class source_definition


class alarm_definition(alarm_definitionType):
    subclass = None
    superclass = alarm_definitionType
    def __init__(self, visual_fields=None, instant=False, cause='', action='', consequence='', priority='0', responsible_id='', piquetGSM='', help_url='', source_name='', location=None, piquetEmail='', fault_code='', fault_family='', fault_member='', valueOf_=''):
        alarm_definitionType.__init__(self, visual_fields, instant, cause, action, consequence, priority, responsible_id, piquetGSM, help_url, source_name, location, piquetEmail)
        self.fault_code = fault_code
        self.fault_family = fault_family
        self.fault_member = fault_member
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if alarm_definition.subclass:
            return alarm_definition.subclass(*args_, **kwargs_)
        else:
            return alarm_definition(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_fault_code(self): return self.fault_code
    def set_fault_code(self, fault_code): self.fault_code = fault_code
    def get_fault_family(self): return self.fault_family
    def set_fault_family(self, fault_family): self.fault_family = fault_family
    def get_fault_member(self): return self.fault_member
    def set_fault_member(self, fault_member): self.fault_member = fault_member
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, namespace_='', name_='alarm-definition'):
        showIndent(outfile, level)
        outfile.write('<%s%s' % (namespace_, name_))
        self.exportAttributes(outfile, level, namespace_, name_='alarm-definition')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, namespace_, name_)
        showIndent(outfile, level)
        outfile.write('</%s%s>\n' % (namespace_, name_))
    def exportAttributes(self, outfile, level, namespace_='', name_='alarm-definition'):
        alarm_definitionType.exportAttributes(self, outfile, level, namespace_, name_='alarm-definition')
        outfile.write(' fault-code="%s"' % (quote_attrib(self.get_fault_code()), ))
        outfile.write(' fault-family="%s"' % (quote_attrib(self.get_fault_family()), ))
        outfile.write(' fault-member="%s"' % (quote_attrib(self.get_fault_member()), ))
    def exportChildren(self, outfile, level, namespace_='', name_='alarm-definition'):
        alarm_definitionType.exportChildren(self, outfile, level, namespace_, name_)
    def exportLiteral(self, outfile, level, name_='alarm-definition'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('fault_code = "%s",\n' % (self.get_fault_code(),))
        showIndent(outfile, level)
        outfile.write('fault_family = "%s",\n' % (self.get_fault_family(),))
        showIndent(outfile, level)
        outfile.write('fault_member = "%s",\n' % (self.get_fault_member(),))
        alarm_definitionType.exportLiteralAttributes(self, outfile, level, name_)
    def exportLiteralChildren(self, outfile, level, name_):
        alarm_definitionType.exportLiteralChildren(self, outfile, level, name_)
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('fault-code'):
            self.fault_code = attrs.get('fault-code').value
        if attrs.get('fault-family'):
            self.fault_family = attrs.get('fault-family').value
        if attrs.get('fault-member'):
            self.fault_member = attrs.get('fault-member').value
        alarm_definitionType.buildAttributes(self, attrs)
    def buildChildren(self, child_, nodeName_):
        alarm_definitionType.buildChildren(self, child_, nodeName_)
# end class alarm_definition


from xml.sax import handler, make_parser

class SaxStackElement:
    def __init__(self, name='', obj=None):
        self.name = name
        self.obj = obj
        self.content = ''

#
# SAX handler
#
class Sax_alarm_definitionsHandler(handler.ContentHandler):
    def __init__(self):
        self.stack = []
        self.root = None

    def getRoot(self):
        return self.root

    def setDocumentLocator(self, locator):
        self.locator = locator
    
    def showError(self, msg):
        print '*** (showError):', msg
        sys.exit(-1)

    def startElement(self, name, attrs):
        done = 0
        if name == 'alarm-definitions':
            obj = alarm_definitions.factory()
            stackObj = SaxStackElement('alarm-definitions', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarms-to-create':
            obj = alarm_definition_listType.factory()
            stackObj = SaxStackElement('alarms_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarms-to-update':
            obj = alarm_definition_listType.factory()
            stackObj = SaxStackElement('alarms_to_update', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarms-to-remove':
            obj = alarm_definition_listType.factory()
            stackObj = SaxStackElement('alarms_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'sources-to-create':
            obj = source_definition_listType.factory()
            stackObj = SaxStackElement('sources_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'sources-to-update':
            obj = source_definition_listType.factory()
            stackObj = SaxStackElement('sources_to_update', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'sources-to-remove':
            obj = source_definition_listType.factory()
            stackObj = SaxStackElement('sources_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'categories-to-create':
            obj = category_definition_listType.factory()
            stackObj = SaxStackElement('categories_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'categories-to-update':
            obj = category_definition_listType.factory()
            stackObj = SaxStackElement('categories_to_update', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'categories-to-remove':
            obj = category_definition_listType.factory()
            stackObj = SaxStackElement('categories_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'visual-fields':
            obj = visual_fields.factory()
            stackObj = SaxStackElement('visual_fields', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'system-name':
            stackObj = SaxStackElement('system_name', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'identifier':
            stackObj = SaxStackElement('identifier', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'problem-description':
            stackObj = SaxStackElement('problem_description', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'instant':
            stackObj = SaxStackElement('instant', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'cause':
            stackObj = SaxStackElement('cause', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'action':
            stackObj = SaxStackElement('action', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'consequence':
            stackObj = SaxStackElement('consequence', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'priority':
            stackObj = SaxStackElement('priority', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'responsible-id':
            stackObj = SaxStackElement('responsible_id', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'piquetGSM':
            stackObj = SaxStackElement('piquetGSM', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'help-url':
            stackObj = SaxStackElement('help_url', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'source-name':
            stackObj = SaxStackElement('source_name', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'location':
            obj = locationType.factory()
            stackObj = SaxStackElement('location', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'piquetEmail':
            stackObj = SaxStackElement('piquetEmail', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'description':
            stackObj = SaxStackElement('description', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'connection-timeout':
            stackObj = SaxStackElement('connection_timeout', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'building':
            stackObj = SaxStackElement('building', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'floor':
            stackObj = SaxStackElement('floor', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'room':
            stackObj = SaxStackElement('room', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'mnemonic':
            stackObj = SaxStackElement('mnemonic', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'position':
            stackObj = SaxStackElement('position', None)
            self.stack.append(stackObj)
            done = 1
        elif name == 'links-to-create':
            obj = reduction_link_definition_listType.factory()
            stackObj = SaxStackElement('links_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'links-to-remove':
            obj = reduction_link_definition_listType.factory()
            stackObj = SaxStackElement('links_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'thresholds':
            obj = thresholds.factory()
            stackObj = SaxStackElement('thresholds', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'threshold':
            obj = threshold.factory()
            val = attrs.get('value', None)
            if val is not None:
                try:
                    obj.set_value(int(val))
                except:
                    self.reportError('"value" attribute must be integer')
            stackObj = SaxStackElement('threshold', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarm-definition':
            obj = alarm_definition.factory()
            stackObj = SaxStackElement('alarm_definition', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'parent':
            obj = parent.factory()
            stackObj = SaxStackElement('parent', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'child':
            obj = child.factory()
            stackObj = SaxStackElement('child', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'masks-to-create':
            obj = mask_definition_listType.factory()
            stackObj = SaxStackElement('masks_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'masks-to-remove':
            obj = mask_definition_listType.factory()
            stackObj = SaxStackElement('masks_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'category-definition':
            obj = category_definition.factory()
            stackObj = SaxStackElement('category_definition', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'source-definition':
            obj = source_definition.factory()
            stackObj = SaxStackElement('source_definition', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'maintenance-mask':
            obj = maintenance_mask.factory()
            stackObj = SaxStackElement('maintenance_mask', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'mode-mask':
            obj = mode_mask.factory()
            stackObj = SaxStackElement('mode_mask', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'reduction-link':
            obj = reduction_link.factory()
            stackObj = SaxStackElement('reduction_link', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'category-links-to-create':
            obj = alarm_category_link_definition_listType.factory()
            stackObj = SaxStackElement('category_links_to_create', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'category-links-to-remove':
            obj = alarm_category_link_definition_listType.factory()
            stackObj = SaxStackElement('category_links_to_remove', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'category':
            obj = category.factory()
            stackObj = SaxStackElement('category', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarm':
            obj = alarm.factory()
            stackObj = SaxStackElement('alarm', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'alarm-category-link':
            obj = alarm_category_link.factory()
            stackObj = SaxStackElement('alarm_category_link', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == 'configuration-property':
            obj = configuration_property.factory()
            stackObj = SaxStackElement('configuration_property', obj)
            self.stack.append(stackObj)
            done = 1
        if not done:
            self.reportError('"%s" element not allowed here.' % name)

    def endElement(self, name):
        done = 0
        if name == 'alarm-definitions':
            if len(self.stack) == 1:
                self.root = self.stack[-1].obj
                self.stack.pop()
                done = 1
        elif name == 'alarms-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_alarms_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'alarms-to-update':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_alarms_to_update(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'alarms-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_alarms_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'sources-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_sources_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'sources-to-update':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_sources_to_update(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'sources-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_sources_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'categories-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_categories_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'categories-to-update':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_categories_to_update(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'categories-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_categories_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'visual-fields':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_visual_fields(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'system-name':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_system_name(content)
                self.stack.pop()
                done = 1
        elif name == 'identifier':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_identifier(content)
                self.stack.pop()
                done = 1
        elif name == 'problem-description':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_problem_description(content)
                self.stack.pop()
                done = 1
        elif name == 'instant':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                if content and content in ('true', '1'):
                    content = 1
                else:
                    content = 0
                self.stack[-2].obj.set_instant(content)
                self.stack.pop()
                done = 1
        elif name == 'cause':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_cause(content)
                self.stack.pop()
                done = 1
        elif name == 'action':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_action(content)
                self.stack.pop()
                done = 1
        elif name == 'consequence':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_consequence(content)
                self.stack.pop()
                done = 1
        elif name == 'priority':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_priority(content)
                self.stack.pop()
                done = 1
        elif name == 'responsible-id':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_responsible_id(content)
                self.stack.pop()
                done = 1
        elif name == 'piquetGSM':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_piquetGSM(content)
                self.stack.pop()
                done = 1
        elif name == 'help-url':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_help_url(content)
                self.stack.pop()
                done = 1
        elif name == 'source-name':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_source_name(content)
                self.stack.pop()
                done = 1
        elif name == 'location':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_location(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'piquetEmail':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_piquetEmail(content)
                self.stack.pop()
                done = 1
        elif name == 'description':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_description(content)
                self.stack.pop()
                done = 1
        elif name == 'connection-timeout':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_connection_timeout(content)
                self.stack.pop()
                done = 1
        elif name == 'building':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_building(content)
                self.stack.pop()
                done = 1
        elif name == 'floor':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_floor(content)
                self.stack.pop()
                done = 1
        elif name == 'room':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_room(content)
                self.stack.pop()
                done = 1
        elif name == 'mnemonic':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_mnemonic(content)
                self.stack.pop()
                done = 1
        elif name == 'position':
            if len(self.stack) >= 2:
                content = self.stack[-1].content
                self.stack[-2].obj.set_position(content)
                self.stack.pop()
                done = 1
        elif name == 'links-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_links_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'links-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_links_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'thresholds':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_thresholds(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'threshold':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_threshold(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'alarm-definition':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_alarm_definition(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'parent':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_parent(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'child':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_child(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'masks-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_masks_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'masks-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_masks_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'category-definition':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_category_definition(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'source-definition':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_source_definition(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'maintenance-mask':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_maintenance_mask(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'mode-mask':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_mode_mask(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'reduction-link':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_reduction_link(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'category-links-to-create':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_category_links_to_create(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'category-links-to-remove':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_category_links_to_remove(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'category':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_category(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'alarm':
            if len(self.stack) >= 2:
                self.stack[-2].obj.set_alarm(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'alarm-category-link':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_alarm_category_link(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        elif name == 'configuration-property':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add_configuration_property(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        if not done:
            self.reportError('"%s" element not allowed here.' % name)

    def characters(self, chrs, start, end):
        if len(self.stack) > 0:
            self.stack[-1].content += chrs[start:end]

    def reportError(self, mesg):
        locator = self.locator
        sys.stderr.write('Doc: %s  Line: %d  Column: %d\n' % \
            (locator.getSystemId(), locator.getLineNumber(), 
            locator.getColumnNumber() + 1))
        sys.stderr.write(mesg)
        sys.stderr.write('\n')
        sys.exit(-1)
        #raise RuntimeError

USAGE_TEXT = """
Usage: python <Parser>.py [ -s ] <in_xml_file>
Options:
    -s        Use the SAX parser, not the minidom parser.
"""

def usage():
    print USAGE_TEXT
    sys.exit(-1)


#
# SAX handler used to determine the top level element.
#
class SaxSelectorHandler(handler.ContentHandler):
    def __init__(self):
        self.topElementName = None
    def getTopElementName(self):
        return self.topElementName
    def startElement(self, name, attrs):
        self.topElementName = name
        raise StopIteration


def parseSelect(inFileName):
    infile = file(inFileName, 'r')
    topElementName = None
    parser = make_parser()
    documentHandler = SaxSelectorHandler()
    parser.setContentHandler(documentHandler)
    try:
        try:
            parser.parse(infile)
        except StopIteration:
            topElementName = documentHandler.getTopElementName()
        if topElementName is None:
            raise RuntimeError, 'no top level element'
        topElementName = topElementName.replace('-', '_').replace(':', '_')
        if topElementName not in globals():
            raise RuntimeError, 'no class for top element: %s' % topElementName
        topElement = globals()[topElementName]
        infile.seek(0)
        doc = minidom.parse(infile)
    finally:
        infile.close()
    rootNode = doc.childNodes[0]
    rootObj = topElement.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0)
    return rootObj


def saxParse(inFileName):
    parser = make_parser()
    documentHandler = Sax_alarm_definitionsHandler()
    parser.setDocumentHandler(documentHandler)
    parser.parse('file:%s' % inFileName)
    root = documentHandler.getRoot()
    sys.stdout.write('<?xml version="1.0" ?>\n')
    root.export(sys.stdout, 0)
    return root


def saxParseString(inString):
    parser = make_parser()
    documentHandler = Sax_alarm_definitionsHandler()
    parser.setDocumentHandler(documentHandler)
    parser.feed(inString)
    parser.close()
    rootObj = documentHandler.getRoot()
    #sys.stdout.write('<?xml version="1.0" ?>\n')
    #rootObj.export(sys.stdout, 0)
    return rootObj


def parse(inFileName):
    doc = minidom.parse(inFileName)
    rootNode = doc.documentElement
    rootObj = alarm_definitions.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0, name_="alarm_definitions")
    return rootObj


def parseString(inString):
    doc = minidom.parseString(inString)
    rootNode = doc.documentElement
    rootObj = alarm_definitions.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0, name_="alarm_definitions")
    return rootObj


def parseLiteral(inFileName):
    doc = minidom.parse(inFileName)
    rootNode = doc.documentElement
    rootObj = alarm_definitions.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('from AcsAlarmSystem_xsd import *\n\n')
    sys.stdout.write('rootObj = alarm_definitions(\n')
    rootObj.exportLiteral(sys.stdout, 0, name_="alarm_definitions")
    sys.stdout.write(')\n')
    return rootObj


def main():
    args = sys.argv[1:]
    if len(args) == 2 and args[0] == '-s':
        saxParse(args[1])
    elif len(args) == 1:
        parse(args[0])
    else:
        usage()


if __name__ == '__main__':
    main()
    #import pdb
    #pdb.run('main()')

