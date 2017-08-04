#!/usr/bin/env python2
#
# This script generates BACI Property Reference Document in HTML format
# which contains the following information:
#  * Table of all BACI property types.
#  * List of characteristics of each BACI property type with description.
#  * Hierarchy of BACI property types.
#  * Table of characteristics supported by each BACI property type.
#
# The source of the information is extracted from BACI.xsd.
#
# Run this script with python 2 interpreter without any argument. This
# script reads BACI.xsd in ../config/CDB/schemas (relative path from
# the directory where this script file is located) and generate
# reference.html in the same directory as this script file.
#
# Author: Takashi Nakamoto <takashi.nakamoto@nao.ac.jp>
#

import xml.etree.ElementTree as ET
from graphviz import Digraph
import re
import os
import base64

# It is assumed that this script file is located in
#  ACS/LGPL/CommonSoftware/baciidl/ws/test/
script_dir = os.path.abspath(os.path.dirname(__file__))
baci_xsd_file = os.path.join(script_dir, '../config/CDB/schemas/BACI.xsd')

# Temporary directory where image and other temporary files are generated.
temp_dir = '/tmp'

# Path to BACI Property Reference Document file.
document_path = os.path.join(script_dir, 'reference.html')

class BACICharacteristic:
    def __init__(self, element, nsmap):
        self.name        = element.attrib['name']
        self.xsd_type    = element.attrib['type']
        self.use         = element.attrib['use'] if 'use' in element.attrib else ''
        self.default     = element.attrib['default']
        self.description = ''


class BACIType:
    def __init__(self, name):
        self.name = name
        self.description = ''
        self.characteristics = []

    def has_characteristic(self, characteristic_name):
        return characteristic_name in [c.name for c in self.characteristics]


class BACISchema:
    __NSMAP = {'xs': 'http://www.w3.org/2001/XMLSchema'}
    __GRAPHVIZ_FORMAT = 'png'

    def __init__(self):
        self.baci_types = []
        self.type_hierarchy = Digraph(format=BACISchema.__GRAPHVIZ_FORMAT)

    @classmethod
    def parse(cls, xsd_file):
        schema = BACISchema()
        root = ET.parse(xsd_file)
        baci_type_elements = cls.__find_all_baci_type_elements(root)
        schema.baci_types = [cls.__generate_baci_type(e, root)
                             for e in baci_type_elements]
        cls.__generate_type_hierarchy(root, schema.type_hierarchy)
        return schema

    @classmethod
    def __generate_type_hierarchy(cls, root, dot):
        """
        Generate type hierarchy tree using Graphviz Digraph tree.
        @param root Root element of BACI.xsd parsed by ElementTree.
        @param dot  An instance of graphviz.Digraph.
        """
        processed_elements = []
        queue = cls.__find_all_baci_type_elements(root)

        while queue:
            e = queue.pop(0)
            if not e in processed_elements:
                processed_elements.append(e)
                dot.node(e.attrib['name'])
                
                for parent in cls.__find_extended_parent_type_elements(root, e):
                    dot.edge(e.attrib['name'], parent.attrib['name'])
                    queue.append(parent)

                for parent in cls.__find_restricted_parent_type_elements(root, e):
                    dot.edge(e.attrib['name'], parent.attrib['name'])
                    queue.append(parent)

                    
    @classmethod
    def __generate_baci_type(cls, type_element, root):
        """
        This method returns an instance of BACIType that corresponds to the
        given element.
        @param type_element An complexType element in ElementTree that represents
                            one BACI Type. It must be one of the elements that
                            __find_all_baci_type() method returns.
        @param root         The root element of BACI.xsd parsed by ElementTree.
        """
        baci_type = BACIType(type_element.attrib['name'])
        baci_type.description = cls.__get_annotation(type_element)
        baci_type.characteristics = cls.__get_characteristics(type_element, root)
        return baci_type

    @classmethod
    def __get_characteristics(cls, type_element, root):
        """
        This method first finds all characteristics of the given BACI type
        including the ones that are inherited from ancestor types Then,
        this method creates instances of BACICharacteristics and returns a
        list of those instances. The characteristics are sorted in ascending
        order by the characteristics names.
        @param type_element An complexType element in ElementTree that represents
                            one BACI Type. It must be one of the elements that
                            __find_all_baci_type() method returns.
        @param root         The root element of BACI.xsd parsed by ElementTree.
        """
        characteristics = []

        # Find the parent types specified by <xs:extension base="..."></xs:extension>
        # tags, add the characteristics of the ancestor types and extend the parent
        # with the newly defined characteristics.
        for parent in cls.__find_extended_parent_type_elements(root, type_element):
            characteristics.extend(cls.__get_characteristics(parent, root))
            
            for e in cls.__find_extended_attributes(type_element, parent.attrib['name']):
                c = BACICharacteristic(e, cls.__NSMAP)
                c.description = cls.__get_annotation(e)
                characteristics.append(c)
                
        # Find the parent types specified by <xs:restriction base="..."></xs:restriction>
        # tags, add the characteristics of the ancestor types and replace the
        # ancestors' characteristics by the characteristics specified in
        # <xs:restriction> tags.
        for parent in cls.__find_restricted_parent_type_elements(root, type_element):
            characteristics.extend(cls.__get_characteristics(parent, root))

            for e in cls.__find_restricted_attributes(type_element, parent.attrib['name']):
                c = next(cc for cc in characteristics if cc.name == e.attrib['name'])
                c.xsd_type    = e.attrib['type']
                c.use         = e.attrib['use'] if 'use' in e.attrib else ''
                c.default     = e.attrib['default']
                c.description = cls.__get_annotation(e)

        return sorted(characteristics, key = lambda c: c.name)


    @classmethod
    def __get_annotation(cls, element):
        """
        Returns the annotation document of the given element. If the annotation
        does not exist, this method returns an empty string.
        @param element element in XML schema parsed by ElementTree
        """
        d = element.find('./xs:annotation/xs:documentation', cls.__NSMAP)
        if d is not None and d.text is not None:
            return re.sub(' +', ' ',
                          d.text.strip().replace('\n', ' '))
        else:
            return ''
        
    @classmethod
    def __find_all_baci_type_elements(cls, root):
        return cls.__find_ro_baci_type_elements(root) + \
               cls.__find_rw_baci_type_elements(root)

    @classmethod
    def __find_ro_baci_type_elements(cls, root):
        cond = lambda name: name.startswith('RO')
        return cls.__find_complex_type_elements(root, cond)

    @classmethod
    def __find_rw_baci_type_elements(cls, root):
        cond = lambda name: name.startswith('RW')
        return cls.__find_complex_type_elements(root, cond)
    
    @classmethod
    def __find_complex_type_elements(cls, root, cond):
        """
        Returns a list of xs:complexType elements whose name attribute matches
        a givencondition.
        @param root root element of BACI.xsd parsed by ElementTree
        @param cond lambda function which takes one string argument, evaluates
                    whether the given string matches a certain condition and
                    returns True or False.
        """
        return [e for e in root.findall('./xs:complexType', cls.__NSMAP)
                if cond(e.attrib['name'])]

    @classmethod
    def __find_all_baci_type_names(cls, root):
        return [e.attrib['name'] for e in cls.__find_all_baci_type_elements(root)]

    @classmethod
    def __find_complex_type_elements_by_name(cls, root, name):
        return root.find('./xs:complexType[@name="%s"]' % name, cls.__NSMAP)

    @classmethod
    def __find_extended_parent_type_names(cls, type_element):
        return [e.attrib['base'] for e
                in type_element.findall('./xs:complexContent/xs:extension',
                                        cls.__NSMAP)]
    
    @classmethod
    def __find_extended_parent_type_elements(cls, root, type_element):
        return [cls.__find_complex_type_elements_by_name(root, parent_name) 
                for parent_name
                in cls.__find_extended_parent_type_names(type_element)]

    @classmethod
    def __find_extended_attributes(cls, type_element, parent_name):
        return type_element.findall('./xs:complexContent/xs:extension[@base="%s"]/xs:attribute' % parent_name, cls.__NSMAP)

    @classmethod
    def __find_restricted_parent_type_names(cls, type_element):
        return [e.attrib['base'] for e
                in type_element.findall('./xs:complexContent/xs:restriction',
                                        cls.__NSMAP)]
    @classmethod
    def __find_restricted_parent_type_elements(cls, root, type_element):
        return [cls.__find_complex_type_elements_by_name(root, parent_name) 
                for parent_name
                in cls.__find_restricted_parent_type_names(type_element)]

    @classmethod
    def __find_restricted_attributes(cls, type_element, parent_name):
        return type_element.findall('./xs:complexContent/xs:restriction[@base="%s"]/xs:attribute' % parent_name, cls.__NSMAP)


    def generate_reference_document(self, path):
        """
        Generate the reference document of BACI properties in HTML format.
        @param path Path of the HTML file.
        """

        sorted_baci_types = sorted(self.baci_types,
                                   key = lambda baci_type: baci_type.name)
        ro_scalar_baci_types = [baci_type.name
                                for baci_type in sorted_baci_types
                                if baci_type.name.startswith('RO') and
                                   not baci_type.name.endswith('Seq')]
        rw_scalar_baci_types = [baci_type.name
                                for baci_type in sorted_baci_types
                                if baci_type.name.startswith('RW') and
                                   not baci_type.name.endswith('Seq')]
        ro_seq_baci_types = [baci_type.name
                             for baci_type in sorted_baci_types
                             if baci_type.name.startswith('RO') and
                             baci_type.name.endswith('Seq')]
        rw_seq_baci_types = [baci_type.name
                             for baci_type in sorted_baci_types
                             if baci_type.name.startswith('RW') and
                             baci_type.name.endswith('Seq')]
        type_rows = [ro_scalar_baci_types, rw_scalar_baci_types,
                     ro_seq_baci_types, rw_seq_baci_types]
        
        f = open(path, mode='w')
        f.write('<html><head><title>BACI Property Reference Document</title></head><body>')
        f.write('<h1>BACI Property Reference Document</h1>')

        # Output TOC.
        f.write('<h2>Table of Contents</h2>')
        f.write('<ul>')
        f.write('<li>Types of BACI Properties')
        f.write('<table border="1">')
        for type_row in type_rows:
            f.write('<tr>')
            for type_name in type_row:
                f.write('<td><a href="#{name}">{name}</a></td>'.format(name=type_name))
            f.write('</tr>')
        f.write('</table>')
        f.write('<li><a href="#type_hierarchy">Type Hierarchy</a></li>')
        f.write('<li><a href="#supported_characteristics">Table of Supported Characteristics</a></li>')
        f.write('</ul>')

        # List of characteristics for each type.
        for baci_type in sorted_baci_types:
            f.write('<h2><a name="{name}">{name}</a></h2>'.format(name=baci_type.name))
            f.write('<p>%s</p>' % baci_type.description)
            f.write('<h3>Characteristics</h3>')
            f.write('<table border="1">')
            f.write('<tr><th>name</th><th>type</th><th>use</th><th>default</th><th>description</th></tr>')
            for c in baci_type.characteristics:
                f.write('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>' %
                        (c.name, c.xsd_type, c.use, c.default, c.description))
            f.write('</table><hr>')

        # Type hierarchy.
        f.write('<h2><a name="type_hierarchy">Type Hierarchy</a></h2>')
        f.write('<img src="data:image/{img_format};base64,{img_data}">'.format(img_format=BACISchema.__GRAPHVIZ_FORMAT, img_data=self.generate_type_hierarchy_image()))


        # Table of characteristics supportd by each BACI property type.
        characteristic_names = self.get_all_characteristic_names()
        
        f.write('<h2><a name="supported_characteristics">Table of Supported Characteristics</a></h2>')
        f.write('<p>"o" means that characteristic is supported by that BACI property type.</p>')
        f.write('<table border="1">')
        f.write('<tr><td></td>')
        for baci_type in sorted_baci_types:
            f.write('<td><a href="#{name}">{name}</a></td>'.format(name=baci_type.name))
        f.write('</tr>')

        for characteristic_name in characteristic_names:
            f.write('<tr><td>%s</td>' % characteristic_name)
            for baci_type in sorted_baci_types:
                if baci_type.has_characteristic(characteristic_name):
                    f.write('<td> O </td>')
                else:
                    f.write('<td>   </td>')
            f.write('</tr>')
        
        f.write('</table>')
                    
        f.write('</body></html>')
        f.close()

    def get_all_characteristic_names(self):
        """
        This method returns a list of all characteristic names used by
        any BACI property type. The list is sorted in ascending order
        by the characteristic names.
        """
        characteristic_names = []
        for baci_type in self.baci_types:
            characteristic_names.extend(
                [c.name for c in baci_type.characteristics
                 if not c.name in characteristic_names])
        return sorted(characteristic_names)

    def generate_type_hierarchy_image(self):
        """
        Genearte the type hierarchy chart as an image file and return it as a
        string in base64 encoding.
        """
        image_file_name = 'baci_type_hierarchy'
        self.type_hierarchy.render(image_file_name, directory=temp_dir, cleanup=True)
        image_file_path = os.path.join(temp_dir,
                                       image_file_name + '.' + BACISchema.__GRAPHVIZ_FORMAT)
        with open(image_file_path, 'rb') as image_file:
            encoded_image = base64.b64encode(image_file.read())
        os.remove(image_file_path)
        return encoded_image

if __name__ == '__main__':
    print 'Reading %s.\n' % baci_xsd_file
    baci_schema = BACISchema.parse(baci_xsd_file)
    baci_schema.generate_reference_document(document_path)
    print 'BACI Property Reference Document was successfully generated at: \n %s' % document_path
    


