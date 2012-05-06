package com.cosylab.cdb.jdal.hibernate;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import alma.acs.util.XmlNormalizer;

import com.cosylab.cdb.jdal.DAOImpl;
import com.cosylab.cdb.jdal.XMLTreeNode;

/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * DOM-like Java class introspector.
 * @author msekoranja
 */
public class DOMJavaClassIntrospector {

	public static final char PATH_SEPARATOR = '/';
	public static final String SUBNODES_MAP_NAME = "_";

	public static Object getNode(String path, Object rootNode)
	{
        // System.out.println("Getting node "+path+" for rootNode: "+rootNode.getClass().getName());
        
			// return array of all subnodes
		if (path.length() == 0)
			return rootNode;
		
		// remove trailing slashes, we are quite tolerant to additional slashes
		if (path.charAt(0) == '/')
			return getNode(path.substring(1), rootNode);

		int pos = path.indexOf('/');
		if (pos == -1)
			return getChild(path, rootNode);
        
        if (rootNode instanceof alma.TMCDB.maci.LoggingConfig) {
            // System.out.println("Getting child for LoggingConfig");
            return getChild(path, rootNode);
        }
        
		String parent = path.substring(0, pos);
		String subpath = path.substring(pos+1, path.length());
			
		Object subnode = getNode(parent, rootNode);
		if (subnode != null)
			return getNode(subpath, subnode);

		// not found
		return null;
	}

	public static class NodeAndMutator
	{
		public Object node;
		public Method mutator;
		public NodeAndMutator(Object node, Method mutator) {
			this.node = node;
			this.mutator = mutator;
		}
		
	}
	public static NodeAndMutator getRecursiveMutatorMethod(String path, Object rootNode)
	{
		return getRecursiveMutatorMethod(path, rootNode, null);
	}
	public static NodeAndMutator getRecursiveMutatorMethod(String path, Object rootNode, XMLSaver parentSaver)
	{
         //System.out.println("Getting recursive node "+path+" for rootNode: "+rootNode.getClass().getName());
        
		if (path.length() == 0)
			throw new IllegalArgumentException("empty path");
		
		// remove trailing slashes, we are quite tolerant to additional slashes
		if (path.charAt(0) == '/')
			return getRecursiveMutatorMethod(path.substring(1), rootNode, parentSaver);

                if (rootNode instanceof XMLSaver)
                        parentSaver = (XMLSaver)rootNode;

		int pos = path.indexOf('/');
		if (pos == -1)
			return getMutatorMethod(path, rootNode, parentSaver);
        
        if (rootNode instanceof alma.TMCDB.maci.LoggingConfig)
			return getMutatorMethod(path, rootNode, null);
        
		String parent = path.substring(0, pos);
		String subpath = path.substring(pos+1, path.length());
			
		Object subnode = getNode(parent, rootNode);
		if (subnode != null)
			return getRecursiveMutatorMethod(subpath, subnode, parentSaver);

		// not found
		return null;
	}

	public static final Method getAccessorMethod(Class type, String fieldName)
	{
		try {
			String accessorMethodName = "get" + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
			return type.getMethod(accessorMethodName, (Class[])null);
		} catch (NoSuchMethodException e) {
		}

		try {
			String accessorMethodName = "is" + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
			return type.getMethod(accessorMethodName, (Class[])null);
		} catch (NoSuchMethodException e) {
		}
		
		return null;
	}
	
	public static final Method getMutatorMethod(Class type, String fieldName)
	{
		String mutatorMethodName = "set" + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
		Method[] methods = type.getMethods();
		for (Method method : methods)
			if (method.getName().equals(mutatorMethodName))
				return method;

		return null;
	}

	public static Object getChild(String name, Object node)
	{
		synchronized (node)
		{
	        //System.out.println("Getting child '"+name+"' for node "+node.getClass().getName());
	
	        if (node instanceof DAOImpl)
	        {
	        	return (XMLTreeNode)(((DAOImpl)node).getRootNode().getNodesMap().get(name));
	        }
	        if (node instanceof XMLTreeNode)
	        {
	       		Object n = (XMLTreeNode)(((XMLTreeNode)node).getNodesMap().get(name));
				if (n != null)
					return n;
				else
		 			return ((XMLTreeNode)node).getFieldMap().get(name);
	        }
	        else if (node instanceof Map)
			{
				Map map = (Map)node;
				if (map.containsKey(name))
					return map.get(name);
				
				// not found
				return null;
			}
			else if (node instanceof Element)
			{
				// check attribute
				Element element = (Element)node;
				if (element.hasAttribute(name))
					return element.getAttribute(name);
				
				// check element
				NodeList nodeList = element.getElementsByTagName(name);
				if (nodeList.getLength() > 0)
					return (Element)nodeList.item(0);
					
				return null;
			}
			else
			{
				Field field = null;
				final Class nodeType = node.getClass();
				Class type = nodeType;
				while (field == null && type != null)
				{
					try {
						field = type.getDeclaredField(name);
					} catch (NoSuchFieldException e) { /* noop */ }
					type = type.getSuperclass();
				}
				
				if (field != null)
				{
	
					if (name.equals(SUBNODES_MAP_NAME))
					{
						// this will work only on public fields
						try {
							return field.get(node);
						} catch (IllegalAccessException e) {
							// failed to access the field
							return null;
						}
					}
					else
					{
						// using accessor
						try {
							Method accessorMethod = getAccessorMethod(nodeType, name);
							Object retVal = accessorMethod.invoke(node, (Object[])null);
							// @todo TODO now we consider null retVal as non-existant field
							return retVal;
						} catch (Throwable th) {
							// failed to access the field
							return null;
						}
					}
				}
				else if (!name.equals(SUBNODES_MAP_NAME))
				{
					// check extra data
					if (node instanceof ExtraDataFeature)
					{
						Element extraData = ((ExtraDataFeature)node).getExtraData();
						if (extraData != null)
						{
							Object viaExtra = getChild(name, extraData);
							if (viaExtra != null)
								return viaExtra;
						}
					}
					
					// check for subnodesMap map
					Object subnodesMap = getChild(SUBNODES_MAP_NAME, node);
					if (subnodesMap instanceof Map)
						return ((Map)subnodesMap).get(name);
				}
				
				// not found
				return null;
			}
		}
	}
	
	public interface XMLSaver
	{
		void save();
		void save(String xml);
	}
	
	public static class XMLTreeNodeSetter {
		private final String name;
		private final Map map;
		private final XMLSaver parentSaver;
		
		public XMLTreeNodeSetter(String name, Map map, XMLSaver parentSaver)
		{
			this.name = name;
			this.map = map;
			this.parentSaver = parentSaver;
		}
		
		public void setField(String value) {
			map.put(name, value);
			if (parentSaver != null)
				parentSaver.save();
		}
	}

	public static NodeAndMutator getMutatorMethod(String name, Object node, XMLSaver parentSaver)
	{
         //System.out.println("Getting setter '"+name+"' for node "+node.getClass().getName());
        
        if (node instanceof XMLTreeNode)
        {
        	XMLTreeNode treeNode = (XMLTreeNode)node;
        	if (!treeNode.getFieldMap().containsKey(name))
        		return null;
        	
        	XMLTreeNodeSetter setterObject = new XMLTreeNodeSetter(name, treeNode.getFieldMap(), parentSaver);
        	return new NodeAndMutator(setterObject, getMutatorMethod(setterObject.getClass(), "field"));
        }
        else
        {
			Field field = null;
			final Class nodeType = node.getClass();
			Class type = nodeType;
			while (field == null && type != null)
			{
				try {
					field = type.getDeclaredField(name);
				} catch (NoSuchFieldException e) { /* noop */ }
				type = type.getSuperclass();
			}
			
			if (field != null)
			{

				// using mutator
				return new NodeAndMutator(node, getMutatorMethod(nodeType, name));
			}
			
			// not found
			return null;
		}
		
	}

	/*
	public static String[] getChildren(Object node) {
		if (node instanceof Map)
			return getSubnodes(node);
		else if (node instanceof Element)
		{
			// TODO
			return null;
		}
		else
			return getFields(node);
	}
	*/
	
	public static String[] getAccessibleFields(Object node, boolean primitivesOnly) {
		Set<String> subnodes = new LinkedHashSet<String>();
		final Class nodeType = node.getClass();
		Class type = nodeType;
		while (type != null)
		{
			Field[] fields = type.getDeclaredFields();
			for (Field field : fields)
			{
				boolean isPrimitive = isPrimitive(field.getType());
				if ((isPrimitive ^ !primitivesOnly) && getAccessorMethod(nodeType, field.getName()) != null)
				{
					// do not add null non-primitives check
					if (isPrimitive || getChild(field.getName(), node) != null)
						subnodes.add(field.getName());
				}
			}
			
			type = type.getSuperclass();
		}
		
		// and subnodesMap map
		if (!primitivesOnly)
		{
			Object subnodesMap = getChild(SUBNODES_MAP_NAME, node);
			if (subnodesMap instanceof Map)
			{
				Set keySet = ((Map)subnodesMap).keySet();
				for (Object key : keySet)
					subnodes.add(key.toString());
			}
		}
		
		return subnodes.toArray(new String[subnodes.size()]);
	}

	public static void getAccessibleFieldsObjects(Object node, boolean primitivesOnly, ArrayList<NamedObject> subnodes) {
		final Class nodeType = node.getClass();
		Class type = nodeType;
		while (type != null)
		{
			Field[] fields = type.getDeclaredFields();
			for (Field field : fields)
			{
				boolean isPrimitive = isPrimitive(field.getType());
				if ((isPrimitive ^ !primitivesOnly) && getAccessorMethod(nodeType, field.getName()) != null)
				{
					// do not add null non-primitives check
					Object child = getChild(field.getName(), node);
					if (isPrimitive || child != null)
						subnodes.add(new NamedObject(field.getName(), child));
				}
			}
			
			type = type.getSuperclass();
		}
		
		// and subnodesMap map
		if (!primitivesOnly)
		{
			Object subnodesMap = getChild(SUBNODES_MAP_NAME, node);
			if (subnodesMap instanceof Map)
			{
				Map map = (Map)subnodesMap;
				Set keySet = map.keySet();
				for (Object key : keySet)
					subnodes.add(new NamedObject(key.toString(), map.get(key)));
			}
		}
	}

	public static String[] getElementFields(Object node) {
		Set<String> subnodes = new LinkedHashSet<String>();
		final Class nodeType = node.getClass();
		Class type = nodeType;
		while (type != null)
		{
			Field[] fields = type.getDeclaredFields();
			for (Field field : fields)
			{
				boolean isPrimitive = isPrimitive(field.getType());
				if (!isPrimitive && getAccessorMethod(nodeType, field.getName()) != null)
				{
					// do not add null elements check
					if (getChild(field.getName(), node) != null)
						subnodes.add(field.getName());
				}
			}
			
			type = type.getSuperclass();
		}
		
		Object subnodesMap = getChild(SUBNODES_MAP_NAME, node);
		if (subnodesMap instanceof InternalElementsMap)
		{
			Set keySet = ((Map)subnodesMap).keySet();
			for (Object key : keySet)
				subnodes.add(key.toString());
		}
		
		return subnodes.toArray(new String[subnodes.size()]);
	}

	public static String[] getFields(Object node)
	{
		return getFields(node, null, null);
	}
	
	public static String[] getFields(Object node, String nodeName, Logger log)
	{
		if (node instanceof Map)
			return new String[0];
		else if (node instanceof Element)
		{
			NamedNodeMap attributes = ((Element)node).getAttributes();
			int size = attributes.getLength();
			String[] retVal = new String[size];
			for (int i = 0; i < size; i++)
				retVal[i] = attributes.item(i).getNodeName();
			return retVal;
		}
		else
		{
			if (node instanceof ExtraDataFeature)
			{
				String[] extraFields = null;
				Element extraData = ((ExtraDataFeature)node).getExtraData();
				if (extraData != null)
					extraFields = getFields(extraData);
				
				String[] fields = getAccessibleFields(node, true);
				
				if (extraFields == null)
					return fields;
				
				// concat and remove duplicates
				HashSet<String> set = new HashSet<String>(Arrays.asList(fields));
				for (String ef : extraFields)
					if (!set.add(ef))
						if (log != null) log.warning("Duplicate field '" + nodeName + "/" + ef +"'.");
						
				return set.toArray(new String[0]);
			}
			else
				return getAccessibleFields(node, true);
		}
	}

	public static boolean isPrimitive(Class<?> type)
	{
		return type.isPrimitive() || Number.class.isAssignableFrom(type) || type == Boolean.class ||
			   type == String.class || type.isArray() || ConvertToPrimitiveFeature.class.isAssignableFrom(type);
	}
	
	public static String[] getSubnodes(Object node)
	{
		if (node instanceof alma.TMCDB.maci.ComponentNode)
			return getAccessibleFields(node, false); 

		if (node instanceof Map && !(node instanceof InternalElementsMap))
		{
			Set<String> subnodes = new LinkedHashSet<String>();
			Set keySet = ((Map)node).keySet();
			for (Object key : keySet)
				subnodes.add(key.toString());
			return subnodes.toArray(new String[subnodes.size()]);
		}
		else
			return new String[0];
	}
	
	public static String[] getNodes(Object node)
	{
		return getNodes(node, null, null);
	}
		
	public static String[] getNodes(Object node, String nodeName, Logger log)
	{
        if (node instanceof DAOImpl)
        {
        	return getNodes(((DAOImpl)node).getRootNode().getNodesMap());
        }
        else if (node instanceof XMLTreeNode)
        {
       		 return getNodes(((XMLTreeNode)node).getNodesMap());
        }
        else if (node instanceof Map)
		{
			Set<String> subnodes = new LinkedHashSet<String>();
			Set keySet = ((Map)node).keySet();
			for (Object key : keySet)
				subnodes.add(key.toString());
			return subnodes.toArray(new String[subnodes.size()]);
		}
		else if (node instanceof Element)
		{
			NodeList elements = ((Element)node).getChildNodes();
			int size = elements.getLength();
			List<String> list = new ArrayList<String>();
			for (int i = 0; i < size; i++)
				if (elements.item(i).getNodeType() == Element.ELEMENT_NODE)
					list.add(elements.item(i).getNodeName());
			return list.toArray(new String[list.size()]);
		}
		else
			if (node instanceof ExtraDataFeature)
			{
				String[] extraFields = null;
				Element extraData = ((ExtraDataFeature)node).getExtraData();
				if (extraData != null)
					extraFields = getNodes(extraData);
				
				String[] fields = getAccessibleFields(node, false);
				
				if (extraFields == null)
					return fields;
				
				// concat and remove duplicates
				LinkedHashSet<String> set = new LinkedHashSet<String>(Arrays.asList(fields));
				for (String ef : extraFields)
					if (!set.add(ef))
						if (log != null) log.warning("Duplicate node '" + nodeName + "/" + ef + "'.");
				
				return set.toArray(new String[0]);
			}
			else
				return getAccessibleFields(node, false);
	}
	
	static class NamedObject
	{
		final String name;
		final Object obj;
		
		NamedObject(String name, Object obj)
		{
			this.name = name;
			this.obj = obj;
		}
	}
	
	public static NamedObject[] getNodesObjects(Object node, String nodeName, Logger log)
	{
        if (node instanceof DAOImpl)
        {
        	return getNodesObjects(((DAOImpl)node).getRootNode().getNodesMap(), null, null);
        }
        else if (node instanceof XMLTreeNode)
        {
       		 return getNodesObjects(((XMLTreeNode)node).getNodesMap(), null, null);
        }
        else if (node instanceof Map)
		{
        	ArrayList<NamedObject> subnodes = new ArrayList<NamedObject>();
        	Map nodeMap = (Map)node;
			Set keySet = nodeMap.keySet();
			for (Object key : keySet)
				subnodes.add(new NamedObject(key.toString(), nodeMap.get(key)));
			return subnodes.toArray(new NamedObject[subnodes.size()]);
		}
		else if (node instanceof Element)
		{
			NodeList elements = ((Element)node).getChildNodes();
			int size = elements.getLength();
        	ArrayList<NamedObject> list = new ArrayList<NamedObject>(size);
			for (int i = 0; i < size; i++)
			{
				Node nodeItem = elements.item(i);
				if (nodeItem.getNodeType() == Element.ELEMENT_NODE)
					list.add(new NamedObject(nodeItem.getNodeName(), (Element)nodeItem));
			}
			return list.toArray(new NamedObject[list.size()]);
		}
		else
		{
			ArrayList<NamedObject> list = new ArrayList<NamedObject>();
			if (node instanceof ExtraDataFeature)
			{
				NamedObject[] extraFields = null;
				Element extraData = ((ExtraDataFeature)node).getExtraData();
				if (extraData != null)
					extraFields = getNodesObjects(extraData, null, null);

				getAccessibleFieldsObjects(node, false, list);
				
				if (extraFields != null)
				{
					for (NamedObject no : extraFields)
						list.add(no);
				}
			}
			else
				getAccessibleFieldsObjects(node, false, list);
			
			return list.toArray(new NamedObject[list.size()]);
		}
	}

	public static String[] getElements(Object node)
	{
		return getElements(node, null, null);
	}
	
	// element = internal node (excluding hierarchy)
	public static String[] getElements(Object node, String nodeName, Logger log)
	{
        if (node instanceof DAOImpl)
        {
        	return getNodes(((DAOImpl)node).getRootNode().getNodesMap());
        }
        else if (node instanceof XMLTreeNode)
        {
       		 return getNodes(((XMLTreeNode)node).getNodesMap());
        }
        else if (node instanceof Map)
		{
			if (node instanceof InternalElementsMap)
			{	
				Set<String> subnodes = new LinkedHashSet<String>();
				Set keySet = ((Map)node).keySet();
				for (Object key : keySet)
					subnodes.add(key.toString());
				return subnodes.toArray(new String[subnodes.size()]);
			}
			else
				return new String[0];
		}
		else if (node instanceof Element)
		{
			NodeList elements = ((Element)node).getChildNodes();
			int size = elements.getLength();
			List<String> list = new ArrayList<String>();
			for (int i = 0; i < size; i++)
				if (elements.item(i).getNodeType() == Element.ELEMENT_NODE)
					list.add(elements.item(i).getNodeName());
			return list.toArray(new String[list.size()]);
		}
		else
			if (node instanceof ExtraDataFeature)
			{
				String[] extraFields = null;
				Element extraData = ((ExtraDataFeature)node).getExtraData();
				if (extraData != null)
					extraFields = getElements(extraData);
				
				String[] fields = getElementFields(node);
				
				if (extraFields == null)
					return fields;
				
				// concat and remove duplicates
				HashSet<String> set = new HashSet<String>(Arrays.asList(fields));
				for (String ef : extraFields)
					if (!set.add(ef))
						if (log != null) log.warning("Duplicate element '" + nodeName + "/" + ef + "'.");
				
				return set.toArray(new String[0]);
			}
			else
				return getElementFields(node);
	}

	public static boolean isMapSubnode(String name, Object parentNode)
	{
		// RootMap is a special case
		if (parentNode instanceof RootMap)
			return false;
		else if (parentNode instanceof Map)
			return true;
		else
		{
			Object subnodesMap = getChild(SUBNODES_MAP_NAME, parentNode);
			if (subnodesMap instanceof Map)
				return ((Map)subnodesMap).containsKey(name);
			else
				return false;
		}
	}

	public static String getNodeXMLName(final String name, final Object node)
	{
		if (node instanceof NameOverrideFeature)
		{
			final String overrideName = ((NameOverrideFeature)node).getNameOverride();
			return overrideName != null ? overrideName : name;
		}
		else
			return name;
	}

	public static String getRootNodeXMLName(final String name, final Object node)
	{
		if (node instanceof RootNameOverrideFeature)
		{
			final String overrideName = ((RootNameOverrideFeature)node).getRootNameOverride();
			if (overrideName != null)
				return overrideName;
		}
		
		return getNodeXMLName(name, node);
	}

	public static String toXML(String name, Object node)
	{
		return toXML(name, node, null, null);
	}
	
	public static String toXML(String name, Object node, String nodeName, Logger log)
	{
		// to confirm w/ XML naming
		if (Character.isDigit(name.charAt(0)))
			name = "id" + name;
		// ugly hack
		else if (name.charAt(0) == '*')
			name = "_"; 
		
		// not nice, but adds DAO support
		if (node instanceof DAOImpl)
			return ((DAOImpl)node).getRootNode().toString(false);

		StringBuffer buffer = new StringBuffer();
		buffer.append('<').append(name);
		// RH: Define the amb ns prefix, if it is needed. Otherwise the
		// OpticalTelescope component fails.
		if (name.startsWith("amb:"))
		    buffer.append(" xmlns:amb=\"urn:schemas-cosylab-com:AmbDevice:1.0\" ");
		
		String[] fields = getFields(node, nodeName, log);
		for (String field : fields)
		{
			Object fieldValue = handleInfinity(getChild(field, node));
			fieldValue = escapeString(fieldValue);
			if (fieldValue != null)
				buffer.append(' ').append(field).append('=').append('"').append(fieldValue).append('"');
		}
		buffer.append('>');
		
		/*
		String[] subnodes = getNodes(node, nodeName, log);
		for (String subnode : subnodes)
		{
			Object childNode = getNode(subnode, node);
			if (childNode == null)
			{
				if (!subnode.equals("ComponentLogger"))
					System.out.println("Warning: '/' in name, ignored:" + subnode);
				continue;
			}
			boolean dashAsName = false;
			if (getChild(SUBNODES_MAP_NAME, node) instanceof InternalElementsMap);
			else if (isMapSubnode(subnode, node))
			{
				dashAsName = true;
				Object subsubMap = getChild(SUBNODES_MAP_NAME, childNode);
				if (subsubMap instanceof Map && ((Map)subsubMap).size() > 0)
					dashAsName = false;
			}
			buffer.append(toXML(getNodeXMLName(dashAsName ? "_" : subnode, childNode), childNode, (nodeName == null) ? null : nodeName + "/" + subnode, log));
		}
		*/
		
		NamedObject[] subnodes = getNodesObjects(node, nodeName, log);
		for (NamedObject namedObject : subnodes)
		{
			final String subnode = namedObject.name;
			final Object childNode = namedObject.obj;
			
			// TODO do we really need this?
			if (subnode.indexOf('/') >= 0)
			{
				if (!subnode.equals("ComponentLogger"))
					System.out.println("Warning: '/' in name, ignored:" + subnode);
				continue;
			}

			boolean dashAsName = false;
			if (getChild(SUBNODES_MAP_NAME, node) instanceof InternalElementsMap);
			else if (isMapSubnode(subnode, node))
			{
				dashAsName = true;
				Object subsubMap = getChild(SUBNODES_MAP_NAME, childNode);
				if (subsubMap instanceof Map && ((Map)subsubMap).size() > 0)
					dashAsName = false;
			}
			buffer.append(toXML(getNodeXMLName(dashAsName ? "_" : subnode, childNode), childNode, (nodeName == null) ? null : nodeName + "/" + subnode, log));
		}

		// element value
		if (node instanceof ElementValueFeature)
		{
			String elementValue = ((ElementValueFeature)node).getElementValue();
			if (elementValue != null)
				buffer.append(elementValue);
		}
			
		buffer.append('<').append('/').append(name).append('>');
		
		return buffer.toString();
	}
	
	public static String stringifyArray(Object array)
	{
		return stringifyArray(array, ',');
	}
	
	public static String stringifyArray(Object array, char separator)
	{
		Class type = array.getClass(); 
		if (!type.isArray())
			throw new IllegalArgumentException("not an array");
			
		Class componentType = type.getComponentType();
		if (!componentType.isPrimitive() && componentType != String.class)
			throw new IllegalArgumentException("not array of a primitive or a string");
		
		StringBuffer buffer = new StringBuffer();
		int length = Array.getLength(array);
		for (int index = 0; index < length - 1; index++)
			buffer.append(handleInfinity(Array.get(array, index))).append(separator);
		if (length > 0)
			buffer.append(handleInfinity(Array.get(array, length - 1)));
		
		return buffer.toString();
	}

	public static final Object handleInfinity(final Object value)
	{
		if (value instanceof Double)
		{
			final double val = ((Double)value).doubleValue();
			if (val == Double.POSITIVE_INFINITY)
				return new Double(Double.MAX_VALUE);
			else if (val == Double.NEGATIVE_INFINITY)
				return new Double(Double.MIN_VALUE);
			return value;
		}
		else if (value instanceof Float)
		{
			final float val = ((Float)value).floatValue();
			if (val == Float.POSITIVE_INFINITY)
				return new Float(Float.MAX_VALUE);
			else if (val == Float.NEGATIVE_INFINITY)
				return new Float(Float.MIN_VALUE);
			return value;
		}
		else
			return value;
	}
	
	public static final Object escapeString(final Object value)
	{
		if (value instanceof String)
			return XmlNormalizer.normalize((String)value);
		else
			return value;
	}
	
}
