package com.cosylab.cdb.jdal.hibernate;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

import com.cosylab.CDB.DAOOperations;

public class ExtraDataFeatureUtil {
	
	public static final Set<String> EMPTY_SET = new HashSet<String>();
	private final Logger logger;

	
	public ExtraDataFeatureUtil(Logger logger) {
		this.logger = logger;
	}
	
	/*
	private static final String removeNamespace(String name)
	{
		// only one is allowed, start from beginning (most likely to find it there)
		int pos = name.indexOf(":");
		if (pos == -1)
			return name;
		else
			return name.substring(pos + 1);
	}
	*/
	
	public static Element getExtraDataMap(String xml)
		throws ParserConfigurationException, SAXException, IOException
	{
		if (xml == null)
			return null;
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document xmldoc = builder.parse(new InputSource(new StringReader(xml)));
		return xmldoc.getDocumentElement();
	}

	public String getExtraDataMap(DAOOperations dao, String path, Set<String> expectedAttributes, Set<String> expectedElements)
		throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx, ParserConfigurationException, TransformerException
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = factory.newDocumentBuilder();
		DOMImplementation impl = builder.getDOMImplementation();
		Document xmldoc = impl.createDocument(null, "data", null);
		Element root = xmldoc.getDocumentElement();

		if (getExtraDataMap(root, dao, path, expectedAttributes, expectedElements))
		{
			StringWriter stringWriter = new StringWriter();
			StreamResult streamResult = new StreamResult(stringWriter);
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer serializer = tf.newTransformer();
			serializer.setOutputProperty(OutputKeys.INDENT,"yes");
			serializer.transform(new DOMSource(xmldoc), streamResult); 		
			String ret = stringWriter.toString();
			
			// Oracle XMLTYPE attributes don't like empty XML, thus we convert it to null
			if (ret != null && ret.trim().isEmpty()) {
				ret = null;
			}

			return ret;
		}
		else {
			return null;
		}
	}
	
	protected static boolean getExtraDataMap(Element root, DAOOperations dao, String path, Set<String> expectedAttributes, Set<String> expectedElements)
		throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx
	{
		final String prefix = path == null || path.length() == 0 ? "" : path + "/";
			
		boolean changed = false;
		
		String[] attributes = dao.get_string_seq(prefix + "_attributes");
		for (String attribute : attributes)
			if (!expectedAttributes.contains(attribute))
			{
				root.setAttribute(/*removeNamespace(*/attribute/*)*/, dao.get_string(prefix + attribute));
				changed = true;
			}
		
		String[] elements = dao.get_string_seq(prefix + "_elements");
		for (String element : elements)
			if (!expectedElements.contains(element) && !element.startsWith("xsi:"))
			{
				// special case, array of numbers (array of strings not supported)
				if (Character.isDigit(element.charAt(0)))
				{
					Element parentNode = (Element)root.getParentNode();
					parentNode.removeChild(root);
					parentNode.setAttribute(/*removeNamespace(*/root.getNodeName()/*)*/, dao.get_string(path));
					changed = true;
					break;
				}

				Element newElement = root.getOwnerDocument().createElement(/*removeNamespace(*/element/*)*/);
				root.appendChild(newElement);
				changed = true;

				getExtraDataMap(newElement, dao, prefix + element, EMPTY_SET, EMPTY_SET);
			}
		
		return changed;
	}

}
