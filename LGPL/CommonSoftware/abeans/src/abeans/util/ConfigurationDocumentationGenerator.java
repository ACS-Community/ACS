/*
 * Copyright (c) 2003 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */
 
package abeans.util;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import abeans.core.Identifier;
import abeans.core.defaults.Configurable;
import abeans.pluggable.acs.ACSAbeansEngine;
import abeans.pluggable.acs.DefaultCORBAService;
import abeans.pluggable.acs.NamingServiceRemoteDirectory;
import abeans.pluggable.acs.cdb.dal.CDBDALPlug;
import abeans.pluggable.acs.logging.RemoteLoggingService;
import abeans.pluggable.acs.maci.ACSPlug;

/**
 * Introspects <code>Configurable</code> interface(s) and generates 
 * documentation out of it.
 * @author <a href="mailto:matej.sekoranja@cosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class ConfigurationDocumentationGenerator
{

	/**
	 * Creates <code>Configurable</code> instances out of <code>Class</code>-es.
	 * @param configurableClasses
	 * @return	array of <code>Configurable</code> instances.
	 */
	public static Configurable[] createInstances(Class[] configurableClasses)
	{

		assert (configurableClasses != null);
	
		ArrayList list = new ArrayList();
		
		for (int i = 0; i < configurableClasses.length; i++)
			if (configurableClasses[i] == null) 
				continue;
			else if (Configurable.class.isAssignableFrom(configurableClasses[i]))
			{
				try
				{
					// TODO check duplication
					
					list.add(configurableClasses[i].newInstance());
				}
				catch (Throwable th)
				{
					System.err.println("Failed to create a new instance for class '" + configurableClasses[i] + "': ");
					th.printStackTrace();
				}
			}
			else
			{
				System.err.println("'" + configurableClasses[i] + "' does not implement Configurable interface.");
			}
			
		
		// continue even if list is empty
		Configurable[] configurables = new Configurable[list.size()];
		list.toArray(configurables);
		return configurables;
	}


	/**
	 * Generate documentation out of given <code>Configurable</code> <code>Class</code>-es.
	 * @param configurables	array of <code>Configurable</code> <code>Class</code>-es whose documentation to generate.
	 */
	public static void generatePlain(Class[] configurableClasses)
	{
		assert (configurableClasses != null);
		generatePlain(createInstances(configurableClasses));
	}
	
	/**
	 * Generate documentation out of given <code>Configurable</code>-s.
	 * @param configurables	array of <code>Configurable</code> instances whose documentation to generate.
	 */
	public static void generatePlain(Configurable[] configurables)
	{
		assert (configurables != null);
		
		for (int i = 0; i < configurables.length; i++)
		{
			String name = null;

			Identifier id = configurables[i].getIdentifier();
			if (id != null)
				name = id.getQualifiedLongName(); 

			if (name == null) configurables[i].getClass().getName();
			System.out.println(name);

			// TODO slow
			for (int l = 0; l < name.length(); l++)
				System.out.print('-');
			System.out.println();
			
			System.out.println("Configuration name: " + configurables[i].getConfigurationName() + ".txt");
			System.out.println();
			
			String[][] config = configurables[i].getConfigurationDescriptions();
			if (config != null)
			{
				for (int j = 0; j < config.length; j++)
					if (config[j] != null && config[j].length == 2)
					{ 
						System.out.println(config[j][0] + " = " + config[j][1]);
					}
			}
			
			System.out.println();
		}
	}

	/**
	 * The namespace schema that must be defined by all CosyXML compliant documents.
	 * This will be checked by the parser.
	 */
	public static final String DOCUMENT_SCHEMA = "urn:schemas-cosylab-com:Document";

	// TODO autogenerate title from fileName

	/**
	 * Generate CosyDoc documentation out of given <code>Configurable</code> <code>Class</code>-es.
	 * @param	fileName		name of the output CosyDoc file.
	 * @param	title			document title.
	 * @param configurables	array of <code>Configurable</code> <code>Class</code>-es whose documentation to generate.
	 */
	public static void generateCosyDoc(String fileName, String title, Class[] configurableClasses)
	{
		assert (configurableClasses != null);
		generateCosyDoc(fileName, title, createInstances(configurableClasses));
	}

	/**
	 * Generate a CosyDoc documentation out of given <code>Configurable</code>-s.
	 * @param	fileName		name of the output CosyDoc file.
	 * @param	title			document title.
	 * @param	configurables	array of <code>Configurable</code> instances whose documentation to generate.
	 */
	public static void generateCosyDoc(String fileName, String title, Configurable[] configurables)
	{
		assert (fileName != null);
		assert (configurables != null);

		System.out.println("Generating ' "  + fileName +  "'...");

		try
		{
			// create an empty XML document
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document d = db.newDocument();

			// TODO add those
			/*
			<?xml version="1.0" encoding="ISO-8859-1"?>
			<?xml-stylesheet type="text/xsl" href="../../Common/Documentation/Normal.xsl"?>
			 */ 
			
			
			// create top-level cosydoc element
			Element cosydoc = d.createElement("cosydoc");
			cosydoc.setAttribute("xmlns", DOCUMENT_SCHEMA);

			// create head document
			Element head = d.createElement("head");
			cosydoc.appendChild(head);

			// add title
			Element e = d.createElement("title");
			e.appendChild(d.createTextNode(title));
			head.appendChild(e);

			// add info
			e = d.createElement("info");
			e.setAttribute("file", fileName);
			//e.setAttribute("url", );
			//e.setAttribute("id", );
			// TODO hardcoded
			e.setAttribute("class", "doc");
			e.setAttribute("project", "abeans");
			e.setAttribute("confidentiality", "public");
			head.appendChild(e);

			// add history
			e = d.createElement("contributor");
			e.setAttribute("name", "Documentation Generator");			
			e.setAttribute("id", "generator");			
			e.setAttribute("email", "matej.sekoranja@cosylab.com");			
			head.appendChild(e);
    
			// add history
			e = d.createElement("modification");
			e.setAttribute("version", "1.0");			
			e.setAttribute("by", "generator");			
			e.setAttribute("date", new SimpleDateFormat("yyyy-MM-dd").format(new Date()));
			Element se = d.createElement("section");
			se.appendChild(d.createTextNode("Generated."));
			e.appendChild(se);
						
			head.appendChild(e);

			// create body document
			Element body = d.createElement("body");
			cosydoc.appendChild(body);

			// iterate through the Configurable
			for (int i = 0; i < configurables.length; i++)
			{
				String name = null;

				Identifier id = configurables[i].getIdentifier();
				if (id != null)
					name = id.getQualifiedLongName(); 

				if (name == null) configurables[i].getClass().getName();

				// add configuration sections
				e = d.createElement("section");
				e.setAttribute("title", name);
				e.setAttribute("id", name);
				body.appendChild(e);

				// add table
				Element table = d.createElement("table");
				e.appendChild(table);
				
				// define main header row
				Element mtr = d.createElement("tr");
				table.appendChild(mtr);

				// add configuration name
				Element mth1 = d.createElement("th");
				mth1.appendChild(d.createTextNode(configurables[i].getConfigurationName() + ".txt"));
				mtr.appendChild(mth1);

				// define header row
				Element tr = d.createElement("tr");
				table.appendChild(tr);

				// define header
				Element th1 = d.createElement("th");
				th1.appendChild(d.createTextNode("Key"));
				tr.appendChild(th1);

				Element th2 = d.createElement("th");
				th2.appendChild(d.createTextNode("Description"));
				tr.appendChild(th2);


				String[][] config = configurables[i].getConfigurationDescriptions();
				if (config != null)
				{

					for (int j = 0; j < config.length; j++)
						if (config[j] != null && config[j].length == 2)
						{

							// define row
							tr = d.createElement("tr");
							table.appendChild(tr);

							// define header
							Element td1 = d.createElement("td");
							/*
							Element bold = d.createElement("b");
							bold.appendChild(d.createTextNode(config[j][0]));
							td1.appendChild(bold);
							*/
							td1.appendChild(d.createTextNode(config[j][0]));
							tr.appendChild(td1);

							Element td2 = d.createElement("td");
							td2.appendChild(d.createTextNode(config[j][1]));
							tr.appendChild(td2);
							
						}
				}

			}

			d.appendChild(cosydoc);
			
			// output
			
			OutputStream out = new BufferedOutputStream(new FileOutputStream(fileName));

			try
			{
				// Serialisation through Tranform.
				DOMSource domSource = new DOMSource(d);
				StreamResult streamResult = new StreamResult(out);
				TransformerFactory tf = TransformerFactory.newInstance();
				Transformer serializer = tf.newTransformer();
				serializer.setOutputProperty(OutputKeys.METHOD, "xml");
				//serializer.setOutputProperty(OutputKeys.ENCODING, "ISO-8859-1");
				serializer.setOutputProperty(OutputKeys.INDENT, "yes");
				serializer.transform(domSource, streamResult); 
			}
			catch (Throwable th) {
				throw new RuntimeException("Transform exception.", th);
			}

			out.flush();
			out.close();

			System.out.println("done.");

		} catch (Exception e)
		{
			System.err.println("Exception while generating documentation: ");
			e.printStackTrace();
		}
		
	}


	/**
	 * Java application main method.
	 * @param args	arguments.
	 */
	public static void main(String[] args)
	{
	
		// initialize the engine (initializes all the services and logs into the manager)
		ACSAbeansEngine engine = new ACSAbeansEngine();
		engine.initialize();

		ConfigurationDocumentationGenerator.generateCosyDoc("doc/DOC-ACS_Abeans_Configuration.xml", "ACS Abeans Configuration",  
			new Class[] { 
				DefaultCORBAService.class,
				NamingServiceRemoteDirectory.class,
				RemoteLoggingService.class,
				ACSPlug.class,
				CDBDALPlug.class
			 });	


		// destroy and exit
		engine.destroy();

	}
}
