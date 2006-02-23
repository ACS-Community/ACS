/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.tools.entitybuilder;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import org.exolab.castor.xml.Unmarshaller;
import org.xml.sax.InputSource;

import alma.tools.entitybuilder.generated.EntitySchema;
import alma.tools.entitybuilder.generated.EntitybuilderSettings;
import alma.tools.entitybuilder.generated.XmlNamespace2JPackage;


/**
 * @author hsommer
 */
public class EntitybuilderConfig
{
//	public static final String ENTITYBUILDER_CONFIG_XML = "EntitybuilderConfig.xml";
	
	private EntitybuilderSettings m_entitybuilderSettings;
	
    /** key = (String) schema file name w/o path, value = (String) Java package */
    private Map m_schema2pckMap = new HashMap();

    /** key = (String) xsd namespace, value = (String) Java package */
	private Map m_ns2pckMap = new HashMap();


	/**
	 * Reads the configuration data.
	 * @throws BindingException
	 */
	public void load(File configFile, File[] includeConfigFiles)
			throws BindingException
	{	
//		if (m_entitybuilderSettings != null)
//			return;
		
		m_entitybuilderSettings = parseConfigFile(configFile);
		storePckgMappings(m_entitybuilderSettings);
		
		for (int i = 0; i < includeConfigFiles.length; i++)
		{
			EntitybuilderSettings ebs = parseConfigFile(includeConfigFiles[i]);
			storePckgMappings(ebs);
		}        
	}

    private void storePckgMappings(EntitybuilderSettings ebs)
	{
		XmlNamespace2JPackage[] mappings = ebs.getXmlNamespace2JPackage();
		for (int j = 0; j < mappings.length; j++)
		{
			String namespace = mappings[j].getXmlNamespace();
			String jPackage = mappings[j].getJPackage();
			m_ns2pckMap.put(namespace, jPackage);
		}
        
        EntitySchema[] schemas = ebs.getEntitySchema();
        for (int i = 0; i < schemas.length; i++) {
            String schemaName = schemas[i].getSchemaName();
            String namespace = schemas[i].getXmlNamespace();
            String jPackage = (String) m_ns2pckMap.get(namespace); 
            m_schema2pckMap.put(schemaName, jPackage);
        }
	}

    
    /**
     * Gets all xsd namespaces, even those from included config files for which no code is generated directly.
     */
    public String[] getAllNamespaces()
    {
        return (String[]) m_ns2pckMap.keySet().toArray(new String[1]);
    }
    
    public String getJPackageForNamespace(String ns) 
    {
        String jPackage = (String) m_ns2pckMap.get(ns);
        return jPackage;
    }
    

    /**
     * Gets all schema names, even those from included config files for which no code is generated directly.
     * Only file names, no directory path.
     */
    public String[] getAllSchemaNames()
    {
        return (String[]) m_schema2pckMap.keySet().toArray(new String[1]);
    }
    
    public String getJPackageForSchema(String schemaName) 
    {
        String jPackage = (String) m_schema2pckMap.get(schemaName);
        return jPackage;
    }
    
	
	/**
     * Gets all schema files which should be run explicitly through the code generator.
     * Schema files that are only used indirectly (include, import) are not returned.
     * 
     * todo: return only schema names and let caller handle the path resolution (e.g. using XsdFileFinder)
	 * @param schemaDir
	 */
	public File[] getSchemaFiles(File schemaDir)
	{
		Enumeration schemaEnum = m_entitybuilderSettings.enumerateEntitySchema();
		ArrayList schemaList = new ArrayList();
		while (schemaEnum.hasMoreElements())
		{
			EntitySchema schema = (EntitySchema) schemaEnum.nextElement();
			String relPath = schema.getRelativePathSchemafile();
			if (relPath.trim().length() == 0 || relPath.equals("."))
			{
				relPath = File.separator;
			}
			else
			{
				relPath = File.separator + relPath + File.separator;
			}
			String absPathName = schemaDir.getAbsolutePath() + relPath + schema.getSchemaName();
			File schemaFile = new File(absPathName);
			if (schemaFile.exists())
			{
				schemaList.add(schemaFile);
			}
			else
			{
				System.err.println("specified schema file " + absPathName + " does not exist; will be ignored.");
			}
		}
		return (File[]) schemaList.toArray(new File[0]);
	}


    private EntitybuilderSettings parseConfigFile(File configFile) throws BindingException
    {
        EntitybuilderSettings ebs = null;
        InputStream is = null;
        
        try {
            is = new FileInputStream(configFile);
        }
        catch (Exception e) {
        }
        
        if (is == null) {
            throw new BindingException("xml config file '" + configFile.getAbsolutePath() + "' not found.");
        }
        
        try {
            ebs = (EntitybuilderSettings) Unmarshaller.unmarshal(
                        EntitybuilderSettings.class, new InputSource(is));
        }
        catch (Exception e) {
            String msg = "parsing of schema config file failed. ";
            throw new BindingException(msg, e);
        }
        
        return ebs;
    }
    



//    private void generateXmlTemplate(File xmlFile)
//			throws BindingException
//	{
//		EntitybuilderSettings ebs = new EntitybuilderSettings();
//
//		EntitySchema es1 = new EntitySchema();
//		ebs.addEntitySchema(es1);
//		es1.setSchemaName("ObsProject.xsd");
//		es1.setXmlNamespace("Alma/ObsProject");
//
//		XmlNamespace2JPackage nsp = new XmlNamespace2JPackage();
//		ebs.addXmlNamespace2JPackage(nsp);
//		nsp.setXmlNamespace("Alma/ObsProject");
//		nsp.setJPackage("alma.entity.xmlbinding.obsproject");
//		
//		try
//		{
//			Marshaller.marshal(ebs, new FileWriter(xmlFile));
//		}
//		catch (Exception e)
//		{
//			throw new BindingException("marshalling error: ", e);
//		}
//	}

}
