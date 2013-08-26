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
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.exolab.castor.builder.FieldInfoFactory;
import org.exolab.castor.builder.SourceGenerator;
import org.exolab.castor.xml.schema.reader.SchemaUnmarshaller;

/**
 * Generates Java binding classes from xml schema files using the Castor generator framework.
 * Needs at least one configuration file for the mapping from xml namespaces to Java packages.
 */
public class CastorBuilder
{
    /**
     * @param args  <code>args[0]</code>: xml config file (with path) complying to EntitybuilderSettings.xsd;
     *               for the schemas that need code generation; <br>
     *               <code>args[1]</code>: output directory under which the generated Java files will be put. <br>        
     *               <code>args[2..n]</code>: -I schemaIncludeDirectory (optional)
     */
    public static void main(String[] args) 
    {
//        System.err.println("Running THE NEW CastorBuilder...");
//        System.out.println("--- main args: ----");
//        for (int i = 0; i < args.length; i++) {
//            System.out.println(args[i]);
//        }
//        System.out.println("---- end main args --- ");
        
        if (args.length < 2) 
        {
            System.out.println(
                "usage: java " + CastorBuilder.class.getName() + " configFile javaOutputDir [-I schemaIncludeDir]");
            System.exit(1);
        }

        try
        {
            String configFileConcat = System.getProperty("ACS.schemaconfigfiles");            
//            System.out.println("ACS.schemaconfigfiles: '" + configFileConcat + "'");            
            CastorBuilder castorBuilder = new CastorBuilder();
            castorBuilder.run0(args, configFileConcat);
        }
        catch (Exception ex)
        {
            System.err.println("schema compilation failed!");
            ex.printStackTrace(System.err);
            System.exit(1);
        }
    }

    /**
     * Parses the arguments given to <code>main</code> and then hands over control to method <code>run</code>. 
     * @param args as in main
     * @param configFileConcat space-separated list of xsd binding config files for included schemas.
     * @throws BindingException 
     * @throws FileNotFoundException 
     */
    public void run0(String[] args, String configFileConcat) throws BindingException, FileNotFoundException 
    {
        // the xml config file that knows the schemas that should be compiled,
        // as well as namespace and java package info.
        File primaryConfigFile = new File(args[0]);         

        // the directory with the schemas that should be compiled 
        File schemaDir = primaryConfigFile.getParentFile();
                
        // the root directory for Java class output
        File javaOutputDir = new File(args[1]);
        
        // -- config files for included schemas
        ArrayList<String> otherConfigFileNames = new ArrayList<String>();
        if (configFileConcat != null)
        {
            StringTokenizer includeSchemasConfigFilesTok = new StringTokenizer(configFileConcat, " ");
            while (includeSchemasConfigFilesTok.hasMoreTokens())
            {
                String configFileName = includeSchemasConfigFilesTok.nextToken().trim();
                if (configFileName.length() > 0) {
                    if (!configFileName.endsWith(".xml")) {
                        configFileName += ".xml";
                    }
                    otherConfigFileNames.add(configFileName);
//                    System.out.println("adding config file " + configFileName);
                }
            }
        }

        // include directories (both schema files and config files)     
        ArrayList<File> includeDirs = new ArrayList<File>();
        boolean pendingInclude = false;
        for (int argInd = 2; argInd < args.length; argInd++)
        {
            String arg = args[argInd].trim();
            arg = (arg.charAt(0) == '"' ? arg.substring(1) : arg);
            arg = (arg.charAt(arg.length()-1) == '"' ? arg.substring(0, arg.length()-1) : arg);
            if (pendingInclude)
            {
                includeDirs.add(new File(arg));
                pendingInclude = false;
            }
            else if (arg.startsWith("-I"))
            {
                if (arg.length() == 2)
                {
                    // just -I, no path
                    pendingInclude = true;
                }
                else 
                {
                    // "-I/a/b/dir" or "-I /a/b/dir" is given as one option
                    includeDirs.add(new File(arg.substring(2)));
                    pendingInclude = false;
                }
            }                   
        }
        
        run(schemaDir, primaryConfigFile, otherConfigFileNames, includeDirs, javaOutputDir );
    }

        

    
	/**
     * Runs the Castor code generator.
     * 
	 * @param schemaDir (base) directory where the xsd files are, for which code will be generated.
	 * @param primaryConfigFile config file for the schema code generation. 
     *          Currently must be in the directory <code>schemaDir</code>.
	 * @param otherConfigFileNames Names without paths of schema code generation config files.
     *          While <code>primaryConfigFile</code> must contain the information for the schemas to compile directly,
     *          these config files have similar information for other schemas which are included by the "primary" schemas. 
     *          This data is needed to generate correct Java packages of already existing binding classes. 
	 * @param includeDirs directories from which other xsd files or config files should be included, 
     *          with preference to directories that appear first in case of multiple occurences of the same file.
	 * @param javaOutputDir root directory under which the generated Java binding classes will be put
	 * @throws BindingException 
	 * @throws FileNotFoundException 
	 */
	public void run(File schemaDir, File primaryConfigFile, List<String> otherConfigFileNames, List<File> includeDirs, File javaOutputDir) throws BindingException, FileNotFoundException 
	{
		// the xml config file that knows the schemas that should be compiled 
		if (!primaryConfigFile.exists()) {
			throw new FileNotFoundException(
				"invalid configuration file: " + primaryConfigFile.getAbsolutePath());
		}
        List<String> allConfigFileNames = new ArrayList<String>();
        allConfigFileNames.add(primaryConfigFile.getName());
        allConfigFileNames.addAll(otherConfigFileNames);

        List<File> allIncludeDirs = new ArrayList<File>();
        // the ALMA Makefile explicitly lists the local schema directory as an include dir, 
        // but it does not hurt to try adding it anyway
        allIncludeDirs.add(schemaDir);
        // in some cases the schema dir may be different from the primary config file's directory.
        // Thus we add the latter explicity, because otherwise the config file would no longer be found.
        allIncludeDirs.add(primaryConfigFile.getParentFile());
        // and of course we must add the explicitly given include dirs
        allIncludeDirs.addAll(includeDirs);

        if (!javaOutputDir.exists()) {
            System.out.println("will create output directory " + javaOutputDir.getAbsolutePath());
        }

        XsdFileFinder xsdFileFinder = new XsdFileFinder(allIncludeDirs, allConfigFileNames);
        xsdFileFinder.setVerbose(false);
        
        EntitybuilderConfig ebc = new EntitybuilderConfig();
        ebc.load(primaryConfigFile, xsdFileFinder.getAllXsdConfigFiles());

        // From the config files, the ebc has now learned about all schemas. 
        // We pass that knowledge to the AlmaURIResolver which will later find schema files for the Castor generator.
        AlmaURIResolver uriResolver = new AlmaURIResolver(ebc.getSchemaName2File());
        // Currently the URI resolver can be passed to SchemaUnmarshaller only through a ALMA patch in module Tools/castor;
        // this is needed to search for imported/included schemas.
        // Hopefully later castor will allow to pass in a URIResolver via SourceGenerator.
        SchemaUnmarshaller.setDefaultURIResolver(uriResolver);       
        
        // Create Castor source generator and configure it
        
        // Java2 collection types: Collection, ArrayList instead of Vector
        FieldInfoFactory fieldInfoFactory = new org.exolab.castor.builder.FieldInfoFactory("arraylist");
        SourceGenerator sgen = new SourceGenerator(fieldInfoFactory);

        // set all namespace to package mappings (also for include/import schemas) for Castor
        String[] allNamespaces = ebc.getAllNamespaces();
        for (int i = 0; i < allNamespaces.length; i++) {            
            String jPackage = ebc.getJPackageForNamespace(allNamespaces[i]);
            sgen.setNamespacePackageMapping(allNamespaces[i], jPackage);
        }

        // set all schema file to package mappings (also for included/imported schemas) -- Castor wants it both ways... 
        for (File schemaFile : ebc.getAllSchemaFiles()) {
        	String schemaName = schemaFile.getName();
            String jPackage = ebc.getJPackageForSchema(schemaName);
//            System.out.println("SourceGenerator#setLocationPackageMapping: " + schemaFileWithPath.getAbsolutePath() + " -> " + jPackage);
            sgen.setLocationPackageMapping(schemaFile.getAbsolutePath(), jPackage);
        }
        
		// adjust options here 
		sgen.setDestDir(javaOutputDir.getAbsolutePath());
		sgen.setLineSeparator(System.getProperty("line.separator"));
		sgen.setSuppressNonFatalWarnings(true);
		sgen.setVerbose(false);
		sgen.setDescriptorCreation(true);
		sgen.setCreateMarshalMethods(true);
		sgen.setTestable(false);
		
		// adjust properties (overwrite setting in org.exolab.castor.builder.castorbuilder.properties)
		sgen.setEqualsMethod(false);			// true: generate equals() 
		sgen.setClassDescFieldNames(false);	 // true: expose class members
		sgen.setPrimitiveWrapper(false);		// true: Integer instead of int (does not use null-check instead of hasXXX though!)
		
		
        // schema files that need code generation
		List<File> schemaFiles = ebc.getPrimarySchemaFiles();
		
		for (File schemaFile : schemaFiles) {
			String schemaPackage = ebc.getJPackageForSchema(schemaFile.getName());
			generate(sgen, schemaFile, schemaPackage); 
		}

		System.out.println("schema compile done!\n");
	}


//    private static boolean isIncludeDir(String includeDirName) {
//        if (includeDirName != null)
//        {
//            File includeDir = new File(includeDirName);
//            if (includeDir.exists() && includeDir.isDirectory() && !s_includeDirs.contains(includeDir))
//            {
//                s_includeDirs.add(includeDir);
//                s_absFilePaths.clear();
//                if (DEBUG)
//                {   
//                    System.out.println("appended include directory " + includeDir.getAbsolutePath());
//                }
//            }
//            else if (DEBUG)
//            {
//                System.out.println("faulty or already existing include directory '" + includeDir.getAbsolutePath() + 
//                    "' not appended.");
//            }
//        }
//        
//    }

    
    
    private void generate(SourceGenerator sgen, File schemaFile, String packageName)
		throws FileNotFoundException 
	{
		if (!(schemaFile.exists() && schemaFile.isFile()))
		{
			throw new FileNotFoundException(
				"unable to open XML schema file " + schemaFile.getAbsolutePath());
		}

		System.out.println(
			"\n-- generating classes for " + schemaFile.getAbsolutePath() + " into " + packageName + " --");
			
		sgen.generateSource(schemaFile.getAbsolutePath(), packageName);
	}
	

}