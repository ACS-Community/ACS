/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) UTFSM - Universidad Tecnica Federico Santa Maria, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga 
 * (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * 
 * */


package cl.utfsm.cdbChecker;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Properties;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xerces.parsers.SAXParser;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Repository;
import org.omg.CORBA.RepositoryHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;



public class CDBChecker {
	
	public  String XMLPath = null;
	public  String XSDPath = null;

	private File tmpDir;
	private SAXParser SP;
	private Properties props = new Properties();
	private Hashtable<String, String> xsd_targetns;
	public static final String IR_CORBALOC = "ACS.repository";
	private String schemaFolder;

	private String targetNamespace;
	
	private String componentsFolder = null;
	private String containersFolder = null;
	private boolean foundErr = false;

	/**
	 * This errorFlag is used to signal from the parser
	 * callbacks that something failed int the validation
	 * of one specific file.
	 * It shall be reset before starting the validation of each file.
	 */
	private boolean errorFlag = false;

	public boolean isErrorFlag() {
		return errorFlag;
	}

	public void setErrorFlag(boolean errorFlag) {
		this.errorFlag = errorFlag;
	}

	/**
	 * This globalErrorFlag is used to keep memory of any failure.
	 * It is never reset and it is set to true whenever there is a failure.
	 * If at the end of all validations it is true, it means that something
	 * failed and therefore we have to return with a failure
	 * error code.
	 */
    private boolean globalErrorFlag = false;
	public boolean isGlobalErrorFlag() {
		return globalErrorFlag;
	}

	public void setGlobalErrorFlag(boolean globalErrorFlag) {
		this.globalErrorFlag = globalErrorFlag;
	}

	private Repository rep= null;

	public Repository getIrRep() {
		return rep;
	}

	// Command line parameter flags
	private boolean verbose       = false;
	private boolean network       = false;
	private boolean checkidl      = false;
	private boolean recursive     = true;

	public boolean isVerbose() {
		return verbose;
	}

	public boolean isCheckIdl() {
		return checkidl;
	}

	/* Filename filters used when recursively collecting files from the paths */
	private FilenameFilter xmlFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.endsWith(".xml");
		}
	};
	private FilenameFilter xsdFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.endsWith(".xsd");
		}
	};
	private FilenameFilter dirFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return new File(dir.getAbsolutePath() + File.separator + name).isDirectory();
		}
	};

	/**
	 * This get the filenames of type 'type' from the given path. 
	 * There could be several paths separated by ":". 
	 * 
	 * @param path multiple paths separated by ":" to look for 'type' files.
	 * @param type type of files to get.
	 * @return a vector of strings with the filenames of type 'type' with absolute path.
	 *         An empty vector is returned if paths is empty.
	 */		
	protected Vector<String> getFilenames(String paths[],String type){
		Vector<String> vector = new Vector<String>();
		
		String files[];
		
		/*
		 * Scans the list of paths.
		 */

		for(int i=0;i<paths.length;i++)
		    {
		    if(paths[i].length() != 0) 
			{
			File file = new File(paths[i]);
			if(file.exists())
			    {
			    if(file.isFile())
				{ //Is a File
				if (paths[i].endsWith("."+type))
				    vector.add(paths[i]);
				} 
			    else
				{ //Is a directory
				if(!(paths[i].endsWith(File.separator))) 
				    paths[i]=paths[i]+File.separator;
			    
				//Search for files, filtering for 'type'
				if(type.equals("xml"))   files = (new File(paths[i])).list(xmlFileFilter);
				else files = (new File(paths[i])).list(xsdFileFilter);
				
				//Add the files to the vector.
				if (files!=null)
				    for (int j=0; j < files.length; j++)
					vector.addElement(paths[i]+files[j]);
			    
				if(this.recursive)
				    {
				    String[] dirs = (new File(paths[i])).list(dirFileFilter);
				    if(dirs.length != 0)
					for (int j=0; j < dirs.length; j++){
					dirs[j]=paths[i]+dirs[j]+File.separator;
					}
				    vector.addAll(getFilenames(dirs,type));
				    }
				}
			    }
			}
		    }
		return vector;
	}

	/**
	 * This method validates the file encoding of XSD and XML files.
	 * 
	 * @param File of an XML or XSD file to validate.
	 */
	protected void validateFileEncoding(String filename) throws IOException {
		File file = new File(filename);
		FileInputStream fis;
		fis = new FileInputStream(file);
		//BufferedReader in = new BufferedReader(new InputStreamReader(fis,"ASCII"));
		int ch;
		int line = 1;
		int i = 0;
		boolean crEnding = false;
		while((ch = fis.read())!=-1) {
			i++;
			//There shouldn't be any character over 126 since 127 is <del> and ASCII only goes to 127.
			if(ch >= 127) {
				System.out.print(filename+": [Error] Non-ASCII Character "+ch+" found in XML or XSD at character: "+line+":"+i+".\n");
				errorFlag=true;
				globalErrorFlag=true;
			}
			//There shouldn't be any control character but the line feed or tab.
			if(ch < 32 && ch != 10 && ch != 9) {
				if(ch == 13) {
					if(!crEnding) {
						System.out.print(filename+": [Error] Carriage Return Character ("+ch+") found in XML or XSD at: "+line+":"+i+".\n");
						System.out.print(filename+": This is probably CRLF Windows termination. Further Carriage Return errors are hidden.\n");
						crEnding = true;
					}
				}
				else
					System.out.print(filename+": [Error] Illegal Control Character ("+ch+") found in XML or XSD at: "+line+":"+i+".\n");
				errorFlag=true;
				globalErrorFlag=true;
			}
			if(ch == 10){
				i = 0;
				line++;
			}
		}
	}
	
	/**
	 * This method validates the XSD files.
	 * 
	 * @param filename name with absolute path of the XSD file to validate.
	 */
	protected void XSDValidate(Vector<String> filename){

		System.out.println("*** Will verify XSD files in directory: " + this.XSDPath);

		for(int i=0;i<filename.size();i++){
			File file = new File(filename.get(i));
			if(file.length()!=0){
				if(verbose){
					System.out.print("    " + filename.get(i));
					/*for(int j=0;j<(91-(int)((String)filename.get(i)).length())/8;j++)
                                          System.out.print("\t");
					 */}
				try{
					validateFileEncoding(filename.get(i));
					SP.reset();
					SP.setEntityResolver(new CDBSchemasResolver(this, schemaFolder+File.pathSeparator+XSDPath));
					SP.setFeature("http://xml.org/sax/features/validation",true);
					SP.setFeature("http://apache.org/xml/features/validation/schema",true);
					SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
					SP.setFeature("http://xml.org/sax/features/namespaces",true);
					SP.setErrorHandler(new CDBErrorHandler(this));	
					SP.setProperty("http://apache.org/xml/properties/schema/external-schemaLocation","http://www.w3.org/2001/XMLSchema http://www.w3.org/2001/XMLSchema.xsd");

					FileInputStream fis = new FileInputStream(file);
					InputSource inputSource = new InputSource(fis);
					inputSource.setSystemId("file:///" + file.getAbsolutePath());
					SP.parse(inputSource);
					fis.close();
					if(verbose && !errorFlag)
						System.out.println("[OK]");
				}catch (SAXException e){e.printStackTrace();}
				catch (IOException e){System.out.println("[IOException] Probably "+ filename.get(i)+" doesn't exists.");}
			}else{
				System.out.print( filename.get(i)+": [Warning] file is empty.\n");
			}
		}	
	}

	/**
	 * This method check if the idl types on CDB are available
	 * 
	 */

	protected void checkIdlTypes(){
		//first check if IR is available
		org.omg.CORBA.Object repRef = null;
		ORB orb = org.omg.CORBA.ORB.init(new String[0], null);	
		String IRloc = props.getProperty(IR_CORBALOC);
		try{
			repRef = orb.string_to_object(IRloc);
		}catch(Exception e){
			System.out.println("[Error] - Interface repository is not running, no check will be done.");
		}
		rep = RepositoryHelper.narrow(repRef);

		//iterate trough all idlTypes
		//done in parseElement
			

	}
	
	/**
	 * This method validates the XML files.
	 * 
	 * @param filenames name with absolute path of the XML file to validate.
	 */	
	protected void XMLValidate(Vector<String> filenames){

		System.out.println("*** Will verify XML files in directory: " + this.XMLPath);

		for(int i=0;i<filenames.size();i++){

			File file = new File(filenames.get(i));
			if(file.length()!=0){
				if(verbose){
					System.out.print("    "+ filenames.get(i));
					/*for(int j=0;j<(90-(int)((String)filename.get(i)).length())/8;j++)
                                          System.out.print("\t");
					 */}
				String targetNamespace;
				targetNamespace = ((xsd_targetns.toString()).replace(',',' ')).replace('=',' ').replace('{',' ').replace('}',' ');
				errorFlag=false;
				try{
					validateFileEncoding(filenames.get(i));

					SP.reset();
					SP.setFeature("http://xml.org/sax/features/validation",true);
					SP.setFeature("http://apache.org/xml/features/validation/schema", true);
					SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
					SP.setFeature("http://xml.org/sax/features/namespaces",true);
					SP.setErrorHandler(new CDBErrorHandler(this));
					SP.setProperty("http://apache.org/xml/properties/schema/external-schemaLocation",targetNamespace);

					FileInputStream fis = new FileInputStream(file);
					InputSource inputSource = new InputSource(fis);
					inputSource.setSystemId("file:///" + file.getAbsolutePath());
					SP.parse(inputSource);
					fis.close();
					if(verbose && !errorFlag)
						System.out.println("[OK]");
				}catch(SAXException e){System.out.println("[SAXException] " + e.getMessage());}
				catch(IOException e){System.out.println("[IOException] Probably "+ filenames.get(i) + " doesn't exists.");}
			}else{
				System.out.print( filenames.get(i) + ": [Warning] file is empty.\n");
			}

		}
	}
	
	
	/**
	 * This method checks for the targetNamespace defined by the schema files and fills the CDBChecker.xsd_targetns with pairs {targetNamespace, XSD filename}
	 * 
	 * @param XSDFilenames Vector with all the XSD filenames with absolute path.
	 */	
	protected void getTargetNamespace(Vector<String> XSDFilenames){

		String filename;

		for(int i=0;i<XSDFilenames.size();i++){

			filename= XSDFilenames.get(i);
			File file = new File(XSDFilenames.get(i));
			if(file.length()!=0){
				SP.setContentHandler(new CDBContentHandler(this));
				SP.reset();
				try{
					SP.setFeature("http://xml.org/sax/features/validation",false);
					SP.setFeature("http://apache.org/xml/features/validation/schema",false);
					SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
					SP.setFeature("http://xml.org/sax/features/namespaces",true);
					SP.setErrorHandler(new CDBErrorHandler(this));	

					FileInputStream fis = new FileInputStream(file);
					InputSource inputSource = new InputSource(fis);
					inputSource.setSystemId("file:///" + file.getAbsolutePath());
					SP.parse(inputSource);
					fis.close();
				} catch(SAXException e){e.getMessage();}
				catch (IOException e){System.out.println("[IOException] Probably "+filename+" doesn't exists.");}

				if(targetNamespace!=null)
				{
					/* GCH
					 * If the targetNamespace has been already registered,
					 * I skip registering it again.
					 * In this way I give priority to definitions that come first.
					 * Since the search order depends on the order int the ACS.cdbPath
					 * property, standard definitions can be overwritten byte newer ones in
					 * the standard search path algorithm.
					 */
					if(xsd_targetns.containsKey(targetNamespace))
					{
						/*
						 * If the same targetNamespace appears int files with
						 * the same name, then we are simply overriding the
						 * default version with a new one and we need to warning.
						 * Otherwise, a warning can be useful to discover
						 * inconsistencies.
						 */
						String[] newArr = filename.split("/");
						String[] oldArr = ((String)xsd_targetns.get(targetNamespace)).split("/");
						if(newArr[newArr.length-1].compareTo(oldArr[oldArr.length-1])!=0)
						{
							System.out.println("[Warning] The XSD files \""+ XSDFilenames.get(i) + "\" and \""+xsd_targetns.get(targetNamespace)+"\" have same targetNamespace: \""+targetNamespace+"\". Skipping this one.");
						}
					}
					else
					{
						xsd_targetns.put(targetNamespace, "file:///" + XSDFilenames.get(i));
					}
				}
			}else{
				System.out.print( XSDFilenames.get(i) + ": [Warning] file is empty.\n");
			}
		}
	}
	
	/**
	 * Sets the static variable CDBChecker.targetNamespace
	 * @param targetNamespace 
	 */
	public void setTargetNamespaceString(String targetNamespace){
		this.targetNamespace = targetNamespace;
	}
	
	/**
	 * Downloads the file from the given URL. Creates the temporary directory directory if it doesn't already exists.
	 * Only downloads the file if it doesn't already exists.
	 * @param url where to download the file from.
	 */
	private void getFile(String url){
		String myFile = new String();
		try{
			String[] arr = url.split("/");
			myFile=schemaFolder+arr[arr.length-1];
			File file = new File(myFile);
			URL remFile = new URL(url);
			String cad;
			BufferedReader filePage = new BufferedReader(new InputStreamReader( remFile.openStream() ));
			FileWriter output = new FileWriter(file);
			
			while ((cad = filePage.readLine()) != null) {
				output.write(cad+"\n");
			}
			output.flush();
			output.close();
		} catch( MalformedURLException e ){e.printStackTrace();}
		catch( IOException e) {
			System.out.println("[IOexception] Probably "+myFile+" couldn't be written.");
			e.printStackTrace();
		}		
	}
	
	public void cleanUp()
	{
		if(tmpDir!=null) {
			if(verbose) { System.out.println("*** Deleting Temporary files"); }
			deleteTmp();
		}
	}

	/**
	 * Calls CDBChecker.getFile() to download files usually needed by XSD schema files.
	 * @param reqSchemas Vector that contains the required schemas, to be downloaded.
	 */
	public void downloadSchemas(List<String> reqSchemas){
		System.out.print("*** Downloading remote schemas");
		if(verbose) {
			System.out.print("\n*** Storing schemas in: " + schemaFolder);
		}
		for(int i=0;i<reqSchemas.size();i++){
			String fileToGet = reqSchemas.get(i);
			getFile(fileToGet);
			if(verbose) {
				System.out.print("\n\tDownloaded file: " + fileToGet);
			}
		}
		System.out.println("\n*** Remote schemas succesfully downloaded");
	}
	
	/**
	 * Prints usage information.
	 *
	 */
	protected static void printUsage(){
		System.out.println("\n[usage:]\n\n    cdbChecker [-flags] [XMLPath] [XSDPath]");
		System.out.println("\n\n    Flags:\n");
		System.out.println("      -v | --verbose        Verbose output");
		System.out.println("      -r | --recursive      Disable recursive traversal of XMLPath and XSDPath");
		System.out.println("                            when searching for .xml/.xsd files");
		System.out.println("      -n | --network        Get required schemas from the network");
		System.out.println("      -c | --checkIdlTypes  Check if the idl types in CDB are available");
		System.out.println("      -h | --help           Show this help");
		System.out.println("\n    The XMLPath and XSDPath can have multiple paths separated by \":\".");
		System.out.println("    The paths must be absolute (i.e. they should start with '/')");
		System.out.println("    The checker will search for files recursively inside the given paths,");
		System.out.println("    unless the -r flag is specified.\n");
		System.out.println("    NOTE: 1) the value passed in as the XSDPath will be pre-pended to the");
		System.out.println("    value from the ACS.cdbPath property; 2) if not specified, the XMLPath");
		System.out.println("    will default to $ACS_CDB/CDB (if ACS_CDB environment variable is set).\n");
   		System.out.println("    ACS_CDB is used if XMLPath is not given"); 
	}

	/**
	 * Checks the command line arguments given to the program and capture the given flags.
	 * @param args command line arguments
	 * @return True if arguments are OK, false otherwise 
	 */
	protected boolean checkArgs(String[] args)
	{
		boolean retVal = true;
		int c;
		LongOpt[] longopts = new LongOpt[5];
		longopts[0] = new LongOpt("help", LongOpt.NO_ARGUMENT, null, 'h');
		longopts[1] = new LongOpt("network", LongOpt.NO_ARGUMENT, null, 'n'); 
		longopts[2] = new LongOpt("verbose", LongOpt.NO_ARGUMENT, null, 'v');
		longopts[3] = new LongOpt("recursive", LongOpt.NO_ARGUMENT, null, 'r');
		longopts[4] = new LongOpt("checkIdlTypes", LongOpt.NO_ARGUMENT, null, 'c');
		
		Getopt myGetOpt = new Getopt("cdbChecker", args, "rhncvaW;", longopts);
		myGetOpt.setOpterr(false); // We'll do our own error handling

		while ((c = myGetOpt.getopt()) != -1) {
			switch (c) {
				case 'n':
					this.network = true;
					break;
				case 'r':
					this.recursive = false;
					break;
				case 'v':
					this.verbose = true;
					break;
				case 'h':
					retVal = false;
					break;
				case 'c':
					this.checkidl = true;
					break;
				case 'W':
					System.out.println("[Error] : you tried a -W with an incorrect long option name");
					globalErrorFlag = true;
					retVal = false;
					break;
				case '?':
					if(0 == myGetOpt.getOptopt()) {
						System.out.println("[Error] : the long option '" + args[myGetOpt.getOptind() - 1] + "' is not valid");
					}
					else {
						System.out.println("[Error] : the option '" + (char)myGetOpt.getOptopt() + "' is not valid");
					}
					globalErrorFlag = true;
					retVal = false;
					break;
				default:
					globalErrorFlag = true;
					retVal = false;
					break;
			}
		}

		// do the following only if we aren't already needing to pretVal is not false)
		if(retVal) {
			// check for the additional (optional) command line arguments, XMLPath and XSDPath
			for (int i = myGetOpt.getOptind(); i < args.length ; i++) {
				if(myGetOpt.getOptind() == i) {
					if(args[i].startsWith("/") || args[i].matches("[a-zA-Z]:.*")) {
						this.XMLPath = args[i];
					}
					else {
						System.out.println("[Error] : XMLPath must start with '/'");
						globalErrorFlag = true;
						retVal = false;
					}
				}
				else {
					if(args[i].startsWith("/") || args[i].matches("[a-zA-Z]:.*")) {
						this.XSDPath = args[i];
					}
					else {
						System.out.println("[Error] : XSDPath must start with '/'");
						globalErrorFlag = true;
						retVal = false;
					}
					break;
				}
			}

			// finally, if XMLPath wasn't specified, use a sensible default

			if(retVal && null == this.XMLPath) {
      				String acsCdbPath = System.getenv("ACS_CDB");
				if(null != acsCdbPath)
				{
					acsCdbPath += File.separator+"CDB";
					System.out.println("*** XML path not specified; defaulting to $ACS_CDB"+File.separator+"CDB: " + acsCdbPath);
					XMLPath = acsCdbPath;
				}
				else
				{
					System.out.println("\n[Error] XML path not specified and $ACS_CDB environment variable not set; no default possible.");
					globalErrorFlag = true;
					retVal = false;
				}
			}
		}	

		return retVal;
	}
	
   private boolean configLoader(){
      String config_path;
      String tmp_path;

      List<String> reqSchemas = new ArrayList<String>();
      
      if((config_path = props.getProperty("ACS.config_path"))==null){
	 System.out.println("config_path not defined");
	 return false;
      }
      
      //Use the default ACS_TMP directory to download the schemas from the network
      tmp_path = System.getProperty("ACS.tmp");
      // else use the systems default
      if (tmp_path == null)
		{
			tmp_path = File.separator+"tmp";
		}
      
      
      SP.setContentHandler(new ConfigurationCH(reqSchemas));
      SP.reset();
      try{
	 SP.setFeature("http://xml.org/sax/features/validation",false);
	 SP.setFeature("http://apache.org/xml/features/validation/schema",false);
	 SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
	 SP.setFeature("http://xml.org/sax/features/namespaces",true);
	 SP.parse(config_path+File.separator+"config"+File.separator+"reqSchemas.xml");
      }catch(SAXException e){e.getMessage();}
      catch (IOException e){
	 System.out.println("[IOException] Probably the configuration file doesn't exist.");return false;
      }
      if(this.network){
	 tmpDir=new File(tmp_path, "cdbchecker." + System.currentTimeMillis () +
			 ".tmp");
	 tmpDir.mkdirs();
	 if (!tmpDir.exists ()) {
	    System.out.println("[Error] No permission to create temporary directory " + tmpDir.getAbsolutePath());
	    return false;
	 } 
	 schemaFolder=tmpDir.getAbsolutePath()+File.separator;
	 downloadSchemas(reqSchemas);
      }
      else{
	 schemaFolder=config_path+File.separator+"config"+File.separator+"CDB"+File.separator+"schemas"+File.separator;
	 if(!(new File(schemaFolder)).exists()){
	    System.out.println("[Error] The required schema files are missing, please run the tool with the '-n' option.");
	    return false;
	 }
      }
      return true;
   }
	
	
	protected void deleteTmp(){
		String list[] = tmpDir.list();
		for(int i=0;i<list.length;i++)
			(new File(tmpDir.getAbsolutePath()+File.separator+list[i])).delete();
		tmpDir.delete();
	}
	
    /**
     * Main function to run the cdbChecker tool
     * System.exit(0/1) is used to return success if everything if fine
     * or failure int case errors were encountered
     */

	public static void main(String[] args) {
		
		CDBChecker cdbchecker=new CDBChecker();
		cdbchecker.props = System.getProperties();
		
		
		/* Blank lines to see the output clearly */
		System.out.println("\n\n");

		/*
		 * Retrieves the CDB path from the propery, is given
		 */
		String ACS_cdbpath = cdbchecker.props.getProperty("ACS.cdbpath");

                /* 
                 * Check for Paths and flags received from command line
		 * and sets accordiningly member variables.
		 * These non explicit side effects should be avoided.
		 */
       
       if(cdbchecker.checkArgs(args)) {
       		//add panta@naoj 2009/10/05
       		String pathsMulti[]=cdbchecker.XMLPath.split(File.pathSeparator);
       	
       		for(int i = 0; i < pathsMulti.length; i++){
				File file_ = new File(pathsMulti[i]);
				if(!file_.exists()){
					System.out.println("*** ImplLang Check: Specified path " + file_+ " does not exist");
					cdbchecker.setGlobalErrorFlag(true);
					cdbchecker.showEndResult();
					break;
				}
			}
       		//add panta@naoj 2009/10/05 end

			//Creating the parser
//			System.setProperty("org.apache.xerces.xni.parser.XMLParserConfiguration", "org.apache.xerces.parsers.XIncludeAwareParserConfiguration");
			cdbchecker.SP=new SAXParser();
			cdbchecker.xsd_targetns=new Hashtable<String,String>();
			
			//Download the required Schemas
			if(cdbchecker.verbose)System.out.println("*** Reading required schema files");			
			if(cdbchecker.configLoader()){
			
			        /*
				 * Retrieves all schema files with absolute paths
			         */
 			        if(cdbchecker.verbose)System.out.println("*** Reading given schema files");
		                // Appends command line schema files, if any
				if(ACS_cdbpath != null)
				    {
				    // We assume that cdbchecker.XSDPath is at least
                                    // initialised to the empty string and never null
				    	//Modify panta@naoj 2009/10/15
				    	if(cdbchecker.XSDPath == null){
				    		cdbchecker.XSDPath = ACS_cdbpath;
				    	}
				    	else{
				    		cdbchecker.XSDPath = cdbchecker.XSDPath + ":" + ACS_cdbpath;
				    	}
				    }
				if(cdbchecker.verbose && cdbchecker.checkidl){
					System.out.println("*** Checking Idl Types");
				}
				if (cdbchecker.checkidl)
					cdbchecker.checkIdlTypes();

				String paths[]=cdbchecker.XSDPath.split(File.pathSeparator);
				Vector<String> XSDFilenames=new Vector<String>();
				XSDFilenames=cdbchecker.getFilenames(paths,"xsd");
			
				if(cdbchecker.verbose)System.out.println("*** Reading given XML files");			
				// We assume that cdbchecker.XMLPath is at least
				// initialised to the empty string and never null
		
				paths=cdbchecker.XMLPath.split(File.pathSeparator);
				Vector<String> XMLFilenames=new Vector<String>();
				XMLFilenames=cdbchecker.getFilenames(paths,"xml");
				
				//Fill the map with the targetNamespace and the filenames
				if(cdbchecker.verbose)System.out.println("*** Getting TargetNamespaces from schema files");			
				cdbchecker.getTargetNamespace(XSDFilenames);
			
				//Validating Schemas
				if(cdbchecker.verbose)System.out.println("*** Validating Schemas");			
				cdbchecker.XSDValidate(XSDFilenames);
				
				//Validating XML files
				if(cdbchecker.verbose)System.out.println("*** Validating XML files");						
				cdbchecker.XMLValidate(XMLFilenames);
				
				//add panta@naoj 2009/10/05
				//checks if implLang matches, those written in XXComponents.xml and XXContainers.xml
				
				for(int i = 0; i < pathsMulti.length; i++){
					cdbchecker.componentsFolder= pathsMulti[i] + File.separator+"MACI"+File.separator+"Components";
					cdbchecker.containersFolder= pathsMulti[i] + File.separator+"MACI"+File.separator+"Containers";
				
					File compFolder = new File(cdbchecker.componentsFolder);
					File contFolder = new File(cdbchecker.containersFolder);
					//System.out.println("compFolder: " + compFolder);
					//System.out.println("contFolder: " + contFolder);
					
					if(compFolder.exists() && contFolder.exists()){
						cdbchecker.setGlobalErrorFlag(cdbchecker.checkImplLangMatch(compFolder, contFolder));

						//exit if error
						if( cdbchecker.isGlobalErrorFlag() ) {
						    break;
						}
					}
				}
				//add panta@naoj 2009/10/05 end
				
			}
		} 
      else {
			printUsage();
		}

		cdbchecker.cleanUp();
		
		cdbchecker.showEndResult();
		
	}
	
	private void showEndResult(){
		if(globalErrorFlag==true) {
		    System.out.println("\n[Error] CDBChecker exiting. Errors were found\n");
		    System.exit(1);
		}
		else {
		    System.out.println("\nCDBChecker exiting. No errors found\n");
		    System.exit(0);
		}
	}
	
	/******************************************************************
	 * This method finds files in "Components" and "Containers" 
	 * directories and sub-directories. It then extracts "implLang" 
	 * properties, and compares. Error messages are displayed if
	 * Components.xml's implLang and Containers.xml's implLang
	 * don't match.
	 * Returns 'true' if error is found, false otherwise
	 * added by panta@naoj 2009/10/05 
	 *****************************************************************/
	protected boolean checkImplLangMatch(File compFolder, File contFolder){ 
		
		File[] files = compFolder.listFiles();
	 
		search:
	  	for (int x = 0; x < files.length; x++){
	  		
	  		if(foundErr){
  				break search;
  			}

	  		if (files[x].isDirectory()){
	    		if(!files[x].getName().equals("CVS")){
	    			checkImplLangMatch(files[x], contFolder); //recursive call
	    		}
	    	}
	    	else{
	    		//only process .xml files
	   			String ext = "";
	    		
	    		int iExt = files[x].getName().lastIndexOf(".");
	   			ext=files[x].getName().substring(iExt+1,files[x].getName().length());
	    		
    			if(!ext.equals("xml")){
    				continue;
    			}
	    		//System.out.println("\nChecking.. " + files[x]);
		    	DocumentBuilderFactory dbfComp = DocumentBuilderFactory.newInstance();
				DocumentBuilder dbComp = null;
				try {
					dbComp = dbfComp.newDocumentBuilder();
				} catch (ParserConfigurationException e) {
					e.printStackTrace();
				}	
				Document docComp = null;
				try {
					docComp = dbComp.parse(files[x]);
				} catch (SAXException e) {
				e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
				
	    		docComp.getDocumentElement().normalize();
				NodeList compNodeList = docComp.getElementsByTagName( "Component" ); 
				
				//modify bhola.panta@naoj 2010-03-15
				if(compNodeList.getLength() == 1) { //only the variant where a single component is configured in its own file
					String compName = ((Element) compNodeList.item(0)).getAttribute("Name" );
					//add bhola.panta@naoj 2010-03-03
	    			//fileName and "Name" property must match
					if(!compName.equals("*") && !files[x].getName().equals("Components.xml")){ 
						//must support hierarchical component names (separated by "/") 
						if(!checkHierarchical(files[x], compName)){
		    				if(!(compName + ".xml").equals(files[x].getName())){
		    					System.out.print("\nMismatch between component name and XML file name.");
		    					System.out.print("\nComponent File: " + files[x]);
		    					System.out.print("\nComponent name: '" + compName +"'");
		    					System.out.print("\nFile name: '" + files[x].getName()+"'");
		    					foundErr = true;
		    					break search;
		    				}
						}
	    			}
				}
				
	    	
	    		if(compNodeList.getLength() == 0) {
	    			compNodeList = docComp.getElementsByTagName( "_" );
	    			if(compNodeList.getLength() == 0) {
	    				continue;
	    			}
	    		}
	    	
		    	//this part extracts "implLang" for each component
	    		for( int j = 0; j < compNodeList.getLength(); j++ ) {
		      
		      		Element elm = (Element) compNodeList.item( j );
		      		String compName = null;
		      		String implLang = null;
		      		String tempContainersFolder = null;
		      
		      		compName = elm.getAttribute("Name" );
	    			implLang = elm.getAttribute("ImplLang" );
					//System.out.println("\ncompName being checked: " + compName );
 	    
	    			if(compName.equals("*")){ //--> dynamic component
	    	  			if(implLang.equals("") || implLang.equals("*")){
				  			continue;
			  			}
	    	  			//some dynamic components may not have predefined Containers
	    	  			if(elm.getAttribute("Container" ).equals("") || elm.getAttribute("Container" ).equals("*")){
		    				continue;
		    			}
	    	  		}
                    //add bhola.panta@naoj 2011/07/21
                    //component does have a name, but container is dynamic (?), that is, "*"
					else if(elm.getAttribute("Container" ).equals("*")){ 
						continue; 
					}
	    			else{//actually, ImpLang field in the CDB is mandatory since ACS 8
	    				if(implLang.equals("")){
	    					System.out.println("\nFile being checked: " + files[x] );
				  			System.out.print("\n'ImplLang' missing for component: " + compName);
	    					foundErr = true;
	    					break search;
	              		}
	    			}

		 			//go get containers at the "Container" location
		  			tempContainersFolder = containersFolder + File.separator + elm.getAttribute("Container" );
	    			
		      		//open the container file and have a look
		  			DocumentBuilderFactory dbfCont = DocumentBuilderFactory.newInstance();
		  			DocumentBuilder dbCont = null;
		  			try {
		  				dbCont = dbfCont.newDocumentBuilder();
		  			} catch (ParserConfigurationException e) {
		  				e.printStackTrace();
		  			}
		  			Document docCont = null;
		  			try {
		  				//System.out.println("\ntempContainersFolder " + tempContainersFolder);
		  				File contFile = new File(tempContainersFolder + File.separator + new File(tempContainersFolder).getName()+ ".xml");
						//System.out.println("\ncontainerFile " + contFile);
		  				if(contFile.exists()){
		  					docCont = dbCont.parse(contFile);
		  					
		  					docCont.getDocumentElement().normalize();
		  					
				  			NodeList contNodeList = docCont.getElementsByTagName( "Container" );
			  	  		
				  			//Go through Container files and check ImplLang
				  			for( int k = 0; k < contNodeList.getLength(); k++ ) {
				  				Element elmCont = (Element) contNodeList.item( k );
				  				//check if Component ImplLang and Container ImplLang match
				  				if(implLang.equals(elmCont.getAttribute("ImplLang" ))){
				  				}
				  				else{
				  					System.out.println("\nComponent File being checked: " + files[x] );
				  					System.out.println("Container File being checked: " + contFile);
				  					System.out.println("'ImplLang' does not match for component: " + compName +".");
				  					foundErr = true;
				  					break search;
				      			}
					    	}//Container for loop
		  				}
		  				else{
		  					System.out.print("\nComponent File being checked: " + files[x] );
		  					System.out.print("\nMissing Container " + new File(tempContainersFolder));
		  					System.out.println("");
		  					foundErr = true;
		  					break search;
		  				}
		  			} catch (SAXException e) {
		  				e.printStackTrace();
		  			} catch (IOException e) {
		  				e.printStackTrace();
		  			}
	    		}//Component for loop
	   		}//is a file (not dir)
	  	}//all files for loop
		
		return foundErr;
	}
	
	/**************************************************************************************************************************
	* Add bhola.panta@naoj 2010/05/12, in order to support hierarchical component names (separated by "/")
	* I am quoting a comment from Heiko Sommer, ref. #COMP-4247
	* We must support hierarchical component names (separated by "/"), where the path of the xml 
	* file should match the component name.
	* eg. File path: /alma/ACS-9.0/acsdata/config/defaultCDB/CDB/MACI/Components/ARCHIVE/TMCDB/MONITOR_BLOBBER2/MONITOR_BLOBBER2.xml
	* Component name: 'ARCHIVE/TMCDB/MONITOR_BLOBBER2'
	* File name: 'MONITOR_BLOBBER2.xml'
	* Under "CDB/MACI/Components", there is the path "ARCHIVE/TMCDB/MONITOR_BLOBBER2", 
	* which does match the component name. 
	* (It's a convention of the CDB that for hierarchical components, but also for other nodes, 
	* the xml file name and the directory name where the xml file is in, are the same 
	* except for the .xml ending. For matching the component name we count it only once, 
	* instead of taking "ARCHIVE/TMCDB/MONITOR_BLOBBER2/MONITOR_BLOBBER2".)
	* Credit: http://java.sun.com/docs/books/tutorial/java/data/comparestrings.html
	*****************************************************************************************************************************/
	private boolean checkHierarchical(File fSearchMe, String findMe){
		String searchMe = fSearchMe.getPath();
		int searchMeLength = searchMe.length();
		int findMeLength = findMe.length();
		boolean foundIt = false;
		for (int i = 0; i <= (searchMeLength - findMeLength); i++) {
		   if (searchMe.regionMatches(i, findMe, 0, findMeLength)) {
		      foundIt = true;
		      break;	
		   }
		}
		return foundIt;
	}

}//class
