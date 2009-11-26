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
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xerces.parsers.SAXParser;
import org.omg.CORBA.ORB;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import org.omg.CORBA.Repository;
import org.omg.CORBA.RepositoryHelper;



public class CDBChecker {
	
	public  String XMLPath = null;
	public  String XSDPath = null;

	private File tmpDir;
	private SAXParser SP;
	private Properties props = new Properties();
	private Hashtable xsd_targetns;
	public static final String IR_CORBALOC = "ACS.repository";
	private String schemaFolder;

	private static String targetNamespace;
	public  static Vector reqSchemas;
	
	private String componentsFolder = null;
	private String containersFolder = null;
	
	private boolean foundErr = false;

  /**
	 * This errorFlag is used to signal from the parser
	 * callbacks that something failed int the validation
	 * of one specific file.
	 * It shall be reset before starting the validation of each file.
	 */
	public  static boolean errorFlag       = false;

	/**
	 * This globalErrorFlag is used to keep memory of any failure.
	 * It is never reset and it is set to true whenever there is a failure.
	 * If at the end of all validations it is true, it means that something
	 * failed and therefore we have to return with a failure
	 * error code.
	 */
    public  static boolean globalErrorFlag = false;
	public static Repository rep= null;

	// Command line parameter flags
	public  static  boolean verbose       = false;
	private         boolean network       = false;
	public  static 	boolean checkidl      = false;
	private         boolean recursive     = true;
	public  static 	boolean allImplLang   = false;
	
	/**
	 * This get the filenames of type 'type' from the given path. 
	 * There could be several paths separated by ":". 
	 * 
	 * @param path multiple paths separated by ":" to look for 'type' files.
	 * @param type type of files to get.
	 * @return a vector of strings with the filenames of type 'type' with absolute path.
	 *         An empty vector is returned if paths is empty.
	 */		
	protected Vector getFilenames(String paths[],String type){
		Vector vector = new Vector();
		
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
				if(!(paths[i].endsWith("/"))) 
				    paths[i]=paths[i]+"/";
			    
				//Search for files, filtering for 'type'
				if(type.equals("xml"))   files = (new File(paths[i])).list(new XMLFilter());
				else files = (new File(paths[i])).list(new XSDFilter());
				
				//Add the files to the vector.
				if (files!=null)
				    for (int j=0; j < files.length; j++)
					vector.addElement(paths[i]+files[j]);
			    
				if(this.recursive)
				    {
				    String[] dirs = (new File(paths[i])).list(new DirFilter());
				    if(dirs.length != 0)
					for (int j=0; j < dirs.length; j++){
					dirs[j]=paths[i]+dirs[j]+"/";
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
	 * This method validates the XSD files.
	 * 
	 * @param filename name with absolute path of the XSD file to validate.
	 */
	protected void XSDValidate(Vector filename){
		System.out.println("*** Will verify XSD files in directory: " + this.XSDPath);
		for(int i=0;i<filename.size();i++){
                        File file = new File((String)filename.get(i));
                        if(file.length()!=0){
                                if(verbose){
                                        System.out.print("    "+(String)filename.get(i));
                                        /*for(int j=0;j<(91-(int)((String)filename.get(i)).length())/8;j++)
                                          System.out.print("\t");
                                         */}
                                try{
                                        SP.reset();
                                        SP.setEntityResolver(new CDBSchemasResolver(schemaFolder+":"+XSDPath));
                                        SP.setFeature("http://xml.org/sax/features/validation",true);
                                        SP.setFeature("http://apache.org/xml/features/validation/schema",true);
                                        SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
                                        SP.setFeature("http://xml.org/sax/features/namespaces",true);
                                        SP.setErrorHandler(new CDBErrorHandler());	
                                        SP.setProperty("http://apache.org/xml/properties/schema/external-schemaLocation","http://www.w3.org/2001/XMLSchema http://www.w3.org/2001/XMLSchema.xsd");
                                	    SP.parse((String)filename.get(i));
                                        if(verbose && !errorFlag)
                                                System.out.println("[OK]");
                                }catch (SAXException e){e.printStackTrace();}
                                catch (IOException e){System.out.println("[IOException] Probably "+(String)filename.get(i)+" doesn't exists.");}
                        }else{
                                System.out.print((String)filename.get(i)+": [Warning] file is empty.\n");
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
	 * @param filename name with absolute path of the XML file to validate.
	 */	
	protected void XMLValidate(Vector filename){
		System.out.println("*** Will verify XML files in directory: " + this.XMLPath);
		for(int i=0;i<filename.size();i++){
                        File file = new File((String)filename.get(i));
                        if(file.length()!=0){
                                if(verbose){
                                        System.out.print("    "+(String)filename.get(i));
                                        /*for(int j=0;j<(90-(int)((String)filename.get(i)).length())/8;j++)
                                          System.out.print("\t");
                                         */}
                                String targetNamespace;
                                targetNamespace = ((xsd_targetns.toString()).replace(',',' ')).replace('=',' ').replace('{',' ').replace('}',' ');
                                CDBChecker.errorFlag=false;
                                try{
                                        SP.reset();
                                        SP.setFeature("http://xml.org/sax/features/validation",true);
                                        SP.setFeature("http://apache.org/xml/features/validation/schema", true);
                                        SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
                                        SP.setFeature("http://xml.org/sax/features/namespaces",true);
                                        SP.setErrorHandler(new CDBErrorHandler());
                                        SP.setProperty("http://apache.org/xml/properties/schema/external-schemaLocation",targetNamespace);
                                        SP.parse((String)filename.get(i));
                                        if(verbose && !errorFlag)
                                                System.out.println("[OK]");
                                }catch(SAXException e){e.getMessage();}
                                catch(IOException e){System.out.println("[IOException] Probably "+(String)filename.get(i)+" doesn't exists.");}
                        }else{
                                System.out.print((String)filename.get(i)+": [Warning] file is empty.\n");
                        }

		}
	}
	
	
	/**
	 * This method checks for the targetNamespace defined by the schema files and fills the CDBChecker.xsd_targetns with pairs {targetNamespace, XSD filename}
	 * 
	 * @param XSDFilenames Vector with all the XSD filenames with absolute path.
	 */	
	protected void getTargetNamespace(Vector XSDFilenames){
		String filename;
		
		for(int i=0;i<XSDFilenames.size();i++){
                        filename=(String)XSDFilenames.get(i);
                        File file = new File((String)XSDFilenames.get(i));
                        if(file.length()!=0){
                                SP.setContentHandler(new CDBContentHandler());
                                SP.reset();
                                try{
                                        SP.setFeature("http://xml.org/sax/features/validation",false);
                                        SP.setFeature("http://apache.org/xml/features/validation/schema",false);
                                        SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
                                        SP.setFeature("http://xml.org/sax/features/namespaces",true);
                                        SP.setErrorHandler(new CDBErrorHandler());	

                                        SP.parse(filename);
                                }catch(SAXException e){e.getMessage();}
                                catch (IOException e){System.out.println("[IOException] Probably "+filename+" doesn't exists.");}

                                if(targetNamespace!=null)
                                {
                                        /* GCH
                                         * If the targetNamespace has been already registered,
                                         * I skip registering it again.
                                         * In this way I give priority to definitions that come first.
                                         * Since the search order depents on the order int the ACS.cdbPath
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
                                                        System.out.println("[Warning] The XSD files \""+(String)XSDFilenames.get(i)+"\" and \""+xsd_targetns.get(targetNamespace)+"\" have same targetNamespace: \""+targetNamespace+"\". Skipping this one.");
                                                }
                                        }
                                        else
                                        {
                                                xsd_targetns.put(targetNamespace,XSDFilenames.get(i));
                                        }
                                }
                        }else{
                                System.out.print((String)XSDFilenames.get(i)+": [Warning] file is empty.\n");
                        }
		}
	}
	
	/**
	 * Sets the static variable CDBChecker.targetNamespace
	 * @param targetNamespace 
	 */
	public static void setTargetNamespaceString(String targetNamespace){
		CDBChecker.targetNamespace=targetNamespace;
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
	public void downloadSchemas(Vector reqSchemas){
		System.out.print("*** Downloading remote schemas");
		if(verbose) {
			System.out.print("\n*** Storing schemas in: " + schemaFolder);
		}
		for(int i=0;i<reqSchemas.size();i++){
			String fileToGet = (String) reqSchemas.get(i);
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
		System.out.println("      -a | --allImplLang    Check all files for implLang match (default is false, independent of -r)");
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
		String arg;
		LongOpt[] longopts = new LongOpt[6];
		longopts[0] = new LongOpt("help", LongOpt.NO_ARGUMENT, null, 'h');
		longopts[1] = new LongOpt("network", LongOpt.NO_ARGUMENT, null, 'n'); 
		longopts[2] = new LongOpt("verbose", LongOpt.NO_ARGUMENT, null, 'v');
		longopts[3] = new LongOpt("recursive", LongOpt.NO_ARGUMENT, null, 'r');
		longopts[4] = new LongOpt("checkIdlTypes", LongOpt.NO_ARGUMENT, null, 'c');
		longopts[5] = new LongOpt("allImplLang", LongOpt.NO_ARGUMENT, null, 'a');
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
				case 'a':
					this.allImplLang = true;
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
					if(args[i].startsWith("/")) {
						this.XMLPath = args[i];
					}
					else {
						System.out.println("[Error] : XMLPath must start with '/'");
						globalErrorFlag = true;
						retVal = false;
					}
				}
				else {
					if(args[i].startsWith("/")) {
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
					acsCdbPath += "/CDB";
					System.out.println("*** XML path not specified; defaulting to $ACS_CDB/CDB: " + acsCdbPath);
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
      
      if((config_path = props.getProperty("ACS.config_path"))==null){
	 System.out.println("config_path not defined");
	 return false;
      }
      
      //Use the default ACS_TMP directory to download the schemas from the network
      tmp_path = System.getProperty("ACS.tmp");
      // else use the systems default
      if (tmp_path == null)
		{
			tmp_path = File.separatorChar+"tmp";
		}
      
      
      SP.setContentHandler(new ConfigurationCH());
      SP.reset();
      try{
	 SP.setFeature("http://xml.org/sax/features/validation",false);
	 SP.setFeature("http://apache.org/xml/features/validation/schema",false);
	 SP.setFeature("http://xml.org/sax/features/namespace-prefixes",false);
	 SP.setFeature("http://xml.org/sax/features/namespaces",true);
	 SP.parse(config_path+"/config/reqSchemas.xml");
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
	 schemaFolder=tmpDir.getAbsolutePath()+"/";
	 downloadSchemas(reqSchemas);
      }
      else{
	 schemaFolder=config_path+"/config/CDB/schemas/";
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
			(new File(tmpDir.getAbsolutePath()+"/"+list[i])).delete();
		tmpDir.delete();
	}
	
    /**
     * Main function to run the cdbChecker tool
     * System.exit(0/1) is used to return success if everything if fine
     * or failure int case errors were encountered
     */

	public static void main(String[] args) {
		
		CDBChecker cdbchecker=new CDBChecker();
		reqSchemas = new Vector();
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
			//Creating the parser
			cdbchecker.SP=new SAXParser();
			cdbchecker.xsd_targetns=new Hashtable();
			
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
				if(cdbchecker.verbose && checkidl){
					System.out.println("*** Checking Idl Types");
				}
				if (checkidl)
					cdbchecker.checkIdlTypes();

				String paths[]=cdbchecker.XSDPath.split(""+File.pathSeparatorChar);
				Vector XSDFilenames=new Vector();
				XSDFilenames=cdbchecker.getFilenames(paths,"xsd");
			
				String pathsMulti[]=cdbchecker.XMLPath.split(":");
				for(int i = 0; i < pathsMulti.length; i++){
					File file_ = new File(pathsMulti[i]);
					if(!file_.exists()){
						System.out.println("*** Specified path " + file_+ " does not exist");
						globalErrorFlag = true;
						cdbchecker.showEndResult();
						break;
					}
				}
				
				
				if(cdbchecker.verbose)System.out.println("*** Reading given XML files");			
				// We assume that cdbchecker.XMLPath is at least
				// initialised to the empty string and never null
		
				paths=cdbchecker.XMLPath.split(""+File.pathSeparatorChar);
				Vector XMLFilenames=new Vector();
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
					cdbchecker.componentsFolder= pathsMulti[i] + "/MACI/Components";
					cdbchecker.containersFolder= pathsMulti[i] + "/MACI/Containers";
				
					File compFolder = new File(cdbchecker.componentsFolder);
					File contFolder = new File(cdbchecker.containersFolder);
					
					if(compFolder.exists()){
						if(contFolder.exists()){
							//System.out.println("compFolder: " + compFolder);
							//System.out.println("contFolder: " + contFolder);
							globalErrorFlag = cdbchecker.checkImplLangMatch(compFolder, contFolder);
							
							if(cdbchecker.allImplLang){
								if(globalErrorFlag==true) {
								    System.out.println("\n[Error] Errors were found\n");
								}
								else {
								    System.out.println("\nNo errors found\n");
								}
								
							}
						}
						else{
							System.out.println("\nContainers folder not found for " + pathsMulti[i]);
						}
					}
					else{
						System.out.println("\nComponents folder not found for " + pathsMulti[i]);
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
		
		CDBChecker cdbchecker=new CDBChecker();
		
		File[] files = compFolder.listFiles();
	 
		search:
	  	for (int x = 0; x < files.length; x++){
	  		
	  		if(!cdbchecker.allImplLang){
	  			if(foundErr){
	  				break search;
	  			}
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
	    			
	    	  		if(compName.equals("*")){ //--> dynamic component
	    	  			if(implLang.equals("") || implLang.equals("*")){
				  			continue;
			  			}
	    	  			//some dynamic components may not have predefined Containers
	    	  			if(elm.getAttribute("Container" ).equals("") || elm.getAttribute("Container" ).equals("*")){
		    				continue;
		    			}
	    	  		}
	    			else{//actually, ImpLang field in the CDB is mandatory since ACS 8
	    				if(implLang.equals("")){
	    					System.out.println("\nFile being checked: " + files[x] );
				  			System.out.print("\n'ImplLang' missing for component: " + compName);
	    					foundErr = true;
				  			if(!cdbchecker.allImplLang){
	    						break search;
	    					}
	              		}
	    			}
	    			
		 			//go get containers at the "Container" location
		  			tempContainersFolder = containersFolder + "/" + elm.getAttribute("Container" );
	    			
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
		  				File contFile = new File(tempContainersFolder + "//" + new File(tempContainersFolder).getName()+ ".xml");
		  				if(contFile.exists()){
		  					//System.out.println("Container file being checked : " + contFile);
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
				  					System.out.println("\nFile being checked: " + files[x] );
				  					System.out.print("'ImplLang' does not match for component: " + compName +".");
				  					System.out.println("\nCorresponding Container is : " + contFile);
				  					foundErr = true;
						  			if(!cdbchecker.allImplLang){
			    						break search;
			    					}
				      			}
					    	}//Container for loop
		  				}
		  				else{
		  					System.out.print("\nFile being checked: " + files[x] );
		  					System.out.print("\nMissing Container " + new File(tempContainersFolder));
		  					System.out.println("");
		  					foundErr = true;
				  			if(!cdbchecker.allImplLang){
	    						break search;
	    					}
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

}//class
