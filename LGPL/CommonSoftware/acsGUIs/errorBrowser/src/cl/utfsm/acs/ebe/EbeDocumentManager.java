/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.ebe;

import java.io.File;
import java.util.ArrayList;
import java.util.TreeMap;

import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.xml.XmlSeeker;

/** Error Browser and Editor Document manager. This class manages
  * the XML files, and offers a EbeDocument abstraction. This class
  * manages those objects in a generic form. For this, the schema file
  * must be loaded, specific classes initializated, and create a document pool
  * @author Mauricio Araya (maray[at]inf.utfsm.cl)
  */
public class EbeDocumentManager {
        /** the list of current documents */
        TreeMap<String,EbeDocument> documents;
        /** The AcsError Schema class */
        ErrorSchema schema;
        /** Reads the schema files, setup classes and init the pool */
        public EbeDocumentManager(){
        	ErrorBrowserEditor.log("=== Welcome to Error Browser ===");
        	ErrorBrowserEditor.log("\tDeveloped by ACS-UTFSM Group\n");
        	ErrorBrowserEditor.log("[Initializating Document Manager]");
        	ErrorBrowserEditor.log("   * Reading Schema file");
        	schema=new ErrorSchema();
        	ErrorBrowserEditor.log("   * Configuring Classes");
        	Member.setClassType((AcsComplexType)schema.getType("Member_"));
        	Error.setClassType(schema.getErrorSchema());
        	Completion.setClassType(schema.getCompletionSchema());
        	EbeDocument.setClassType(schema.getTypeSchema());
        	ErrorBrowserEditor.log("   * Initializating Document pool");
        	documents=new TreeMap<String,EbeDocument>();
        	ErrorBrowserEditor.log("[Done]");
        }
        /** Create a new document (a new file). This class setup the classic 
          * document info.
          * @param path the path where the file should be
          * @param name the document internal name
          */
        public void newDocument(String path, String name){
        		
                EbeDocument myDoc=new EbeDocument();
                myDoc.setPath(path);                
		myDoc.setDocumentInfo(name);
                myDoc.setValue(name);
                documents.put(myDoc.getValue(),myDoc);
                ErrorBrowserEditor.log("[Created new document: "+name+"]");
        }
        /** Load the document information from the path, and creates a new doc.
          * @param path the filepath to load from
          */
        public void loadDocument(String path){
        		
                EbeDocument myDoc=new EbeDocument();
                myDoc.setPath(path);
                try {
                	myDoc.load();
                } catch (RuntimeException e) {
                	ErrorBrowserEditor.log("Failed to load document " + path);
                	throw e;
                }
                documents.put(myDoc.getValue(),myDoc);
                ErrorBrowserEditor.log("[Document "+path+" loaded]");
                ErrorBrowserEditor.log("   * "+myDoc.getNodes().size()+" definitions loaded");
        }
        /** Remove a document from the manager by name.
          * @param name the name to remove */
        public void removeDocument(String name){
                documents.remove(name);
            	ErrorBrowserEditor.log("[Document "+name+" removed]");
                
        }
        /** Remove all the documents from the manager */
        public void removeAll(){
                documents.clear();
                ErrorBrowserEditor.log("[Document list cleaned]");
        }
        /** Permanently delete from the HD the file
          * @param name the filename to remove */
        public void deleteDocument(String name){
                EbeDocument d=documents.get(name);
                File f=new File(d.getPath());
		if (f.delete()==true){
			documents.remove(name);
			ErrorBrowserEditor.log("[File "+name+" successfully deleted from the Hard Disk]");
		}
		else {
			ErrorBrowserEditor.log("[ATENTION: File "+name+" *cannot* be deleted from the Hard Disk, please check permissions]");
		}
        }
        /** Load a filelist to the manager.
          * @param lst The Arraylist to load
          */
        private void addFileList(ArrayList<File> lst){
                ErrorBrowserEditor.log("Adding Files...");
                for (File file: lst){
                        loadDocument(file.getAbsolutePath());
                }
                ErrorBrowserEditor.log("[Done]");
        }
        
        /** Add defaults directories, and load the files. */
	@SuppressWarnings("unchecked")
	public void addDefaults(){
     	  	ErrorBrowserEditor.log("[Adding default directories]");
                XmlSeeker seeker=new XmlSeeker();
                String modroot = System.getProperty("ACS.modroot");
                String introot = System.getProperty("ACS.introot");
                String acsroot = System.getProperty("ACS.acsroot");
                seeker.addDir(modroot + File.separator + "idl");
                seeker.addDir(introot + File.separator + "idl");
                seeker.addDir(acsroot + File.separator + "idl");
                addFileList(seeker.getXmls("Alma/ACSError ACSError.xsd"));
        }
        
        /** Add a specific directory to the manager 
          * @param path the directory Path.
          */
	@SuppressWarnings("unchecked")
	public void addDirectory(String path){
		ErrorBrowserEditor.log("[Adding "+path+" directory]");
                XmlSeeker seeker=new XmlSeeker();
                seeker.addDir(path);
                addFileList(seeker.getXmls("Alma/ACSError ACSError.xsd"));
        }
        /** Get the document TreeMap
          * @return the documents TreeMap.
          */
        public TreeMap<String,EbeDocument> getDocuments(){
                return documents;
        }
}
