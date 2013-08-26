/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.xml;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import alma.acs.makesupport.AcsFileFinder;


/** This class is an ACS specific filter and seeker for XML files.
 * This class can seek for XML files over standard ACS directories,
 * new directories. Also, this seeker search if the XML implements
 * a specific XSD schema.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class XmlSeeker implements FilenameFilter {
        /** Seeker directories */
        private ArrayList<File> dirs;
        /** Simple contructor */
        public XmlSeeker(){
                super();
                dirs=new ArrayList<File>();
        }
         
        /**
         * Accepts only filenames finishing with ".xml"
         * 
         * @param dir
         *            the directory in which the file was found.
         * @param name
         *            the name of the file.
         * @return true if file ends with ".xml" ; false otherwise.
         */
        public boolean accept(File dir, String name) {
                return(name.endsWith(".xml"));
        }
        
        /**
         * Get an Arraylist with the XMLs on the added dirs that conforms with the xsd filename.
         * @param xsd the filename of the xsd to seek inside the XML files
         * @return the XMLs list */
        public ArrayList getXmls(String xsd) {
                File fileArr[];
                ArrayList<File> files=new ArrayList<File>();
                File dirArr[]=new File[dirs.size()];
                dirs.toArray(dirArr);
                AcsFileFinder fileFinder = new AcsFileFinder(dirArr,this,null);
                fileArr = fileFinder.getAllFiles();
                for (int i = 0; i < fileArr.length; i++) {
                        DOMParser dp;
                        dp = new DOMParser();
                        try{
                                dp.parse(fileArr[i].getAbsolutePath());
                        }
                        catch(Exception e){
                                e.printStackTrace();  
                        }
                        Document doc = dp.getDocument();
                        // Hack to support both Errors and Logs 
                        // TODO: Please do this thing in a generic way
                        Node typeNode = doc.getElementsByTagName("Type").item(0);
                        
                        /*
                        if (typeNode == null) 
                                typeNode = doc.getElementsByTagName("LogDefinitionType").item(0);
                        */
                        // Type node may not even exist
                        if (typeNode != null) {
                                NamedNodeMap atributes = typeNode.getAttributes();
                                Node schema = atributes.getNamedItem("xsi:schemaLocation");
                                if (schema.getNodeValue().contentEquals(xsd))
                                        files.add(fileArr[i]);
                        }
                }
                return(files);
        }
        /** Add a new directory for searching for XMLs.
         * @param path the directory absolute path
         */
        public void addDir(String path){
                dirs.add(new File(path));
        }
        
        /** Clean all the dirs with no exception */
        public void clearDirs()
        {
                dirs.clear();
        }
        /** remove a directory from the list */
        public void removeDir(String path)
        {
                for (File dir:dirs)
                {
                        if (path.compareTo(dir.getAbsolutePath())==0)
                        {
                                dirs.remove(dir);
				return;
                        }
                }
        }
}
