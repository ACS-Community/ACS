/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.ebe;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.TreeMap;

import org.apache.xerces.parsers.DOMParser;
import org.apache.xml.serialize.Method;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.apache.xerces.dom.DocumentImpl;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;

import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.ComplexObject;

/** Error Browser and Editor Document object. This object is the representation
 * of a document (and a file). This is the start point for any client.
 * Each document is a ComplexObject, as all its contents.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class EbeDocument extends ComplexObject {
        /** The common DOM parser */
        static private DOMParser parser;
        /** The class type */
        protected static AcsComplexType typeType;
        /** The nodes (completions and errors) TreeMap */
        protected TreeMap<String,ComplexObject> nodes;
        /** The file path */
        private String path;
        /** The DOM document */
        private Document doc;
        /** The class type setter */
        public static void setClassType(AcsComplexType t){
                typeType=t;
        }
        /** The class type getter */
        public static AcsComplexType getClassType(){
                return(typeType);
        }
        /** Initialize a new EbeDocument */
        public EbeDocument(){
                super(typeType);
                nodes=new TreeMap<String,ComplexObject>();
        }
        /** Set the Path of the Document
         * @param path the absolute path to the file */
        public void setPath(String path){
                this.path=path;
        }
        /** Get the current filename Path
         * @return the path*/
        public String getPath(){
                return(path);
        }
        /** Read the node into a ComplexObject
         * @param no the DOM node to read 
         * @param obj the Complex Object to fill
         */
        private void readNode(Node no,ComplexObject obj){
                if (no==null) return;
                NamedNodeMap attrs = no.getAttributes();
                for (int i=0;i<attrs.getLength();i++){
                        String name=attrs.item(i).getNodeName();
                        String val=attrs.item(i).getNodeValue();
                        if (name.compareTo("name")==0)
                                obj.setValue(val);
                        obj.setAttributeValue(name,val);
                }
        }
        
        /** Load the data from the selected path */
        public void load(){
                if(parser==null){
                        parser = new DOMParser();
                }
                try {
                        parser.parse(path);
                        doc = parser.getDocument();
                } catch (Exception e) {
                        e.getMessage();
                }
                nodes.clear();
                // Only one Type
                Node no=doc.getElementsByTagName(EbeDocument.getClassType().name).item(0);
                readNode(no,this);
                // Check for Completions
                NodeList completionNodes = doc.getElementsByTagName(Completion.getClassType().name);
                for (int i = 0; i < completionNodes.getLength(); i++){
                        Node coNo=completionNodes.item(i);
                        Completion coObj = new Completion();
                        readNode(coNo,coObj);
                        this.putNode(coObj);
                }
                NodeList errorNodes = doc.getElementsByTagName(Error.getClassType().name);
                for (int i = 0; i < errorNodes.getLength(); i++){
                        Node erNo=errorNodes.item(i);
                        Error erObj = new Error();
                        readNode(erNo,erObj);
                        Node meNo = erNo.getFirstChild();
                        while (meNo != null) {
                                if (meNo.getNodeName().matches("Member")){
                                        Member meObj=new Member();
                                        readNode(meNo,meObj);
                                        erObj.putMember(meObj);
                                }
                                meNo = meNo.getNextSibling();
                        }
                        this.putNode(erObj);
                }
                
        }
        /** Fill a DOM Element from a ComplexObject (for save) 
         * @param obj the ComplexObject with attributes
         * @param toFill the DOM element to fill*/
        private void fillAttributes(ComplexObject obj,Element toFill){
        	 for (String attr: obj.getAttributes().keySet()){
               	toFill.setAttribute(attr,obj.getAttributeValue(attr));
        	 }
        }
        
        /** Save the data to the selected path 
         * @throws IOException 
         * @throws FileNotFoundException */
        public void save() throws FileNotFoundException, IOException{
            Document docFile=new DocumentImpl();
            Element typeElement=docFile.createElement(EbeDocument.getClassType().name);
            fillAttributes(this,typeElement);
            for (ComplexObject node: nodes.values()){
            	Element nodeElement;
            	if (node instanceof Error)
            		nodeElement=docFile.createElement("ErrorCode");
            	else 
            		nodeElement=docFile.createElement("Code");
                fillAttributes(node,nodeElement);
                if (node instanceof Error){
                	Error err=(Error)node;
                	for (ComplexObject memb: err.getMembers().values()){
                		Element membElement=docFile.createElement("Member");
                		fillAttributes(memb,membElement);
                		nodeElement.appendChild(membElement);
                	}     	
                }
                typeElement.appendChild(nodeElement);
            }
            docFile.appendChild(typeElement);
            saveXmlDocument(docFile,getPath());
        }
        
        /** Save the DOM document into the name path 
         * @param docu The DOM document
         * @param name the filename path*/
        private void saveXmlDocument(Document docu,String name) throws FileNotFoundException,
        	java.io.IOException{
        	OutputFormat outFormat = new OutputFormat(Method.XML,null,true);
        	outFormat.setEncoding("UTF-8");
        	outFormat.setVersion("1.0");
         FileOutputStream out = new FileOutputStream(name);
        	XMLSerializer xmlSerializer=new XMLSerializer(
        			new PrintWriter(out), outFormat);
        	xmlSerializer.serialize(docu);
         out.close();
        }
        
        /** Add a node (completion/error) to the document 
         * @param obj the Object to add*/
        public void putNode(ComplexObject obj){
            nodes.put(obj.getValue(),obj);
        }
        /** Get the nodes (completion/error) TreeMap
         * @return the nodes TreeMap
         */
        public TreeMap<String,ComplexObject> getNodes(){
                return(nodes);
        }
        /** Get a node by name 
         * @param name a name to get the node
         * @return a ComplexNode  */
        public ComplexObject getNode(String name){
                return(nodes.get(name));
        }
        /** Set the basic document info
         * @param _prefix prefix (like alma)
         * @param xsi xsi info
         * @param xsiLocation the xsi location
         * @param name the document name
         */
        public void setDocumentInfo(String _prefix,String xsi,String xsiNamespace, String xsiLocation, String name){
                setAttributeValue("xsi:schemaLocation",xsiNamespace + " " + xsiLocation);
                setAttributeValue("xmlns",xsiNamespace);
                setAttributeValue("xmlns:xsi",xsi);
                setAttributeValue("_prefix",_prefix);
                setAttributeValue("name",name);
        }
        /** Set the basic document info defaults
         * @param name the document name
         */
        public void setDocumentInfo(String name){
                setDocumentInfo("alma","http://www.w3.org/2001/XMLSchema-instance","Alma/ACSError","ACSError.xsd",name);
        }
}

