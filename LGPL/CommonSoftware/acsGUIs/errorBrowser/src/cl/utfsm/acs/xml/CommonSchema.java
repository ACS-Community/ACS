/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.xml;

import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.*;
import java.util.ArrayList;
import cl.utfsm.acs.types.*;

/** The common schema reads the COMMON_TYPES schema file, and setup general types definitions.
 * The main purpose of this class is to have the information of the schema files centralized in
 * a schema class. This object automatically parse the schema file and save all the simple and
 * complex types into an internal Arraylist.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class CommonSchema{
        /** The common types schema filename */
        static protected String COMMON_TYPES="commontypes.xsd";
        /** The local DOM parser reference */
	protected DOMParser parser;
        /** The DOM Document object. */
	protected Document commonTypes;
        /** The list of types on the schema */
        protected ArrayList<AcsType> typeList;
        
        /** Parse the schema file and initialize the class. Also adds basic types 
         * from the classic schema definition.
         * <p>
         * TODO: add all the classic schema definitions */
        public CommonSchema(){
                parser = new DOMParser();
                String acsroot = System.getProperty("ACS.acsroot");
                // Parse the schema file. This is far away from optimal, but its done only once
                if (acsroot != null) {
                        try {
                                parser.parse(acsroot + "/idl/" + COMMON_TYPES);
                                commonTypes=parser.getDocument();
                        } catch (Exception e) {
                                e.getMessage(); // Improve this catch
                        }
                } else {
                        throw new NullPointerException(
                                        "Property 'ACS.acsroot' must be defined!");
                }
                searchSimpleTypes(commonTypes,"common");
                //Add some types from xs:
                typeList.add(new AcsSimpleType("xs","string","A string",null));
                typeList.add(new AcsSimpleType("xs","boolean","A boolean",null));
                searchComplexTypes(commonTypes,"common");
        }
        
        /** Search for SimpleTypes in the parsed document and add them to the Arraylist.
         * @param doc The DOM document to search
         * @param namespace The namespace that those types should have
         */
        public void searchSimpleTypes(Document doc,String namespace){
                if (typeList==null)
                        typeList=new ArrayList<AcsType>();
                //Simple Types
                String documentation="";
                NodeList sTypes=doc.getElementsByTagName("xs:simpleType");
                for (int i=0;i<sTypes.getLength();i++){
                        Node item=sTypes.item(i);
                        String name=item.getAttributes().getNamedItem("name").getNodeValue();
                        NodeList lvl1=item.getChildNodes();
                        // Get the documentation and the "restriction"
                        for (int j=0;j<lvl1.getLength();j++){
                                Node one=lvl1.item(j);
                                String oneName=one.getLocalName();
                                if (oneName != null && oneName.compareTo("annotation")==0){
                                        documentation=readDocumentation(one);
                                }
                                //TODO: Add Restriction value
                        }
                        AcsSimpleType aType=new AcsSimpleType(namespace,name,documentation,null);
                        typeList.add(aType);
                }

        }
        /** Read a DOM attribute node and return a new AcsAttribute object 
         * @param node the DOM attribute node
         * @return A new AcsAttribute object from the node*/
        public AcsAttribute readAttribute(Node one){
                String atrUse="";
                String atrName=one.getAttributes().getNamedItem("name").getNodeValue();
                String atrType=one.getAttributes().getNamedItem("type").getNodeValue();
                try{
                        atrUse=one.getAttributes().getNamedItem("use").getNodeValue();
                } catch (java.lang.NullPointerException e){
                        atrUse="";
                }
                AcsSimpleType simple;
                try{
                	simple = (AcsSimpleType)getType(atrType.split(":")[1]);
                }catch(IndexOutOfBoundsException ex){
                	simple = (AcsSimpleType)getType(atrType);
                }
                AcsAttribute atr = new 
                        AcsAttribute(atrName,simple,atrUse);
                return(atr);
        }
        
        /** Read a DOM annotation node with documentation, and return the string documentation.
         * @param anno the DOM node
         * @return The String with the documentation */
        public String readDocumentation(Node anno){
                String documentation="";
                NodeList lvl=anno.getChildNodes();
                for (int k=0;k<lvl.getLength();k++){
                        Node two=lvl.item(k);
                        String twoName=two.getLocalName();
                        if (twoName == null) continue;
                        if (twoName.compareTo("documentation")==0){
                                documentation=two.getFirstChild().getNodeValue();
                        }
                }
                return(documentation);
        }
        
        /** Search for ComplexTypes in the parsed document and add them to the Arraylist.
         * This class is unstable, and untested. I asure that works with the commontypes file only.
         * @param doc The DOM document to search
         * @param namespace The namespace that those types should have
         **/
        public void searchComplexTypes(Document doc,String namespace){
                if (typeList==null)
                        typeList=new ArrayList<AcsType>();
                //Complex Types
                NodeList cTypes=doc.getElementsByTagName("xs:complexType");
                for (int i=0;i<cTypes.getLength();i++){
                        Node item=cTypes.item(i);
                        String name=item.getAttributes().getNamedItem("name").getNodeValue();
                        NodeList lvl1=item.getChildNodes();
                        String documentation="";
                        // Get the documentation 
                        for (int j=0;j<lvl1.getLength();j++){
                                Node one=lvl1.item(j);
                                String oneName=one.getLocalName();
                                if (oneName != null && oneName.compareTo("annotation")==0)
                                        documentation=readDocumentation(one);
                        }
                        AcsComplexType aType=new AcsComplexType(namespace,name,documentation);
                        for (int j=0;j<lvl1.getLength();j++){
                                Node one=lvl1.item(j);
                                String oneName=one.getLocalName();
                                if (oneName != null && oneName.compareTo("attribute")==0){
                                        aType.addAttr(readAttribute(one));
                                }
                        }
                        typeList.add(aType);
                }

        }
        
        /** Get a type (simple or complex) by name.
         * @param name the name of the type
         * @return the type, or null if not found.*/
        public AcsType getType(String name){
                for (AcsType type : typeList){
                        if (type.name.compareTo(name)==0)
                                return type;
                }
                return null;
        }
}
