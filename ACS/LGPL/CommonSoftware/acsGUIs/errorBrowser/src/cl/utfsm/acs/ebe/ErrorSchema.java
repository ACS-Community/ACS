/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.ebe;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import cl.utfsm.acs.types.AcsAttribute;
import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.AcsSimpleType;
import cl.utfsm.acs.xml.CommonSchema;

/** This schema definition reads the ERROR_XSD schema file.
 * Also reads the common types from his parent class. 
 * There is three definitions of the type, completions and error.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class ErrorSchema extends CommonSchema{ 
        /** The ACSERROR XSD files */
        static protected String ERROR_XSD="ACSError.xsd";
        /** The DOM error document */
        protected Document errorDoc;
        /** The type schema reference */
        AcsComplexType typeSchema;
        /** The completion schema reference */
        AcsComplexType completionSchema;
        /** The error schema reference */
        AcsComplexType errorSchema;
        /** Constructor. Fill the schema */
        public ErrorSchema(){
                super();
                String acsroot = System.getProperty("ACS.acsroot");
                if (acsroot != null) {
                        try {
                                parser.parse(acsroot + "/idl/" + ERROR_XSD);
                                errorDoc=parser.getDocument();
                        } catch (Exception e) {
                                e.getMessage(); // Improve this catch
                        }
                } else {
                        throw new NullPointerException(
                                        "Property 'ACS.acsroot' must be defined!");
                }
                searchSimpleTypes(errorDoc,"acserr");
                fillSchema();
        }
        /** Get the type shema. 
         * @return type type :P*/
        public AcsComplexType getTypeSchema(){
                return(typeSchema);
        }
        /** Get the completion shema. 
         * @return completion type*/
        public AcsComplexType getCompletionSchema(){
                return(completionSchema);
        }
        /** Get the error shema. 
         * @return error type*/
        public AcsComplexType getErrorSchema(){
                return(errorSchema);
        }
        /** Read the schema information and fill the schema */
        public void fillSchema(){
                String documentation="";
                String name="";
                String namespace="acserr";
                Node no=errorDoc.getElementsByTagName("xs:complexType").item(0);
                Node localNode = no.getFirstChild();
                // Dont know why does not work, now, bypassing
                //name=localNode.getAttributes().getNamedItem("name").getNodeValue();
                name="Type";
                //Get the Documentation
                while (localNode != null) {
                        String nodeName = localNode.getLocalName();
                        if (nodeName != null && nodeName.equals("annotation")){
                                readDocumentation(localNode);
                        }
                        localNode = localNode.getNextSibling();
                }
                typeSchema=new AcsComplexType(namespace,name,documentation);
                localNode = no.getFirstChild();
                while (localNode != null) {
                        String nodeName = localNode.getLocalName();
                        if (nodeName != null && nodeName.equals("attribute")){
                                typeSchema.addAttr(readAttribute(localNode));
                        }
                        localNode = localNode.getNextSibling();
                }
                typeSchema.addAttr(new AcsAttribute("xmlns",(AcsSimpleType)getType("string"),"required"));
                typeSchema.addAttr(new AcsAttribute("xmlns:xsi",(AcsSimpleType)getType("string"),"required"));
		typeSchema.addAttr(new AcsAttribute("xsi:schemaLocation",(AcsSimpleType)getType("string"),"required"));

		// Removed on ACS 7.0 
                //typeSchema.addAttr(new AcsAttribute("xsi:noNamespaceSchemaLocation",(AcsSimpleType)getType("string"),"required"));

                /* Search for Completions and Error definitions */
                /* Spagetti code... improve please */
                localNode = no.getFirstChild();
                while (localNode != null){
                        String nodeName = localNode.getLocalName();
                        if (nodeName != null && nodeName.equals("choice")) break;
                        localNode = localNode.getNextSibling();
                }
                localNode = localNode.getFirstChild();
                // COMPLETIONS
                while (localNode != null){
                        String nodeName = localNode.getLocalName();
                        if (nodeName != null && nodeName.equals("element")) break;
                        localNode = localNode.getNextSibling();
                }
                Node elementNode = localNode.getFirstChild();
                // Dont know why does not work, now, bypassing
                //name=elementNode.getAttributes().getNamedItem("name").getNodeValue();
                name="Code";
                while (elementNode != null){
                        String nodeName = elementNode.getLocalName();
                        if (nodeName != null && nodeName.equals("annotation")){
                                documentation=readDocumentation(elementNode);          
                                break;
                        }
                        elementNode = elementNode.getNextSibling();
                }
                completionSchema=new AcsComplexType(namespace,name,documentation);
                elementNode = localNode.getFirstChild();
                while (elementNode != null){
                        String nodeName = elementNode.getLocalName();
                        if (nodeName != null && nodeName.equals("complexType")) break;
                        elementNode = elementNode.getNextSibling();
                }
                Node inNode=elementNode.getFirstChild();
                while (inNode != null){
                        String nodeName = inNode.getLocalName();
                        if (nodeName != null && nodeName.equals("attribute")){
                                completionSchema.addAttr(readAttribute(inNode));
                        }
                        inNode = inNode.getNextSibling();
                }
                // ERROR
                localNode = localNode.getNextSibling();
                while (localNode != null){
                        String nodeName = localNode.getLocalName();
                        if (nodeName != null && nodeName.equals("element")) break;
                        localNode = localNode.getNextSibling();
                }
                elementNode = localNode.getFirstChild();
                // Dont know why does not work, now, bypassing
                // name=elementNode.getAttributes().getNamedItem("name").getNodeValue();
                name="ErrorCode";
                while (elementNode != null){
                        String nodeName = elementNode.getLocalName();
                        if (nodeName != null && nodeName.equals("annotation")){
                                documentation=readDocumentation(elementNode);          
                                break;
                        }
                        elementNode = elementNode.getNextSibling();
                }
                errorSchema=new AcsComplexType(namespace,name,documentation);
                elementNode = localNode.getFirstChild();
                while (elementNode != null){
                        String nodeName = elementNode.getLocalName();
                        if (nodeName != null && nodeName.equals("complexType")) break;
                        elementNode = elementNode.getNextSibling();
                }
                inNode=elementNode.getFirstChild();
                while (inNode != null){
                        String nodeName = inNode.getLocalName();
                        if (nodeName != null && nodeName.equals("attribute")){
                                errorSchema.addAttr(readAttribute(inNode));
                        }
                        inNode = inNode.getNextSibling();
                }
                // END SPagetti
        }
}


