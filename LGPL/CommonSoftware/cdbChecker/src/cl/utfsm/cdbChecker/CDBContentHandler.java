/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.DefaultHandler;

class CDBContentHandler extends DefaultHandler implements ContentHandler{
	public void startElement(String namespace, String localname, String type, Attributes attributes ){ 
		if(localname=="schema"){
			CDBChecker.setTargetNamespaceString(attributes.getValue("targetNamespace"));
		}

		if(CDBChecker.checkidl && ( (localname == "Component") || (localname == "_" && namespace == "urn:schemas-cosylab-com:Components:1.0") ) ){
			String idl = attributes.getValue("Type");
			if(CDBChecker.rep.lookup_id(idl) == null){
				System.out.println("[Error] IDL: "+idl+ " not found in Interface Repository.");
				CDBChecker.globalErrorFlag = true;
			}else if(CDBChecker.verbose) System.out.println("    "+idl+ " [OK]");
		}
	}
}

class ConfigurationCH extends DefaultHandler implements ContentHandler{
	public void startElement(String namespace, String localname, String type, Attributes attributes ){ 
		if(localname=="SchemaLocation"){
			(CDBChecker.reqSchemas).add(attributes.getValue("Url"));
		}
	}
}

