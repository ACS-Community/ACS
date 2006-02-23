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
	}
}

class ConfigurationCH extends DefaultHandler implements ContentHandler{
	public void startElement(String namespace, String localname, String type, Attributes attributes ){ 
		if(localname=="SchemaLocation"){
			(CDBChecker.reqSchemas).add(attributes.getValue("Url"));
		}
	}
}

