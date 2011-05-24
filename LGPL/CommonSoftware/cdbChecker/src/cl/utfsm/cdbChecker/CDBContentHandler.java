/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.DefaultHandler;

class CDBContentHandler extends DefaultHandler implements ContentHandler{

	private CDBChecker _checker;

	public CDBContentHandler(CDBChecker checker) {
		_checker = checker;
	}

	public void startElement(String namespace, String localname, String type, Attributes attributes ){ 
		if(localname=="schema"){
			_checker.setTargetNamespaceString(attributes.getValue("targetNamespace"));
		}

		if( _checker.isCheckIdl() && ( (localname == "Component") || (localname == "_" && namespace == "urn:schemas-cosylab-com:Components:1.0") ) ){
			String idl = attributes.getValue("Type");
			if(_checker.getIrRep().lookup_id(idl) == null){
				System.out.println("[Error] IDL: "+idl+ " not found in Interface Repository.");
				_checker.setGlobalErrorFlag(true);
			}
			else if( _checker.isVerbose() )
				System.out.println("    "+idl+ " [OK]");
		}
	}
}

class ConfigurationCH extends DefaultHandler implements ContentHandler{

	private List<String> _reqSchemas;

	public ConfigurationCH(List<String> reqSchemas) {
		_reqSchemas = reqSchemas;
	}

	public void startElement(String namespace, String localname, String type, Attributes attributes ){ 
		if(localname=="SchemaLocation"){
			(_reqSchemas).add(attributes.getValue("Url"));
		}
	}
}

