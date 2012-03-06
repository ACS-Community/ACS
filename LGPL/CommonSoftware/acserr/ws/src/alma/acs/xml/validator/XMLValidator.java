package alma.acs.xml.validator;

import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ErrorHandler;

import java.io.FileReader;

public class XMLValidator {
	static boolean error;
	public class XMLErrorHandler implements ErrorHandler {
		String xml;
		public XMLErrorHandler(String xml) {
			this.xml = xml;
		}
		public void warning(SAXParseException ex) {
			XMLValidator.error = true;
			System.err.println(xml+": "+ex.getMessage());
		}
		public void error(SAXParseException ex) {
			XMLValidator.error = true;
			System.err.println(xml+": "+ex.getMessage());
		}
		public void fatalError(SAXParseException ex) throws SAXException {
			XMLValidator.error = true;
			throw ex;
		}
	}
	public void run(String[] args) {
		try {
			if(args.length != 2) {
				System.out.println("Incorrect arguments. You need to provide the XML and XSD files.");
				System.exit(3);
			}
			XMLValidator.error = false;
			// define the type of schema - we use W3C:
			String schemaLang = "http://www.w3.org/2001/XMLSchema";
			
			// get validation driver:
			SchemaFactory factory = SchemaFactory.newInstance(schemaLang);
			
			// create schema by reading it from an XSD file:
			Schema schema = factory.newSchema(new StreamSource(args[1]));
			Validator validator = schema.newValidator();
			ErrorHandler eh = new XMLErrorHandler(args[0]);
			validator.setErrorHandler(eh);
			
			// at last perform validation:
			XMLInputFactory xFact = XMLInputFactory.newInstance();
			XMLStreamReader xRead = xFact.createXMLStreamReader(new FileReader(args[0]));
			if(xRead.getVersion() == null) {
				System.err.println(args[0]+": There is no XML Definition.");
				XMLValidator.error = true;
			} else if(xRead.getCharacterEncodingScheme() == null) {
				System.err.println(args[0]+": The encoding attribute is not defined in the XML Definition.");
				XMLValidator.error = true;
			} else if(xRead.getCharacterEncodingScheme().compareTo("ISO-8859-1") != 0) {
				System.err.println(args[0]+": Incorrect encoding type in the XML Definition.");
				XMLValidator.error = true;
			}
				
			validator.validate(new StreamSource(args[0]));
		}catch (SAXException ex) {
			System.err.println("Fatal Error");
			System.err.println(args[0]+": "+ex.getMessage());
			XMLValidator.error = true;
		} catch (Exception ex) {
			ex.printStackTrace();
			XMLValidator.error = true;
		}
			if(XMLValidator.error) {
				//System.exit(1); //Error
				System.exit(0); //Warning
			} else {
				System.exit(0);
			}
	}
	public static void main(String[] args) {
		XMLValidator xml = new XMLValidator();
		xml.run(args);
	}
}
