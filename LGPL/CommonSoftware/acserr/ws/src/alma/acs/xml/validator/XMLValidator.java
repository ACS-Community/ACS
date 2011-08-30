package alma.acs.xml.validator;
//import javax.xml.transform.Source;
//import javax.xml.transform.stream.StreamSource;
//import javax.xml.validation.Schema;
//import javax.xml.validation.SchemaFactory;
//import javax.xml.validation.Validator;
//
//import java.io.File;

import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ErrorHandler;

public class XMLValidator {
	static boolean error;
	public class XMLErrorHandler implements ErrorHandler {
		public void warning(SAXParseException ex) {
			XMLValidator.error = true;
			System.err.println(ex.getMessage());
		}
		public void error(SAXParseException ex) {
			XMLValidator.error = true;
			System.err.println(ex.getMessage());
		}
		public void fatalError(SAXParseException ex) throws SAXException {
			XMLValidator.error = true;
			throw ex;
		}
	}
	public void run(String[] args) {
		try {
			if(args.length != 2) {
				System.out.println("Incorrect arguments. You need to provide the XML and XSD files");
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
			ErrorHandler eh = new XMLErrorHandler();
			validator.setErrorHandler(eh);
			
			// at last perform validation:
			validator.validate(new StreamSource(args[0]));
		}catch (SAXException ex) {
			System.err.println("Fatal Error");
			System.err.println(ex.getMessage());
		} catch (Exception ex) {
			ex.printStackTrace();
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
