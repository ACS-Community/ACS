package alma.xercesj.test;

import java.io.FileReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;


public class DumpXML {

	/**
	 * @param args
	 */
	
	public static void main(String[] args) throws Throwable {
	
		if (args.length != 1)
		{
			System.err.println("Usage: java " + DumpXML.class.getName() + " <xml file>");
			System.exit(1);
		}
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		factory.setXIncludeAware(true);

		DocumentBuilder builder = factory.newDocumentBuilder();
		Document xmldoc = builder.parse(new InputSource(new FileReader(args[0])));
		
		Source src = new DOMSource(xmldoc);
		Result dest = new StreamResult(System.out);

		TransformerFactory tranFactory = TransformerFactory.newInstance();
		Transformer transformer = tranFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.transform(src, dest);
	}

}
