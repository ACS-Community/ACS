/**
 * 
 */
package alma.acs.releasedoc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.ParseException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * @author jschwarz
 *
 */
public class ProcessCvs2clOutput {
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ProcessCvs2clOutput pc = new ProcessCvs2clOutput();
		Set<Cvs2clXmlEntry> entries1 = new HashSet<Cvs2clXmlEntry>();
		
		Cvs2clXmlParser parser = new Cvs2clXmlParser();
		
		File xmlFile1 = new File(args[0]);
		if (!xmlFile1.exists()) System.err.println("File "+xmlFile1.getAbsolutePath()+" does not exist.");


		
		try {
			Document doc = parser.parseXml(xmlFile1);
			storeEntries(doc, entries1);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XPathExpressionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		TWikiFormatter formatter = new TWikiFormatter();
		List<Cvs2clXmlEntry> sortedEntries = formatter.sortByDate(entries1);
		formatter.printTwiki(sortedEntries);
	}
	
	private static void storeEntries(Document doc, Set<Cvs2clXmlEntry> entries) throws XPathExpressionException, ParseException {
		XPath xpath = XPathFactory.newInstance().newXPath();
		NodeList entryNodes = (NodeList) xpath.evaluate("//entry", doc, XPathConstants.NODESET);
		System.out.println("Number of entries: " + entryNodes.getLength());
		for (int i = 0; i < entryNodes.getLength(); i++) {
			Element entryElem = (Element) entryNodes.item(i);
			Cvs2clXmlEntry entry = new Cvs2clXmlEntry(entryElem);
			boolean wasNewEntry = entries.add(entry);
			if (!wasNewEntry) {
				System.out.println("**** Multiple entry " + entry.getDate() + " author = " + entry.getAuthor());
			}
		}
	}

}
